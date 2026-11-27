!< BARUCH, the block parser's state: the current line and the position in it, the open blocks, the tip's lines.
module baruch_blocks_state
!< BARUCH, the block parser's state: the current line and the position in it, the open blocks, the tip's lines.
!<
!< `block_parser` is what one parse (`baruch_blocks`) works on, and only it: nothing here is module state, so parses run
!< side by side. The source is copied once; a line is the slice `src(lf:ll)` and the position in it is an **offset**
!< (the first byte not consumed, 1-based in the line) and a **column** (0-based, tab stops of 4, CommonMark 2.2). A
!< container's marker is consumed by moving them, never by cutting the line. When a marker ends inside a tab (a list
!< item's content indented by a tab, `>` followed by a tab), the tab is **partly consumed**: the offset stays on it, the
!< column moves, and a line added to a block from there starts with the tab's remaining columns as spaces (`pad`).
!<
!< The open blocks are a stack of arrays, the root at depth 1 and the tip (the deepest open block) on top: node, owner
!< (0 the core, else the options' slot of the extension that opened it), content (`CONTENT_*` or `CONTENT_PARAGRAPH`),
!< first and last byte of the span so far, and two integers the core keeps per block (`a`: an item's content column,
!< a fence's indentation, an HTML block's type; `b`: the line an item opened on). Only the tip takes lines, so one set
!< of arrays holds the tip's content lines: first and last byte, and the pad of each.
!<
!< `flags` holds, per node, what deciding tight and loose lists needs (CommonMark 5.3): the last line of the block was
!< blank, that was already checked, a list's last line was blank (the guard against quadratic blank lines in deep
!< lists, as cmark).
use penf,               only : I4P
use baruch_blocks_scan, only : is_space_or_tab
implicit none
private
public :: block_parser
public :: CONTENT_PARAGRAPH, FLAG_LAST_LINE_BLANK, FLAG_LAST_LINE_CHECKED, FLAG_LIST_LAST_LINE_BLANK
public :: TAB, LF, TAB_STOP, CODE_INDENT
public :: grow

integer(I4P), parameter :: STACK_DEFAULT = 32_I4P  !< Open blocks and tip lines allocated up front (both grow).
integer(I4P), parameter :: CONTENT_PARAGRAPH = -1_I4P !< Content of a paragraph: lines, but blocks may start in it.
integer(I4P), parameter :: TAB_STOP = 4_I4P         !< Tab stop (CommonMark 2.2).
integer(I4P), parameter :: CODE_INDENT = 4_I4P      !< Indentation of an indented code block (CommonMark 4.4).
integer(I4P), parameter :: FLAG_LAST_LINE_BLANK      = 1_I4P !< The block's last line was blank.
integer(I4P), parameter :: FLAG_LAST_LINE_CHECKED    = 2_I4P !< `ends_with_blank_line` already answered for it.
integer(I4P), parameter :: FLAG_LIST_LAST_LINE_BLANK = 4_I4P !< A list: its last line was blank.
character,    parameter :: TAB = achar(9)  !< Tab.
character,    parameter :: LF  = achar(10) !< Line feed.

type :: block_parser
   !< The state of one parse.
   character(:), allocatable :: src                   !< The document's source, copied once.
   integer(I4P)              :: ln = 0_I4P            !< Current line number.
   integer(I4P)              :: lf = 0_I4P            !< Its first byte in `src`.
   integer(I4P)              :: ll = 0_I4P            !< Its last byte (ending excluded).
   logical                   :: line_blank = .true.   !< The whole line is spaces and tabs.
   integer(I4P)              :: offset = 1_I4P        !< First byte of the line not consumed (1-based in the line).
   integer(I4P)              :: column = 0_I4P        !< Column of `offset`.
   logical                   :: partial_tab = .false. !< The tab at `offset` is partly consumed.
   integer(I4P)              :: nonspace = 0_I4P      !< First byte at or after `offset` not a space or tab.
   integer(I4P)              :: nonspace_column = 0_I4P !< Its column.
   integer(I4P)              :: indent = 0_I4P        !< Columns from `offset` to `nonspace`.
   logical                   :: blank = .true.        !< Nothing but spaces and tabs from `offset`.
   integer(I4P)              :: break_fail = 0_I4P    !< Where a thematic break scan failed on this line (0 none).
   logical                   :: tombstones = .false.  !< The parse unlinked nodes: the tree is compacted at the end.
   integer(I4P)              :: depth = 0_I4P         !< Open blocks, the root included.
   integer(I4P), allocatable :: open_node(:)          !< Node of each open block, root first.
   integer(I4P), allocatable :: open_owner(:)         !< Its owner: 0 the core, else the slot of its extension.
   integer(I4P), allocatable :: open_content(:)       !< Its content: `CONTENT_*` or `CONTENT_PARAGRAPH`.
   integer(I4P), allocatable :: open_first(:)         !< First byte of its span.
   integer(I4P), allocatable :: open_last(:)          !< Last byte of its span so far.
   integer(I4P), allocatable :: open_a(:)             !< Item: content column; code: fence indentation; HTML: type.
   integer(I4P), allocatable :: open_b(:)             !< Item: the line it opened on.
   integer(I4P)              :: tip_lines = 0_I4P     !< Content lines of the tip.
   integer(I4P), allocatable :: tip_first(:)          !< First byte of each.
   integer(I4P), allocatable :: tip_last(:)           !< Last byte of each.
   integer(I4P), allocatable :: tip_pad(:)            !< Spaces before each (a partly consumed tab).
   integer(I4P), allocatable :: flags(:)              !< `FLAG_*` bits per node.
   integer(I4P), allocatable :: active(:)             !< Slots of the enabled extensions, in order.
   contains
      procedure, pass(self) :: start_line      !< Make a line current, its position at its start.
      procedure, pass(self) :: char_at         !< Return the byte at a position of the line, LF beyond its end.
      procedure, pass(self) :: find_nonspace   !< Find the first byte at or after the offset not a space or tab.
      procedure, pass(self) :: advance_bytes   !< Consume bytes of the line.
      procedure, pass(self) :: advance_columns !< Consume columns of the line (a tab partly, possibly).
      procedure, pass(self) :: remaining       !< Return the bytes of the line not consumed.
      procedure, pass(self) :: push            !< Push an open block.
      procedure, pass(self) :: add_line        !< Give the rest of the line, from the offset, to the tip.
      procedure, pass(self) :: drop_lines      !< Drop the tip's first lines.
      procedure, pass(self) :: flag            !< Return a flag of a node.
      procedure, pass(self) :: set_flag        !< Set or clear a flag of a node.
endtype block_parser

contains
   subroutine start_line(self, ln, lf, ll)
   !< Make line `ln`, bytes `lf` to `ll` of the source, current: nothing consumed.
   class(block_parser), intent(inout) :: self !< Parser.
   integer(I4P),        intent(in)    :: ln   !< Line number.
   integer(I4P),        intent(in)    :: lf   !< First byte.
   integer(I4P),        intent(in)    :: ll   !< Last byte (ending excluded).
   integer(I4P)                       :: k    !< Byte.

   self%ln = ln
   self%lf = lf
   self%ll = ll
   self%offset = 1_I4P
   self%column = 0_I4P
   self%partial_tab = .false.
   self%nonspace = 0_I4P
   self%nonspace_column = 0_I4P
   self%break_fail = 0_I4P
   self%line_blank = .true.
   do k=lf, ll
      if (.not. is_space_or_tab(self%src(k:k))) then
         self%line_blank = .false.
         exit
      endif
   enddo
   endsubroutine start_line

   pure function char_at(self, i) result(c)
   !< Return the byte at position `i` of the line; LF beyond its end (or before its start).
   class(block_parser), intent(in) :: self !< Parser.
   integer(I4P),        intent(in) :: i    !< Position, 1-based in the line.
   character                       :: c    !< Byte.

   c = LF
   if (i >= 1_I4P .and. self%lf + i - 1_I4P <= self%ll) c = self%src(self%lf+i-1:self%lf+i-1)
   endfunction char_at

   subroutine find_nonspace(self)
   !< Find the first byte at or after the offset that is not a space or a tab, its column, the indentation in columns
   !< from the offset, and whether the rest of the line is blank. A position found before is kept while the offset has
   !< not passed it.
   class(block_parser), intent(inout) :: self !< Parser.
   integer(I4P)                       :: i    !< Position.
   integer(I4P)                       :: col  !< Column.
   character                          :: c    !< Byte.

   if (self%nonspace <= self%offset) then
      i = self%offset
      col = self%column
      do
         c = self%char_at(i)
         if (c == ' ') then
            col = col + 1_I4P
         elseif (c == TAB) then
            col = col + TAB_STOP - mod(col, TAB_STOP)
         else
            exit
         endif
         i = i + 1_I4P
      enddo
      self%nonspace = i
      self%nonspace_column = col
   endif
   self%indent = self%nonspace_column - self%column
   self%blank = self%lf + self%nonspace - 1_I4P > self%ll
   endsubroutine find_nonspace

   subroutine advance_bytes(self, count)
   !< Consume `count` bytes of the line: a tab to the next stop, a UTF-8 continuation byte no column, any other byte
   !< one.
   class(block_parser), intent(inout) :: self  !< Parser.
   integer(I4P), value, intent(in)    :: count !< Bytes (a copy: callers pass expressions of the parser's state).
   integer(I4P)                       :: k     !< Counter.
   integer                            :: c     !< Byte value.

   do k=1, count
      if (self%lf + self%offset - 1_I4P > self%ll) exit
      c = iachar(self%src(self%lf+self%offset-1:self%lf+self%offset-1))
      if (c == 9) then
         self%column = self%column + TAB_STOP - mod(self%column, TAB_STOP)
      elseif (c < 128 .or. c >= 192) then
         self%column = self%column + 1_I4P
      endif
      self%offset = self%offset + 1_I4P
   enddo
   self%partial_tab = .false.
   endsubroutine advance_bytes

   subroutine advance_columns(self, count)
   !< Consume `count` columns of the line: a tab wider than what is left to consume is consumed partly (the offset
   !< stays on it, `partial_tab` is set).
   class(block_parser), intent(inout) :: self    !< Parser.
   integer(I4P), value, intent(in)    :: count   !< Columns (a copy: callers pass `p%open_a(d)`).
   integer(I4P)                       :: left    !< Columns left to consume.
   integer(I4P)                       :: to_stop !< Columns to the next tab stop.

   left = count
   do while (left > 0_I4P .and. self%lf + self%offset - 1_I4P <= self%ll)
      if (self%src(self%lf+self%offset-1:self%lf+self%offset-1) == TAB) then
         to_stop = TAB_STOP - mod(self%column, TAB_STOP)
         self%partial_tab = to_stop > left
         self%column = self%column + min(left, to_stop)
         if (.not. self%partial_tab) self%offset = self%offset + 1_I4P
         left = left - min(left, to_stop)
      else
         self%partial_tab = .false.
         self%offset = self%offset + 1_I4P
         self%column = self%column + 1_I4P
         left = left - 1_I4P
      endif
   enddo
   endsubroutine advance_columns

   pure function remaining(self) result(bytes)
   !< Return the bytes of the line not consumed.
   class(block_parser), intent(in) :: self  !< Parser.
   integer(I4P)                    :: bytes !< Bytes.

   bytes = max(0_I4P, self%ll - (self%lf + self%offset - 1_I4P) + 1_I4P)
   endfunction remaining

   subroutine push(self, node, owner, content, first, a, b)
   !< Push an open block on the stack; a block taking lines starts with none.
   class(block_parser), intent(inout) :: self    !< Parser.
   integer(I4P), value, intent(in)    :: node    !< Node.
   integer(I4P), value, intent(in)    :: owner   !< Owner.
   integer(I4P), value, intent(in)    :: content !< Content.
   integer(I4P), value, intent(in)    :: first   !< First byte of its span.
   integer(I4P), value, intent(in)    :: a       !< Core integer `a`.
   integer(I4P), value, intent(in)    :: b       !< Core integer `b`.

   if (self%depth == size(self%open_node, kind=I4P)) then
      call grow(self%open_node)
      call grow(self%open_owner)
      call grow(self%open_content)
      call grow(self%open_first)
      call grow(self%open_last)
      call grow(self%open_a)
      call grow(self%open_b)
   endif
   self%depth = self%depth + 1_I4P
   self%open_node(self%depth) = node
   self%open_owner(self%depth) = owner
   self%open_content(self%depth) = content
   self%open_first(self%depth) = first
   self%open_last(self%depth) = self%ll
   self%open_a(self%depth) = a
   self%open_b(self%depth) = b
   endsubroutine push

   subroutine add_line(self)
   !< Give the rest of the line, from the offset, to the tip: a partly consumed tab becomes the spaces it has left.
   class(block_parser), intent(inout) :: self !< Parser.
   integer(I4P)                       :: pad  !< Spaces before the line.
   integer(I4P)                       :: from !< Its first byte.

   if (self%tip_lines == size(self%tip_first, kind=I4P)) then
      call grow(self%tip_first)
      call grow(self%tip_last)
      call grow(self%tip_pad)
   endif
   pad = 0_I4P
   from = self%lf + self%offset - 1_I4P
   if (self%partial_tab) then
      pad = TAB_STOP - mod(self%column, TAB_STOP)
      from = from + 1_I4P
   endif
   self%tip_lines = self%tip_lines + 1_I4P
   self%tip_first(self%tip_lines) = from
   self%tip_last(self%tip_lines) = self%ll
   self%tip_pad(self%tip_lines) = pad
   endsubroutine add_line

   subroutine drop_lines(self, lines)
   !< Drop the tip's first `lines` lines.
   class(block_parser), intent(inout) :: self  !< Parser.
   integer(I4P),        intent(in)    :: lines !< Lines to drop.
   integer(I4P)                       :: k     !< Counter.

   if (lines <= 0_I4P) return
   do k=1, self%tip_lines - lines
      self%tip_first(k) = self%tip_first(k + lines)
      self%tip_last(k) = self%tip_last(k + lines)
      self%tip_pad(k) = self%tip_pad(k + lines)
   enddo
   self%tip_lines = max(0_I4P, self%tip_lines - lines)
   endsubroutine drop_lines

   pure function flag(self, node, bit) result(is)
   !< Return a flag of a node (false for a node never flagged).
   class(block_parser), intent(in) :: self !< Parser.
   integer(I4P),        intent(in) :: node !< Node.
   integer(I4P),        intent(in) :: bit  !< `FLAG_*`.
   logical                         :: is   !< Result.

   is = .false.
   if (node >= 1_I4P .and. node <= size(self%flags, kind=I4P)) is = iand(self%flags(node), bit) /= 0_I4P
   endfunction flag

   subroutine set_flag(self, node, bit, on)
   !< Set (or clear) a flag of a node.
   class(block_parser), intent(inout) :: self  !< Parser.
   integer(I4P), value, intent(in)    :: node  !< Node (a copy: callers pass `p%open_node(d)`, the flags may grow).
   integer(I4P), value, intent(in)    :: bit   !< `FLAG_*`.
   logical,      value, intent(in)    :: on    !< Set it, or clear it.
   integer(I4P), allocatable          :: grown(:) !< Grown flags.

   if (node < 1_I4P) return
   if (node > size(self%flags, kind=I4P)) then
      if (.not. on) return
      allocate(grown(max(2_I4P * size(self%flags, kind=I4P), node)))
      grown = 0_I4P
      grown(1:size(self%flags)) = self%flags
      call move_alloc(grown, self%flags)
   endif
   if (on) then
      self%flags(node) = ior(self%flags(node), bit)
   else
      self%flags(node) = iand(self%flags(node), not(bit))
   endif
   endsubroutine set_flag

   pure subroutine grow(a)
   !< Double an integer array, keeping its values.
   integer(I4P), allocatable, intent(inout) :: a(:) !< Array.
   integer(I4P), allocatable                :: b(:) !< Grown copy.

   allocate(b(2_I4P * max(1_I4P, size(a, kind=I4P))))
   b = 0_I4P
   b(1:size(a)) = a
   call move_alloc(b, a)
   endsubroutine grow
endmodule baruch_blocks_state
