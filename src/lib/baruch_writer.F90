!< BARUCH, the tree writer: a document's tree as Markdown, in the style the parser recorded or in a canonical one.
module baruch_writer
!< BARUCH, the tree writer: a document's tree as Markdown, in the style the parser recorded or in a canonical one.
!<
!< The writer is the inverse of the parser (settled design 2 of the plan): what it writes, the parser reads back to the
!< same tree (`markdown_document%equal`), not to the same bytes. It reuses the **style slots** the parser recorded
!< (`baruch_kinds_nodes`: emphasis character, bullet, ordered delimiter, ATX or setext and the closing `#`s, fence
!< character and length, thematic break character, link form and label, hard break form) and falls back to the
!< **canonical style** where a slot holds nothing, or everywhere when asked (`canonical`): ATX headings (setext only
!< for a heading of level 1 or 2 spanning lines), `-` bullets, `1.` numbering, backtick fences, `***`, `*` and `**`,
!< inline links (autolinks when the link is one), backslash hard breaks.
!<
!< **Escaping** is minimal and decided by the parser's rules (settled design 3): a character is escaped only where
!< reading it back would change the tree, and how is decided by its context:
!<
!< - at the start of a line of a paragraph or of a setext heading, what would open a block there (`#`, `>`, a bullet or
!<   `1.`/`1)` and a space, a fence, `=` or `-` under a paragraph, `<` and an HTML block start, a thematic break): the
!<   block scanners of `baruch_blocks_scan` are asked; leading white space (it would be stripped, or indent a code
!<   block) and trailing white space (stripped, or a hard break) are written as character references;
!< - inline: `\` before ASCII punctuation or a line end, `*` and `_` runs that could open or close emphasis (the
!<   flanking rules, the characters around them as they are written), backticks when another backtick follows in the
!<   paragraph, `[` and `]` that could make a link (a pair followed by `(`, `[` or `:`, or whose content is a defined
!<   label; a lone one inside a link's text, or after a lone `[`), `!` before a link, `&` before a character reference,
!<   `<` before anything but white space, the trigger and delimiter characters of the enabled extensions;
!< - a code span takes the shortest backtick string its content does not hold, padded with a space when the content
!<   starts or ends with a backtick or with spaces at both ends; a fence is longer than any fence-like line inside;
!< - destinations go between `<` and `>` when they hold white space, titles between `"` (or `'`), each with their own
!<   escapes; raw HTML and titles spanning lines indent a continuation line that would open a block by 4 spaces (a
!<   paragraph's continuation lines lose their indentation).
!<
!< **Containers** are line prefixes, kept on a stack: `> ` for a block quote, the item's marker on its first line and
!< as many spaces after it; a blank line between blocks (none between the items and the blocks of a tight list, unless
!< the second block could not follow the first without merging). The walk is iterative, over the tree's links: no
!< recursion, any depth. Link reference definitions are written in place; a reference link keeps its form when the
!< definition it would find is the same, else it is written inline.
!<
!< **Extensions** write their nodes through their mandatory `write_markdown` hook, which the writer offers every node
!< on entering and on leaving it (core kinds too, in registration order; the first that handles the event writes it).
!< For a block, the hook's text is written as lines behind the containers' prefixes; on entering a block that has
!< children, the text after its last line feed is the prefix of every line of the children (`!> ` for a container
!< like a block quote, empty for a fenced one like `::: tip`). For an inline node it is written as it is, before and
!< after the children. A node of an extension kind that no enabled extension writes is written transparently: its
!< children as if it were not there, a leaf's literal as text.
use penf,                only : I4P
use baruch_document,     only : markdown_document
use baruch_extension,    only : extension_slot
use baruch_kinds_nodes,  only : NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK, NODE_HTML_BLOCK, &
                                NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION,      &
                                NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH,      &
                                NODE_STRONG, NODE_LINK, NODE_IMAGE,                                                     &
                                SLOT_HEADING_FORM, SLOT_HEADING_CLOSING, SLOT_LIST_MARKER, SLOT_CODE_FORM,              &
                                SLOT_CODE_FENCE_CHAR, SLOT_CODE_FENCE_LENGTH, SLOT_BREAK_CHAR, SLOT_EMPHASIS_CHAR,      &
                                SLOT_LINK_FORM, SLOT_LINEBREAK_FORM, SLOT_LINEBREAK_SPACES, SLOT_CODE_TICKS, STRING_RAW, &
                                NODE_EXTENSION_FIRST, HEADING_SETEXT, LIST_ORDERED, CODE_INDENTED, LINK_FULL,           &
                                LINK_COLLAPSED, LINK_SHORTCUT, LINK_AUTOLINK, LINEBREAK_SPACES
use baruch_blocks_scan,  only : BACKSLASH, is_space_or_tab, is_ascii_punctuation, scan_thematic_break, scan_atx_start, &
                                scan_open_fence, scan_close_fence, scan_setext_underline, scan_list_marker,             &
                                scan_html_block_start
use baruch_inlines_scan, only : is_ascii_alnum, is_ascii_space, classify, scan_reference, decode_references, &
                                scan_autolink_uri, scan_autolink_email, scan_html_tag, code_content, unescape_string
use baruch_references,   only : reference_map
use baruch_blocks_definitions, only : scan_destination, scan_title, parse_definition, LABEL_MAX
use baruch_status
implicit none
private
public :: write_markdown_document
public :: escape_markdown

character, parameter :: LF  = achar(10) !< Line feed.
character, parameter :: CR  = achar(13) !< Carriage return.
character, parameter :: TAB = achar(9)  !< Tab.

! what follows a text run, for its escaping
integer(I4P), parameter :: NEXT_END   = 0_I4P !< The end of the block's inline content.
integer(I4P), parameter :: NEXT_LINE  = 1_I4P !< A line ending (a soft or hard line break).
integer(I4P), parameter :: NEXT_LINK  = 2_I4P !< A link (its `[`).
integer(I4P), parameter :: NEXT_OPEN  = 3_I4P !< The opening delimiter of an emphasis.
integer(I4P), parameter :: NEXT_CLOSE = 4_I4P !< The closing delimiter of the emphasis the run ends.
integer(I4P), parameter :: NEXT_OTHER = 5_I4P !< Anything else.
! how the inline content of a block is written
integer(I4P), parameter :: INLINE_LINES = 1_I4P !< Lines of a paragraph or of a setext heading: line starts escaped.
integer(I4P), parameter :: INLINE_ATX   = 2_I4P !< One line after `# `: no line start.
integer(I4P), parameter :: INLINE_PLAIN = 3_I4P !< Behind an extension's prefix: no line start checked.

type :: md_buffer
   !< Output buffer, grown by doubling.
   character(:), allocatable :: chars        !< Bytes, `chars(1:used)` written.
   integer(I4P)              :: used = 0_I4P !< Bytes written.
   contains
      procedure, pass(self) :: put  !< Append bytes.
      procedure, pass(self) :: last_char !< Return the last UTF-8 character written, empty if none.
endtype md_buffer

type :: md_writer
   !< The state of one write: the output, the container prefixes, the choices made per node.
   logical                   :: canonical = .false.    !< Ignore the recorded style.
   type(md_buffer)           :: out                    !< Output.
   type(reference_map)       :: refs                   !< The document's definitions, by label.
   character(:), allocatable :: ext_chars              !< Trigger and delimiter characters of the enabled extensions.
   ! container prefixes: frame k writes first(k) on the next line once, rest(k) on the others
   character(:), allocatable :: first_buf              !< First-line prefixes, concatenated.
   character(:), allocatable :: rest_buf               !< Other-line prefixes, concatenated.
   integer(I4P), allocatable :: first_lo(:), first_hi(:) !< Bounds of each frame's first-line prefix.
   integer(I4P), allocatable :: rest_lo(:), rest_hi(:)   !< Bounds of each frame's other-line prefix.
   integer(I4P), allocatable :: bullet(:)              !< The bullet of an item frame (`iachar`), 0 otherwise.
   logical,      allocatable :: pending(:)             !< The frame's first line is still to write.
   integer(I4P)              :: depth = 0_I4P          !< Frames.
   integer(I4P)              :: pendings = 0_I4P       !< Frames whose first line is pending.
   ! per node
   integer(I4P), allocatable :: mark(:)                !< List: marker; emphasis: delimiter; link: 1 if an autolink.
   integer(I4P), allocatable :: number(:)              !< List: next item number.
   integer(I4P), allocatable :: by_ext(:)              !< Entered by an extension: 1, and a frame pushed: 2.
   ! between blocks
   logical                   :: glued = .false.        !< The current block follows an open paragraph, no blank line.
endtype md_writer

type :: inline_state
   !< The state of the inline content of one block.
   type(md_buffer)           :: buf                    !< The content, lines joined by LF, no prefix.
   integer(I4P)              :: mode = INLINE_LINES    !< `INLINE_LINES`, `INLINE_ATX` or `INLINE_PLAIN`.
   logical                   :: line_start = .true.    !< Nothing written on the current line yet.
   logical                   :: first_line = .true.    !< The current line is the block's first.
   integer(I4P)              :: ticks = 0_I4P          !< Backticks still to be written in the block.
   integer(I4P), allocatable :: lone(:)                !< Buffer position of each active `[` written as text.
   integer(I4P)              :: lones = 0_I4P          !< Active `[` written as text, outside link texts.
   integer(I4P)              :: depth = 0_I4P          !< Link and image texts open.
   integer(I4P)              :: delim_end = -1_I4P     !< Buffer position of the last emphasis delimiter written.
   logical                   :: delim_open = .false.   !< That delimiter opened its emphasis.
   integer(I4P), allocatable :: link_start(:)          !< Buffer position of each open link text's `[`.
   integer(I4P)              :: openers(2) = 0_I4P     !< Runs of `*`, `_` written unescaped that can open.
   integer(I4P)              :: inside(2) = 0_I4P      !< Emphases delimited by `*`, `_` open around the position.
   integer(I4P), allocatable :: saved(:,:)             !< `openers` and `inside` outside each open link text.
   integer(I4P), allocatable :: emph(:,:)              !< Each open emphasis: its delimiter, `openers` at its start.
   integer(I4P)              :: emphs = 0_I4P          !< Open emphases.
   integer(I4P)              :: indent = 0_I4P         !< Spaces to write before the next line's first byte.
endtype inline_state

contains
   subroutine write_markdown_document(doc, slots, canonical, markdown, ierr, msg)
   !< Write a document's tree as Markdown with the enabled extensions of `slots`: the recorded style, or the canonical
   !< one. `ierr` (0 on success) and `msg` report an extension hook failing; `markdown` then holds what was written
   !< before it.
   type(markdown_document),   intent(in)    :: doc       !< Document.
   type(extension_slot),      intent(in)    :: slots(:)  !< Extensions of the options.
   logical,                   intent(in)    :: canonical !< Ignore the recorded style.
   character(:), allocatable, intent(out)   :: markdown  !< Markdown.
   integer(I4P),              intent(out)   :: ierr      !< Status.
   character(:), allocatable, intent(inout) :: msg       !< Message, set with a nonzero `ierr`.
   type(md_writer)                          :: w         !< Writer.
   integer(I4P)                             :: root      !< Root.
   integer(I4P)                             :: node      !< Current node.
   integer(I4P)                             :: last      !< Last node of a run of inlines at block level.
   integer(I4P)                             :: s         !< Counter.
   logical                                  :: descend   !< Visit the children.

   ierr = BARUCH_OK
   markdown = ''
   root = doc%root()
   if (root == 0_I4P) return
   w%canonical = canonical
   allocate(character(max(1024_I4P, 2_I4P * doc%source_length())) :: w%out%chars)
   allocate(character(256) :: w%first_buf, w%rest_buf)
   allocate(w%first_lo(16), w%first_hi(16), w%rest_lo(16), w%rest_hi(16), w%bullet(16), w%pending(16))
   allocate(w%mark(doc%last_node()), w%number(doc%last_node()), w%by_ext(doc%last_node()))
   w%mark = 0_I4P
   w%number = 0_I4P
   w%by_ext = 0_I4P
   call w%refs%build(doc)
   w%ext_chars = ''
   do s=1, size(slots, kind=I4P)
      if (.not. slots(s)%enabled) cycle
      w%ext_chars = w%ext_chars//slots(s)%ext%inline_triggers()//slots(s)%ext%delimiter_chars()
   enddo
   node = doc%first_child(root)
   do while (node /= 0_I4P)
      if (doc%kind_is_inline(doc%node_kind(node))) then ! inlines at block level (a tree built by hand): a paragraph
         last = node
         do while (doc%next_sibling(last) /= 0_I4P)
            if (.not. doc%kind_is_inline(doc%node_kind(doc%next_sibling(last)))) exit
            last = doc%next_sibling(last)
         enddo
         if (doc%prev_sibling(node) /= 0_I4P) call separate(w, doc, doc%prev_sibling(node), node)
         call write_inline_lines(w, doc, slots, doc%parent(node), node, last, INLINE_LINES, '', ierr, msg)
         node = last
         descend = .false.
      else
         call enter_block(w, doc, slots, node, descend, ierr, msg)
      endif
      if (ierr /= BARUCH_OK) exit
      if (descend .and. doc%first_child(node) /= 0_I4P) then
         node = doc%first_child(node)
         cycle
      endif
      do
         if (.not. doc%kind_is_inline(doc%node_kind(node))) call leave_block(w, doc, slots, node, ierr, msg)
         if (ierr /= BARUCH_OK) exit
         if (doc%next_sibling(node) /= 0_I4P) then
            node = doc%next_sibling(node)
            exit
         endif
         node = doc%parent(node)
         if (node == root .or. node == 0_I4P) then
            node = 0_I4P
            exit
         endif
      enddo
      if (ierr /= BARUCH_OK) exit
   enddo
   if (ierr == BARUCH_OK .and. .not. canonical) call source_end(w, doc)
   markdown = w%out%chars(1:w%out%used)
   endsubroutine write_markdown_document

   subroutine source_end(w, doc)
   !< End the output as the source ends (recorded style): the blank lines after its last block, or no line ending after
   !< it.
   type(md_writer),         intent(inout) :: w    !< Writer.
   type(markdown_document), intent(in)    :: doc  !< Document.
   character(:), allocatable              :: tail !< The source after the last block.
   integer(I4P)                           :: last !< Last block of the document.
   integer(I4P)                           :: ends !< Line endings in the tail.
   integer(I4P)                           :: k    !< Position.

   last = doc%last_child(doc%root())
   if (last == 0_I4P .or. w%out%used == 0_I4P) return
   if (doc%span_last(last) < 1_I4P .or. doc%span_last(last) > doc%source_length()) return
   tail = doc%source_slice(doc%span_last(last) + 1_I4P, doc%source_length())
   if (verify(tail, ' '//TAB//LF//CR) > 0) return ! the last block's span stops before its end (a fence left open)
   ends = 0_I4P
   do k=1, len(tail, kind=I4P)
      if (tail(k:k) == LF) then
         ends = ends + 1_I4P
      elseif (tail(k:k) == CR) then
         ends = ends + 1_I4P
         if (k < len(tail, kind=I4P)) then
            if (tail(k+1:k+1) == LF) ends = ends - 1_I4P
         endif
      endif
   enddo
   if (ends == 0_I4P) then
      if (w%out%chars(w%out%used:w%out%used) == LF) w%out%used = w%out%used - 1_I4P
   else
      do k=2, ends
         call w%out%put(LF)
      enddo
   endif
   endsubroutine source_end

   function escape_markdown(text, line_start, first_line) result(escaped)
   !< Return a text escaped so that, written inside a paragraph, it reads back as that text, whatever is written
   !< around it: as the writer escapes a text node, its neighbours unknown (the most cautious reading of each rule). With
   !< `line_start` (default false) the text starts a line, the first of a paragraph with `first_line` (default true):
   !< what would open a block there is escaped too. For the streaming writer and for clients composing Markdown.
   character(*), intent(in)           :: text       !< Text.
   logical,      intent(in), optional :: line_start !< The text starts a line.
   logical,      intent(in), optional :: first_line !< That line is a paragraph's first.
   character(:), allocatable          :: escaped    !< Escaped text.
   type(md_writer)                    :: w          !< A writer with nothing defined.
   type(inline_state)                 :: s          !< Inline state.

   allocate(character(max(64, 2 * len(text))) :: s%buf%chars)
   w%ext_chars = ''
   s%mode = INLINE_LINES
   s%line_start = .false.
   if (present(line_start)) s%line_start = line_start
   s%first_line = .true.
   if (present(first_line)) s%first_line = first_line
   s%ticks = 1000000000_I4P     ! every backtick escaped
   s%depth = 1_I4P               ! every lone bracket escaped
   s%inside = 1_I4P              ! every run that can open or close emphasis escaped
   allocate(s%lone(1), s%emph(3, 1))
   s%emphs = 1_I4P
   s%emph(:, 1) = [0_I4P, 0_I4P, 0_I4P]
   call escape_text(w, s, text, NEXT_OTHER, 'a', .false.)
   escaped = s%buf%chars(1:s%buf%used)
   endfunction escape_markdown

   ! blocks
   subroutine enter_block(w, doc, slots, node, descend, ierr, msg)
   !< Write what comes before a block's children (all of a leaf block); `descend` tells whether its children are
   !< blocks still to visit.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Block.
   logical,                   intent(out)   :: descend  !< Visit its children.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   character(:), allocatable                :: marker   !< An item's marker.
   integer(I4P)                             :: list     !< An item's list.
   integer(I4P)                             :: indent   !< Spaces before the block's first line.
   logical                                  :: handled  !< An extension wrote the event.

   descend = .false.
   w%glued = .false.
   if (doc%prev_sibling(node) /= 0_I4P) call separate(w, doc, doc%prev_sibling(node), node)
   call offer(doc, slots, node, .true., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (handled) then
      w%by_ext(node) = 1_I4P
      call block_piece(w, doc, slots, node, piece, descend, ierr, msg)
      return
   endif
   indent = source_indent(w, doc, node)
   select case(doc%node_kind(node))
   case(NODE_DOCUMENT)
      descend = .true.
   case(NODE_BLOCK_QUOTE)
      call push(w, repeat(' ', indent)//'> ', '> ', 0_I4P)
      descend = .true.
   case(NODE_LIST)
      call choose_list_marker(w, doc, node)
      descend = .true.
   case(NODE_ITEM)
      list = doc%parent(node)
      if (doc%list_type(list) == LIST_ORDERED) then
         marker = istr(w%number(list))//char1(w%mark(list))
         w%number(list) = w%number(list) + 1_I4P
      else
         marker = char1(w%mark(list))
      endif
      marker = repeat(' ', indent)//marker//repeat(' ', item_padding(w, doc, node, len(marker, kind=I4P)))
      call push(w, marker, repeat(' ', len(marker)), merge(0_I4P, w%mark(list), doc%list_type(list) == LIST_ORDERED))
      descend = .true.
   case(NODE_PARAGRAPH)
      call push(w, repeat(' ', indent), '', 0_I4P)
      call write_inline_lines(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_LINES, '', &
                              ierr, msg)
      call pop(w)
   case(NODE_HEADING)
      call push(w, repeat(' ', indent), '', 0_I4P)
      call write_heading(w, doc, slots, node, ierr, msg)
      call pop(w)
   case(NODE_CODE_BLOCK)
      call write_code_block(w, doc, node)
   case(NODE_HTML_BLOCK)
      call emit_text(w, doc%literal(node), .false.)
   case(NODE_THEMATIC_BREAK)
      call emit_line(w, repeat(' ', indent)//break_line(w, doc, node))
   case(NODE_LINK_REFERENCE_DEFINITION)
      call emit_text(w, repeat(' ', indent)//'['//doc%label(node)//']: '//destination_text(doc%destination(node), &
                     .true.)//title_text(doc%title(node)), .true.)
   case default ! an extension kind no extension writes: transparent
      if (doc%first_child(node) == 0_I4P) then
         if (len(doc%literal(node)) > 0) call emit_text(w, escape_markdown(doc%literal(node), .true.), .true.)
      elseif (doc%kind_is_inline(doc%node_kind(doc%first_child(node)))) then
         call write_inline_lines(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_LINES, &
                                 '', ierr, msg)
      else
         descend = .true.
      endif
   endselect
   endsubroutine enter_block

   function line_indent(w, doc, block, node) result(indent)
   !< Return the spaces before an inline node that starts a line of a paragraph or heading of the document's own, in
   !< the source (recorded style: a continuation line's indentation, which the parser strips); 0 otherwise.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: block  !< The paragraph or heading.
   integer(I4P),            intent(in) :: node   !< The node after a line break.
   integer(I4P)                        :: indent !< Spaces.
   integer(I4P)                        :: k      !< Position.

   indent = 0_I4P
   if (w%canonical .or. node == 0_I4P) return
   if (doc%parent(block) /= doc%root()) return
   k = doc%span_first(node) - 1_I4P
   if (k < 1_I4P .or. k > doc%source_length()) return
   do while (k >= 1_I4P)
      if (doc%source_slice(k, k) /= ' ') exit
      k = k - 1_I4P
   enddo
   if (k < 1_I4P) return
   if (doc%source_slice(k, k) /= LF .and. doc%source_slice(k, k) /= CR) return
   indent = doc%span_first(node) - 1_I4P - k
   endfunction line_indent

   function source_indent(w, doc, node) result(indent)
   !< Return the spaces before a block's first line in the source (recorded style), 0 to 3, for a block of the
   !< document's own (a list's items for a list of the document's): a paragraph, a heading, a thematic break, a
   !< definition, a block quote, an item. 0 otherwise.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: node   !< Block.
   integer(I4P)                        :: indent !< Spaces.
   integer(I4P)                        :: up     !< The document, or the list of an item.
   integer(I4P)                        :: k      !< Position.

   indent = 0_I4P
   if (w%canonical) return
   select case(doc%node_kind(node))
   case(NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION, NODE_BLOCK_QUOTE)
      up = doc%parent(node)
   case(NODE_ITEM)
      up = doc%parent(doc%parent(node))
   case default
      return
   endselect
   if (up /= doc%root()) return
   k = doc%span_first(node) - 1_I4P
   if (k < 0_I4P .or. k > doc%source_length()) return
   do while (k >= 1_I4P)
      if (doc%source_slice(k, k) /= ' ') exit
      k = k - 1_I4P
   enddo
   if (k >= 1_I4P) then
      if (doc%source_slice(k, k) /= LF .and. doc%source_slice(k, k) /= CR) return
   endif
   indent = doc%span_first(node) - 1_I4P - k
   if (indent > 3_I4P) indent = 0_I4P
   endfunction source_indent

   function item_padding(w, doc, item, width) result(pad)
   !< Return the spaces between an item's marker (`width` bytes) and its content: as in the source (recorded style), 1
   !< to 4 on the marker's line before a first block that is not an indented code block; else 1.
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: item  !< Item.
   integer(I4P),            intent(in) :: width !< Bytes of its marker.
   integer(I4P)                        :: pad   !< Spaces.
   character(:), allocatable           :: gap   !< The source between marker and content.
   integer(I4P)                        :: child !< First block.
   integer(I4P)                        :: a, b  !< Bounds.

   pad = 1_I4P
   if (w%canonical) return
   child = doc%first_child(item)
   if (child == 0_I4P) return
   if (doc%node_kind(child) == NODE_CODE_BLOCK .or. doc%node_kind(child) == NODE_HTML_BLOCK) return
   a = doc%span_first(item) + width
   b = doc%span_first(child) - 1_I4P
   if (a < 2_I4P .or. b < a .or. b - a + 1_I4P > 4_I4P .or. b > doc%source_length()) return
   gap = doc%source_slice(a, b)
   if (verify(gap, ' ') == 0) pad = len(gap, kind=I4P)
   endfunction item_padding

   subroutine leave_block(w, doc, slots, node, ierr, msg)
   !< Write what comes after a block's children: close its container prefix (an empty block quote or item writes its
   !< marker alone), then what an extension writes on leaving it.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Block.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   logical                                  :: handled  !< An extension wrote the event.

   if (w%by_ext(node) == 0_I4P) then
      select case(doc%node_kind(node))
      case(NODE_BLOCK_QUOTE, NODE_ITEM)
         if (w%pending(w%depth)) call emit_line(w, '')
         call pop(w)
      endselect
   elseif (w%by_ext(node) == 2_I4P) then
      call pop(w)
   endif
   call offer(doc, slots, node, .false., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (handled .and. len(piece) > 0) call emit_text(w, piece, .false.)
   endsubroutine leave_block

   subroutine block_piece(w, doc, slots, node, piece, descend, ierr, msg)
   !< Write an extension's Markdown for entering a block: its lines; after the last line feed, the prefix of the lines
   !< of the block's children (or, for a leaf, its last line).
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Block.
   character(*),              intent(in)    :: piece    !< The extension's Markdown.
   logical,                   intent(out)   :: descend  !< Visit its children.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: k        !< Last line feed of `piece`.

   descend = .false.
   if (doc%first_child(node) == 0_I4P) then
      if (len(piece) > 0) call emit_text(w, piece, .false.)
      return
   endif
   k = index(piece, LF, back=.true.)
   if (k > 0) call emit_text(w, piece(1:k), .false.)
   if (doc%kind_is_inline(doc%node_kind(doc%first_child(node)))) then
      call write_inline_lines(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_PLAIN, &
                              piece(k+1:), ierr, msg)
   else
      if (len(piece) > k) then
         call push(w, piece(k+1:), piece(k+1:), 0_I4P)
         w%by_ext(node) = 2_I4P
      endif
      descend = .true.
   endif
   endsubroutine block_piece

   subroutine separate(w, doc, prev, node)
   !< Write what separates a block from its previous sibling: a blank line, or nothing between the items and the
   !< blocks of a tight list (unless the block could not follow the previous one without merging with it) and between
   !< two link reference definitions.
   type(md_writer),         intent(inout) :: w     !< Writer.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: prev  !< Previous sibling.
   integer(I4P),            intent(in)    :: node  !< Block.
   integer(I4P)                           :: up    !< Parent.
   integer(I4P)                           :: lines !< Blank lines.
   logical                                :: blank !< A blank line.

   up = doc%parent(node)
   blank = .true.
   lines = 1_I4P
   if (doc%node_kind(up) == NODE_LIST) then
      blank = .not. doc%list_tight(up)
   elseif (doc%node_kind(up) == NODE_ITEM) then
      if (doc%list_tight(doc%parent(up))) blank = needs_blank(w, doc, prev, node)
   elseif (w%canonical .or. source_blank(doc, prev, node) < 0_I4P) then
      if (doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION .and. &
          doc%node_kind(node) == NODE_LINK_REFERENCE_DEFINITION) blank = .false.
   elseif (source_blank(doc, prev, node) == 0_I4P) then
      blank = needs_blank(w, doc, prev, node)
   else
      lines = source_blank(doc, prev, node)
   endif
   if (blank) then
      do while (lines > 0_I4P)
         call emit_line(w, '')
         lines = lines - 1_I4P
      enddo
   else
      w%glued = doc%node_kind(prev) == NODE_PARAGRAPH .or. doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION
   endif
   endsubroutine separate

   function source_blank(doc, prev, node) result(blank)
   !< Return the blank lines the source has between two sibling blocks, -1 if unknown (a block without a span, built or
   !< moved by hand).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: prev  !< Previous sibling.
   integer(I4P),            intent(in) :: node  !< Block.
   integer(I4P)                        :: blank !< Result.
   character(:), allocatable           :: gap   !< The source between them.
   integer(I4P)                        :: a, b  !< Their bounds.
   integer(I4P)                        :: k     !< Position.
   integer(I4P)                        :: ends  !< Line endings in the gap.

   blank = -1_I4P
   a = doc%span_last(prev)
   b = doc%span_first(node)
   if (a < 1_I4P .or. b <= a .or. b > doc%source_length() + 1_I4P) return
   gap = doc%source_slice(a + 1_I4P, b - 1_I4P)
   ends = 0_I4P
   do k=1, len(gap, kind=I4P)
      if (gap(k:k) == LF) then
         ends = ends + 1_I4P
      elseif (gap(k:k) == CR) then
         ends = ends + 1_I4P
         if (k < len(gap, kind=I4P)) then
            if (gap(k+1:k+1) == LF) ends = ends - 1_I4P
         endif
      endif
   enddo
   blank = max(0_I4P, ends - 1_I4P)
   endfunction source_blank

   function needs_blank(w, doc, prev, node) result(blank)
   !< Return true if a block cannot follow its previous sibling directly (no blank line) and be read as a new block: an
   !< open paragraph before it that it cannot interrupt, an HTML block that only a blank line ends, a definition.
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: prev  !< Previous sibling.
   integer(I4P),            intent(in) :: node  !< Block.
   logical                             :: blank !< Result.
   integer(I4P)                        :: t     !< HTML block type.
   integer(I4P)                        :: item  !< First item of a list.

   blank = .false.
   select case(doc%node_kind(prev))
   case(NODE_HTML_BLOCK)
      t = html_type(doc%literal(prev))
      blank = t == 6_I4P .or. t == 7_I4P .or. t == 0_I4P
      return
   case(NODE_LINK_REFERENCE_DEFINITION) ! read as paragraph lines: a paragraph after it may not look like a title
      select case(doc%node_kind(node))
      case(NODE_LINK_REFERENCE_DEFINITION)
         return
      case(NODE_PARAGRAPH)
         blank = len(doc%title(prev)) == 0
         if (blank) blank = index('"''(', first_text_char(doc, node)) > 0
         return
      endselect
   case(NODE_BLOCK_QUOTE)
      blank = doc%node_kind(node) == NODE_BLOCK_QUOTE
      if (blank) return
   endselect
   if (.not. (ends_open_paragraph(doc, prev) .or. doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION)) return
   select case(doc%node_kind(node)) ! can it interrupt a paragraph?
   case(NODE_HEADING)
      blank = written_setext(w, doc, node)
   case(NODE_CODE_BLOCK)
      blank = written_indented(w, doc, node)
   case(NODE_HTML_BLOCK)
      t = html_type(doc%literal(node))
      blank = t == 7_I4P .or. t == 0_I4P
   case(NODE_THEMATIC_BREAK, NODE_BLOCK_QUOTE)
      blank = .false.
   case(NODE_LIST)
      item = doc%first_child(node)
      blank = item == 0_I4P
      if (.not. blank) blank = doc%first_child(item) == 0_I4P
      if (.not. blank .and. doc%list_type(node) == LIST_ORDERED) blank = doc%list_start(node) /= 1_I4P
   case default
      blank = .true.
   endselect
   endfunction needs_blank

   function first_text_char(doc, node) result(c)
   !< Return the first character of a block's first text, `a` if it does not start with a text.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Block.
   character                           :: c    !< Character.
   character(:), allocatable           :: t    !< Literal.

   c = 'a'
   if (doc%first_child(node) == 0_I4P) return
   if (doc%node_kind(doc%first_child(node)) /= NODE_TEXT) return
   t = doc%literal(doc%first_child(node))
   if (len(t) > 0) c = t(1:1)
   endfunction first_text_char

   function ends_open_paragraph(doc, node) result(open_)
   !< Return true if a block ends with a paragraph left open (itself, or the last block of its last items and quotes):
   !< a line after it would continue that paragraph lazily.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Block.
   logical                             :: open_ !< Result.
   integer(I4P)                        :: n     !< Current block.

   n = node
   do
      select case(doc%node_kind(n))
      case(NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM)
         if (doc%last_child(n) == 0_I4P) exit
         n = doc%last_child(n)
      case default
         exit
      endselect
   enddo
   open_ = doc%node_kind(n) == NODE_PARAGRAPH
   endfunction ends_open_paragraph

   subroutine choose_list_marker(w, doc, list)
   !< Choose a list's bullet or delimiter (recorded, else `-` and `.`) and its first number: a list right after another
   !< of the same type takes another marker, else the two would be one list.
   type(md_writer),         intent(inout) :: w      !< Writer.
   type(markdown_document), intent(in)    :: doc    !< Document.
   integer(I4P),            intent(in)    :: list   !< List.
   character(*), parameter                :: BULLETS = '-*+' !< Bullets, canonical first.
   integer(I4P)                           :: prev   !< Previous sibling.
   integer(I4P)                           :: marker !< Marker.
   integer(I4P)                           :: k      !< Counter.

   marker = 0_I4P
   if (.not. w%canonical) marker = doc%int_slot(list, SLOT_LIST_MARKER)
   prev = doc%prev_sibling(list)
   if (prev /= 0_I4P) then
      if (doc%node_kind(prev) /= NODE_LIST) then
         prev = 0_I4P
      elseif (doc%list_type(prev) /= doc%list_type(list)) then
         prev = 0_I4P
      endif
   endif
   if (doc%list_type(list) == LIST_ORDERED) then
      if (marker /= iachar('.') .and. marker /= iachar(')')) marker = iachar('.')
      if (prev /= 0_I4P) then
         if (w%mark(prev) == marker) marker = merge(iachar(')'), iachar('.'), marker == iachar('.'))
      endif
      w%number(list) = doc%list_start(list)
   else
      if (index(BULLETS, char1(max(0_I4P, min(127_I4P, marker)))) == 0 .or. marker == 0_I4P) marker = iachar('-')
      if (prev /= 0_I4P) then
         if (w%mark(prev) == marker) then
            do k=1, len(BULLETS)
               if (iachar(BULLETS(k:k)) /= marker) exit
            enddo
            marker = iachar(BULLETS(k:k))
         endif
      endif
   endif
   w%mark(list) = marker
   endsubroutine choose_list_marker

   function break_char(w, doc, node) result(c)
   !< Return a thematic break's character: recorded (else `*`), unless it would make the line something else: `-` under
   !< an open paragraph (a setext underline), the bullet of an item whose marker is on the same line (one break).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Thematic break.
   character                           :: c     !< Character.
   character(4)                        :: order !< Candidates.
   integer(I4P)                        :: k, f  !< Counters.
   logical                             :: ok    !< A candidate is safe.

   order = '*-_*'
   if (.not. w%canonical) then
      k = doc%int_slot(node, SLOT_BREAK_CHAR)
      if (k == iachar('-') .or. k == iachar('_') .or. k == iachar('*')) order(1:1) = achar(k)
   endif
   do k=1, 4
      c = order(k:k)
      ok = .not. (c == '-' .and. w%glued)
      do f=1, w%depth
         if (w%pending(f) .and. w%bullet(f) == iachar(c)) ok = .false.
      enddo
      if (ok) return
   enddo
   c = order(1:1)
   endfunction break_char

   function break_line(w, doc, node) result(line)
   !< Return a thematic break's line: its line in the source (recorded style) when it is one of the character chosen,
   !< else three of the character.
   type(md_writer),         intent(in) :: w    !< Writer.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Thematic break.
   character(:), allocatable           :: line !< Line.
   character                           :: c    !< Character.
   integer(I4P)                        :: code !< Character of the source's line.
   integer(I4P)                        :: fail !< Where its scan failed.

   c = break_char(w, doc, node)
   line = repeat(c, 3)
   if (w%canonical) return
   line = source_line(doc, doc%span_first(node), doc%span_last(node))
   call scan_thematic_break(line, 1_I4P, code, fail)
   if (code /= iachar(c, kind=I4P)) line = repeat(c, 3)
   endfunction break_line

   function source_line(doc, first, last) result(line)
   !< Return the last line of a span of the source without its leading spaces, empty for no span.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: first !< First byte.
   integer(I4P),            intent(in) :: last  !< Last byte.
   character(:), allocatable           :: line  !< Line.
   integer(I4P)                        :: k     !< Position.

   line = ''
   if (first < 1_I4P .or. last < first .or. last > doc%source_length()) return
   line = doc%source_slice(first, last)
   k = max(index(line, LF, back=.true.), index(line, CR, back=.true.))
   line = line(k+1:)
   k = verify(line, ' ')
   if (k == 0) then
      line = ''
   else
      line = line(k:)
   endif
   endfunction source_line

   subroutine write_heading(w, doc, slots, node, ierr, msg)
   !< Write a heading, ATX or setext.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Heading.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: text     !< Its content.
   character(:), allocatable                :: line     !< The ATX line.
   integer(I4P)                             :: level    !< Level.
   integer(I4P)                             :: closing  !< Closing `#`s.
   integer(I4P)                             :: k, j     !< Positions.
   integer(I4P)                             :: chars    !< Characters of the last line.

   level = doc%heading_level(node)
   if (written_setext(w, doc, node)) then
      call inline_content(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_LINES, text, &
                          ierr, msg)
      if (ierr /= BARUCH_OK) return
      call emit_text(w, text, .false.)
      k = index(text, LF, back=.true.)
      chars = 0_I4P
      do j=k+1, len(text, kind=I4P)
         if (iand(iachar(text(j:j)), 192) /= 128) chars = chars + 1_I4P
      enddo
      line = repeat(merge('=', '-', level == 1_I4P), max(1_I4P, chars))
      if (.not. w%canonical) then ! the underline of the source
         text = source_line(doc, doc%span_first(node), doc%span_last(node))
         if (scan_setext_underline(text, 1_I4P) == level) line = text
      endif
      call emit_line(w, line)
      return
   endif
   call inline_content(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_ATX, text, ierr, msg)
   if (ierr /= BARUCH_OK) return
   closing = 0_I4P
   if (.not. w%canonical) closing = max(0_I4P, doc%int_slot(node, SLOT_HEADING_CLOSING))
   line = repeat('#', max(1_I4P, min(6_I4P, level)))
   if (len(text) > 0) then
      if (closing == 0_I4P) then ! a closing sequence of the content: escape its first `#`
         k = len(text, kind=I4P)
         do while (k >= 1_I4P)
            if (text(k:k) /= '#') exit
            k = k - 1_I4P
         enddo
         if (k < len(text, kind=I4P)) then
            if (k == 0_I4P) then
               text = BACKSLASH//text
            elseif (is_space_or_tab(text(k:k))) then
               text = text(1:k)//BACKSLASH//text(k+1:)
            endif
         endif
      endif
      line = line//' '//text
   endif
   if (closing > 0_I4P) line = line//' '//repeat('#', closing)
   call emit_line(w, line)
   endsubroutine write_heading

   function written_setext(w, doc, node) result(setext)
   !< Return true if a heading is written setext: level 1 or 2 with content, recorded so or spanning lines.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: node   !< Heading.
   logical                             :: setext !< Result.
   integer(I4P)                        :: n      !< Node of its content.

   setext = .false.
   if (doc%heading_level(node) > 2_I4P .or. doc%first_child(node) == 0_I4P) return
   if (.not. w%canonical) setext = doc%int_slot(node, SLOT_HEADING_FORM) == HEADING_SETEXT
   if (setext) return
   n = doc%first_child(node)
   do while (n /= 0_I4P .and. n /= node) ! a line break anywhere in it
      if (doc%node_kind(n) == NODE_SOFTBREAK .or. doc%node_kind(n) == NODE_LINEBREAK) then
         setext = .true.
         return
      endif
      n = next_in(doc, n, node)
   enddo
   endfunction written_setext

   subroutine write_code_block(w, doc, node)
   !< Write a code block, indented or fenced.
   type(md_writer),         intent(inout) :: w     !< Writer.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: node  !< Code block.
   character(:), allocatable              :: text  !< Literal.
   character(:), allocatable              :: info  !< Info string.
   character                              :: c     !< Fence character.
   integer(I4P)                           :: n     !< Fence length.
   integer(I4P)                           :: i, j  !< Bounds of a line.
   integer(I4P)                           :: k, r  !< Position, run.

   text = doc%literal(node)
   if (written_indented(w, doc, node)) then
      i = 1_I4P
      do while (i <= len(text, kind=I4P))
         j = index(text(i:), LF) + i - 1_I4P
         if (j < i) j = len(text, kind=I4P) + 1_I4P
         if (j > i) then
            call emit_line(w, '    '//text(i:j-1))
         else
            call emit_line(w, '')
         endif
         i = j + 1_I4P
      enddo
      return
   endif
   info = doc%info(node)
   c = '`'
   n = 3_I4P
   if (.not. w%canonical) then
      if (doc%int_slot(node, SLOT_CODE_FENCE_CHAR) == iachar('~')) c = '~'
      n = max(3_I4P, doc%int_slot(node, SLOT_CODE_FENCE_LENGTH))
   endif
   if (index(info, '`') > 0) c = '~'
   i = 1_I4P
   do while (i <= len(text, kind=I4P)) ! longer than any line inside that would close it
      j = index(text(i:), LF) + i - 1_I4P
      if (j < i) j = len(text, kind=I4P) + 1_I4P
      k = i
      do while (k < j .and. k - i < 3_I4P)
         if (text(k:k) /= ' ') exit
         k = k + 1_I4P
      enddo
      r = 0_I4P
      do while (k + r < j)
         if (text(k+r:k+r) /= c) exit
         r = r + 1_I4P
      enddo
      if (r >= n) then
         if (len_trim(text(k+r:j-1)) == 0 .or. verify(text(k+r:j-1), ' '//TAB) == 0) n = r + 1_I4P
      endif
      i = j + 1_I4P
   enddo
   call emit_line(w, fence_line(w, doc, node, c, n, .true., repeat(c, n)//escape_string(info, '')))
   if (len(text) > 0) then
      if (text(len(text):len(text)) /= LF) text = text//LF
      call emit_text(w, text, .false.)
   endif
   call emit_line(w, fence_line(w, doc, node, c, n, .false., repeat(c, n)))
   endsubroutine write_code_block

   function fence_line(w, doc, node, c, n, opening, default) result(line)
   !< Return a fence of a code block: its line in the source (recorded style) when it is the fence chosen (the opening
   !< one with the same info string, the closing one at least as long), else `default`.
   type(md_writer),         intent(in) :: w       !< Writer.
   type(markdown_document), intent(in) :: doc     !< Document.
   integer(I4P),            intent(in) :: node    !< Code block.
   character,               intent(in) :: c       !< Fence character chosen.
   integer(I4P),            intent(in) :: n       !< Fence length chosen.
   logical,                 intent(in) :: opening !< The opening fence (else the closing one).
   character(*),            intent(in) :: default !< The fence written otherwise.
   character(:), allocatable           :: line    !< Fence.
   character(:), allocatable           :: span    !< The block's source.
   character(:), allocatable           :: info    !< An info string read back.
   integer(I4P)                        :: k       !< Position.

   line = default
   if (w%canonical) return
   if (doc%span_first(node) < 1_I4P .or. doc%span_last(node) > doc%source_length() .or. &
       doc%span_last(node) < doc%span_first(node)) return
   span = doc%source_slice(doc%span_first(node), doc%span_last(node))
   if (opening) then
      k = scan(span, LF//CR)
      if (k > 0) span = span(1:k-1)
      if (scan_open_fence(span, 1_I4P) /= n .or. span(1:1) /= c) return
      info = span(n+1:)
      k = verify(info, ' '//TAB)
      if (k == 0) then
         info = ''
      else
         info = info(k:verify(info, ' '//TAB, back=.true.))
      endif
      info = unescape_string(info)
      if (info == doc%info(node) .and. len(info) == len(doc%info(node))) line = span
   else
      k = scan(span, LF//CR, back=.true.)
      if (k == 0) return ! one line: no closing fence in the source
      span = span(k+1:)
      k = verify(span, ' ')
      if (k == 0 .or. k > 4) return
      span = span(k:)
      if (span(1:1) /= c) return
      if (scan_close_fence(span, 1_I4P, c) >= n) line = span
   endif
   endfunction fence_line

   function written_indented(w, doc, node) result(indented)
   !< Return true if a code block is written indented: recorded so, with no info string, its first and last lines not
   !< blank, and no code block or list just before it (they would take its lines).
   type(md_writer),         intent(in) :: w        !< Writer.
   type(markdown_document), intent(in) :: doc      !< Document.
   integer(I4P),            intent(in) :: node     !< Code block.
   logical                             :: indented !< Result.
   character(:), allocatable           :: text     !< Literal.
   integer(I4P)                        :: prev     !< Previous sibling.
   integer(I4P)                        :: j        !< Position.

   indented = .false.
   if (w%canonical .or. doc%int_slot(node, SLOT_CODE_FORM) /= CODE_INDENTED .or. len(doc%info(node)) > 0) return
   prev = doc%prev_sibling(node)
   if (prev /= 0_I4P) then ! an indented code block or a list before it would take its lines
      if (doc%node_kind(prev) == NODE_LIST) return
      if (doc%node_kind(prev) == NODE_CODE_BLOCK) then
         if (doc%int_slot(prev, SLOT_CODE_FORM) == CODE_INDENTED) return
      endif
   endif
   text = doc%literal(node)
   if (len(text) == 0) return
   j = index(text, LF)
   if (j == 0) j = len(text) + 1
   if (verify(text(1:j-1), ' '//TAB) == 0) return
   if (text(len(text):len(text)) == LF) text = text(1:len(text)-1)
   j = index(text, LF, back=.true.)
   if (verify(text(j+1:), ' '//TAB) == 0) return
   indented = .true.
   endfunction written_indented

   ! inlines
   subroutine write_inline_lines(w, doc, slots, block, first, last, mode, prefix, ierr, msg)
   !< Write the inline content of a block (its nodes `first` to `last`) as lines, each behind `prefix` (an extension's,
   !< for `INLINE_PLAIN`).
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: first    !< First inline node.
   integer(I4P),              intent(in)    :: last     !< Last inline node, a sibling of `first`.
   integer(I4P),              intent(in)    :: mode     !< How the content is written.
   character(*),              intent(in)    :: prefix   !< Prefix of every line.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: text     !< The content.

   call inline_content(w, doc, slots, block, first, last, mode, text, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (len(prefix) > 0) call push(w, prefix, prefix, 0_I4P)
   if (len(text) > 0 .or. len(prefix) > 0) call emit_text(w, text, .false.)
   if (len(prefix) > 0) call pop(w)
   endsubroutine write_inline_lines

   subroutine inline_content(w, doc, slots, block, first, last, mode, text, ierr, msg)
   !< Return the inline content of a block (its nodes `first` to `last`), lines joined by LF, no prefix.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: first    !< First inline node.
   integer(I4P),              intent(in)    :: last     !< Last inline node, a sibling of `first`.
   integer(I4P),              intent(in)    :: mode     !< How the content is written.
   character(:), allocatable, intent(out)   :: text     !< The content.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   type(inline_state)                       :: s        !< State.
   integer(I4P)                             :: node     !< Current node.
   integer(I4P)                             :: skip     !< Last node of a text run.
   logical                                  :: descend  !< Visit the children.

   text = ''
   if (first == 0_I4P) return
   allocate(character(256) :: s%buf%chars)
   allocate(s%link_start(8), s%saved(4, 8), s%lone(8), s%emph(3, 8))
   s%mode = mode
   s%line_start = mode == INLINE_LINES
   call prepass(w, doc, s, first, last)
   node = first
   do while (node /= 0_I4P)
      call enter_inline(w, doc, slots, s, block, node, descend, skip, ierr, msg)
      if (ierr /= BARUCH_OK) return
      if (skip /= 0_I4P) node = skip
      if (descend .and. doc%first_child(node) /= 0_I4P) then
         node = doc%first_child(node)
         cycle
      endif
      do
         call leave_inline(w, doc, slots, s, block, node, ierr, msg)
         if (ierr /= BARUCH_OK) return
         if (node == last) then
            node = 0_I4P
            exit
         endif
         if (doc%next_sibling(node) /= 0_I4P) then
            node = doc%next_sibling(node)
            exit
         endif
         node = doc%parent(node)
         if (node == block .or. node == 0_I4P) then
            node = 0_I4P
            exit
         endif
      enddo
   enddo
   text = s%buf%chars(1:s%buf%used)
   endsubroutine inline_content

   subroutine prepass(w, doc, s, first, last)
   !< Before writing a block's inline content: count its backticks (a backtick of a text is escaped when another
   !< follows) and choose the delimiter of each emphasis (recorded, else `*`; the other one when the same character
   !< would touch the delimiter of the emphasis before it or of the one around it).
   type(md_writer),         intent(inout) :: w      !< Writer.
   type(markdown_document), intent(in)    :: doc    !< Document.
   type(inline_state),      intent(inout) :: s      !< State.
   integer(I4P),            intent(in)    :: first  !< First inline node.
   integer(I4P),            intent(in)    :: last   !< Last inline node.
   integer(I4P)                           :: n      !< Current node.
   integer(I4P)                           :: up     !< Its parent.
   integer(I4P)                           :: prev   !< Its previous sibling.
   integer(I4P)                           :: c, alt !< Delimiters.
   integer(I4P)                           :: kind   !< Its kind.

   s%ticks = 0_I4P
   n = first
   do while (n /= 0_I4P)
      kind = doc%node_kind(n)
      select case(kind)
      case(NODE_TEXT, NODE_HTML_INLINE)
         s%ticks = s%ticks + count_char(doc%literal(n), '`')
      case(NODE_CODE)
         s%ticks = s%ticks + count_char(doc%literal(n), '`') + 2_I4P
      case(NODE_LINK, NODE_IMAGE)
         s%ticks = s%ticks + count_char(doc%destination(n)//doc%title(n)//doc%label(n), '`')
         if (kind == NODE_LINK) then
            if (is_autolink(w, doc, n)) w%mark(n) = 1_I4P
         endif
      case(NODE_EMPH, NODE_STRONG)
         c = 0_I4P
         if (.not. w%canonical) c = doc%int_slot(n, SLOT_EMPHASIS_CHAR)
         if (c /= iachar('_')) c = iachar('*')
         alt = merge(iachar('*'), iachar('_'), c == iachar('_'))
         up = doc%parent(n)
         prev = doc%prev_sibling(n)
         if (delimiter_clash(w, doc, n, up, prev, c)) then
            if (.not. delimiter_clash(w, doc, n, up, prev, alt) .and. underscore_fits(doc, n, alt)) c = alt
         endif
         w%mark(n) = c
      case default
         if (kind >= NODE_EXTENSION_FIRST) s%ticks = s%ticks + 1_I4P
      endselect
      n = next_in_range(doc, n, first, last)
   enddo
   endsubroutine prepass

   function delimiter_clash(w, doc, node, up, prev, c) result(clash)
   !< Return true if an emphasis delimited by `c` would touch a delimiter of the same character it must not touch: the
   !< emphasis just before it, or the emphasis around it when it is all of its content and an emphasis (`**x**` is
   !< strong, `***x***` is an emphasis around strong).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Emphasis.
   integer(I4P),            intent(in) :: up    !< Its parent.
   integer(I4P),            intent(in) :: prev  !< Its previous sibling.
   integer(I4P),            intent(in) :: c     !< Delimiter.
   logical                             :: clash !< Result.

   clash = .false.
   if (prev /= 0_I4P) then
      if (doc%node_kind(prev) == NODE_EMPH .or. doc%node_kind(prev) == NODE_STRONG) clash = w%mark(prev) == c
   endif
   if (clash .or. up == 0_I4P) return
   if (doc%node_kind(up) == NODE_EMPH .or. doc%node_kind(up) == NODE_STRONG) then
      clash = w%mark(up) == c .and. doc%node_kind(node) == NODE_EMPH .and. prev == 0_I4P .and. &
              doc%next_sibling(node) == 0_I4P
   endif
   endfunction delimiter_clash

   function underscore_fits(doc, node, c) result(fits)
   !< Return true unless `c` is `_` and a letter or digit touches the emphasis from outside (`_` does not open or close
   !< inside a word).
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Emphasis.
   integer(I4P),            intent(in) :: c    !< Delimiter.
   logical                             :: fits !< Result.
   character(:), allocatable           :: t    !< A neighbour's literal.
   integer(I4P)                        :: n    !< A neighbour.

   fits = .true.
   if (c /= iachar('_')) return
   n = doc%prev_sibling(node)
   if (n /= 0_I4P) then
      if (doc%node_kind(n) == NODE_TEXT) then
         t = doc%literal(n)
         if (len(t) > 0) fits = .not. (is_ascii_alnum(t(len(t):len(t))) .or. iachar(t(len(t):len(t))) > 127)
      endif
   endif
   n = doc%next_sibling(node)
   if (fits .and. n /= 0_I4P) then
      if (doc%node_kind(n) == NODE_TEXT) then
         t = doc%literal(n)
         if (len(t) > 0) fits = .not. (is_ascii_alnum(t(1:1)) .or. iachar(t(1:1)) > 127)
      endif
   endif
   endfunction underscore_fits

   subroutine enter_inline(w, doc, slots, s, block, node, descend, skip, ierr, msg)
   !< Write what comes before an inline node's children (all of a leaf); a text node writes the whole run of text nodes
   !< it starts, and `skip` is the run's last node.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   type(inline_state),        intent(inout) :: s        !< State.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(out)   :: descend  !< Visit its children.
   integer(I4P),              intent(out)   :: skip     !< Last node of a text run, 0 if none.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   character(:), allocatable                :: text     !< A run's text.
   character(:), allocatable                :: nc       !< The character after a run.
   integer(I4P)                             :: n        !< A node of a run.
   integer(I4P)                             :: next     !< What follows a run.
   integer(I4P)                             :: k        !< Counter.
   logical                                  :: handled  !< An extension wrote the event.

   descend = .false.
   skip = 0_I4P
   call offer(doc, slots, node, .true., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (handled) then
      w%by_ext(node) = 1_I4P
      call put_raw(s, piece)
      descend = .true.
      return
   endif
   select case(doc%node_kind(node))
   case(NODE_TEXT)
      text = doc%literal(node)
      n = node
      do while (doc%next_sibling(n) /= 0_I4P)
         if (doc%node_kind(doc%next_sibling(n)) /= NODE_TEXT) exit
         if (size(slots) > 0) then
            call offer(doc, slots, doc%next_sibling(n), .true., piece, handled, ierr, msg)
            if (ierr /= BARUCH_OK) return
            if (handled) exit
         endif
         n = doc%next_sibling(n)
         text = text//doc%literal(n)
      enddo
      skip = n
      call what_follows(w, doc, block, n, next, nc)
      call escape_text(w, s, text, next, nc, is_last_in_block(doc, block, n))
   case(NODE_SOFTBREAK)
      call put_raw(s, LF)
      s%indent = line_indent(w, doc, block, doc%next_sibling(node))
   case(NODE_LINEBREAK)
      if (.not. w%canonical .and. doc%int_slot(node, SLOT_LINEBREAK_FORM) == LINEBREAK_SPACES) then
         call put_raw(s, repeat(' ', max(2_I4P, doc%int_slot(node, SLOT_LINEBREAK_SPACES)))//LF)
      else
         call put_raw(s, BACKSLASH//LF)
      endif
      s%indent = line_indent(w, doc, block, doc%next_sibling(node))
   case(NODE_CODE)
      call write_code_span(w, doc, s, node)
   case(NODE_HTML_INLINE)
      text = doc%literal(node)
      s%ticks = s%ticks - count_char(text, '`')
      call put_guarded(s, text)
   case(NODE_EMPH, NODE_STRONG)
      k = merge(1_I4P, 2_I4P, doc%node_kind(node) == NODE_EMPH)
      call put_raw(s, repeat(char1(w%mark(node)), k))
      s%delim_end = s%buf%used
      s%delim_open = .true.
      k = merge(1_I4P, 2_I4P, w%mark(node) == iachar('*'))
      s%inside(k) = s%inside(k) + 1_I4P
      s%emphs = s%emphs + 1_I4P
      if (s%emphs > size(s%emph, 2, kind=I4P)) call grow_matrix(s%emph)
      s%emph(:, s%emphs) = [w%mark(node), s%openers(1), s%openers(2)]
      descend = .true.
   case(NODE_LINK, NODE_IMAGE)
      s%ticks = s%ticks - count_char(doc%destination(node)//doc%title(node)//doc%label(node), '`')
      if (w%mark(node) == 1_I4P .and. doc%node_kind(node) == NODE_LINK) then
         text = doc%literal(doc%first_child(node))
         s%ticks = s%ticks - count_char(text, '`')
         call put_raw(s, '<'//text//'>')
         return
      endif
      if (doc%node_kind(node) == NODE_IMAGE) then
         call put_raw(s, '![')
      else
         call put_raw(s, '[')
      endif
      s%depth = s%depth + 1_I4P
      if (s%depth > size(s%link_start, kind=I4P)) then
         call grow(s%link_start)
         call grow_matrix(s%saved)
      endif
      s%link_start(s%depth) = s%buf%used
      s%saved(1:2, s%depth) = s%openers ! a link's text is read apart: its delimiters pair among themselves
      s%saved(3:4, s%depth) = s%inside
      s%openers = 0_I4P
      s%inside = 0_I4P
      descend = .true.
   case default ! an extension kind no extension writes: transparent
      descend = doc%first_child(node) /= 0_I4P
      if (.not. descend) then
         text = doc%literal(node)
         s%ticks = s%ticks - 1_I4P
         if (len(text) > 0) then
            call what_follows(w, doc, block, node, next, nc)
            call escape_text(w, s, text, next, nc, is_last_in_block(doc, block, node))
         endif
      endif
   endselect
   endsubroutine enter_inline

   subroutine leave_inline(w, doc, slots, s, block, node, ierr, msg)
   !< Write what comes after an inline node's children: a closing delimiter, the end of a link.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   type(inline_state),        intent(inout) :: s        !< State.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: node     !< Node.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   integer(I4P)                             :: k        !< Delimiter index.
   logical                                  :: handled  !< An extension wrote the event.

   if (doc%node_kind(node) == NODE_TEXT .and. w%by_ext(node) == 0_I4P) return
   call offer(doc, slots, node, .false., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (w%by_ext(node) /= 0_I4P) then ! an extension entered it: it leaves it
      if (handled) call put_raw(s, piece)
      return
   endif
   if (handled) then ! the core entered it, an extension writes its end
      call put_raw(s, piece)
      if (doc%node_kind(node) == NODE_LINK .or. doc%node_kind(node) == NODE_IMAGE) then
         if (w%mark(node) /= 1_I4P) call close_link_text(s)
      endif
      return
   endif
   select case(doc%node_kind(node))
   case(NODE_EMPH, NODE_STRONG)
      call put_raw(s, repeat(char1(w%mark(node)), merge(1, 2, doc%node_kind(node) == NODE_EMPH)))
      s%delim_end = s%buf%used
      s%delim_open = .false.
      k = merge(1_I4P, 2_I4P, w%mark(node) == iachar('*'))
      s%inside(k) = s%inside(k) - 1_I4P
      if (s%emphs > 0_I4P) then ! the delimiters between its opener and its closer are gone
         s%openers = s%emph(2:3, s%emphs)
         s%emphs = s%emphs - 1_I4P
      endif
   case(NODE_LINK, NODE_IMAGE)
      if (doc%node_kind(node) == NODE_LINK .and. w%mark(node) == 1_I4P) return ! an autolink, written whole
      call finish_link(w, doc, s, block, node)
   endselect
   endsubroutine leave_inline

   subroutine finish_link(w, doc, s, block, node)
   !< Write the end of a link or an image: a reference (full, collapsed, shortcut) when recorded and the definition its
   !< label finds is the link's, else inline.
   type(md_writer),         intent(inout) :: w      !< Writer.
   type(markdown_document), intent(in)    :: doc    !< Document.
   type(inline_state),      intent(inout) :: s      !< State.
   integer(I4P),            intent(in)    :: block  !< Block.
   integer(I4P),            intent(in)    :: node   !< Link or image.
   character(:), allocatable              :: raw    !< Its text as written.
   character(:), allocatable              :: dest   !< Destination.
   character(:), allocatable              :: title  !< Title.
   character(:), allocatable              :: label  !< Label as recorded.
   character(:), allocatable              :: nc     !< The character after it.
   integer(I4P)                           :: form   !< Recorded form.
   integer(I4P)                           :: next   !< What follows it.
   logical                                :: by_text  !< Its text finds its definition.
   logical                                :: by_label !< Its label finds its definition.

   raw = s%buf%chars(s%link_start(s%depth)+1:s%buf%used)
   call close_link_text(s)
   if (doc%node_kind(node) == NODE_LINK) s%lones = 0_I4P ! a link deactivates the `[` before it
   dest = doc%destination(node)
   title = doc%title(node)
   form = 0_I4P
   if (.not. w%canonical) form = doc%int_slot(node, SLOT_LINK_FORM)
   if (form == LINK_FULL .or. form == LINK_COLLAPSED .or. form == LINK_SHORTCUT) then
      label = doc%label(node)
      by_label = .false.
      if (len(label) > 0) by_label = defines(doc, w%refs%lookup(label), dest, title)
      by_text = defines(doc, w%refs%lookup(raw), dest, title)
      if (form == LINK_SHORTCUT .and. by_text) then
         call what_follows(w, doc, block, node, next, nc)
         if (next /= NEXT_LINK .and. .not. tail_follows(doc, node, nc)) then
            call put_raw(s, ']')
            return
         endif
      endif
      if (form /= LINK_FULL .and. by_text) then
         call put_raw(s, '][]')
         return
      endif
      if (by_label) then
         call put_raw(s, ']['//label//']')
         return
      endif
      if (by_text) then
         call put_raw(s, '][]')
         return
      endif
   endif
   if (len(dest) == 0 .and. len(title) == 0) then
      call put_raw(s, ']()')
   else
      call put_guarded(s, ']('//destination_text(dest, .false.)//title_text(title)//')')
   endif
   endsubroutine finish_link

   function tail_follows(doc, node, nc) result(tail)
   !< Return true if what follows a link written `[label]` would make it another link: an inline link's tail (`(` in
   !< the text after it), a reference (`[`), a definition (`:`, when the link starts its paragraph).
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Link.
   character(*),            intent(in) :: nc   !< The character after it.
   logical                             :: tail !< Result.
   character(:), allocatable           :: t    !< The text after it.

   tail = nc == '(' .or. nc == '[' .or. nc == ':' ! `[`: any label after it is read as the link's (cmark)
   if (.not. tail .or. doc%next_sibling(node) == 0_I4P) return
   if (doc%node_kind(doc%next_sibling(node)) /= NODE_TEXT) return
   t = doc%literal(doc%next_sibling(node))
   select case(nc)
   case('(')
      tail = inline_tail(t, 1_I4P)
   case(':')
      tail = doc%prev_sibling(node) == 0_I4P
   endselect
   endfunction tail_follows

   pure subroutine close_link_text(s)
   !< Close the innermost link text: the emphasis counts of outside it come back.
   type(inline_state), intent(inout) :: s !< State.

   s%openers = s%saved(1:2, s%depth)
   s%inside = s%saved(3:4, s%depth)
   s%depth = s%depth - 1_I4P
   endsubroutine close_link_text

   function defines(doc, definition, dest, title) result(same)
   !< Return true if a definition (0: none) gives a destination and a title.
   type(markdown_document), intent(in) :: doc        !< Document.
   integer(I4P),            intent(in) :: definition !< Definition node.
   character(*),            intent(in) :: dest       !< Destination.
   character(*),            intent(in) :: title      !< Title.
   logical                             :: same       !< Result.

   same = .false.
   if (definition == 0_I4P) return
   same = doc%destination(definition) == dest .and. len(doc%destination(definition)) == len(dest) .and. &
          doc%title(definition) == title .and. len(doc%title(definition)) == len(title)
   endfunction defines

   function is_autolink(w, doc, node) result(auto)
   !< Return true if a link is written as an autolink: recorded so (or canonical), with one text child that is its
   !< destination (or its email address, the destination `mailto:` it), read back as such, and no title.
   type(md_writer),         intent(in) :: w    !< Writer.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Link.
   logical                             :: auto !< Result.
   character(:), allocatable           :: text !< Its text.
   character(:), allocatable           :: dest !< Its destination.
   integer(I4P)                        :: t    !< Its child.

   auto = .false.
   if (.not. w%canonical .and. doc%int_slot(node, SLOT_LINK_FORM) /= LINK_AUTOLINK) return
   t = doc%first_child(node)
   if (t == 0_I4P .or. len(doc%title(node)) > 0) return
   if (doc%node_kind(t) /= NODE_TEXT .or. doc%next_sibling(t) /= 0_I4P) return
   text = doc%literal(t)
   dest = doc%destination(node)
   if (len(text) == 0) return
   if (decode_references(text) /= text) return
   if (dest == text .and. len(dest) == len(text)) then
      auto = scan_autolink_uri('<'//text//'>') == len(text, kind=I4P) + 2_I4P
   elseif (dest == 'mailto:'//text .and. len(dest) == len(text) + 7) then
      auto = scan_autolink_email('<'//text//'>') == len(text, kind=I4P) + 2_I4P
   endif
   endfunction is_autolink

   subroutine write_code_span(w, doc, s, node)
   !< Write a code span: its content as written (recorded: padding and line endings), else its literal with a space
   !< inside each end when it starts or ends with a backtick, or with a space at both ends (the parser strips one
   !< there); between backtick strings of the recorded length, else the shortest the content does not hold.
   type(md_writer),         intent(in)    :: w    !< Writer.
   type(markdown_document), intent(in)    :: doc  !< Document.
   type(inline_state),      intent(inout) :: s    !< State.
   integer(I4P),            intent(in)    :: node !< Code span.
   character(:), allocatable              :: code !< Content as written.
   character(:), allocatable              :: raw  !< Content as recorded.
   integer(I4P)                           :: n    !< Backticks of the delimiters.
   logical                                :: pad  !< Pad with spaces.

   code = doc%literal(node)
   n = 0_I4P
   raw = ''
   if (.not. w%canonical) then
      raw = doc%string_slot(node, STRING_RAW)
      n = doc%int_slot(node, SLOT_CODE_TICKS)
   endif
   pad = .false.
   if (len(raw) > 0) then
      if (raw(1:1) /= '`' .and. raw(len(raw):len(raw)) /= '`' .and. code_content(raw) == code .and. &
          len(code_content(raw)) == len(code)) then
         code = raw
      else
         raw = ''
      endif
   endif
   if (len(raw) == 0 .and. len(code) > 0) then
      if (code(1:1) == '`' .or. code(len(code):len(code)) == '`') pad = .true.
      if (code(1:1) == ' ' .and. code(len(code):len(code)) == ' ' .and. verify(code, ' ') > 0) pad = .true.
   endif
   if (n < 1_I4P .or. has_tick_run(code, n)) then
      n = 1_I4P
      do while (has_tick_run(code, n))
         n = n + 1_I4P
      enddo
   endif
   s%ticks = s%ticks - count_char(code, '`') - 2_I4P
   if (pad) code = ' '//code//' '
   call put_guarded(s, repeat('`', n)//code//repeat('`', n))
   endsubroutine write_code_span

   subroutine what_follows(w, doc, block, node, next, nc)
   !< Return what is written right after an inline node: its class (`NEXT_*`) and its first character (empty at the end
   !< of the block).
   type(md_writer),           intent(in)  :: w     !< Writer.
   type(markdown_document),   intent(in)  :: doc   !< Document.
   integer(I4P),              intent(in)  :: block !< Block.
   integer(I4P),              intent(in)  :: node  !< Node.
   integer(I4P),              intent(out) :: next  !< Class.
   character(:), allocatable, intent(out) :: nc    !< First character.
   integer(I4P)                           :: n     !< The next node, or the parent.

   n = doc%next_sibling(node)
   if (n /= 0_I4P) then
      next = NEXT_OTHER
      select case(doc%node_kind(n))
      case(NODE_SOFTBREAK)
         next = NEXT_LINE
         nc = LF
      case(NODE_LINEBREAK)
         next = NEXT_LINE
         nc = ' '
         if (w%canonical .or. doc%int_slot(n, SLOT_LINEBREAK_FORM) /= LINEBREAK_SPACES) nc = BACKSLASH
      case(NODE_CODE)
         nc = '`'
      case(NODE_HTML_INLINE)
         nc = '<'
      case(NODE_EMPH, NODE_STRONG)
         next = NEXT_OPEN
         nc = char1(w%mark(n))
      case(NODE_LINK)
         if (w%mark(n) == 1_I4P) then
            nc = '<'
         else
            next = NEXT_LINK
            nc = '['
         endif
      case(NODE_IMAGE)
         nc = '!'
      case(NODE_TEXT)
         nc = doc%literal(n)
         if (len(nc) > 0) nc = nc(1:1)
      case default
         nc = 'a'
      endselect
      return
   endif
   n = doc%parent(node)
   if (n == block .or. n == 0_I4P) then
      next = NEXT_END
      nc = ''
      return
   endif
   select case(doc%node_kind(n))
   case(NODE_EMPH, NODE_STRONG)
      next = NEXT_CLOSE
      nc = char1(w%mark(n))
   case(NODE_LINK, NODE_IMAGE)
      next = NEXT_OTHER
      nc = ']'
   case default
      next = NEXT_OTHER
      nc = 'a'
   endselect
   endsubroutine what_follows

   function is_last_in_block(doc, block, node) result(is)
   !< Return true if nothing is written after an inline node in its block.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: block !< Block.
   integer(I4P),            intent(in) :: node  !< Node.
   logical                             :: is    !< Result.
   integer(I4P)                        :: n     !< Ancestor.

   is = .false.
   n = node
   do while (n /= block .and. n /= 0_I4P)
      if (doc%next_sibling(n) /= 0_I4P) return
      n = doc%parent(n)
   enddo
   is = .true.
   endfunction is_last_in_block

   subroutine escape_text(w, s, text, next, nc, at_end)
   !< Write a run of text escaped: what follows it is `next` (a `NEXT_*` class) and starts with `nc`; `at_end` when
   !< nothing follows it in the block.
   type(md_writer),    intent(in)    :: w      !< Writer.
   type(inline_state), intent(inout) :: s      !< State.
   character(*),       intent(in)    :: text   !< Text.
   integer(I4P),       intent(in)    :: next   !< What follows: class.
   character(*),       intent(in)    :: nc     !< What follows: first character.
   logical,            intent(in)    :: at_end !< Nothing follows in the block.
   character(:), allocatable         :: probe  !< The line the run starts.
   character(:), allocatable         :: pc     !< The character before a delimiter run.
   character(:), allocatable         :: ac     !< The character after it.
   integer(I4P)                      :: n      !< Length.
   integer(I4P)                      :: i, k   !< Positions.
   integer(I4P)                      :: e      !< Position to escape at a line start, 0 if none.
   integer(I4P)                      :: l      !< Bytes of a character.
   integer(I4P)                      :: d      !< Index of a delimiter character (1 `*`, 2 `_`).
   integer(I4P)                      :: r      !< Length of a backtick string.
   integer(I4P)                      :: last_gt !< Position of the text's last `>`, 0 if none.
   integer(I4P), allocatable         :: mate(:) !< The bracket matching each bracket, 0 if none.
   integer(I4P)                      :: closer_after(2) !< Start of the text's last run of `*`, `_` that can close.
   logical                           :: line_end   !< A line end follows the run.
   logical                           :: open_start !< The run starts an emphasis's content.
   logical                           :: esc    !< Escape.
   logical                           :: bs, bp       !< Space and punctuation of a character.
   logical                           :: can_open, can_close !< A delimiter run can open, close emphasis.
   character                         :: c      !< Current byte.

   n = len(text, kind=I4P)
   if (n == 0_I4P) return
   line_end = next == NEXT_END .or. next == NEXT_LINE
   open_start = s%delim_open .and. s%delim_end == s%buf%used
   s%ticks = s%ticks - count_char(text, '`')
   e = 0_I4P
   if (s%mode == INLINE_LINES .and. s%line_start .and. s%indent < 4_I4P .and. .not. is_space_or_tab(text(1:1))) then
      k = index(text, LF)
      if (k > 0) then
         probe = text(1:k-1)
      elseif (line_end) then
         probe = text
      else
         probe = text//'a'
      endif
      e = block_start_escape(probe, .not. s%first_line)
   endif
   call match_brackets(text, mate)
   call last_closers(text, s%buf%last_char(), nc, closer_after)
   last_gt = index(text, '>', back=.true.)
   i = 1_I4P
   do while (i <= n)
      c = text(i:i)
      if (i == e) then
         call put_raw(s, BACKSLASH//c)
         i = i + 1_I4P
         cycle
      endif
      select case(c)
      case(' ', TAB)
         esc = .false.
         if (s%line_start .and. s%mode == INLINE_LINES) esc = .true.
         if (i == 1_I4P .and. open_start) esc = .true.
         if (i == n .and. next == NEXT_CLOSE) esc = .true.
         if (line_end .and. (s%mode == INLINE_LINES .or. s%mode == INLINE_ATX)) then
            if (verify(text(i:), ' '//TAB) == 0) esc = .true.
         endif
         if (s%mode == INLINE_ATX .and. s%buf%used == 0_I4P) esc = .true.
         if (esc) then
            call put_raw(s, '&#'//istr(iachar(c, kind=I4P))//';')
         else
            call put_raw(s, c)
         endif
      case(LF, CR)
         call put_raw(s, '&#'//istr(iachar(c, kind=I4P))//';')
      case(BACKSLASH)
         if (i < n) then
            esc = is_ascii_punctuation(text(i+1:i+1))
         else
            esc = next == NEXT_LINE
            if (len(nc) > 0) esc = esc .or. is_ascii_punctuation(nc(1:1))
         endif
         if (esc) then
            call put_raw(s, BACKSLASH//BACKSLASH)
         else
            call put_raw(s, BACKSLASH)
         endif
      case('*', '_')
         k = i
         do while (k <= n)
            if (text(k:k) /= c) exit
            k = k + 1_I4P
         enddo
         pc = s%buf%last_char()
         if (k <= n) then
            ac = utf8_at(text, k)
         else
            ac = nc
         endif
         call run_flanking(c, pc, ac, can_open, can_close)
         d = merge(1_I4P, 2_I4P, c == '*')
         ! a closer pairs with an opener before it (a text's, or an emphasis's still open); an opener with a closer
         ! after it: of the text's later runs (each checks the openers before it) or of an emphasis around it
         esc = can_close .and. (s%openers(d) > 0_I4P .or. s%inside(d) > 0_I4P)
         if (can_open .and. s%emphs > 0_I4P) esc = esc .or. s%emph(1, s%emphs) == iachar(c) .or. &
                                                    s%emph(1, s%emphs) == 0_I4P
         if (i == 1_I4P .and. s%delim_end == s%buf%used .and. len(pc) > 0) esc = esc .or. pc == c
         if (k > n .and. (next == NEXT_OPEN .or. next == NEXT_CLOSE)) esc = esc .or. nc == c
         if (index(w%ext_chars, c) > 0) esc = .true.
         if (.not. esc .and. can_open) then ! a closer later in the text: escape the opener rather than the closer
            if (closer_after(d) >= k) then
               esc = .true.
            else
               s%openers(d) = s%openers(d) + 1_I4P
            endif
         endif
         do while (i < k)
            if (esc) then
               call put_raw(s, BACKSLASH//c)
            else
               call put_raw(s, c)
            endif
            i = i + 1_I4P
         enddo
         cycle
      case('`')
         k = i
         do while (k <= n)
            if (text(k:k) /= c) exit
            k = k + 1_I4P
         enddo
         r = k - i
         esc = s%ticks > 0_I4P .or. has_tick_run(text(k:), r)
         pc = s%buf%last_char()
         if (pc == '`') esc = .true.
         if (k > n .and. nc == '`') esc = .true.
         do while (i < k)
            if (esc) then
               call put_raw(s, BACKSLASH//c)
            else
               call put_raw(s, c)
            endif
            i = i + 1_I4P
         enddo
         cycle
      case('[')
         esc = s%depth > 0_I4P
         if (mate(i) > 0_I4P) then
            esc = pair_links(w, text, i, mate(i), next, nc, s%buf%used == 0_I4P .and. s%mode == INLINE_LINES)
            if (esc) mate(mate(i)) = 0_I4P ! its `]` is now a lone one
         endif
         if (.not. esc .and. s%depth == 0_I4P) then ! active: a `]` after it may close it
            s%lones = s%lones + 1_I4P
            if (s%lones > size(s%lone, kind=I4P)) call grow(s%lone)
            s%lone(s%lones) = s%buf%used
            if (mate(i) > 0_I4P) mate(mate(i)) = 0_I4P
         endif
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case(']')
         if (mate(i) > 0_I4P) then ! a pair inside a link's text that makes no link
            esc = .false.
         elseif (s%depth > 0_I4P) then ! it would end the link's text
            esc = .true.
         elseif (s%lones > 0_I4P) then ! it closes the last active `[`: a link, or text and that `[` is gone
            esc = closes_link(w, s, text, i, next, nc)
            if (.not. esc) s%lones = s%lones - 1_I4P
         else
            esc = .false.
         endif
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case('!')
         if (i == n .and. next == NEXT_LINK) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case('&')
         esc = scan_entity_like(text, i)
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case('<')
         esc = opens_html(text(i:), at_end, last_gt > i)
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case default
         l = utf8_length(text(i:i))
         if (l == 1_I4P) then
            if (index(w%ext_chars, c) > 0 .and. is_ascii_punctuation(c)) then
               call put_raw(s, BACKSLASH//c)
            else
               call put_raw(s, c)
            endif
         else
            l = min(l, n - i + 1_I4P)
            esc = .false.
            if ((i == 1_I4P .and. open_start) .or. (i + l - 1_I4P == n .and. next == NEXT_CLOSE)) then
               call classify(text(i:i+l-1), 1_I4P, bs, bp)
               esc = bs
            endif
            if (esc) then
               call put_raw(s, '&#'//istr(utf8_code(text(i:i+l-1)))//';')
            else
               call put_raw(s, text(i:i+l-1))
            endif
            i = i + l
            cycle
         endif
      endselect
      i = i + 1_I4P
   enddo
   endsubroutine escape_text

   function closes_link(w, s, text, j, next, nc) result(links)
   !< Return true if a `]` of a text, `text(j)`, would close the last active `[` into a link (the bytes between them
   !< already written).
   type(md_writer),    intent(in) :: w     !< Writer.
   type(inline_state), intent(in) :: s     !< State.
   character(*),       intent(in) :: text  !< Text.
   integer(I4P),       intent(in) :: j     !< The `]`.
   integer(I4P),       intent(in) :: next  !< What follows the text: class.
   character(*),       intent(in) :: nc    !< What follows the text: first character.
   logical                        :: links !< Result.
   character(:), allocatable      :: label !< The bytes between the brackets.
   integer(I4P)                   :: open_ !< Position of the `[`.

   open_ = s%lone(s%lones)
   if (s%buf%used - open_ > 4_I4P * LABEL_MAX) then ! no label is that long: no reference, no definition
      label = repeat('a', LABEL_MAX + 1_I4P)
   else
      label = s%buf%chars(open_+1:s%buf%used)
   endif
   links = after_bracket(w, text, j, next, nc, label, open_ == 0_I4P .and. s%mode == INLINE_LINES)
   endfunction closes_link

   function after_bracket(w, text, j, next, nc, label, block_start) result(links)
   !< Return true if what follows a `]` at `text(j)` makes a link of the brackets holding `label`: an inline link's
   !< tail, a reference to a defined label, the label itself defined (a shortcut), a link node; a definition's `:` at
   !< the start of a paragraph. What cannot be told within the text counts as a link.
   type(md_writer), intent(in) :: w           !< Writer.
   character(*),    intent(in) :: text        !< Text.
   integer(I4P),    intent(in) :: j           !< The `]`.
   integer(I4P),    intent(in) :: next        !< What follows the text: class.
   character(*),    intent(in) :: nc          !< What follows the text: first character.
   character(*),    intent(in) :: label       !< The bytes between the brackets, as written.
   logical,         intent(in) :: block_start !< The `[` starts a paragraph.
   logical                     :: links       !< Result.
   integer(I4P)                :: k           !< Position.

   links = .false.
   if (len(label) > 0) links = w%refs%lookup(label) /= 0_I4P
   if (links) return
   if (j == len(text)) then
      links = next == NEXT_LINK .or. nc == '(' .or. nc == '['
      return
   endif
   select case(text(j+1:j+1))
   case('(')
      links = inline_tail(text, j + 1_I4P)
   case('[')
      k = index(text(j+2:), ']')
      if (k == 0) then
         links = .true.
      elseif (k > 1) then
         links = w%refs%lookup(text(j+2:j+k)) /= 0_I4P
      endif
   case(':')
      if (block_start) links = defines_there('['//label//text(j:), next)
   endselect
   endfunction after_bracket

   function defines_there(text, next) result(defines)
   !< Return true if a paragraph starting with `text` would start with a link reference definition: when more follows
   !< on later lines (`next` not `NEXT_END`), whenever it could.
   character(*), intent(in)  :: text        !< The paragraph's text, from its start.
   integer(I4P), intent(in)  :: next        !< What follows the text: class.
   logical                   :: defines     !< Result.
   character(:), allocatable :: destination !< Destination.
   character(:), allocatable :: title       !< Title.
   integer(I4P)              :: after       !< Position after a definition, 0 if none.
   integer(I4P)              :: a, b        !< Bounds of its label.

   defines = next /= NEXT_END
   if (defines) return
   call parse_definition(text//LF, 1_I4P, after, a, b, destination, title)
   defines = after > 0_I4P
   endfunction defines_there

   function inline_tail(text, k) result(tail)
   !< Return true if an inline link's tail, `(destination "title")`, starts at `text(k)`, or might (the text ends first).
   character(*), intent(in) :: text  !< Text.
   integer(I4P), intent(in) :: k     !< Position of the `(`.
   logical                  :: tail  !< Result.
   integer(I4P)             :: pos   !< Position.
   integer(I4P)             :: f, l  !< Bounds of the destination.
   integer(I4P)             :: t     !< Bytes of the title.
   integer(I4P)             :: n     !< Length.
   logical                  :: ok    !< A destination.

   tail = .true.
   n = len(text, kind=I4P)
   pos = skip_white(text, k + 1_I4P)
   if (pos > n) return
   if (text(pos:pos) == ')') return
   call scan_destination(text, pos, .true., f, l, ok)
   if (.not. ok) then
      if (text(pos:pos) == '<') then ! written `\<` (an escaped `<`), it starts a plain destination
         t = pos
         call scan_destination(text(1:pos-1)//'a'//text(pos+1:), t, .true., f, l, ok)
         if (ok) pos = t
      endif
   endif
   if (.not. ok) then
      tail = index(text(k:), ')') == 0
      return
   endif
   pos = skip_white(text, pos)
   if (pos > n) return
   if (text(pos:pos) == ')') return
   t = scan_title(text, pos)
   if (t == 0_I4P) then
      tail = .false.
      return
   endif
   pos = skip_white(text, pos + t)
   if (pos > n) return
   tail = text(pos:pos) == ')'
   endfunction inline_tail

   pure function skip_white(text, k) result(pos)
   !< Return the first position from `k` that is not a space or a tab.
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: k    !< Position.
   integer(I4P)             :: pos  !< Result.

   pos = k
   do while (pos <= len(text, kind=I4P))
      if (.not. is_space_or_tab(text(pos:pos))) exit
      pos = pos + 1_I4P
   enddo
   endfunction skip_white

   pure function opens_html(text, at_end, gt) result(opens)
   !< Return true if a `<`, `text(1)`, would start raw HTML or an autolink: one of them is there whole, or it could end
   !< in what is written after the text (`at_end` false); a comment, a processing instruction, a declaration or a CDATA
   !< section is always taken as one.
   character(*), intent(in) :: text   !< Text from the `<`.
   logical,      intent(in) :: at_end !< Nothing follows the text in its block.
   logical,      intent(in) :: gt     !< A `>` follows in the text.
   logical                  :: opens  !< Result.

   opens = .false.
   if (len(text) < 2) then
      opens = .not. at_end
      return
   endif
   if (is_ascii_space(text(2:2)) .or. text(2:2) == '<') return
   if (text(2:2) == '!' .or. text(2:2) == '?') then
      opens = .true.
   elseif (.not. gt) then
      opens = .not. at_end
   else
      opens = scan_autolink_uri(text) > 0_I4P .or. scan_autolink_email(text) > 0_I4P .or. scan_html_tag(text) > 0_I4P
   endif
   endfunction opens_html

   subroutine run_flanking(c, before, after, can_open, can_close)
   !< Whether a run of `c` (`*` or `_`) between the characters `before` and `after` (empty: the start, the end) can open
   !< and can close emphasis (CommonMark 6.2, as the parser decides).
   character,    intent(in)  :: c         !< Delimiter character.
   character(*), intent(in)  :: before    !< The character before the run.
   character(*), intent(in)  :: after     !< The character after it.
   logical,      intent(out) :: can_open  !< It can open.
   logical,      intent(out) :: can_close !< It can close.
   logical                   :: bs, bp    !< White space, punctuation before.
   logical                   :: as_, ap   !< White space, punctuation after.
   logical                   :: left      !< Left-flanking.
   logical                   :: right     !< Right-flanking.

   call classify(before, merge(1_I4P, 0_I4P, len(before) > 0), bs, bp)
   call classify(after, merge(1_I4P, 0_I4P, len(after) > 0), as_, ap)
   left = .not. as_ .and. (.not. ap .or. bs .or. bp)
   right = .not. bs .and. (.not. bp .or. as_ .or. ap)
   if (c == '_') then
      can_open = left .and. (.not. right .or. bp)
      can_close = right .and. (.not. left .or. ap)
   else
      can_open = left
      can_close = right
   endif
   endsubroutine run_flanking

   subroutine last_closers(text, before, after, closer)
   !< Return the start of the last run of `*` (`closer(1)`) and of `_` (`closer(2)`) of a text that can close emphasis
   !< (0 if none), its characters read as they stand in the text, `before` and `after` around it.
   character(*), intent(in)  :: text      !< Text.
   character(*), intent(in)  :: before    !< The character before the text.
   character(*), intent(in)  :: after     !< The character after it.
   integer(I4P), intent(out) :: closer(2) !< Last closing run of each character.
   character(:), allocatable :: pc, ac    !< Characters around a run.
   integer(I4P)              :: i, k      !< Bounds of a run.
   logical                   :: can_open, can_close !< Flanking.

   closer = 0_I4P
   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      if (text(i:i) /= '*' .and. text(i:i) /= '_') then
         i = i + 1_I4P
         cycle
      endif
      k = i
      do while (k <= len(text, kind=I4P))
         if (text(k:k) /= text(i:i)) exit
         k = k + 1_I4P
      enddo
      if (i == 1_I4P) then
         pc = before
      else
         pc = text(prev_char_start(text, i):i-1)
      endif
      if (k <= len(text, kind=I4P)) then
         ac = utf8_at(text, k)
      else
         ac = after
      endif
      call run_flanking(text(i:i), pc, ac, can_open, can_close)
      if (can_close) closer(merge(1, 2, text(i:i) == '*')) = i
      i = k
   enddo
   endsubroutine last_closers

   pure function prev_char_start(text, i) result(first)
   !< Return the first byte of the UTF-8 character ending at `i - 1`.
   character(*), intent(in) :: text  !< Text.
   integer(I4P), intent(in) :: i     !< Position after the character.
   integer(I4P)             :: first !< Its first byte.

   first = i - 1_I4P
   do while (first > 1_I4P)
      if (iand(iachar(text(first:first)), 192) /= 128) exit
      first = first - 1_I4P
   enddo
   endfunction prev_char_start

   pure function has_tick_run(text, r) result(has)
   !< Return true if a text holds a backtick string of exactly `r` backticks (it would close a code span `r` opens).
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: r    !< Length.
   logical                  :: has  !< Result.
   integer(I4P)             :: i, k !< Positions.

   has = .false.
   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      if (text(i:i) == '`') then
         k = i
         do while (k <= len(text, kind=I4P))
            if (text(k:k) /= '`') exit
            k = k + 1_I4P
         enddo
         if (k - i == r) then
            has = .true.
            return
         endif
         i = k
      else
         i = i + 1_I4P
      endif
   enddo
   endfunction has_tick_run

   function pair_links(w, text, i, j, next, nc, block_start) result(links)
   !< Return true if a pair of brackets of a text, `text(i)` and `text(j)`, could make a link when read back: followed by
   !< `(` or `[`, a link, or holding a defined label; or a definition, followed by `:` at the start of a paragraph.
   type(md_writer), intent(in) :: w     !< Writer.
   character(*),    intent(in) :: text  !< Text.
   integer(I4P),    intent(in) :: i     !< The `[`.
   integer(I4P),    intent(in) :: j     !< The matching `]`.
   integer(I4P),    intent(in) :: next  !< What follows the text: class.
   character(*),    intent(in) :: nc    !< What follows the text: first character.
   logical,         intent(in) :: block_start !< The `[` starts a paragraph.
   logical                     :: links !< Result.

   links = after_bracket(w, text, j, next, nc, text(i+1:j-1), block_start .and. i == 1)
   endfunction pair_links

   pure subroutine match_brackets(text, mate)
   !< Pair the brackets of a text, innermost first: `mate(k)` is the position of the bracket matching the one at `k`, 0
   !< for a bracket with no mate (and for any other character).
   character(*),              intent(in)  :: text    !< Text.
   integer(I4P), allocatable, intent(out) :: mate(:) !< Mates.
   integer(I4P), allocatable              :: stack(:) !< Open brackets.
   integer(I4P)                           :: top     !< Stack top.
   integer(I4P)                           :: k       !< Position.

   allocate(mate(len(text)), stack(len(text)))
   mate = 0_I4P
   top = 0_I4P
   do k=1, len(text, kind=I4P)
      if (text(k:k) == '[') then
         top = top + 1_I4P
         stack(top) = k
      elseif (text(k:k) == ']' .and. top > 0_I4P) then
         mate(k) = stack(top)
         mate(stack(top)) = k
         top = top - 1_I4P
      endif
   enddo
   endsubroutine match_brackets

   pure function scan_entity_like(text, i) result(is)
   !< Return true if a character reference starts at `text(i)`, an `&`.
   character(*), intent(in)  :: text  !< Text.
   integer(I4P), intent(in)  :: i     !< Position of the `&`.
   logical                   :: is    !< Result.
   character(:), allocatable :: bytes !< Its character.
   integer(I4P)              :: l     !< Its length.

   call scan_reference(text, i, bytes, l)
   is = l > 0_I4P
   endfunction scan_entity_like

   function block_start_escape(line, continuation) result(e)
   !< Return the position to escape in a line of a paragraph so that it opens no block (0 if it opens none): its first
   !< character, or the delimiter of an ordered list marker. A `continuation` line can only open what interrupts a
   !< paragraph, and a setext underline.
   character(*), intent(in) :: line         !< The line, as written, without its prefix.
   logical,      intent(in) :: continuation !< Not the paragraph's first line.
   integer(I4P)             :: e            !< Position to escape, 0 for none.
   integer(I4P)             :: k            !< Counter.
   integer(I4P)             :: marker, start, length !< A list marker.
   logical                  :: ordered      !< An ordered marker.

   e = 1_I4P
   if (len(line) == 0) then
      e = 0_I4P
      return
   endif
   if (line(1:1) == '>') return
   call scan_atx_start(line, 1_I4P, k, length)
   if (k > 0_I4P) return
   if (scan_open_fence(line, 1_I4P) > 0_I4P) return
   call scan_thematic_break(line, 1_I4P, k, length)
   if (k > 0_I4P) return
   if (continuation) then
      if (scan_setext_underline(line, 1_I4P) > 0_I4P) return
   endif
   if (scan_html_block_start(line, 1_I4P, .not. continuation) > 0_I4P) return
   call scan_list_marker(line, 1_I4P, continuation, ordered, marker, start, length)
   if (length > 0_I4P) then
      if (ordered) e = length ! the delimiter, after the digits
      return
   endif
   e = 0_I4P
   endfunction block_start_escape

   subroutine put_guarded(s, text)
   !< Write text whose bytes are not escaped (raw HTML, the end of an inline link): a line it starts or continues that
   !< would open a block is indented by 4 spaces, which a paragraph's continuation line loses.
   type(inline_state), intent(inout) :: s    !< State.
   character(*),       intent(in)    :: text !< Text.
   integer(I4P)                      :: i, j !< Bounds of a line.

   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      j = index(text(i:), LF) + i - 1_I4P
      if (j < i) j = len(text, kind=I4P) + 1_I4P
      if (s%mode == INLINE_LINES .and. s%line_start .and. .not. s%first_line .and. j > i) then
         if (block_start_escape(text(i:j-1), .true.) > 0_I4P .or. text(i:i) == ' ') call put_raw(s, '    ')
      endif
      if (j <= len(text, kind=I4P)) then
         call put_raw(s, text(i:j))
      else
         call put_raw(s, text(i:j-1))
      endif
      i = j + 1_I4P
   enddo
   endsubroutine put_guarded

   subroutine put_raw(s, text)
   !< Write text as it is, keeping the line state (a line's recorded indentation before its first byte).
   type(inline_state), intent(inout) :: s    !< State.
   character(*),       intent(in)    :: text !< Text.

   if (len(text) == 0) return
   if (s%line_start .and. s%indent > 0_I4P) call s%buf%put(repeat(' ', s%indent))
   s%indent = 0_I4P
   call s%buf%put(text)
   if (index(text, LF) > 0) s%first_line = .false.
   s%line_start = text(len(text):len(text)) == LF
   endsubroutine put_raw

   ! strings
   function destination_text(dest, definition) result(text)
   !< Return a destination as written in a link or a definition: between `<` and `>` when empty (a definition), when
   !< it holds white space or control characters, unbalanced parentheses or starts with `<`; else as it is. Backslashes
   !< before punctuation and `&` before a character reference are escaped.
   character(*), intent(in)  :: dest       !< Destination.
   logical,      intent(in)  :: definition !< Of a link reference definition.
   character(:), allocatable :: text       !< As written.
   integer(I4P)              :: i          !< Position.
   integer(I4P)              :: depth      !< Parentheses open.
   logical                   :: pointy     !< Between `<` and `>`.
   logical                   :: balanced   !< Its parentheses are balanced.

   pointy = len(dest) == 0
   balanced = .true.
   depth = 0_I4P
   do i=1, len(dest, kind=I4P)
      if (iachar(dest(i:i)) <= 32 .or. iachar(dest(i:i)) == 127) pointy = .true.
      if (dest(i:i) == '(') depth = depth + 1_I4P
      if (dest(i:i) == ')') depth = depth - 1_I4P
      if (depth < 0_I4P) balanced = .false.
   enddo
   if (depth /= 0_I4P) balanced = .false.
   if (len(dest) > 0) then
      if (dest(1:1) == '<') pointy = .true.
   endif
   if (pointy .or. .not. balanced) then
      text = '<'//escape_string(dest, '<>')//'>'
   else
      text = escape_string(dest, '')
   endif
   if (definition .and. len(dest) == 0) text = '<>'
   endfunction destination_text

   function title_text(title) result(text)
   !< Return a title as written after a destination (with the space before it), empty for none: between `"`, the `"`
   !< inside escaped.
   character(*), intent(in)  :: title !< Title.
   character(:), allocatable :: text  !< As written.
   character                 :: q     !< Quote.

   text = ''
   if (len(title) == 0) return
   q = '"'
   text = ' '//q//escape_string(title, q)//q
   endfunction title_text

   pure function escape_string(text, specials) result(escaped)
   !< Return a string (an info string, a destination, a title) escaped for a place where backslash escapes and
   !< character references are read: a backslash before punctuation or at the end, `&` before a reference, and the
   !< characters of `specials`.
   character(*), intent(in)  :: text     !< String.
   character(*), intent(in)  :: specials !< Characters escaped wherever they are.
   character(:), allocatable :: escaped  !< Escaped string.
   integer(I4P)              :: i        !< Position.
   character(:), allocatable :: buf      !< Escaped bytes.
   integer(I4P)              :: n        !< Bytes written.
   logical                   :: esc      !< Escape.

   allocate(character(2 * len(text)) :: buf)
   n = 0_I4P
   do i=1, len(text, kind=I4P)
      esc = index(specials, text(i:i)) > 0
      if (text(i:i) == BACKSLASH) then
         esc = i == len(text, kind=I4P)
         if (.not. esc) esc = is_ascii_punctuation(text(i+1:i+1))
      elseif (text(i:i) == '&') then
         esc = scan_entity_like(text, i)
      endif
      if (esc) then
         n = n + 1_I4P
         buf(n:n) = BACKSLASH
      endif
      n = n + 1_I4P
      buf(n:n) = text(i:i)
   enddo
   escaped = buf(1:n)
   endfunction escape_string

   ! lines and prefixes
   subroutine emit_text(w, text, guard)
   !< Write a text as lines behind the prefixes (a final line feed ends the last line, it adds no empty one); with
   !< `guard`, a continuation line that would open a block is indented by 4 spaces (a definition's title, its label).
   type(md_writer), intent(inout) :: w     !< Writer.
   character(*),    intent(in)    :: text  !< Text.
   logical,         intent(in)    :: guard !< Guard continuation lines.
   integer(I4P)                   :: i, j  !< Bounds of a line.

   i = 1_I4P
   if (len(text) == 0) then
      call emit_line(w, '')
      return
   endif
   do while (i <= len(text, kind=I4P))
      j = index(text(i:), LF) + i - 1_I4P
      if (j < i) j = len(text, kind=I4P) + 1_I4P
      if (guard .and. i > 1_I4P .and. j > i) then
         if (block_start_escape(text(i:j-1), .true.) > 0_I4P) then
            call emit_line(w, '    '//text(i:j-1))
            i = j + 1_I4P
            cycle
         endif
      endif
      call emit_line(w, text(i:j-1))
      i = j + 1_I4P
   enddo
   endsubroutine emit_text

   subroutine emit_line(w, text)
   !< Write one line behind the prefixes of the open containers (each frame's first-line prefix once, its other-line
   !< prefix after); a blank line drops the prefixes' trailing white space.
   type(md_writer), intent(inout) :: w      !< Writer.
   character(*),    intent(in)    :: text   !< The line, its ending excluded.
   character(:), allocatable      :: prefix !< Prefix.
   integer(I4P)                   :: f      !< Frame.
   integer(I4P)                   :: k      !< Length.

   if (w%depth == 0_I4P) then
      prefix = ''
   elseif (w%pendings == 0_I4P) then
      prefix = w%rest_buf(1:w%rest_hi(w%depth))
   else
      k = 0_I4P
      do f=1, w%depth
         if (w%pending(f)) then
            k = k + w%first_hi(f) - w%first_lo(f) + 1_I4P
         else
            k = k + w%rest_hi(f) - w%rest_lo(f) + 1_I4P
         endif
      enddo
      allocate(character(k) :: prefix)
      k = 0_I4P
      do f=1, w%depth
         if (w%pending(f)) then
            prefix(k+1:k+w%first_hi(f)-w%first_lo(f)+1) = w%first_buf(w%first_lo(f):w%first_hi(f))
            k = k + w%first_hi(f) - w%first_lo(f) + 1_I4P
            w%pending(f) = .false.
         else
            prefix(k+1:k+w%rest_hi(f)-w%rest_lo(f)+1) = w%rest_buf(w%rest_lo(f):w%rest_hi(f))
            k = k + w%rest_hi(f) - w%rest_lo(f) + 1_I4P
         endif
      enddo
      w%pendings = 0_I4P
   endif
   if (len(text) == 0) then
      k = len(prefix, kind=I4P)
      do while (k > 0_I4P)
         if (.not. is_space_or_tab(prefix(k:k))) exit
         k = k - 1_I4P
      enddo
      call w%out%put(prefix(1:k)//LF)
   else
      call w%out%put(prefix//text//LF)
   endif
   endsubroutine emit_line

   subroutine push(w, first, rest, bullet)
   !< Open a container: its first line starts with `first`, the others with `rest`.
   type(md_writer), intent(inout) :: w      !< Writer.
   character(*),    intent(in)    :: first  !< First-line prefix.
   character(*),    intent(in)    :: rest   !< Other-line prefix.
   integer(I4P),    intent(in)    :: bullet !< The bullet of an item (`iachar`), 0 otherwise.
   integer(I4P)                   :: f, r   !< Bytes in use.

   if (w%depth == size(w%pending, kind=I4P)) then
      call grow(w%first_lo)
      call grow(w%first_hi)
      call grow(w%rest_lo)
      call grow(w%rest_hi)
      call grow(w%bullet)
      call grow_logical(w%pending)
   endif
   f = 0_I4P
   r = 0_I4P
   if (w%depth > 0_I4P) then
      f = w%first_hi(w%depth)
      r = w%rest_hi(w%depth)
   endif
   call ensure(w%first_buf, f + len(first, kind=I4P))
   call ensure(w%rest_buf, r + len(rest, kind=I4P))
   w%depth = w%depth + 1_I4P
   w%first_buf(f+1:f+len(first)) = first
   w%rest_buf(r+1:r+len(rest)) = rest
   w%first_lo(w%depth) = f + 1_I4P
   w%first_hi(w%depth) = f + len(first, kind=I4P)
   w%rest_lo(w%depth) = r + 1_I4P
   w%rest_hi(w%depth) = r + len(rest, kind=I4P)
   w%bullet(w%depth) = bullet
   w%pending(w%depth) = .true.
   w%pendings = w%pendings + 1_I4P
   endsubroutine push

   subroutine pop(w)
   !< Close the innermost container.
   type(md_writer), intent(inout) :: w !< Writer.

   if (w%depth == 0_I4P) return
   if (w%pending(w%depth)) w%pendings = w%pendings - 1_I4P
   w%depth = w%depth - 1_I4P
   endsubroutine pop

   ! extensions
   subroutine offer(doc, slots, node, entering, piece, handled, ierr, msg)
   !< Offer an event of a node to the enabled extensions' `write_markdown` hooks, in registration order.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering (else leaving).
   character(:), allocatable, intent(out)   :: piece    !< The Markdown of the extension that handled it.
   logical,                   intent(out)   :: handled  !< An extension handled it.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: s        !< Counter.

   handled = .false.
   piece = ''
   do s=1, size(slots, kind=I4P)
      if (.not. slots(s)%enabled) cycle
      if (.not. allocated(msg)) msg = ''
      call slots(s)%ext%write_markdown(doc, node, entering, piece, handled, ierr, msg)
      if (.not. allocated(piece)) piece = ''
      if (ierr /= BARUCH_OK) then
         msg = 'markdown_options%write_markdown: extension "'//slots(s)%ext%name()//'": '//msg
         handled = .false.
         return
      endif
      if (handled) return
   enddo
   piece = ''
   endsubroutine offer

   ! helpers
   function html_type(literal) result(htype)
   !< Return the type (1 to 7) of the HTML block a literal starts, 0 if none.
   character(*), intent(in) :: literal !< Literal.
   integer(I4P)             :: htype   !< Type.
   integer(I4P)             :: i, j    !< Bounds of its first line.

   j = index(literal, LF)
   if (j == 0) j = len(literal) + 1
   i = 1_I4P
   do while (i < j)
      if (literal(i:i) /= ' ') exit
      i = i + 1_I4P
   enddo
   htype = scan_html_block_start(literal(1:j-1), i, .true.)
   endfunction html_type

   function next_in(doc, n, top) result(next)
   !< Return the node after `n` in document order inside the subtree of `top` (0 after its last).
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Node.
   integer(I4P),            intent(in) :: top  !< Subtree root.
   integer(I4P)                        :: next !< Next node.

   next = doc%first_child(n)
   if (next /= 0_I4P) return
   next = n
   do while (next /= top .and. next /= 0_I4P)
      if (doc%next_sibling(next) /= 0_I4P) then
         next = doc%next_sibling(next)
         return
      endif
      next = doc%parent(next)
   enddo
   next = 0_I4P
   endfunction next_in

   function next_in_range(doc, n, first, last) result(next)
   !< Return the node after `n` in document order among the siblings `first` to `last` and their subtrees.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: n     !< Node.
   integer(I4P),            intent(in) :: first !< First sibling.
   integer(I4P),            intent(in) :: last  !< Last sibling.
   integer(I4P)                        :: next  !< Next node, 0 after the last.
   integer(I4P)                        :: top   !< Parent of the siblings.

   top = doc%parent(first)
   next = doc%first_child(n)
   if (next /= 0_I4P) return
   next = n
   do while (next /= top .and. next /= 0_I4P)
      if (next == last) then
         next = 0_I4P
         return
      endif
      if (doc%next_sibling(next) /= 0_I4P) then
         next = doc%next_sibling(next)
         return
      endif
      next = doc%parent(next)
   enddo
   next = 0_I4P
   endfunction next_in_range

   pure function count_char(text, c) result(k)
   !< Return how many times a character occurs in a text.
   character(*), intent(in) :: text !< Text.
   character,    intent(in) :: c    !< Character.
   integer(I4P)             :: k    !< Count.
   integer(I4P)             :: i    !< Position.

   k = 0_I4P
   do i=1, len(text, kind=I4P)
      if (text(i:i) == c) k = k + 1_I4P
   enddo
   endfunction count_char

   pure function utf8_length(lead) result(l)
   !< Return the bytes of the UTF-8 character a lead byte starts (1 for a continuation byte).
   character, intent(in) :: lead !< Lead byte.
   integer(I4P)          :: l    !< Bytes.
   integer               :: b    !< Its value.

   b = iachar(lead)
   if (b >= 240) then
      l = 4_I4P
   elseif (b >= 224) then
      l = 3_I4P
   elseif (b >= 192) then
      l = 2_I4P
   else
      l = 1_I4P
   endif
   endfunction utf8_length

   pure function utf8_at(text, i) result(c)
   !< Return the UTF-8 character starting at `text(i)`.
   character(*), intent(in)  :: text !< Text.
   integer(I4P), intent(in)  :: i    !< Position.
   character(:), allocatable :: c    !< Character.

   c = text(i:min(len(text, kind=I4P), i + utf8_length(text(i:i)) - 1_I4P))
   endfunction utf8_at

   pure function utf8_code(c) result(code)
   !< Return the code point of one UTF-8 character.
   character(*), intent(in) :: c    !< Character.
   integer(I4P)             :: code !< Code point.
   integer(I4P)             :: k    !< Byte.

   select case(len(c))
   case(2)
      code = iand(iachar(c(1:1), kind=I4P), 31_I4P)
   case(3)
      code = iand(iachar(c(1:1), kind=I4P), 15_I4P)
   case(4)
      code = iand(iachar(c(1:1), kind=I4P), 7_I4P)
   case default
      code = iachar(c(1:1), kind=I4P)
      return
   endselect
   do k=2, len(c, kind=I4P)
      code = code * 64_I4P + iand(iachar(c(k:k), kind=I4P), 63_I4P)
   enddo
   endfunction utf8_code

   elemental function char1(code) result(c)
   !< Return the ASCII character of a code as a one-byte result: nvfortran 26.9 gives a deferred-length variable
   !< assigned `achar(i)` (i not a constant) the length `i` (szaghi/BARUCH#2, reproducer
   !< notes/nvfortran-achar-deferred-length.f90); a fixed-length result has its length.
   integer(I4P), intent(in) :: code !< Code.
   character                :: c    !< Character.

   c = achar(code)
   endfunction char1

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr

   pure subroutine grow(a)
   !< Double an integer array.
   integer(I4P), allocatable, intent(inout) :: a(:) !< Array.
   integer(I4P), allocatable                :: b(:) !< Grown copy.

   allocate(b(2 * size(a)))
   b = 0_I4P
   b(1:size(a)) = a
   call move_alloc(b, a)
   endsubroutine grow

   pure subroutine grow_matrix(a)
   !< Double the columns of an integer matrix.
   integer(I4P), allocatable, intent(inout) :: a(:,:) !< Matrix.
   integer(I4P), allocatable                :: b(:,:) !< Grown copy.

   allocate(b(size(a, 1), 2 * size(a, 2)))
   b = 0_I4P
   b(:, 1:size(a, 2)) = a
   call move_alloc(b, a)
   endsubroutine grow_matrix

   pure subroutine grow_logical(a)
   !< Double a logical array.
   logical, allocatable, intent(inout) :: a(:) !< Array.
   logical, allocatable                :: b(:) !< Grown copy.

   allocate(b(2 * size(a)))
   b = .false.
   b(1:size(a)) = a
   call move_alloc(b, a)
   endsubroutine grow_logical

   pure subroutine ensure(buf, n)
   !< Make a buffer hold at least `n` bytes, keeping its content.
   character(:), allocatable, intent(inout) :: buf !< Buffer.
   integer(I4P),              intent(in)    :: n   !< Bytes needed.
   character(:), allocatable                :: big !< Grown copy.

   if (len(buf, kind=I4P) >= n) return
   allocate(character(max(2_I4P * len(buf, kind=I4P), n)) :: big)
   big(1:len(buf)) = buf
   buf = big ! not move_alloc: nvfortran 26.9 loses the length (notes/nvfortran-move-alloc-deferred-length-char.f90)
   endsubroutine ensure

   ! md_buffer
   pure subroutine put(self, bytes)
   !< Append bytes, growing the buffer by doubling.
   class(md_buffer), intent(inout) :: self  !< Buffer.
   character(*),     intent(in)    :: bytes !< Bytes.
   character(:), allocatable       :: big   !< Grown buffer.
   integer(I4P)                    :: n     !< Bytes.

   n = len(bytes, kind=I4P)
   if (n == 0_I4P) return
   if (.not. allocated(self%chars)) allocate(character(max(256_I4P, 2_I4P * n)) :: self%chars)
   if (self%used + n > len(self%chars, kind=I4P)) then
      allocate(character(max(2_I4P * len(self%chars, kind=I4P), self%used + n)) :: big)
      big(1:self%used) = self%chars(1:self%used)
      self%chars = big ! not move_alloc: nvfortran 26.9 loses the length (notes/nvfortran-move-alloc-deferred-length-char.f90)
   endif
   self%chars(self%used+1:self%used+n) = bytes
   self%used = self%used + n
   endsubroutine put

   pure function last_char(self) result(c)
   !< Return the last UTF-8 character written, empty if nothing is written or the last byte is a line feed.
   class(md_buffer), intent(in) :: self !< Buffer.
   character(:), allocatable    :: c    !< Character.
   integer(I4P)                 :: k    !< Its first byte.

   c = ''
   if (self%used == 0_I4P) return
   if (self%chars(self%used:self%used) == LF) return
   k = self%used
   do while (k > 1_I4P)
      if (iand(iachar(self%chars(k:k)), 192) /= 128) exit
      k = k - 1_I4P
   enddo
   c = self%chars(k:self%used)
   endfunction last_char
endmodule baruch_writer
