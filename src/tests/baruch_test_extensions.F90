!< BARUCH test extensions: block extensions written through the public interface only, for the contract test.
module baruch_test_extensions
!< BARUCH test extensions: block extensions written through the public interface only, for the contract test.
!<
!< - `percent_extension` ("percent"): consecutive lines starting with `%%` are one `percent_block` (kind 1001), a leaf
!<   taking lines; its literal is the lines after the `%%`, joined by LF;
!< - `note_extension` ("note"): lines starting with `!>` are a `note` (kind 1002), a container like a block quote that
!<   cannot hold another note;
!< - `caption_extension` ("caption"): a line `^^^` under a paragraph turns the paragraph's last line into a `caption`
!<   (kind 1003), a one-line leaf: the block-start hook reads the open paragraph's lines;
!< - `broken_extension` (`mode` 1 to 6): each mode breaks the hook contract one way;
!< - `clash_extension` ("clash"): registers kind 1001 under another name;
!< - `math_extension` ("math"): an inline kind `math` (1005), never parsed (no inline hooks before step 2.5), built by
!<   hand: the walk reports it as a text, the HTML is `<span class="math">` around its literal; `mode` 1 breaks the
!<   `walk_event` contract (an event that is not one), mode 2 makes `write_html` fail;
!< - `rule_extension` ("rule"): writes the core's thematic breaks as `<hr class="rule" />`.
!<
!< The note writes itself as HTML (`<aside class="note">`); the percent block has no `write_html`, so the renderer's
!< default for an extension node applies to it.
use penf,   only : I4P
use baruch, only : markdown_document, markdown_extension, BARUCH_OK, BARUCH_ERR_ARGUMENT, NODE_PARAGRAPH, &
                   NODE_THEMATIC_BREAK, BLOCK_UNMATCHED, BLOCK_MATCHED, CONTENT_NONE, CONTENT_BLOCKS, CONTENT_LINES, &
                   EVENT_TEXT
implicit none
private
public :: percent_extension, note_extension, caption_extension, broken_extension, clash_extension, math_extension
public :: rule_extension
public :: KIND_PERCENT, KIND_NOTE, KIND_CAPTION, KIND_BROKEN, KIND_MATH

integer(I4P), parameter :: KIND_PERCENT = 1001_I4P !< `%%` block.
integer(I4P), parameter :: KIND_NOTE    = 1002_I4P !< `!>` note.
integer(I4P), parameter :: KIND_CAPTION = 1003_I4P !< `^^^` caption.
integer(I4P), parameter :: KIND_BROKEN  = 1004_I4P !< Blocks of the broken extension.
integer(I4P), parameter :: KIND_MATH    = 1005_I4P !< Inline math, built by hand.
character,    parameter :: LF = achar(10)          !< Line feed.

type, extends(markdown_extension) :: percent_extension
   !< `%%` lines: a leaf taking lines.
   contains
      procedure, pass(self) :: name           => percent_name
      procedure, pass(self) :: register_kinds => percent_register_kinds
      procedure, pass(self) :: block_start    => percent_block_start
      procedure, pass(self) :: block_continue => percent_block_continue
      procedure, pass(self) :: block_content  => percent_block_content
      procedure, pass(self) :: block_close    => percent_block_close
      procedure, pass(self) :: write_markdown => percent_write_markdown
endtype percent_extension

type, extends(markdown_extension) :: note_extension
   !< `!>` lines: a container.
   contains
      procedure, pass(self) :: name           => note_name
      procedure, pass(self) :: register_kinds => note_register_kinds
      procedure, pass(self) :: block_start    => note_block_start
      procedure, pass(self) :: block_continue => note_block_continue
      procedure, pass(self) :: block_content  => note_block_content
      procedure, pass(self) :: can_contain    => note_can_contain
      procedure, pass(self) :: write_html     => note_write_html
      procedure, pass(self) :: write_markdown => note_write_markdown
endtype note_extension

type, extends(markdown_extension) :: caption_extension
   !< `^^^` under a paragraph: its last line becomes a caption.
   contains
      procedure, pass(self) :: name           => caption_name
      procedure, pass(self) :: register_kinds => caption_register_kinds
      procedure, pass(self) :: block_start    => caption_block_start
      procedure, pass(self) :: write_markdown => caption_write_markdown
endtype caption_extension

type, extends(markdown_extension) :: broken_extension
   !< Breaks the contract on lines starting with `!!`: 1 a container claimed without consuming, 2 more bytes consumed
   !< than the line has, 3 more paragraph lines taken than the paragraph has, 4 a core kind returned, 5 a node already
   !< linked returned, 6 a continuation consuming more than the line has.
   integer(I4P) :: mode = 1_I4P !< How it breaks the contract.
   contains
      procedure, pass(self) :: name           => broken_name
      procedure, pass(self) :: register_kinds => broken_register_kinds
      procedure, pass(self) :: block_start    => broken_block_start
      procedure, pass(self) :: block_continue => broken_block_continue
      procedure, pass(self) :: block_content  => broken_block_content
      procedure, pass(self) :: write_markdown => broken_write_markdown
endtype broken_extension

type, extends(markdown_extension) :: clash_extension
   !< Registers the percent kind's number under another name.
   contains
      procedure, pass(self) :: name           => clash_name
      procedure, pass(self) :: register_kinds => clash_register_kinds
      procedure, pass(self) :: write_markdown => clash_write_markdown
endtype clash_extension

type, extends(markdown_extension) :: math_extension
   !< Inline math, built by hand: a text for the walk, a span of class math in HTML.
   integer(I4P) :: mode = 0_I4P !< 0 well behaved, 1 a walk event that is not one, 2 `write_html` fails.
   contains
      procedure, pass(self) :: name           => math_name
      procedure, pass(self) :: register_kinds => math_register_kinds
      procedure, pass(self) :: write_html     => math_write_html
      procedure, pass(self) :: write_markdown => math_write_markdown
      procedure, pass(self) :: walk_event     => math_walk_event
endtype math_extension

type, extends(markdown_extension) :: rule_extension
   !< Thematic breaks (a core kind) written its own way.
   contains
      procedure, pass(self) :: name           => rule_name
      procedure, pass(self) :: write_html     => rule_write_html
      procedure, pass(self) :: write_markdown => rule_write_markdown
endtype rule_extension

contains
   ! helpers
   pure function skip_spaces(line, offset) result(i)
   !< Return the first byte of `line` at or after `offset` that is not a space.
   character(*), intent(in) :: line   !< Line.
   integer(I4P), intent(in) :: offset !< Start.
   integer(I4P)             :: i      !< First non-space.

   i = offset
   do while (i <= len(line, kind=I4P))
      if (line(i:i) /= ' ') exit
      i = i + 1_I4P
   enddo
   endfunction skip_spaces

   pure function starts(line, i, marker) result(is)
   !< Return true if `line(i:)` starts with `marker`.
   character(*), intent(in) :: line   !< Line.
   integer(I4P), intent(in) :: i      !< Position.
   character(*), intent(in) :: marker !< Marker.
   logical                  :: is     !< Result.

   is = .false.
   if (i + len(marker) - 1 <= len(line)) is = line(i:i+len(marker)-1) == marker
   endfunction starts

   subroutine markdown_lines(text, prefix, markdown)
   !< Write each line of `text` behind `prefix`, one LF after each.
   character(*),              intent(in)  :: text     !< Text.
   character(*),              intent(in)  :: prefix   !< Prefix.
   character(:), allocatable, intent(out) :: markdown !< Markdown.
   integer                                :: i, j     !< Bounds of a line.

   markdown = ''
   i = 1
   do
      j = index(text(i:), LF)
      if (j == 0) exit
      markdown = markdown//prefix//text(i:i+j-2)//LF
      i = i + j
   enddo
   markdown = markdown//prefix//text(i:)//LF
   endsubroutine markdown_lines

   ! percent
   function percent_name(self) result(name)
   class(percent_extension), intent(in) :: self !< Extension.
   character(:), allocatable            :: name !< Name.
   name = 'percent'
   endfunction percent_name

   subroutine percent_register_kinds(self, doc, stat, errmsg)
   class(percent_extension),  intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_PERCENT, 'percent_block', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine percent_register_kinds

   subroutine percent_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                                  paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(percent_extension),  intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< Line.
   integer(I4P),              intent(in)    :: line_number        !< Line number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it.
   integer(I4P),              intent(in)    :: container          !< Block it opens in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines.
   integer(I4P),              intent(out)   :: node               !< New block.
   integer(I4P),              intent(out)   :: consumed           !< Bytes consumed.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.
   integer(I4P)                             :: i                  !< First non-space.

   node = 0_I4P
   consumed = 0_I4P
   paragraph_taken = 0_I4P
   if (indent >= 4_I4P) return
   i = skip_spaces(line, offset)
   if (.not. starts(line, i, '%%')) return
   node = doc%new_node(KIND_PERCENT, stat=stat, errmsg=errmsg)
   consumed = i + 2_I4P - offset
   endsubroutine percent_block_start

   subroutine percent_block_continue(self, doc, node, line, line_number, offset, column, indent, match, consumed, &
                                     stat, errmsg)
   class(percent_extension),  intent(in)    :: self        !< Extension.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: node        !< Open block.
   character(*),              intent(in)    :: line        !< Line.
   integer(I4P),              intent(in)    :: line_number !< Line number.
   integer(I4P),              intent(in)    :: offset      !< First byte not consumed.
   integer(I4P),              intent(in)    :: column      !< Its column.
   integer(I4P),              intent(in)    :: indent      !< White space after it.
   integer(I4P),              intent(out)   :: match       !< Answer.
   integer(I4P),              intent(out)   :: consumed    !< Bytes consumed.
   integer(I4P),              intent(inout) :: stat        !< Status.
   character(:), allocatable, intent(inout) :: errmsg      !< Message.
   integer(I4P)                             :: i           !< First non-space.

   match = BLOCK_UNMATCHED
   consumed = 0_I4P
   if (indent >= 4_I4P) return
   i = skip_spaces(line, offset)
   if (.not. starts(line, i, '%%')) return
   match = BLOCK_MATCHED
   consumed = i + 2_I4P - offset
   endsubroutine percent_block_continue

   function percent_block_content(self, kind) result(content)
   class(percent_extension), intent(in) :: self    !< Extension.
   integer(I4P),             intent(in) :: kind    !< Kind.
   integer(I4P)                         :: content !< Content.
   content = CONTENT_LINES
   endfunction percent_block_content

   subroutine percent_block_close(self, doc, node, lines_first, lines_last, stat, errmsg)
   class(percent_extension),  intent(in)    :: self           !< Extension.
   type(markdown_document),   intent(inout) :: doc            !< Document.
   integer(I4P),              intent(in)    :: node           !< Block.
   integer(I4P),              intent(in)    :: lines_first(:) !< Its lines.
   integer(I4P),              intent(in)    :: lines_last(:)  !< Its lines.
   integer(I4P),              intent(inout) :: stat           !< Status.
   character(:), allocatable, intent(inout) :: errmsg         !< Message.
   character(:), allocatable                :: text           !< Literal.
   integer                                  :: i              !< Counter.

   text = ''
   do i=1, size(lines_first)
      if (i > 1) text = text//LF
      text = text//doc%source_slice(lines_first(i), lines_last(i))
   enddo
   call doc%set_literal(node, text, stat=stat, errmsg=errmsg)
   endsubroutine percent_block_close

   subroutine percent_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(percent_extension),  intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.

   markdown = ''
   handled = doc%node_kind(node) == KIND_PERCENT
   if (handled .and. entering) call markdown_lines(doc%literal(node), '%%', markdown)
   endsubroutine percent_write_markdown

   ! note
   function note_name(self) result(name)
   class(note_extension), intent(in) :: self !< Extension.
   character(:), allocatable         :: name !< Name.
   name = 'note'
   endfunction note_name

   subroutine note_register_kinds(self, doc, stat, errmsg)
   class(note_extension),     intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_NOTE, 'note', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine note_register_kinds

   subroutine note_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                               paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(note_extension),     intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< Line.
   integer(I4P),              intent(in)    :: line_number        !< Line number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it.
   integer(I4P),              intent(in)    :: container          !< Block it opens in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines.
   integer(I4P),              intent(out)   :: node               !< New block.
   integer(I4P),              intent(out)   :: consumed           !< Bytes consumed.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.

   node = 0_I4P
   paragraph_taken = 0_I4P
   call note_marker(line, offset, indent, consumed)
   if (consumed > 0_I4P) node = doc%new_node(KIND_NOTE, stat=stat, errmsg=errmsg)
   endsubroutine note_block_start

   subroutine note_block_continue(self, doc, node, line, line_number, offset, column, indent, match, consumed, &
                                  stat, errmsg)
   class(note_extension),     intent(in)    :: self        !< Extension.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: node        !< Open block.
   character(*),              intent(in)    :: line        !< Line.
   integer(I4P),              intent(in)    :: line_number !< Line number.
   integer(I4P),              intent(in)    :: offset      !< First byte not consumed.
   integer(I4P),              intent(in)    :: column      !< Its column.
   integer(I4P),              intent(in)    :: indent      !< White space after it.
   integer(I4P),              intent(out)   :: match       !< Answer.
   integer(I4P),              intent(out)   :: consumed    !< Bytes consumed.
   integer(I4P),              intent(inout) :: stat        !< Status.
   character(:), allocatable, intent(inout) :: errmsg      !< Message.

   call note_marker(line, offset, indent, consumed)
   match = merge(BLOCK_MATCHED, BLOCK_UNMATCHED, consumed > 0_I4P)
   endsubroutine note_block_continue

   pure subroutine note_marker(line, offset, indent, consumed)
   !< The bytes of a `!>` marker (with up to 3 spaces before it and 1 after) at `line(offset:)`, 0 if none.
   character(*), intent(in)  :: line     !< Line.
   integer(I4P), intent(in)  :: offset   !< First byte not consumed.
   integer(I4P), intent(in)  :: indent   !< White space after it.
   integer(I4P), intent(out) :: consumed !< Bytes of the marker.
   integer(I4P)              :: i        !< First non-space.

   consumed = 0_I4P
   if (indent >= 4_I4P) return
   i = skip_spaces(line, offset)
   if (.not. starts(line, i, '!>')) return
   consumed = i + 2_I4P - offset
   if (starts(line, i + 2_I4P, ' ')) consumed = consumed + 1_I4P
   endsubroutine note_marker

   function note_block_content(self, kind) result(content)
   class(note_extension), intent(in) :: self    !< Extension.
   integer(I4P),          intent(in) :: kind    !< Kind.
   integer(I4P)                      :: content !< Content.
   content = CONTENT_BLOCKS
   endfunction note_block_content

   function note_can_contain(self, kind, child_kind) result(can)
   class(note_extension), intent(in) :: self       !< Extension.
   integer(I4P),          intent(in) :: kind       !< Kind.
   integer(I4P),          intent(in) :: child_kind !< Child kind.
   logical                           :: can        !< Result.
   can = child_kind /= KIND_NOTE
   endfunction note_can_contain

   subroutine note_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(note_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = doc%node_kind(node) == KIND_NOTE
   endsubroutine note_write_markdown

   subroutine note_write_html(self, doc, node, entering, html, handled, stat, errmsg)
   class(note_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: html     !< HTML.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   handled = doc%node_kind(node) == KIND_NOTE
   if (entering) then
      html = '<aside class="note">'//LF
   else
      html = '</aside>'//LF
   endif
   endsubroutine note_write_html

   ! caption
   function caption_name(self) result(name)
   class(caption_extension), intent(in) :: self !< Extension.
   character(:), allocatable            :: name !< Name.
   name = 'caption'
   endfunction caption_name

   subroutine caption_register_kinds(self, doc, stat, errmsg)
   class(caption_extension),  intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_CAPTION, 'caption', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine caption_register_kinds

   subroutine caption_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                                  paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(caption_extension),  intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< Line.
   integer(I4P),              intent(in)    :: line_number        !< Line number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it.
   integer(I4P),              intent(in)    :: container          !< Block it opens in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines.
   integer(I4P),              intent(out)   :: node               !< New block.
   integer(I4P),              intent(out)   :: consumed           !< Bytes consumed.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.
   integer(I4P)                             :: i, n               !< Positions.
   character(:), allocatable                :: last               !< The paragraph's last line.

   node = 0_I4P
   consumed = 0_I4P
   paragraph_taken = 0_I4P
   if (paragraph == 0_I4P .or. indent >= 4_I4P) return
   i = skip_spaces(line, offset)
   if (line(i:) /= '^^^') return
   n = size(paragraph_first, kind=I4P)
   last = adjustl(doc%source_slice(paragraph_first(n), paragraph_last(n)))
   node = doc%new_node(KIND_CAPTION, stat=stat, errmsg=errmsg)
   if (stat == BARUCH_OK) call doc%set_literal(node, trim(last), stat=stat, errmsg=errmsg)
   consumed = len(line, kind=I4P) - offset + 1_I4P
   paragraph_taken = 1_I4P
   endsubroutine caption_block_start

   subroutine caption_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(caption_extension),  intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = doc%node_kind(node) == KIND_CAPTION
   if (handled .and. entering) markdown = doc%literal(node)//LF//'^^^'//LF
   endsubroutine caption_write_markdown

   ! broken
   function broken_name(self) result(name)
   class(broken_extension), intent(in) :: self !< Extension.
   character(:), allocatable           :: name !< Name.
   name = 'broken'
   endfunction broken_name

   subroutine broken_register_kinds(self, doc, stat, errmsg)
   class(broken_extension),   intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_BROKEN, 'broken', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine broken_register_kinds

   subroutine broken_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                                 paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(broken_extension),   intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< Line.
   integer(I4P),              intent(in)    :: line_number        !< Line number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it.
   integer(I4P),              intent(in)    :: container          !< Block it opens in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines.
   integer(I4P),              intent(out)   :: node               !< New block.
   integer(I4P),              intent(out)   :: consumed           !< Bytes consumed.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.
   integer(I4P)                             :: i                  !< First non-space.

   node = 0_I4P
   consumed = 0_I4P
   paragraph_taken = 0_I4P
   i = skip_spaces(line, offset)
   if (.not. starts(line, i, '!!')) return
   select case(self%mode)
   case(1) ! a container claimed without consuming: offered the same position forever
      node = doc%new_node(KIND_BROKEN, stat=stat, errmsg=errmsg)
   case(2) ! more than the line
      node = doc%new_node(KIND_BROKEN, stat=stat, errmsg=errmsg)
      consumed = len(line, kind=I4P) - offset + 2_I4P
   case(3) ! more paragraph lines than the paragraph has
      node = doc%new_node(KIND_BROKEN, stat=stat, errmsg=errmsg)
      consumed = 2_I4P
      paragraph_taken = size(paragraph_first, kind=I4P) + 1_I4P
   case(4) ! a core kind
      node = doc%new_node(NODE_PARAGRAPH, stat=stat, errmsg=errmsg)
      consumed = 2_I4P
   case(5) ! a node already in the tree
      node = doc%root()
      consumed = 2_I4P
   case(6) ! fine here, broken in block_continue
      node = doc%new_node(KIND_BROKEN, stat=stat, errmsg=errmsg)
      consumed = len(line, kind=I4P) - offset + 1_I4P
   endselect
   endsubroutine broken_block_start

   subroutine broken_block_continue(self, doc, node, line, line_number, offset, column, indent, match, consumed, &
                                    stat, errmsg)
   class(broken_extension),   intent(in)    :: self        !< Extension.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: node        !< Open block.
   character(*),              intent(in)    :: line        !< Line.
   integer(I4P),              intent(in)    :: line_number !< Line number.
   integer(I4P),              intent(in)    :: offset      !< First byte not consumed.
   integer(I4P),              intent(in)    :: column      !< Its column.
   integer(I4P),              intent(in)    :: indent      !< White space after it.
   integer(I4P),              intent(out)   :: match       !< Answer.
   integer(I4P),              intent(out)   :: consumed    !< Bytes consumed.
   integer(I4P),              intent(inout) :: stat        !< Status.
   character(:), allocatable, intent(inout) :: errmsg      !< Message.

   match = BLOCK_MATCHED
   consumed = len(line, kind=I4P) - offset + 2_I4P
   endsubroutine broken_block_continue

   function broken_block_content(self, kind) result(content)
   class(broken_extension), intent(in) :: self    !< Extension.
   integer(I4P),            intent(in) :: kind    !< Kind.
   integer(I4P)                        :: content !< Content.
   content = CONTENT_BLOCKS
   if (self%mode == 6) content = CONTENT_NONE
   endfunction broken_block_content

   subroutine broken_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(broken_extension),   intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = .false.
   endsubroutine broken_write_markdown

   ! clash
   function clash_name(self) result(name)
   class(clash_extension), intent(in) :: self !< Extension.
   character(:), allocatable          :: name !< Name.
   name = 'clash'
   endfunction clash_name

   subroutine clash_register_kinds(self, doc, stat, errmsg)
   class(clash_extension),    intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_PERCENT, 'not_percent', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine clash_register_kinds

   subroutine clash_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(clash_extension),    intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = .false.
   endsubroutine clash_write_markdown

   ! math
   function math_name(self) result(name)
   class(math_extension), intent(in) :: self !< Extension.
   character(:), allocatable         :: name !< Name.
   name = 'math'
   endfunction math_name

   subroutine math_register_kinds(self, doc, stat, errmsg)
   class(math_extension),     intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_MATH, 'math', inline=.true., stat=stat, errmsg=errmsg)
   endsubroutine math_register_kinds

   subroutine math_write_html(self, doc, node, entering, html, handled, stat, errmsg)
   class(math_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: html     !< HTML.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   handled = doc%node_kind(node) == KIND_MATH
   if (.not. handled) return
   if (self%mode == 2_I4P) then
      stat = BARUCH_ERR_ARGUMENT
      errmsg = 'math: asked to fail'
      return
   endif
   if (entering) html = '<span class="math">'//doc%literal(node)//'</span>'
   endsubroutine math_write_html

   subroutine math_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(math_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = doc%node_kind(node) == KIND_MATH
   if (handled .and. entering) markdown = '$'//doc%literal(node)//'$'
   endsubroutine math_write_markdown

   subroutine math_walk_event(self, doc, node, entering, event, handled)
   class(math_extension),   intent(in)  :: self     !< Extension.
   type(markdown_document), intent(in)  :: doc      !< Document.
   integer(I4P),            intent(in)  :: node     !< Node.
   logical,                 intent(in)  :: entering !< Entering.
   integer(I4P),            intent(out) :: event    !< Event.
   logical,                 intent(out) :: handled  !< Mine.
   event = EVENT_TEXT
   if (self%mode == 1_I4P) event = 99_I4P
   handled = doc%node_kind(node) == KIND_MATH .and. entering
   endsubroutine math_walk_event

   ! rule
   function rule_name(self) result(name)
   class(rule_extension), intent(in) :: self !< Extension.
   character(:), allocatable         :: name !< Name.
   name = 'rule'
   endfunction rule_name

   subroutine rule_write_html(self, doc, node, entering, html, handled, stat, errmsg)
   class(rule_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: html     !< HTML.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   handled = doc%node_kind(node) == NODE_THEMATIC_BREAK
   html = ''
   if (handled .and. entering) html = '<hr class="rule" />'//LF
   endsubroutine rule_write_html

   subroutine rule_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(rule_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = .false.
   endsubroutine rule_write_markdown
endmodule baruch_test_extensions
