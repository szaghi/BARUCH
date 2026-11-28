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
!<   `walk_event` contract (an event that is not one), mode 2 makes `write_html` fail, mode 3 `write_markdown`;
!< - `rule_extension` ("rule"): writes the core's thematic breaks as `<hr class="rule" />`;
!< - `xvar_extension` ("xvar"): an inline trigger, `%`: `%name%` (letters, digits, `_`) is an `xvar` (kind 1006, inline)
!<   whose literal is the name; its `post_process` numbers them in document order (attribute `index`) and unlinks the
!<   ones named `drop`; HTML `<var>name</var>`;
!< - `mark_extension` ("mark"): a delimiter character, `=`: a run of one or two `=` opens and closes as `*` does (three
!<   or more are text), an opener and a closer of the same length make a `mark` (kind 1007, inline) holding what is
!<   between them, as GFM strikethrough does with `~`; HTML `<mark>`;
!< - `title_extension` ("title"): a line `@@ text` is a `title` block (kind 1008) whose content is inline
!<   (`contains_inlines`): `block_close` gives it one text child, the inline parser reads it as a paragraph;
!< - `broken_inline_extension` (`mode` 1 to 12): each mode breaks the inline hooks' contract one way, or fails.
!<
!< The note writes itself as HTML (`<aside class="note">`); the percent block has no `write_html`, so the renderer's
!< default for an extension node applies to it.
use penf,   only : I4P
use baruch, only : markdown_document, markdown_extension, BARUCH_OK, BARUCH_ERR_ARGUMENT, NODE_PARAGRAPH, &
                   NODE_THEMATIC_BREAK, BLOCK_UNMATCHED, BLOCK_MATCHED, CONTENT_NONE, CONTENT_BLOCKS, CONTENT_LINES, &
                   EVENT_TEXT, NODE_TEXT
implicit none
private
public :: percent_extension, note_extension, caption_extension, broken_extension, clash_extension, math_extension
public :: rule_extension, xvar_extension, mark_extension, title_extension, broken_inline_extension
public :: KIND_PERCENT, KIND_NOTE, KIND_CAPTION, KIND_BROKEN, KIND_MATH, KIND_XVAR, KIND_MARK, KIND_TITLE

integer(I4P), parameter :: KIND_PERCENT = 1001_I4P !< `%%` block.
integer(I4P), parameter :: KIND_NOTE    = 1002_I4P !< `!>` note.
integer(I4P), parameter :: KIND_CAPTION = 1003_I4P !< `^^^` caption.
integer(I4P), parameter :: KIND_BROKEN  = 1004_I4P !< Blocks of the broken extension.
integer(I4P), parameter :: KIND_MATH    = 1005_I4P !< Inline math, built by hand.
integer(I4P), parameter :: KIND_XVAR    = 1006_I4P !< `%name%` inline.
integer(I4P), parameter :: KIND_MARK    = 1007_I4P !< `==marked==` inline.
integer(I4P), parameter :: KIND_TITLE   = 1008_I4P !< `@@` title block, inline content.
integer(I4P), parameter :: KIND_BROKEN_INLINE = 1009_I4P !< Inlines of the broken inline extension.
integer(I4P), parameter :: KIND_BROKEN_BLOCK  = 1010_I4P !< A block kind of the broken inline extension.
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
   integer(I4P) :: mode = 0_I4P !< 0 well behaved, 1 a walk event that is not one, 2 `write_html` fails, 3
                                !< `write_markdown` fails.
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

type, extends(markdown_extension) :: xvar_extension
   !< `%name%`: an inline trigger, a post-process.
   contains
      procedure, pass(self) :: name            => xvar_name
      procedure, pass(self) :: register_kinds  => xvar_register_kinds
      procedure, pass(self) :: inline_triggers => xvar_inline_triggers
      procedure, pass(self) :: inline_start    => xvar_inline_start
      procedure, pass(self) :: post_process    => xvar_post_process
      procedure, pass(self) :: write_html      => xvar_write_html
      procedure, pass(self) :: write_markdown  => xvar_write_markdown
endtype xvar_extension

type, extends(markdown_extension) :: mark_extension
   !< `=`/`==` runs: a delimiter character.
   contains
      procedure, pass(self) :: name            => mark_name
      procedure, pass(self) :: register_kinds  => mark_register_kinds
      procedure, pass(self) :: delimiter_chars => mark_delimiter_chars
      procedure, pass(self) :: delimiter_run   => mark_delimiter_run
      procedure, pass(self) :: delimiter_match => mark_delimiter_match
      procedure, pass(self) :: write_html      => mark_write_html
      procedure, pass(self) :: write_markdown  => mark_write_markdown
endtype mark_extension

type, extends(markdown_extension) :: title_extension
   !< `@@` lines: a block whose content is inline.
   contains
      procedure, pass(self) :: name             => title_name
      procedure, pass(self) :: register_kinds   => title_register_kinds
      procedure, pass(self) :: block_start      => title_block_start
      procedure, pass(self) :: block_content    => title_block_content
      procedure, pass(self) :: block_close      => title_block_close
      procedure, pass(self) :: contains_inlines => title_contains_inlines
      procedure, pass(self) :: write_markdown   => title_write_markdown
endtype title_extension

type, extends(markdown_extension) :: broken_inline_extension
   !< Breaks the inline contract at `%` (inline_start) or on `=` runs (delimiter_match): 1 a claim consuming nothing, 2
   !< more bytes consumed than the subject has, 3 a core kind returned, 4 a block kind returned, 5 a node not created
   !< by the call, 6 a core trigger declared, 7 a non-graphic delimiter declared, 8 more delimiters used than a run has,
   !< 9 a match with no node, 10 a match node with a child; 11 inline_start fails, 12 post_process fails.
   integer(I4P) :: mode = 1_I4P !< How it breaks the contract.
   contains
      procedure, pass(self) :: name            => broken_inline_name
      procedure, pass(self) :: register_kinds  => broken_inline_register_kinds
      procedure, pass(self) :: inline_triggers => broken_inline_triggers
      procedure, pass(self) :: inline_start    => broken_inline_start
      procedure, pass(self) :: delimiter_chars => broken_inline_delimiter_chars
      procedure, pass(self) :: delimiter_match => broken_inline_delimiter_match
      procedure, pass(self) :: post_process    => broken_inline_post_process
      procedure, pass(self) :: write_markdown  => broken_inline_write_markdown
endtype broken_inline_extension

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
   if (handled .and. entering) markdown = '!> ' ! the prefix of its children's lines
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
   if (handled .and. self%mode == 3_I4P) then
      stat = BARUCH_ERR_ARGUMENT
      errmsg = 'math refuses to be written'
   endif
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

   ! xvar
   function xvar_name(self) result(name)
   class(xvar_extension), intent(in) :: self !< Extension.
   character(:), allocatable         :: name !< Name.
   name = 'xvar'
   endfunction xvar_name

   subroutine xvar_register_kinds(self, doc, stat, errmsg)
   class(xvar_extension),     intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_XVAR, 'xvar', inline=.true., stat=stat, errmsg=errmsg)
   endsubroutine xvar_register_kinds

   function xvar_inline_triggers(self) result(chars)
   class(xvar_extension), intent(in) :: self  !< Extension.
   character(:), allocatable         :: chars !< Triggers.
   chars = '%'
   endfunction xvar_inline_triggers

   subroutine xvar_inline_start(self, doc, text, offset, node, consumed, stat, errmsg)
   class(xvar_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   character(*),              intent(in)    :: text     !< Subject.
   integer(I4P),              intent(in)    :: offset   !< Trigger position.
   integer(I4P),              intent(out)   :: node     !< New node, 0: not mine.
   integer(I4P),              intent(out)   :: consumed !< Bytes taken.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   integer(I4P)                             :: k        !< Position.

   node = 0_I4P
   consumed = 0_I4P
   k = offset + 1_I4P
   do while (k <= len(text, kind=I4P))
      if (verify(text(k:k), 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_') /= 0) exit
      k = k + 1_I4P
   enddo
   if (k > len(text, kind=I4P) .or. k == offset + 1_I4P) return
   if (text(k:k) /= '%') return
   node = doc%new_node(KIND_XVAR, stat=stat, errmsg=errmsg)
   if (stat == BARUCH_OK) call doc%set_literal(node, text(offset+1:k-1), stat=stat, errmsg=errmsg)
   consumed = k - offset + 1_I4P
   endsubroutine xvar_inline_start

   subroutine xvar_post_process(self, doc, stat, errmsg)
   !< Number the xvars in document order (attribute `index`), unlink the ones named `drop`: the tree by its links.
   class(xvar_extension),     intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   integer(I4P)                             :: n      !< Node.
   integer(I4P)                             :: next   !< Next node.
   integer(I4P)                             :: count  !< Xvars numbered.
   character(16)                            :: buf    !< Number.

   count = 0_I4P
   n = doc%root()
   do while (n /= 0_I4P .and. stat == BARUCH_OK)
      ! the next node in document order, found before `n` may be unlinked
      next = doc%first_child(n)
      if (next == 0_I4P) then
         next = n
         do while (next /= 0_I4P)
            if (doc%next_sibling(next) /= 0_I4P) then
               next = doc%next_sibling(next)
               exit
            endif
            next = doc%parent(next)
         enddo
      endif
      if (doc%node_kind(n) == KIND_XVAR) then
         if (doc%literal(n) == 'drop') then
            call doc%unlink(n, stat=stat, errmsg=errmsg)
         else
            count = count + 1_I4P
            write(buf, '(I0)') count
            call doc%set_attribute(n, 'index', trim(buf), stat=stat, errmsg=errmsg)
         endif
      endif
      n = next
   enddo
   endsubroutine xvar_post_process

   subroutine xvar_write_html(self, doc, node, entering, html, handled, stat, errmsg)
   class(xvar_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: html     !< HTML.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   html = ''
   handled = doc%node_kind(node) == KIND_XVAR
   if (handled .and. entering) html = '<var>'//doc%literal(node)//'</var>'
   endsubroutine xvar_write_html

   subroutine xvar_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(xvar_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = doc%node_kind(node) == KIND_XVAR
   if (handled .and. entering) markdown = '%'//doc%literal(node)//'%'
   endsubroutine xvar_write_markdown

   ! mark
   function mark_name(self) result(name)
   class(mark_extension), intent(in) :: self !< Extension.
   character(:), allocatable         :: name !< Name.
   name = 'mark'
   endfunction mark_name

   subroutine mark_register_kinds(self, doc, stat, errmsg)
   class(mark_extension),     intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_MARK, 'mark', inline=.true., stat=stat, errmsg=errmsg)
   endsubroutine mark_register_kinds

   function mark_delimiter_chars(self) result(chars)
   class(mark_extension), intent(in) :: self  !< Extension.
   character(:), allocatable         :: chars !< Delimiters.
   chars = '='
   endfunction mark_delimiter_chars

   subroutine mark_delimiter_run(self, doc, text, first, length, left_flanking, right_flanking, before_punct, &
                                 after_punct, can_open, can_close)
   class(mark_extension),   intent(in)  :: self           !< Extension.
   type(markdown_document), intent(in)  :: doc            !< Document.
   character(*),            intent(in)  :: text           !< Subject.
   integer(I4P),            intent(in)  :: first          !< First byte of the run.
   integer(I4P),            intent(in)  :: length         !< Its characters.
   logical,                 intent(in)  :: left_flanking  !< Left-flanking.
   logical,                 intent(in)  :: right_flanking !< Right-flanking.
   logical,                 intent(in)  :: before_punct   !< Punctuation before.
   logical,                 intent(in)  :: after_punct    !< Punctuation after.
   logical,                 intent(out) :: can_open       !< Can open.
   logical,                 intent(out) :: can_close      !< Can close.
   can_open = left_flanking .and. length <= 2_I4P
   can_close = right_flanking .and. length <= 2_I4P
   endsubroutine mark_delimiter_run

   subroutine mark_delimiter_match(self, doc, delimiter, opener_length, closer_length, opener_original, &
                                   closer_original, opener_can_close, closer_can_open, used, node, stat, errmsg)
   class(mark_extension),     intent(in)    :: self             !< Extension.
   type(markdown_document),   intent(inout) :: doc              !< Document.
   character,                 intent(in)    :: delimiter        !< Character.
   integer(I4P),              intent(in)    :: opener_length    !< Left in the opener.
   integer(I4P),              intent(in)    :: closer_length    !< Left in the closer.
   integer(I4P),              intent(in)    :: opener_original  !< Opener as read.
   integer(I4P),              intent(in)    :: closer_original  !< Closer as read.
   logical,                   intent(in)    :: opener_can_close !< Opener can close.
   logical,                   intent(in)    :: closer_can_open  !< Closer can open.
   integer(I4P),              intent(out)   :: used             !< Used from each.
   integer(I4P),              intent(out)   :: node             !< New node.
   integer(I4P),              intent(inout) :: stat             !< Status.
   character(:), allocatable, intent(inout) :: errmsg           !< Message.
   used = 0_I4P
   node = 0_I4P
   if (opener_length /= closer_length) return ! `=a==` is no mark
   used = opener_length
   node = doc%new_node(KIND_MARK, stat=stat, errmsg=errmsg)
   endsubroutine mark_delimiter_match

   subroutine mark_write_html(self, doc, node, entering, html, handled, stat, errmsg)
   class(mark_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: html     !< HTML.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   handled = doc%node_kind(node) == KIND_MARK
   html = merge('<mark> ', '</mark>', entering)
   html = trim(html)
   if (.not. handled) html = ''
   endsubroutine mark_write_html

   subroutine mark_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(mark_extension),     intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = doc%node_kind(node) == KIND_MARK
   if (handled) markdown = '=='
   endsubroutine mark_write_markdown

   ! title
   function title_name(self) result(name)
   class(title_extension), intent(in) :: self !< Extension.
   character(:), allocatable          :: name !< Name.
   name = 'title'
   endfunction title_name

   subroutine title_register_kinds(self, doc, stat, errmsg)
   class(title_extension),    intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status.
   character(:), allocatable, intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_TITLE, 'title', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine title_register_kinds

   subroutine title_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                                paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(title_extension),    intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< Line.
   integer(I4P),              intent(in)    :: line_number        !< Its number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it.
   integer(I4P),              intent(in)    :: container          !< Host block.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines, first bytes.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines, last bytes.
   integer(I4P),              intent(out)   :: node               !< New block.
   integer(I4P),              intent(out)   :: consumed           !< Bytes taken.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.
   integer(I4P)                             :: i                  !< First non-space.

   node = 0_I4P
   consumed = 0_I4P
   paragraph_taken = 0_I4P
   if (indent >= 4_I4P) return
   i = skip_spaces(line, offset)
   if (.not. starts(line, i, '@@ ')) return
   consumed = skip_spaces(line, i + 3_I4P) - offset
   node = doc%new_node(KIND_TITLE, stat=stat, errmsg=errmsg)
   endsubroutine title_block_start

   function title_block_content(self, kind) result(content)
   class(title_extension), intent(in) :: self    !< Extension.
   integer(I4P),           intent(in) :: kind    !< Kind.
   integer(I4P)                       :: content !< Content.
   content = CONTENT_LINES
   endfunction title_block_content

   subroutine title_block_close(self, doc, node, lines_first, lines_last, stat, errmsg)
   !< One text child holding the line's content, spanning its bytes: the inline parser reads it.
   class(title_extension),    intent(in)    :: self           !< Extension.
   type(markdown_document),   intent(inout) :: doc            !< Document.
   integer(I4P),              intent(in)    :: node           !< The block.
   integer(I4P),              intent(in)    :: lines_first(:) !< First bytes.
   integer(I4P),              intent(in)    :: lines_last(:)  !< Last bytes.
   integer(I4P),              intent(inout) :: stat           !< Status.
   character(:), allocatable, intent(inout) :: errmsg         !< Message.
   integer(I4P)                             :: t              !< Text child.

   if (size(lines_first) == 0) return
   if (lines_last(1) < lines_first(1)) return
   t = doc%new_node(NODE_TEXT, stat=stat, errmsg=errmsg)
   if (stat == BARUCH_OK) call doc%set_literal(t, doc%source_slice(lines_first(1), lines_last(1)), stat=stat, &
                                               errmsg=errmsg)
   if (stat == BARUCH_OK) call doc%set_span(t, lines_first(1), lines_last(1), stat=stat, errmsg=errmsg)
   if (stat == BARUCH_OK) call doc%append_child(node, t, stat=stat, errmsg=errmsg)
   endsubroutine title_block_close

   function title_contains_inlines(self, kind) result(inlines)
   class(title_extension), intent(in) :: self    !< Extension.
   integer(I4P),           intent(in) :: kind    !< Kind.
   logical                            :: inlines !< Inline content.
   inlines = kind == KIND_TITLE
   endfunction title_contains_inlines

   subroutine title_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(title_extension),    intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering.
   character(:), allocatable, intent(out)   :: markdown !< Markdown.
   logical,                   intent(out)   :: handled  !< Mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   handled = doc%node_kind(node) == KIND_TITLE
   markdown = ''
   if (handled .and. entering) markdown = '@@ ' ! the prefix of its inline content's line
   endsubroutine title_write_markdown

   ! broken inline
   function broken_inline_name(self) result(name)
   class(broken_inline_extension), intent(in) :: self !< Extension.
   character(:), allocatable                  :: name !< Name.
   name = 'broken_inline'
   endfunction broken_inline_name

   subroutine broken_inline_register_kinds(self, doc, stat, errmsg)
   class(broken_inline_extension), intent(in)    :: self   !< Extension.
   type(markdown_document),        intent(inout) :: doc    !< Document.
   integer(I4P),                   intent(inout) :: stat   !< Status.
   character(:), allocatable,      intent(inout) :: errmsg !< Message.
   call doc%register_kind(KIND_BROKEN_INLINE, 'broken_inline', inline=.true., stat=stat, errmsg=errmsg)
   if (stat == BARUCH_OK) call doc%register_kind(KIND_BROKEN_BLOCK, 'broken_block', inline=.false., stat=stat, &
                                                 errmsg=errmsg)
   endsubroutine broken_inline_register_kinds

   function broken_inline_triggers(self) result(chars)
   class(broken_inline_extension), intent(in) :: self  !< Extension.
   character(:), allocatable                  :: chars !< Triggers.
   chars = '%'
   if (self%mode == 6_I4P) chars = '%*'
   endfunction broken_inline_triggers

   subroutine broken_inline_start(self, doc, text, offset, node, consumed, stat, errmsg)
   class(broken_inline_extension), intent(in)    :: self     !< Extension.
   type(markdown_document),        intent(inout) :: doc      !< Document.
   character(*),                   intent(in)    :: text     !< Subject.
   integer(I4P),                   intent(in)    :: offset   !< Trigger position.
   integer(I4P),                   intent(out)   :: node     !< New node.
   integer(I4P),                   intent(out)   :: consumed !< Bytes taken.
   integer(I4P),                   intent(inout) :: stat     !< Status.
   character(:), allocatable,      intent(inout) :: errmsg   !< Message.

   node = 0_I4P
   consumed = 1_I4P
   select case(self%mode)
   case(1_I4P)
      node = doc%new_node(KIND_BROKEN_INLINE)
      consumed = 0_I4P
   case(2_I4P)
      node = doc%new_node(KIND_BROKEN_INLINE)
      consumed = len(text, kind=I4P) - offset + 2_I4P
   case(3_I4P)
      node = doc%new_node(NODE_TEXT)
   case(4_I4P)
      node = doc%new_node(KIND_BROKEN_BLOCK)
   case(5_I4P)
      node = doc%root()
   case(11_I4P)
      stat = BARUCH_ERR_ARGUMENT
      errmsg = 'asked to fail'
   endselect
   endsubroutine broken_inline_start

   function broken_inline_delimiter_chars(self) result(chars)
   class(broken_inline_extension), intent(in) :: self  !< Extension.
   character(:), allocatable                  :: chars !< Delimiters.
   chars = '='
   if (self%mode == 7_I4P) chars = '= '
   endfunction broken_inline_delimiter_chars

   subroutine broken_inline_delimiter_match(self, doc, delimiter, opener_length, closer_length, opener_original, &
                                            closer_original, opener_can_close, closer_can_open, used, node, stat, errmsg)
   class(broken_inline_extension), intent(in)    :: self             !< Extension.
   type(markdown_document),        intent(inout) :: doc              !< Document.
   character,                      intent(in)    :: delimiter        !< Character.
   integer(I4P),                   intent(in)    :: opener_length    !< Left in the opener.
   integer(I4P),                   intent(in)    :: closer_length    !< Left in the closer.
   integer(I4P),                   intent(in)    :: opener_original  !< Opener as read.
   integer(I4P),                   intent(in)    :: closer_original  !< Closer as read.
   logical,                        intent(in)    :: opener_can_close !< Opener can close.
   logical,                        intent(in)    :: closer_can_open  !< Closer can open.
   integer(I4P),                   intent(out)   :: used             !< Used from each.
   integer(I4P),                   intent(out)   :: node             !< New node.
   integer(I4P),                   intent(inout) :: stat             !< Status.
   character(:), allocatable,      intent(inout) :: errmsg           !< Message.
   integer(I4P)                                  :: child            !< A child.

   used = min(opener_length, closer_length)
   node = doc%new_node(KIND_BROKEN_INLINE)
   select case(self%mode)
   case(8_I4P)
      used = used + 1_I4P
   case(9_I4P)
      call doc%unlink(node)
      node = 0_I4P
   case(10_I4P)
      child = doc%new_node(NODE_TEXT)
      call doc%append_child(node, child)
   endselect
   endsubroutine broken_inline_delimiter_match

   subroutine broken_inline_post_process(self, doc, stat, errmsg)
   class(broken_inline_extension), intent(in)    :: self   !< Extension.
   type(markdown_document),        intent(inout) :: doc    !< Document.
   integer(I4P),                   intent(inout) :: stat   !< Status.
   character(:), allocatable,      intent(inout) :: errmsg !< Message.
   if (self%mode /= 12_I4P) return
   stat = BARUCH_ERR_ARGUMENT
   errmsg = 'asked to fail'
   endsubroutine broken_inline_post_process

   subroutine broken_inline_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
   class(broken_inline_extension), intent(in)    :: self     !< Extension.
   type(markdown_document),        intent(in)    :: doc      !< Document.
   integer(I4P),                   intent(in)    :: node     !< Node.
   logical,                        intent(in)    :: entering !< Entering.
   character(:), allocatable,      intent(out)   :: markdown !< Markdown.
   logical,                        intent(out)   :: handled  !< Mine.
   integer(I4P),                   intent(inout) :: stat     !< Status.
   character(:), allocatable,      intent(inout) :: errmsg   !< Message.
   markdown = ''
   handled = .false.
   endsubroutine broken_inline_write_markdown
endmodule baruch_test_extensions
