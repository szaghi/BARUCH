module percent_blocks
!< Tutorial chapter 2: an extension making the lines that start with `%%` one raw block.
use baruch
use penf, only : I4P
implicit none
private
public :: percent_extension, careless_extension, NODE_PERCENT

integer(I4P), parameter :: NODE_PERCENT = 1001_I4P !< The kind of a `%%` block.
character,    parameter :: LF = achar(10)          !< Line feed.

type, extends(markdown_extension) :: percent_extension
   !< `%%` lines: one block taking the rest of each line.
   contains
      procedure, pass(self) :: name           => percent_name
      procedure, pass(self) :: register_kinds => percent_register_kinds
      procedure, pass(self) :: block_start    => percent_block_start
      procedure, pass(self) :: block_continue => percent_block_continue
      procedure, pass(self) :: block_content  => percent_block_content
      procedure, pass(self) :: block_close    => percent_block_close
      procedure, pass(self) :: write_markdown => percent_write_markdown
endtype percent_extension

type, extends(percent_extension) :: careless_extension
   !< The same, but its block holds blocks and its start consumes nothing: it breaks the contract.
   contains
      procedure, pass(self) :: name          => careless_name
      procedure, pass(self) :: block_start   => careless_block_start
      procedure, pass(self) :: block_content => careless_block_content
endtype careless_extension

contains
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

   call doc%register_kind(NODE_PERCENT, 'percent_block', inline=.false., stat=stat, errmsg=errmsg)
   endsubroutine percent_register_kinds

   subroutine percent_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                                  paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(percent_extension),  intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< The line.
   integer(I4P),              intent(in)    :: line_number        !< Its number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it, in columns.
   integer(I4P),              intent(in)    :: container          !< Block it would open in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph, 0 if none.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines, first bytes.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines, last bytes.
   integer(I4P),              intent(out)   :: node               !< New block, 0: not mine.
   integer(I4P),              intent(out)   :: consumed           !< Bytes of line(offset:) taken.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.

   node = 0
   paragraph_taken = 0
   consumed = marker(line, offset, indent)
   if (consumed > 0) node = doc%new_node(NODE_PERCENT, stat=stat, errmsg=errmsg)
   endsubroutine percent_block_start

   subroutine percent_block_continue(self, doc, node, line, line_number, offset, column, indent, match, consumed, &
                                     stat, errmsg)
   class(percent_extension),  intent(in)    :: self        !< Extension.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: node        !< The open block.
   character(*),              intent(in)    :: line        !< The line.
   integer(I4P),              intent(in)    :: line_number !< Its number.
   integer(I4P),              intent(in)    :: offset      !< First byte not consumed.
   integer(I4P),              intent(in)    :: column      !< Its column.
   integer(I4P),              intent(in)    :: indent      !< White space after it, in columns.
   integer(I4P),              intent(out)   :: match       !< Does the line continue the block.
   integer(I4P),              intent(out)   :: consumed    !< Bytes of line(offset:) taken.
   integer(I4P),              intent(inout) :: stat        !< Status.
   character(:), allocatable, intent(inout) :: errmsg      !< Message.

   consumed = marker(line, offset, indent)
   match = merge(BLOCK_MATCHED, BLOCK_UNMATCHED, consumed > 0)
   endsubroutine percent_block_continue

   pure function marker(line, offset, indent) result(bytes)
   !< The bytes of a `%%` marker at line(offset:), up to 3 spaces before it; 0 if there is none.
   character(*), intent(in) :: line   !< The line.
   integer(I4P), intent(in) :: offset !< First byte not consumed.
   integer(I4P), intent(in) :: indent !< White space after it, in columns.
   integer(I4P)             :: bytes  !< Bytes of the marker.
   integer(I4P)             :: i      !< First byte after the spaces.

   bytes = 0
   if (indent >= 4) return ! an indented code block's business
   i = verify(line(offset:)//'x', ' ') + offset - 1
   if (line(i:min(i + 1, len(line))) == '%%') bytes = i + 2 - offset
   endfunction marker

   function percent_block_content(self, kind) result(content)
   class(percent_extension), intent(in) :: self    !< Extension.
   integer(I4P),             intent(in) :: kind    !< One of its kinds.
   integer(I4P)                         :: content !< What the block holds.

   content = CONTENT_LINES
   endfunction percent_block_content

   subroutine percent_block_close(self, doc, node, lines_first, lines_last, stat, errmsg)
   class(percent_extension),  intent(in)    :: self           !< Extension.
   type(markdown_document),   intent(inout) :: doc            !< Document.
   integer(I4P),              intent(in)    :: node           !< The block.
   integer(I4P),              intent(in)    :: lines_first(:) !< Its lines, first bytes.
   integer(I4P),              intent(in)    :: lines_last(:)  !< Its lines, last bytes.
   integer(I4P),              intent(inout) :: stat           !< Status.
   character(:), allocatable, intent(inout) :: errmsg         !< Message.
   character(:), allocatable                :: text           !< The block's text.
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
   logical,                   intent(in)    :: entering !< Entering it, else leaving it.
   character(:), allocatable, intent(out)   :: markdown !< Its Markdown.
   logical,                   intent(out)   :: handled  !< The node is mine.
   integer(I4P),              intent(inout) :: stat     !< Status.
   character(:), allocatable, intent(inout) :: errmsg   !< Message.
   character(:), allocatable                :: text     !< The block's text.
   integer                                  :: i        !< Counter.

   markdown = ''
   handled = doc%node_kind(node) == NODE_PERCENT
   if (.not. (handled .and. entering)) return
   text = doc%literal(node)
   markdown = '%%'
   do i=1, len(text)
      markdown = markdown//text(i:i)
      if (text(i:i) == LF) markdown = markdown//'%%'
   enddo
   markdown = markdown//LF
   endsubroutine percent_write_markdown

   function careless_name(self) result(name)
   class(careless_extension), intent(in) :: self !< Extension.
   character(:), allocatable             :: name !< Name.

   name = 'careless'
   endfunction careless_name

   subroutine careless_block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                                   paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   class(careless_extension), intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< The line.
   integer(I4P),              intent(in)    :: line_number        !< Its number.
   integer(I4P),              intent(in)    :: offset             !< First byte not consumed.
   integer(I4P),              intent(in)    :: column             !< Its column.
   integer(I4P),              intent(in)    :: indent             !< White space after it, in columns.
   integer(I4P),              intent(in)    :: container          !< Block it would open in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph, 0 if none.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< Its lines, first bytes.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Its lines, last bytes.
   integer(I4P),              intent(out)   :: node               !< New block, 0: not mine.
   integer(I4P),              intent(out)   :: consumed           !< Bytes of line(offset:) taken.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Paragraph lines taken.
   integer(I4P),              intent(inout) :: stat               !< Status.
   character(:), allocatable, intent(inout) :: errmsg             !< Message.

   node = 0
   consumed = 0 ! the bug: the marker is recognised but not consumed
   paragraph_taken = 0
   if (marker(line, offset, indent) > 0) node = doc%new_node(NODE_PERCENT, stat=stat, errmsg=errmsg)
   endsubroutine careless_block_start

   function careless_block_content(self, kind) result(content)
   class(careless_extension), intent(in) :: self    !< Extension.
   integer(I4P),              intent(in) :: kind    !< One of its kinds.
   integer(I4P)                          :: content !< What the block holds.

   content = CONTENT_BLOCKS
   endfunction careless_block_content
endmodule percent_blocks

program t02_extension
!< Tutorial chapter 2: write an extension, register it, parse with it, break its contract.
use baruch
use percent_blocks
implicit none
character, parameter      :: LF = achar(10) !< Line feed.
type(markdown_document)   :: doc            !< The document.
type(markdown_options)    :: options        !< What a parse does.
integer                   :: n              !< A node.
integer                   :: stat           !< Status.
character(:), allocatable :: errmsg         !< Message.
character(:), allocatable :: markdown       !< Markdown written by the extension.
logical                   :: handled        !< The node was the extension's.

call doc%load('A paragraph.'//LF//                     &
              '%% raw line, *not* emphasis'//LF//      &
              '  %%   indented marker'//LF//           &
              LF//                                     &
              'Another paragraph.'//LF)
call options%register(percent_extension())
call options%parse(doc, stat=stat, errmsg=errmsg)
print '(A,I0)', 'parse stat: ', stat
call print_tree
print '(A)', ''

n = doc%next_sibling(doc%first_child(doc%root()))
block
   type(percent_extension) :: percent !< The extension, to call one of its hooks.

   call percent%write_markdown(doc, n, .true., markdown, handled, stat, errmsg)
endblock
print '(A)', 'the block written back:'
write(*, '(A)', advance='no') markdown
print '(A)', ''

call options%disable('percent')
call options%parse(doc)
print '(A)', 'with "percent" disabled:'
call print_tree
print '(A)', ''

call options%register(careless_extension())
call options%parse(doc, stat=stat, errmsg=errmsg)
print '(A,I0,A,L1)', 'stat: ', stat, ', BARUCH_ERR_CONTRACT: ', stat == BARUCH_ERR_CONTRACT
print '(A)', errmsg

contains
   subroutine print_tree()
   !< Print the tree in document order, by its links: no recursion, no stack.
   integer                   :: n     !< Node.
   integer                   :: depth !< Its depth.
   character(:), allocatable :: line  !< Its line.

   n = doc%root()
   depth = 0
   do while (n /= 0)
      line = repeat('  ', depth)//doc%kind_name(doc%node_kind(n))
      if (doc%node_kind(n) == NODE_TEXT .or. doc%node_kind(n) == NODE_PERCENT) &
         line = line//' "'//escaped(doc%literal(n))//'"'
      print '(A)', line
      if (doc%first_child(n) /= 0) then
         n = doc%first_child(n)
         depth = depth + 1
      else
         do while (n /= 0)
            if (doc%next_sibling(n) /= 0) then
               n = doc%next_sibling(n)
               exit
            endif
            n = doc%parent(n)
            depth = depth - 1
         enddo
      endif
   enddo
   endsubroutine print_tree

   pure function escaped(text) result(s)
   !< A text with its line feeds shown as \n.
   character(*), intent(in)  :: text !< Text.
   character(:), allocatable :: s    !< Shown.
   integer                   :: i    !< Counter.

   s = ''
   do i=1, len(text)
      if (text(i:i) == LF) then
         s = s//'\n'
      else
         s = s//text(i:i)
      endif
   enddo
   endfunction escaped
endprogram t02_extension
