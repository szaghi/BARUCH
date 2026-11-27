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
