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
