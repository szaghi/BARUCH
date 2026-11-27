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
