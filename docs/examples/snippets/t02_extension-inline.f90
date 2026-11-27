function percent_inline_triggers(self) result(chars)
class(percent_extension), intent(in) :: self  !< Extension.
character(:), allocatable            :: chars !< Bytes handing it an inline position.

chars = '%'
endfunction percent_inline_triggers

subroutine percent_inline_start(self, doc, text, offset, node, consumed, stat, errmsg)
class(percent_extension),  intent(in)    :: self     !< Extension.
type(markdown_document),   intent(inout) :: doc      !< Document.
character(*),              intent(in)    :: text     !< The block's inline content.
integer(I4P),              intent(in)    :: offset   !< Where a `%` stands.
integer(I4P),              intent(out)   :: node     !< New inline node, 0: not mine.
integer(I4P),              intent(out)   :: consumed !< Bytes of text(offset:) taken.
integer(I4P),              intent(inout) :: stat     !< Status.
character(:), allocatable, intent(inout) :: errmsg   !< Message.
integer                                  :: k        !< Bytes of the name.

node = 0
consumed = 0
k = verify(text(offset+1:)//'%', 'abcdefghijklmnopqrstuvwxyz_') - 1 ! a lowercase name
if (k == 0 .or. offset + k + 1 > len(text)) return
if (text(offset+k+1:offset+k+1) /= '%') return ! `%` then a name then `%`, else not mine: text
node = doc%new_node(NODE_PERCENT_NAME, stat=stat, errmsg=errmsg)
if (stat == 0) call doc%set_literal(node, text(offset+1:offset+k), stat=stat, errmsg=errmsg)
consumed = k + 2
endsubroutine percent_inline_start
