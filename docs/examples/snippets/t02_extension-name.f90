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
if (stat == 0) call doc%register_kind(NODE_PERCENT_NAME, 'percent_name', inline=.true., stat=stat, errmsg=errmsg)
endsubroutine percent_register_kinds
