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
