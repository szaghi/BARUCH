subroutine walk_outline()
!< The walk: the headings with their bytes in the source, and how many events of each kind.
integer :: node   !< Current node.
integer :: event  !< Current event.
integer :: first  !< First byte of the node's span.
integer :: last   !< Last byte.
integer :: counts(5) !< Events of each kind.

counts = 0
node = 0
do
   call options%walk(doc, node, event, first=first, last=last)
   if (node == 0) exit
   counts(event) = counts(event) + 1
   if (event == EVENT_ENTER_BLOCK .and. doc%node_kind(node) == NODE_HEADING) &
      print '(A,I0,A,I0,A,I0,A)', 'heading ', doc%heading_level(node), ' at bytes ', first, '-', last, ': "'// &
            doc%literal(doc%first_child(node))//'"'
enddo
print '(A,I0,A,I0,A,I0)', 'blocks entered ', counts(EVENT_ENTER_BLOCK), ', left ', counts(EVENT_LEAVE_BLOCK), &
                          ', texts ', counts(EVENT_TEXT)
endsubroutine walk_outline
