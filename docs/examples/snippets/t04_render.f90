program t04_render
!< Tutorial chapter 4: a parsed document written as an HTML fragment, as a standalone page into a file, and read as the
!< walk's events.
use baruch
implicit none
type(markdown_document)   :: doc     !< The document.
type(markdown_options)    :: options !< What the parse and the renderer do: CommonMark, no extension.
character(:), allocatable :: html    !< The HTML.
integer                   :: stat    !< Status.
character(:), allocatable :: errmsg  !< Message.

call doc%load_file('report.md', stat=stat, errmsg=errmsg)
if (stat /= BARUCH_OK) then
   print '(A)', errmsg
   stop
endif
call options%parse(doc)

call options%render_html(doc, html, stat=stat, errmsg=errmsg)
write(*, '(A)', advance='no') html
print '(A)', ''

call options%render_html_file(doc, 'report.html', standalone=.true., stat=stat, errmsg=errmsg)
print '(A,I0)', 'report.html written, stat ', stat
print '(A)', ''

call walk_outline

contains
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
endprogram t04_render
