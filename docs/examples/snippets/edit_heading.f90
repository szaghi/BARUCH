program edit_heading
!< Cookbook: change one heading of a page and write the page back: the diff is that one line.
use baruch
implicit none
type(markdown_document)   :: doc     !< The page.
type(markdown_options)    :: options !< CommonMark, no extension.
character(:), allocatable :: errmsg  !< Message.
integer                   :: node    !< A block of the page.
integer                   :: stat    !< Status.

call doc%load_file('guide.md')
call options%parse(doc)
node = doc%first_child(doc%root())
do while (node /= 0) ! the heading "Running"
   if (doc%node_kind(node) == NODE_HEADING) then
      if (doc%literal(doc%first_child(node)) == 'Running') exit
   endif
   node = doc%next_sibling(node)
enddo
call doc%set_literal(doc%first_child(node), 'Running the solver')
call options%write_markdown_file(doc, 'guide-new.md', stat=stat, errmsg=errmsg)
if (stat /= BARUCH_OK) print '(A)', errmsg
endprogram edit_heading
