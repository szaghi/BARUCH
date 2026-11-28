subroutine compare(label, x, y)
!< Parse two sources and say whether they mean the same, or where they first differ.
character(*), intent(in)  :: label      !< What the pair shows.
character(*), intent(in)  :: x          !< First source.
character(*), intent(in)  :: y          !< Second source.
type(markdown_document)   :: a          !< First document.
type(markdown_document)   :: b          !< Second document.
character(:), allocatable :: difference !< Where and what.

call a%load(x)
call options%parse(a)
call b%load(y)
call options%parse(b)
if (a%equal(b, difference=difference)) then
   print '(A)', label//': equal'
else
   print '(A)', label//': '//difference
endif
endsubroutine compare
