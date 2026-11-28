!run equal equal
program equal_example
!< Reference, Equality: two documents compared by their meaning, the source style ignored; the first difference named
!< by its path.
use baruch
implicit none
character, parameter   :: LF = achar(10) !< Line feed.
type(markdown_options) :: options        !< CommonMark, no extension.

!region pairs
call compare('emphasis', '*Hello*, world', '_Hello_, world')
call compare('heading and bullets', '# Title'//LF//LF//'- one'//LF//'- two', &
                                    'Title'//LF//'====='//LF//LF//'+ one'//LF//'+ two')
call compare('strong for emphasis', '- one'//LF//LF//'1. *two*', '- one'//LF//LF//'1. **two**')
call compare('a definition', '[BARUCH](https://github.com/szaghi/BARUCH)', &
                             '[BARUCH]'//LF//LF//'[baruch]: https://github.com/szaghi/BARUCH')
!endregion pairs

contains
   !region compare
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
   !endregion compare
endprogram equal_example
