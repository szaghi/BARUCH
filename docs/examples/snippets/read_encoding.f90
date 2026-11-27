program read_encoding
!< Cookbook: read a Markdown file written in windows-1252 into a document, whose source is UTF-8.
use baruch
implicit none
type(markdown_document)   :: doc    !< The document.
character(:), allocatable :: errmsg !< Message.
integer                   :: stat   !< Status.
integer                   :: i      !< Line.

call doc%load_file('notes-1252.md', encoding='windows-1252', stat=stat, errmsg=errmsg)
if (stat /= BARUCH_OK) then
   print '(A)', errmsg
   stop 1
endif
do i=1, doc%lines_number()
   print '(I0,A,I0,A,I0,A)', i, ' (bytes ', doc%line_first(i), ' to ', doc%line_last(i), '): '//doc%line(i)
enddo
print '(A)', ''

call doc%load_file('notes-1252.md')
print '(A,I0)', 'read as UTF-8, replacement characters (U+FFFD): ', count_replacements(doc%source())

call doc%load_file('missing.md', stat=stat, errmsg=errmsg)
print '(A,L1)', 'a missing file is BARUCH_ERR_IO: ', stat == BARUCH_ERR_IO

contains
   pure function count_replacements(text) result(n)
   !< Count the U+FFFD (bytes EF BF BD in UTF-8) of a text.
   character(*), intent(in) :: text !< Text.
   integer                  :: n    !< Count.
   integer                  :: k    !< Where the search starts.
   integer                  :: j    !< Where it finds one.

   n = 0
   k = 1
   do
      j = index(text(k:), char(239)//char(191)//char(189))
      if (j == 0) exit
      n = n + 1
      k = k + j + 2
   enddo
   endfunction count_replacements
endprogram read_encoding
