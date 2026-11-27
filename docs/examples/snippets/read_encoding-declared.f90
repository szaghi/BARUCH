call doc%load_file('notes-1252.md', encoding='windows-1252', stat=stat, errmsg=errmsg)
if (stat /= BARUCH_OK) then
   print '(A)', errmsg
   stop 1
endif
do i=1, doc%lines_number()
   print '(I0,A,I0,A,I0,A)', i, ' (bytes ', doc%line_first(i), ' to ', doc%line_last(i), '): '//doc%line(i)
enddo
