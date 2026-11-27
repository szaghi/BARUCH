call doc%load_file('report.md', stat=stat, errmsg=errmsg)
if (stat /= BARUCH_OK) then
   print '(A)', errmsg
   stop
endif
call options%parse(doc, stat=stat, errmsg=errmsg)
print '(A,I0,A,I0,A)', 'parse stat: ', stat, ', ', doc%nodes_number(), ' nodes'
