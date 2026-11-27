call doc%load('A paragraph.'//LF//                     &
              '%% raw line, *not* emphasis'//LF//      &
              '  %%   indented marker'//LF//           &
              LF//                                     &
              'Another paragraph.'//LF)
call options%register(percent_extension())
call options%parse(doc, stat=stat, errmsg=errmsg)
print '(A,I0)', 'parse stat: ', stat
call print_tree
