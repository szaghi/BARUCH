call doc%load_file('missing.md', stat=stat, errmsg=errmsg)
print '(A,L1)', 'a missing file is BARUCH_ERR_IO: ', stat == BARUCH_ERR_IO
