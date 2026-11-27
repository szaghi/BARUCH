call options%register(careless_extension())
call options%parse(doc, stat=stat, errmsg=errmsg)
print '(A,I0,A,L1)', 'stat: ', stat, ', BARUCH_ERR_CONTRACT: ', stat == BARUCH_ERR_CONTRACT
print '(A)', errmsg
