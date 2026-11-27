level = doc%heading_level(doc%first_child(item(1)), stat=stat, errmsg=errmsg)
print '(A,I0,A,I0)', 'heading_level of a paragraph: ', level, ', stat ', stat
print '(A)', errmsg
