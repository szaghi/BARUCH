call inline_options%register(percent_extension())
call inline_options%register(mark_extension())
call page%load('Dear %name%, the ==deadline== is =Friday=, not ==Monday=.'//LF// &
               '%% a raw %name% line'//LF)
call inline_options%parse(page, stat=stat, errmsg=errmsg)
print '(A,I0)', 'parse stat: ', stat
call print_tree(page)
call inline_options%render_html(page, html)
write(*, '(A)', advance='no') html
