call options%render_html(doc, html, stat=stat, errmsg=errmsg)
write(*, '(A)', advance='no') html
