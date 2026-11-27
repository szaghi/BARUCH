call options%render_html_file(doc, 'report.html', standalone=.true., stat=stat, errmsg=errmsg)
print '(A,I0)', 'report.html written, stat ', stat
