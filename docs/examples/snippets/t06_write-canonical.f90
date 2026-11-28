call options%write_markdown(doc, markdown, canonical=.true.)
print '(A)', '----- canonical'
write(*, '(A)', advance='no') markdown
