call options%disable('percent')
call options%parse(doc)
print '(A)', 'with "percent" disabled:'
call print_tree(doc)
