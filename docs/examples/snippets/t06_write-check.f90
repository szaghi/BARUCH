call again%load(markdown)
call options%parse(again)
print '(A,L1)', 'reads back to the same tree: ', doc%equal(again, difference=difference)
