! the second item of the ordered list: its span is its bytes in the source, nested list included
n = doc%first_child(doc%root())
do while (doc%node_kind(n) /= NODE_LIST)
   n = doc%next_sibling(n)
enddo
n = doc%next_sibling(doc%first_child(n))
print '(A)', 'the second item, bytes '//str(doc%span_first(n))//' to '//str(doc%span_last(n))//':'
print '(A)', doc%source_slice(doc%span_first(n), doc%span_last(n))
