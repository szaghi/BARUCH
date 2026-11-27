n = doc%next_sibling(doc%first_child(doc%root()))
block
   type(percent_extension) :: percent !< The extension, to call one of its hooks.

   call percent%write_markdown(doc, n, .true., markdown, handled, stat, errmsg)
endblock
print '(A)', 'the block written back:'
write(*, '(A)', advance='no') markdown
