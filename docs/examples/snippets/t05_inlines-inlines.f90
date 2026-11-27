block = doc%first_child(doc%root())
do while (block /= 0)
   call print_inlines(block)
   block = doc%next_sibling(block)
enddo
