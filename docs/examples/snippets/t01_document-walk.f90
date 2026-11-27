subroutine print_tree()
!< Print the tree in document order, by its links: no recursion, no stack.
integer                   :: n     !< Node.
integer                   :: depth !< Its depth.
character(:), allocatable :: line  !< Its line.

n = doc%root()
depth = 0
do while (n /= 0)
   line = repeat('  ', depth)//doc%kind_name(doc%node_kind(n))
   select case(doc%node_kind(n))
   case(NODE_HEADING)
      line = line//' level='//str(doc%heading_level(n))
   case(NODE_LIST)
      line = line//' tight='//trim(merge('yes', 'no ', doc%list_tight(n)))//' marker='//achar(doc%int_slot(n, SLOT_LIST_MARKER))
   case(NODE_TEXT)
      line = line//' "'//doc%literal(n)//'"'
   endselect
   print '(A)', line
   ! next in document order: the first child, else the next sibling of the node or of its nearest ancestor
   if (doc%first_child(n) /= 0) then
      n = doc%first_child(n)
      depth = depth + 1
   else
      do while (n /= 0)
         if (doc%next_sibling(n) /= 0) then
            n = doc%next_sibling(n)
            exit
         endif
         n = doc%parent(n)
         depth = depth - 1
      enddo
   endif
enddo
endsubroutine print_tree
