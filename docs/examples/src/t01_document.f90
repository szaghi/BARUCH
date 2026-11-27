!run t01_document t01_document
program t01_document
!< Tutorial chapter 1: a document built by hand, printed by following its links, misused, edited and compacted.
use baruch
implicit none
type(markdown_document)   :: doc            !< The document.
integer                   :: h, p, e, l, t  !< Handles: heading, paragraph, emphasis, list, text.
integer                   :: item(2)        !< List items.
integer                   :: zero           !< The item added by the edit.
integer                   :: k              !< Counter.
integer                   :: level          !< Heading level.
integer                   :: stat           !< Status.
character(:), allocatable :: errmsg         !< Message.
integer,      allocatable :: map(:)         !< Old to new handles.

!region build
! # BARUCH
h = doc%new_node(NODE_HEADING)
call doc%set_heading_level(h, 1)
call doc%set_int_slot(h, SLOT_HEADING_FORM, HEADING_ATX)
call doc%append_child(doc%root(), h)
call add_text(h, 'BARUCH')
! A *flat* tree.
p = doc%new_node(NODE_PARAGRAPH)
call doc%append_child(doc%root(), p)
call add_text(p, 'A ')
e = doc%new_node(NODE_EMPH)
call doc%set_int_slot(e, SLOT_EMPHASIS_CHAR, iachar('*'))
call doc%append_child(p, e)
call add_text(e, 'flat')
call add_text(p, ' tree.')
! - one
! - two
l = doc%new_node(NODE_LIST)
call doc%set_list_type(l, LIST_BULLET)
call doc%set_list_tight(l, .true.)
call doc%set_int_slot(l, SLOT_LIST_MARKER, iachar('-'))
call doc%append_child(doc%root(), l)
do k=1, 2
   item(k) = doc%new_node(NODE_ITEM)
   call doc%append_child(l, item(k))
   p = doc%new_node(NODE_PARAGRAPH)
   call doc%append_child(item(k), p)
   call add_text(p, merge('one', 'two', k == 1))
enddo
!endregion build

print '(A)', 'The document:'
call print_tree
print '(A,I0,A)', 'nodes: ', doc%nodes_number(), new_line('a')

!region misuse
level = doc%heading_level(doc%first_child(item(1)), stat=stat, errmsg=errmsg)
print '(A,I0,A,I0)', 'heading_level of a paragraph: ', level, ', stat ', stat
print '(A)', errmsg
!endregion misuse
print '(A)', ''

!region edit
! a third item before the first, then the first unlinked: a tombstone until compact
zero = doc%new_node(NODE_ITEM)
call doc%prepend_child(l, zero)
p = doc%new_node(NODE_PARAGRAPH)
call doc%append_child(zero, p)
call add_text(p, 'zero')
call doc%unlink(item(1))
print '(A,I0,A,I0)', 'after unlink: nodes ', doc%nodes_number(), ', last handle ', doc%last_node()
print '(A,L1)', 'item(1) is still a node: ', doc%is_node(item(1))
call doc%compact(map=map)
print '(A,I0,A,I0)', 'after compact: nodes ', doc%nodes_number(), ', last handle ', doc%last_node()
print '(A,I0,A,I0)', 'the new item, handle ', zero, ', is now ', map(zero)
!endregion edit
print '(A)', ''
call print_tree

contains
   subroutine add_text(parent, literal)
   !< Append a text node to a parent.
   integer,      intent(in) :: parent  !< Parent.
   character(*), intent(in) :: literal !< Text.
   integer                  :: n       !< Text node.

   n = doc%new_node(NODE_TEXT)
   call doc%set_literal(n, literal)
   call doc%append_child(parent, n)
   endsubroutine add_text

   !region walk
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
   !endregion walk

   pure function str(i) result(s)
   !< An integer as text.
   integer, intent(in)       :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(11)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction str
endprogram t01_document
