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
