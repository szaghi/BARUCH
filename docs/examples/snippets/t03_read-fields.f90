function fields(n) result(line)
!< What a node records: its meaning, then the source style the parser kept.
integer, intent(in)       :: n    !< Node.
character(:), allocatable :: line !< Its fields.

line = ''
select case(doc%node_kind(n))
case(NODE_HEADING)
   line = ' level '//str(doc%heading_level(n))
   if (doc%int_slot(n, SLOT_HEADING_FORM) == HEADING_ATX) then
      line = line//', ATX, closing #s '//str(doc%int_slot(n, SLOT_HEADING_CLOSING))
   else
      line = line//', setext'
   endif
case(NODE_LIST)
   if (doc%list_type(n) == LIST_BULLET) then
      line = ' bullet "'//achar(doc%int_slot(n, SLOT_LIST_MARKER))//'"'
   else
      line = ' ordered from '//str(doc%list_start(n))//', delimiter "'//achar(doc%int_slot(n, SLOT_LIST_MARKER))//'"'
   endif
   line = line//merge(', tight', ', loose', doc%list_tight(n))
case(NODE_CODE_BLOCK)
   if (doc%int_slot(n, SLOT_CODE_FORM) == CODE_FENCED) then
      line = ' fenced '//repeat(achar(doc%int_slot(n, SLOT_CODE_FENCE_CHAR)), &
                                doc%int_slot(n, SLOT_CODE_FENCE_LENGTH))//', info "'//doc%info(n)//'"'
   else
      line = ' indented'
   endif
   line = line//', literal '//quoted(doc%literal(n))
case(NODE_THEMATIC_BREAK)
   line = ' "'//achar(doc%int_slot(n, SLOT_BREAK_CHAR))//'"'
case(NODE_LINK_REFERENCE_DEFINITION)
   line = ' label "'//doc%label(n)//'", destination "'//doc%destination(n)//'", title "'//doc%title(n)//'"'
case(NODE_HTML_BLOCK, NODE_TEXT)
   line = ' '//quoted(doc%literal(n))
endselect
endfunction fields
