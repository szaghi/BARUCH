call doc%load_file('page.md')
call options%parse(doc)
node = 0
do
   call options%walk(doc, node, event)
   if (node == 0) exit
   if (event /= EVENT_ENTER_SPAN) cycle
   kind = doc%node_kind(node)
   if (kind /= NODE_LINK .and. kind /= NODE_IMAGE) cycle
   print '(I2,2X,A6,1X,A10,1X,A)', line_of(doc%span_first(node)), doc%kind_name(kind), &
         form_name(doc%int_slot(node, SLOT_LINK_FORM)), doc%destination(node)
   print '(22X,A)', 'text  "'//text_of(node)//'"'
   if (len(doc%title(node)) > 0) print '(22X,A)', 'title "'//doc%title(node)//'"'
   if (len(doc%label(node)) > 0) print '(22X,A)', 'label "'//doc%label(node)//'"'
enddo
