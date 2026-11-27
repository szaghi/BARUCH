function text_of(top) result(text)
!< The text of a link or an image: the literals of the text and code nodes under it, walked with `top`.
integer, intent(in)       :: top   !< Link or image.
character(:), allocatable :: text  !< Text.
integer                   :: n     !< The inner walk's node.
integer                   :: e     !< The inner walk's event.

text = ''
n = 0
do
   call options%walk(doc, n, e, top=top)
   if (n == 0) exit
   if (e /= EVENT_TEXT) cycle
   select case(doc%node_kind(n))
   case(NODE_TEXT, NODE_CODE)
      text = text//doc%literal(n)
   case(NODE_SOFTBREAK, NODE_LINEBREAK)
      text = text//' '
   endselect
enddo
endfunction text_of
