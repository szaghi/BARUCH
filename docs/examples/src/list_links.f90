!run list_links list_links
program list_links
!< Cookbook: list every link and image of a page (inline, reference, autolink) with the line it is on, its form, its
!< destination, its title and its text, by walking the parsed tree.
use baruch
implicit none
type(markdown_document)   :: doc     !< The page.
type(markdown_options)    :: options !< CommonMark, no extension.
integer                   :: node    !< The walk's current node.
integer                   :: event   !< The walk's current event.
integer                   :: kind    !< The node's kind.

!region walk
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
!endregion walk

contains
   !region text
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
   !endregion text

   function form_name(form) result(name)
   !< The name of a link form.
   integer, intent(in)       :: form !< `SLOT_LINK_FORM`.
   character(:), allocatable :: name !< Name.

   select case(form)
   case(LINK_INLINE)    ; name = 'inline'
   case(LINK_FULL)      ; name = 'full'
   case(LINK_COLLAPSED) ; name = 'collapsed'
   case(LINK_SHORTCUT)  ; name = 'shortcut'
   case(LINK_AUTOLINK)  ; name = 'autolink'
   case default         ; name = '?'
   endselect
   endfunction form_name

   function line_of(byte) result(line)
   !< The line of the source holding a byte.
   integer, intent(in) :: byte !< Byte of the source.
   integer             :: line !< Line.

   line = 1
   do while (line < doc%lines_number())
      if (doc%line_first(line + 1) > byte) exit
      line = line + 1
   enddo
   endfunction line_of
endprogram list_links
