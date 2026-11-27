program t03_read
!< Tutorial chapter 3: a Markdown file parsed, its tree printed with what each block records, a span read back.
use baruch
implicit none
type(markdown_document)   :: doc     !< The document.
type(markdown_options)    :: options !< What the parse does: CommonMark, no extension.
integer                   :: stat    !< Status.
character(:), allocatable :: errmsg  !< Message.
integer                   :: n       !< A node.

call doc%load_file('report.md', stat=stat, errmsg=errmsg)
if (stat /= BARUCH_OK) then
   print '(A)', errmsg
   stop
endif
call options%parse(doc, stat=stat, errmsg=errmsg)
print '(A,I0,A,I0,A)', 'parse stat: ', stat, ', ', doc%nodes_number(), ' nodes'
print '(A)', ''
call print_tree
print '(A)', ''

! the second item of the ordered list: its span is its bytes in the source, nested list included
n = doc%first_child(doc%root())
do while (doc%node_kind(n) /= NODE_LIST)
   n = doc%next_sibling(n)
enddo
n = doc%next_sibling(doc%first_child(n))
print '(A)', 'the second item, bytes '//str(doc%span_first(n))//' to '//str(doc%span_last(n))//':'
print '(A)', doc%source_slice(doc%span_first(n), doc%span_last(n))

contains
   subroutine print_tree()
   !< Print the tree in document order, by its links (no recursion), with what each kind records.
   integer :: n     !< Node.
   integer :: depth !< Its depth.

   n = doc%root()
   depth = 0
   do while (n /= 0)
      print '(A)', repeat('  ', depth)//doc%kind_name(doc%node_kind(n))//fields(n)
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

   function quoted(text) result(q)
   !< A text between quotes, its line feeds shown as \n.
   character(*), intent(in)  :: text !< Text.
   character(:), allocatable :: q    !< Quoted text.
   integer                   :: i    !< Counter.

   q = '"'
   do i=1, len(text)
      if (text(i:i) == achar(10)) then
         q = q//achar(92)//'n'
      else
         q = q//text(i:i)
      endif
   enddo
   q = q//'"'
   endfunction quoted

   pure function str(i) result(s)
   !< An integer as text.
   integer, intent(in)       :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(11)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction str
endprogram t03_read
