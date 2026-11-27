program t05_inlines
!< Tutorial chapter 5: the inline content of a parsed document, node by node with its bytes in the source, and as HTML.
use baruch
implicit none
character, parameter      :: LF = achar(10)  !< Line feed.
character, parameter      :: BS = achar(92)  !< Backslash.
type(markdown_document)   :: doc     !< The document.
type(markdown_options)    :: options !< What the parse does: CommonMark, no extension.
character(:), allocatable :: source  !< The Markdown.
character(:), allocatable :: html    !< The HTML.
integer                   :: block   !< A block.

source = '# Run `42` &mdash; done'//LF//                                 &
         LF//                                                          &
         '> Set `dt = 1e-3` in <b>solver.ini</b>, see'//LF//            &
         '> <https://example.org/dt?step=1&amp;max=9>  '//LF//          &
         '> or write to <help@example.org>.'//LF//                      &
         LF//                                                          &
         'Not code: '//BS//'`dt'//BS//'`, not a tag: 3 < 4 &copy;'//BS//LF// &
         'a hard break above, a soft one below'//LF//                  &
         'and a comment: <!-- the end -->'//LF
print '(A)', source
call doc%load(source)
call options%parse(doc)

block = doc%first_child(doc%root())
do while (block /= 0)
   call print_inlines(block)
   block = doc%next_sibling(block)
enddo

call options%render_html(doc, html)
print '(A)', ''
write(*, '(A)', advance='no') html

contains
   subroutine print_inlines(block)
   !< Print the inline nodes of a paragraph or a heading (a block quote: of its paragraph) with their bytes.
   integer, intent(in) :: block !< Block.
   integer             :: b     !< The block holding the inline nodes.
   integer             :: n     !< An inline node.

   b = block
   if (doc%node_kind(b) == NODE_BLOCK_QUOTE) b = doc%first_child(b)
   print '(A)', doc%kind_name(doc%node_kind(b))
   n = doc%first_child(b)
   do while (n /= 0)
      print '(A)', '  '//pad(doc%kind_name(doc%node_kind(n)))//pad(at(n), 9)//what(n)
      n = doc%next_sibling(n)
   enddo
   endsubroutine print_inlines

   function what(n) result(text)
   !< What an inline node holds: a literal, or a link's destination and text.
   integer, intent(in)       :: n    !< Node.
   character(:), allocatable :: text !< Description.

   select case(doc%node_kind(n))
   case(NODE_TEXT, NODE_CODE, NODE_HTML_INLINE)
      text = '"'//doc%literal(n)//'"'
   case(NODE_LINK)
      text = '-> '//doc%destination(n)//'  "'//doc%literal(doc%first_child(n))//'"'
   case default
      text = ''
   endselect
   endfunction what

   function at(n) result(text)
   !< The bytes of a node in the source, `first-last`.
   integer, intent(in)       :: n    !< Node.
   character(:), allocatable :: text !< Bytes.
   character(24)             :: buf  !< Buffer.

   write(buf, '(I0,A,I0)') doc%span_first(n), '-', doc%span_last(n)
   text = trim(buf)
   endfunction at

   function pad(text, width) result(padded)
   !< A text padded with blanks to a width (default 12), followed by one blank.
   character(*), intent(in)           :: text   !< Text.
   integer,      intent(in), optional :: width  !< Width.
   character(:), allocatable          :: padded !< Padded text.
   integer                            :: w      !< Width.

   w = 12
   if (present(width)) w = width
   padded = text//repeat(' ', max(0, w - len(text)))//' '
   endfunction pad
endprogram t05_inlines
