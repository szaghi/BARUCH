!< BARUCH test: the CommonMark block parser, the trees it builds, the style it records, deep nesting.
program baruch_test_blocks
!< BARUCH test: the CommonMark block parser, the trees it builds, the style it records, deep nesting.
!<
!< The gate of plan step 1.5 beside the conformance suite (`scripts/spec.sh commonmark --blocks-only`, which checks the
!< HTML): here the tree itself. Every style slot the parser records (bullet, ordered delimiter, ATX or setext and the
!< closing `#`s, fence character and length, thematic break character), the strings (code literal and info string,
!< HTML literal, link reference definitions with their label, destination and title), spans, tabs consumed in part,
!< lazy continuation lines, tight and loose lists, a parsed tree free of tombstones, and a 10^5-deep `>` nesting (and as
!< many nested list items) parsed within the default stack (the parse is iterative; the test runs under `ulimit -s
!< 8192`).
use baruch
use penf, only : I4P
implicit none
character, parameter :: LF = achar(10)  !< Line feed.
character, parameter :: TAB = achar(9)  !< Tab.
character, parameter :: BS = achar(92)  !< Backslash (not a literal: nvfortran reads it as an escape).
integer              :: failures        !< Failed checks.

failures = 0
call test_headings
call test_breaks
call test_lists
call test_code
call test_html
call test_definitions
call test_tabs_and_lazy
call test_spans
call test_deep_nesting
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_blocks: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_blocks: ok'

contains
   subroutine parse(doc, source)
   !< Load a source and parse it with no extension; check the parse succeeds and leaves no tombstone.
   type(markdown_document), intent(inout) :: doc     !< Document.
   character(*),            intent(in)    :: source  !< Markdown.
   type(markdown_options)                 :: options !< Options.
   integer(I4P)                           :: stat    !< Status.
   character(:), allocatable              :: errmsg  !< Message.

   call doc%load(source)
   call options%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'parse: stat '//errmsg)
   call check(doc%nodes_number() == doc%last_node(), 'parse: no tombstone left')
   endsubroutine parse

   function child(doc, n, i) result(c)
   !< Return the i-th child of a node (0 if none).
   type(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),            intent(in) :: n   !< Node.
   integer,                 intent(in) :: i   !< Index, from 1.
   integer(I4P)                        :: c   !< Child.
   integer                             :: k   !< Counter.

   c = doc%first_child(n)
   do k=2, i
      if (c == 0_I4P) return
      c = doc%next_sibling(c)
   enddo
   endfunction child

   function text_of(doc, n) result(text)
   !< Return the inline content of a block as the block parser read it: its texts' literals, a soft line break as LF
   !< (empty if none). The blocks tested here hold no other inline construct.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Block.
   character(:), allocatable           :: text !< Text.
   integer(I4P)                        :: c    !< Child.

   text = ''
   c = doc%first_child(n)
   do while (c /= 0_I4P)
      if (doc%node_kind(c) == NODE_SOFTBREAK) then
         text = text//LF
      else
         text = text//doc%literal(c)
      endif
      c = doc%next_sibling(c)
   enddo
   endfunction text_of

   function span_of(doc, n) result(text)
   !< Return the source bytes of a node's span.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Node.
   character(:), allocatable           :: text !< Bytes.

   text = doc%source_slice(doc%span_first(n), doc%span_last(n))
   endfunction span_of

   subroutine test_headings()
   !< ATX headings with and without a closing sequence, an empty one, setext headings of both levels.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: h   !< Heading.

   call parse(doc, '# One ##'//LF//'### Three #not closing'//LF//'##'//LF//'Two'//LF//'lines'//LF//'---'//LF)
   call check(doc%children_number(doc%root()) == 4, 'headings: four blocks')
   h = child(doc, doc%root(), 1)
   call check(doc%node_kind(h) == NODE_HEADING .and. doc%heading_level(h) == 1, 'headings: # is level 1')
   call check(doc%int_slot(h, SLOT_HEADING_FORM) == HEADING_ATX, 'headings: ATX form')
   call check(doc%int_slot(h, SLOT_HEADING_CLOSING) == 2, 'headings: two closing #s')
   call check(text_of(doc, h) == 'One', 'headings: text "'//text_of(doc, h)//'"')
   h = child(doc, doc%root(), 2)
   call check(doc%heading_level(h) == 3 .and. doc%int_slot(h, SLOT_HEADING_CLOSING) == 0, 'headings: no closing')
   call check(text_of(doc, h) == 'Three #not closing', 'headings: # inside the text kept')
   h = child(doc, doc%root(), 3)
   call check(doc%heading_level(h) == 2 .and. doc%first_child(h) == 0, 'headings: ## alone, empty')
   h = child(doc, doc%root(), 4)
   call check(doc%heading_level(h) == 2 .and. doc%int_slot(h, SLOT_HEADING_FORM) == HEADING_SETEXT, &
              'headings: setext 2')
   call check(text_of(doc, h) == 'Two'//LF//'lines', 'headings: setext text of two lines')
   call check(span_of(doc, h) == 'Two'//LF//'lines'//LF//'---', 'headings: setext span "'//span_of(doc, h)//'"')
   call parse(doc, 'Level one'//LF//'=='//LF)
   h = doc%first_child(doc%root())
   call check(doc%heading_level(h) == 1 .and. doc%int_slot(h, SLOT_HEADING_FORM) == HEADING_SETEXT, &
              'headings: setext 1')
   endsubroutine test_headings

   subroutine test_breaks()
   !< The three thematic break characters; `---` under a paragraph is a setext underline, not a break.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: n   !< Node.
   integer                 :: k   !< Counter.
   character(3), parameter :: chars = '*-_' !< Break characters in order.

   call parse(doc, '***'//LF//' - - -'//LF//'_____'//LF)
   call check(doc%children_number(doc%root()) == 3, 'breaks: three')
   do k=1, 3
      n = child(doc, doc%root(), k)
      call check(doc%node_kind(n) == NODE_THEMATIC_BREAK .and. doc%int_slot(n, SLOT_BREAK_CHAR) == iachar(chars(k:k)), &
                 'breaks: break '//chars(k:k))
   enddo
   call check(span_of(doc, child(doc, doc%root(), 2)) == '- - -', 'breaks: span from the first marker')
   endsubroutine test_breaks

   subroutine test_lists()
   !< Bullet and ordered lists: type, start, marker, tightness; a change of bullet starts a new list.
   type(markdown_document) :: doc  !< Document.
   integer(I4P)            :: l    !< List.

   call parse(doc, '- a'//LF//'- b'//LF//'+ c'//LF//LF//'7) x'//LF//LF//'8) y'//LF)
   call check(doc%children_number(doc%root()) == 3, 'lists: three lists')
   l = child(doc, doc%root(), 1)
   call check(doc%list_type(l) == LIST_BULLET .and. doc%int_slot(l, SLOT_LIST_MARKER) == iachar('-'), 'lists: - bullet')
   call check(doc%children_number(l) == 2 .and. doc%list_tight(l), 'lists: two items, tight')
   call check(text_of(doc, doc%first_child(doc%first_child(l))) == 'a', 'lists: first item text')
   l = child(doc, doc%root(), 2)
   call check(doc%int_slot(l, SLOT_LIST_MARKER) == iachar('+') .and. doc%children_number(l) == 1, 'lists: + bullet')
   call check(doc%list_tight(l), 'lists: a one-item list followed by a blank line is tight')
   l = child(doc, doc%root(), 3)
   call check(doc%list_type(l) == LIST_ORDERED .and. doc%list_start(l) == 7, 'lists: ordered from 7')
   call check(doc%int_slot(l, SLOT_LIST_MARKER) == iachar(')'), 'lists: ) delimiter')
   call check(doc%children_number(l) == 2 .and. .not. doc%list_tight(l), 'lists: loose')
   ! nested lists, and an item holding a code block
   call parse(doc, '1. one'//LF//'   - two'//LF//LF//'         code'//LF)
   l = doc%first_child(doc%root())
   call check(doc%children_number(doc%root()) == 1 .and. doc%children_number(l) == 1, 'lists: one item outside')
   l = child(doc, doc%first_child(l), 2)
   call check(doc%node_kind(l) == NODE_LIST .and. doc%list_type(l) == LIST_BULLET, 'lists: the nested list')
   l = child(doc, doc%first_child(l), 2)
   call check(doc%node_kind(l) == NODE_CODE_BLOCK .and. doc%literal(l) == 'code'//LF, 'lists: code in the nested item')
   endsubroutine test_lists

   subroutine test_code()
   !< Fenced code (character, length, info string, literal) and indented code (trailing blank lines dropped).
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: c   !< Code block.

   call parse(doc, '~~~~  fortran  run '//LF//'a = 1'//LF//'~~~'//LF//LF//'~~~~~'//LF//LF//'    x'//LF//'      y'// &
                   LF//'    '//LF//LF)
   c = child(doc, doc%root(), 1)
   call check(doc%node_kind(c) == NODE_CODE_BLOCK .and. doc%int_slot(c, SLOT_CODE_FORM) == CODE_FENCED, 'code: fenced')
   call check(doc%int_slot(c, SLOT_CODE_FENCE_CHAR) == iachar('~') .and. &
              doc%int_slot(c, SLOT_CODE_FENCE_LENGTH) == 4, 'code: ~ fence of 4')
   call check(doc%info(c) == 'fortran  run', 'code: info "'//doc%info(c)//'"')
   call check(doc%literal(c) == 'a = 1'//LF//'~~~'//LF//LF, 'code: a shorter fence is content "'//doc%literal(c)//'"')
   call parse(doc, '```'//LF//'x'//LF//'```'//LF//'    a'//LF//'      b'//LF//'    '//LF//LF)
   c = child(doc, doc%root(), 1)
   call check(doc%literal(c) == 'x'//LF .and. len(doc%info(c)) == 0, 'code: backtick fence, no info')
   call check(span_of(doc, c) == '```'//LF//'x'//LF//'```', 'code: fenced span to the closing fence')
   c = child(doc, doc%root(), 2)
   call check(doc%int_slot(c, SLOT_CODE_FORM) == CODE_INDENTED, 'code: indented')
   call check(doc%literal(c) == 'a'//LF//'  b'//LF, 'code: indented literal "'//doc%literal(c)//'"')
   call parse(doc, '```a'//BS//'_b'//LF//'```'//LF)
   call check(doc%info(doc%first_child(doc%root())) == 'a_b', 'code: backslash escape in the info string')
   endsubroutine test_code

   subroutine test_html()
   !< HTML blocks: a type 1 block ends at its end condition, a type 6 one at a blank line; a type 7 cannot interrupt a
   !< paragraph.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: h   !< HTML block.

   call parse(doc, '<pre>'//LF//LF//'x</pre> tail'//LF//'after'//LF//LF//'  <div>'//LF//'*a*'//LF//LF//'b'//LF)
   h = child(doc, doc%root(), 1)
   call check(doc%node_kind(h) == NODE_HTML_BLOCK, 'html: type 1 block')
   call check(doc%literal(h) == '<pre>'//LF//LF//'x</pre> tail'//LF, 'html: type 1 literal "'//doc%literal(h)//'"')
   call check(doc%node_kind(child(doc, doc%root(), 2)) == NODE_PARAGRAPH, 'html: the paragraph after it')
   h = child(doc, doc%root(), 3)
   call check(doc%literal(h) == '  <div>'//LF//'*a*'//LF, 'html: type 6 literal, indentation kept')
   call parse(doc, 'text'//LF//'<custom-tag>'//LF)
   call check(doc%children_number(doc%root()) == 1, 'html: type 7 does not interrupt a paragraph')
   endsubroutine test_html

   subroutine test_definitions()
   !< Link reference definitions: nodes in place, label as written, destination and title processed; a paragraph of
   !< definitions only goes; a definition cannot interrupt a paragraph.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: d   !< Definition.

   call parse(doc, '[Foo  Bar]: <my url> "say '//BS//'"hi'//BS//'""'//LF// &
                   '[b]:'//LF//'  /b'//LF//'  (t)'//LF//'text'//LF//LF//'[c]: /c'//LF//LF//'para'//LF//'[d]: /d'//LF)
   call check(doc%children_number(doc%root()) == 5, 'definitions: 2 definitions, paragraph, definition, paragraph')
   d = child(doc, doc%root(), 1)
   call check(doc%node_kind(d) == NODE_LINK_REFERENCE_DEFINITION, 'definitions: a node')
   call check(doc%label(d) == 'Foo  Bar', 'definitions: label as written "'//doc%label(d)//'"')
   call check(doc%destination(d) == 'my url', 'definitions: destination without <> "'//doc%destination(d)//'"')
   call check(doc%title(d) == 'say "hi"', 'definitions: title unescaped "'//doc%title(d)//'"')
   d = child(doc, doc%root(), 2)
   call check(doc%label(d) == 'b' .and. doc%destination(d) == '/b' .and. doc%title(d) == 't', &
              'definitions: over three lines')
   call check(span_of(doc, d) == '[b]:'//LF//'  /b'//LF//'  (t)', 'definitions: span "'//span_of(doc, d)//'"')
   call check(text_of(doc, child(doc, doc%root(), 3)) == 'text', 'definitions: the paragraph keeps the rest')
   call check(doc%node_kind(child(doc, doc%root(), 4)) == NODE_LINK_REFERENCE_DEFINITION, &
              'definitions: a paragraph of a definition only goes')
   call check(text_of(doc, child(doc, doc%root(), 5)) == 'para'//LF//'[d]: /d', &
              'definitions: not after paragraph text')
   ! a setext underline under definitions only is paragraph text (spec example 216)
   call parse(doc, '[foo]: /url'//LF//'==='//LF)
   call check(doc%children_number(doc%root()) == 2, 'definitions: definition and paragraph')
   call check(text_of(doc, child(doc, doc%root(), 2)) == '===', 'definitions: the underline is text')
   endsubroutine test_definitions

   subroutine test_tabs_and_lazy()
   !< A tab consumed in part by a list item (its remaining columns are content), a lazy continuation line, a list item
   !< in a block quote closed by a line without `>`.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: n   !< Node.

   call parse(doc, '- foo'//LF//LF//TAB//TAB//'bar'//LF)
   n = doc%first_child(doc%first_child(doc%root()))
   n = child(doc, n, 2)
   call check(doc%node_kind(n) == NODE_CODE_BLOCK, 'tabs: code in the item')
   call check(doc%literal(n) == '  bar'//LF, 'tabs: two columns of the tab are content "'//doc%literal(n)//'"')
   call parse(doc, '>'//TAB//TAB//'foo'//LF)
   n = doc%first_child(doc%first_child(doc%root()))
   call check(doc%node_kind(n) == NODE_CODE_BLOCK .and. doc%literal(n) == '  foo'//LF, 'tabs: > then two tabs')
   call parse(doc, '> a'//LF//'b'//LF//'> c'//LF)
   n = doc%first_child(doc%root())
   call check(doc%children_number(doc%root()) == 1 .and. doc%node_kind(n) == NODE_BLOCK_QUOTE, 'lazy: one quote')
   call check(text_of(doc, doc%first_child(n)) == 'a'//LF//'b'//LF//'c', 'lazy: one paragraph of three lines')
   call parse(doc, '> - a'//LF//'- b'//LF)
   call check(doc%children_number(doc%root()) == 2, 'lazy: a list marker is not lazy text')
   endsubroutine test_tabs_and_lazy

   subroutine test_spans()
   !< Spans: a block quote and its paragraph, a list item, the root; a list's end, in and out of a quote.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: q   !< Block quote.

   call parse(doc, 'x'//LF//LF//'> quoted'//LF//'> more'//LF//LF//'- item'//LF)
   q = child(doc, doc%root(), 2)
   call check(span_of(doc, q) == '> quoted'//LF//'> more', 'spans: block quote "'//span_of(doc, q)//'"')
   call check(span_of(doc, doc%first_child(q)) == 'quoted'//LF//'> more', 'spans: its paragraph')
   call check(span_of(doc, doc%first_child(child(doc, doc%root(), 3))) == '- item', 'spans: the item')
   call check(doc%span_first(doc%root()) == 1 .and. doc%span_last(doc%root()) == doc%source_length(), 'spans: root')
   ! a list ends with its last item, not at the line that closes it; a `>` alone does not extend a list in the quote
   call parse(doc, '- a'//LF//'- b'//LF//LF//'end'//LF//'> - x'//LF//'>'//LF//'> y'//LF)
   call check(span_of(doc, child(doc, doc%root(), 1)) == '- a'//LF//'- b', &
              'spans: list "'//span_of(doc, child(doc, doc%root(), 1))//'"')
   q = doc%first_child(child(doc, doc%root(), 3))
   call check(span_of(doc, q) == '- x' .and. span_of(doc, doc%first_child(q)) == '- x', &
              'spans: list and item in a quote "'//span_of(doc, q)//'"')
   endsubroutine test_spans

   subroutine test_deep_nesting()
   !< 10^5 nested block quotes, on two lines: parsed with no recursion, every level a block quote, the paragraph at the
   !< bottom holding both lines; then 10^5 nested list items.
   type(markdown_document)   :: doc    !< Document.
   character(:), allocatable :: source !< Markdown.
   integer(I4P)              :: n      !< Node.
   integer                   :: depth  !< Nesting.
   integer                   :: levels !< Block quotes walked.

   ! a variable, not a named constant: nvfortran 26.9 fails (internal error) on a repeat() of a long constant length
   ! folded in a contained procedure (reproducer notes/nvfortran-ilm-long-constant-string.f90)
   depth = 100000
   source = repeat('>', depth)//' deep'//LF//repeat('>', depth)//' down'//LF
   call parse(doc, source)
   n = doc%first_child(doc%root())
   levels = 0
   do while (n /= 0_I4P)
      if (doc%node_kind(n) /= NODE_BLOCK_QUOTE) exit
      levels = levels + 1
      n = doc%first_child(n)
   enddo
   print '(A,I0)', 'deep nesting: block quotes ', levels
   call check(levels == depth, 'deep: every level a block quote')
   call check(doc%node_kind(n) == NODE_PARAGRAPH, 'deep: a paragraph at the bottom')
   if (doc%node_kind(n) == NODE_PARAGRAPH) call check(text_of(doc, n) == 'deep'//LF//'down', 'deep: its two lines')
   call check(doc%nodes_number() == depth + 5, 'deep: root, quotes, paragraph, text, soft break, text')
   ! as many nested list items on one line: a list and an item per marker
   source = repeat('- ', depth)//'x'//LF
   call parse(doc, source)
   n = doc%first_child(doc%root())
   levels = 0
   do while (n /= 0_I4P)
      if (doc%node_kind(n) /= NODE_LIST) exit
      levels = levels + 1
      n = doc%first_child(doc%first_child(n))
   enddo
   print '(A,I0)', 'deep nesting: lists ', levels
   call check(levels == depth .and. doc%node_kind(n) == NODE_PARAGRAPH, 'deep: every level a list, a paragraph last')
   endsubroutine test_deep_nesting

   subroutine check(condition, label)
   !< Count a failed check and print it.
   logical,      intent(in) :: condition !< What must hold.
   character(*), intent(in) :: label     !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL '//label
   endif
   endsubroutine check
endprogram baruch_test_blocks
