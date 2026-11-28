!< BARUCH test: tree equality, by meaning, spans and style ignored, with the path of the first difference.
program baruch_test_equal
!< BARUCH test: tree equality, by meaning, spans and style ignored, with the path of the first difference.
!<
!< The gate of plan step 3.1. Each of the 652 CommonMark examples (`src/tests/spec/commonmark-0.31.2`, read from the
!< project's root, where `scripts/run_tests.sh` and `fpm test` run) is parsed and must equal itself, its deep copy, a
!< second parse of its source, a copy holding a tombstone and a different root span, and that copy compacted. Pairs
!< written in different styles (emphasis character, bullet, ordered delimiter, ATX and setext, fences, breaks, link
!< forms, line endings) must be equal; pairs differing in content must be unequal, with the path of the first differing
!< node. Then trees built by hand (text runs, attributes, extension kinds, style slots, spans, detached nodes, empty
!< documents), and a 10^5-deep nesting of block quotes and of lists compared with no recursion.
use baruch
use penf, only : I4P
implicit none
character, parameter :: LF = achar(10) !< Line feed.
character, parameter :: CR = achar(13) !< Carriage return.
integer              :: failures       !< Failed checks.

failures = 0
call test_fixtures
call test_style_pairs
call test_content_pairs
call test_by_hand
call test_deep
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_equal: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_equal: ok'

contains
   subroutine test_fixtures()
   !< Every example of the CommonMark fixtures equals itself, its copy, a second parse, a copy with a tombstone and the
   !< same copy compacted.
   character(*), parameter   :: DIR = 'src/tests/spec/commonmark-0.31.2/' !< Fixtures, from the project's root.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_document)   :: copy     !< Its deep copy.
   type(markdown_document)   :: again    !< A second parse of the same source.
   type(markdown_options)    :: options  !< Options.
   integer(I4P)              :: stat     !< Status.
   character(:), allocatable :: errmsg   !< Message.
   character(:), allocatable :: path     !< Path of a difference.
   character(4)              :: stem     !< Example number on four digits.
   integer                   :: examples !< Examples in the index.
   integer                   :: k        !< Counter.
   integer                   :: good     !< Examples passing.
   integer(I4P)              :: p        !< A paragraph made a tombstone.
   logical                   :: ok       !< One example passes.

   examples = index_lines(DIR//'index.tsv') - 1
   call check(examples == 652, 'fixtures: 652 examples in the index, found '//str(int(examples, I4P)))
   good = 0
   do k=1, examples
      write(stem, '(I4.4)') k
      call doc%load_file(DIR//'examples/'//stem//'.md', stat=stat, errmsg=errmsg)
      if (stat == BARUCH_OK) call options%parse(doc, stat=stat, errmsg=errmsg)
      if (stat == BARUCH_OK) call again%load_file(DIR//'examples/'//stem//'.md', stat=stat, errmsg=errmsg)
      if (stat == BARUCH_OK) call options%parse(again, stat=stat, errmsg=errmsg)
      if (stat /= BARUCH_OK) then
         call check(.false., 'fixtures: example '//stem//': '//errmsg)
         cycle
      endif
      copy = doc
      ! a tombstone and a span: neither is meaning
      p = copy%new_node(NODE_PARAGRAPH)
      call copy%append_child(copy%root(), p)
      call copy%unlink(p)
      call copy%set_span(copy%root(), 1_I4P, 0_I4P)
      ok = doc%equal(doc, path=path)
      ok = ok .and. doc%equal(copy, path=path)
      ok = ok .and. copy%equal(doc, path=path)
      ok = ok .and. doc%equal(again, path=path)
      call copy%compact()
      ok = ok .and. doc%equal(copy, path=path) ! renumbered
      call check(ok, 'fixtures: example '//stem//' differs from itself at '//path)
      if (ok) good = good + 1
   enddo
   print '(A,I0,A,I0,A)', 'fixtures: ', good, '/', examples, ' equal to themselves, their copies and a second parse'
   endsubroutine test_fixtures

   subroutine test_style_pairs()
   !< Pairs written in different styles are equal.
   call equal_pair('*a*', '_a_', 'emphasis character')
   call equal_pair('**a**', '__a__', 'strong emphasis character')
   call equal_pair('- a'//LF//'- b', '+ a'//LF//'+ b', 'bullet - and +')
   call equal_pair('- a', '* a', 'bullet - and *')
   call equal_pair('1. a', '1) a', 'ordered delimiter')
   call equal_pair('# a', 'a'//LF//'=', 'ATX and setext, level 1')
   call equal_pair('## a', 'a'//LF//'---', 'ATX and setext, level 2')
   call equal_pair('# a #', '# a', 'closing #s')
   call equal_pair('```'//LF//'x'//LF//'```', '~~~'//LF//'x'//LF//'~~~', 'backtick and tilde fences')
   call equal_pair('```'//LF//'x'//LF//'```', '`````'//LF//'x'//LF//'`````', 'fence length')
   call equal_pair('    x', '```'//LF//'x'//LF//'```', 'indented and fenced code')
   call equal_pair('***', '---', 'thematic break * and -')
   call equal_pair('***', '___', 'thematic break * and _')
   call equal_pair('<https://a.b>', '[https://a.b](https://a.b)', 'autolink and inline link')
   call equal_pair('[a][x]'//LF//LF//'[x]: /u', '[a](/u)'//LF//LF//'[x]: /u', 'full reference and inline link')
   call equal_pair('[x]'//LF//LF//'[x]: /u', '[x][]'//LF//LF//'[x]: /u', 'shortcut and collapsed reference')
   call equal_pair('[x]: /u "t"', '[X]:   </u>  ''t''', 'definition label case, destination and title delimiters')
   call equal_pair('a\*b', 'a&#42;b', 'escape and character reference')
   call equal_pair('a'//CR//LF//'b'//CR//LF, 'a'//LF//'b'//LF, 'CRLF and LF')
   call equal_pair('```'//CR//LF//'x'//CR//LF//'```'//CR//LF, '```'//LF//'x'//LF//'```'//LF, 'code block, CRLF and LF')
   call equal_pair('<div>'//CR//'x'//CR//'</div>', '<div>'//LF//'x'//LF//'</div>', 'HTML block, CR and LF')
   endsubroutine test_style_pairs

   subroutine test_content_pairs()
   !< Pairs differing in content are unequal, the path naming the first differing node.
   call unequal_pair('*a*', '**a**', '/document/paragraph/emph')
   call unequal_pair('a', 'b', '/document/paragraph/text')
   call unequal_pair('# a', '## a', '/document/heading')
   call unequal_pair('1. a', '2. a', '/document/list')
   call unequal_pair('- a', '1. a', '/document/list')
   call unequal_pair('- a'//LF//'- b', '- a'//LF//LF//'- b', '/document/list')
   call unequal_pair('```py'//LF//'x'//LF//'```', '```rb'//LF//'x'//LF//'```', '/document/code_block')
   call unequal_pair('```'//LF//'x'//LF//'```', '```'//LF//'y'//LF//'```', '/document/code_block')
   call unequal_pair('[a](/u)', '[a](/v)', '/document/paragraph/link')
   call unequal_pair('[a](/u "t")', '[a](/u "s")', '/document/paragraph/link')
   call unequal_pair('![a](/u)', '[a](/u)', '/document/paragraph/image')
   call unequal_pair('`x`', '`y`', '/document/paragraph/code')
   call unequal_pair('a <b>', 'a <i>', '/document/paragraph/html_inline')
   call unequal_pair('<div>'//LF//'x'//LF//'</div>', '<div>'//LF//'y'//LF//'</div>', '/document/html_block')
   call unequal_pair('[x]: /u', '[x]: /v', '/document/link_reference_definition')
   call unequal_pair('[x]: /u', '[y]: /u', '/document/link_reference_definition')
   call unequal_pair('- a'//LF//'- b', '- a'//LF//'- c', '/document/list/item[2]/paragraph/text')
   call unequal_pair('a', 'a'//LF//LF//'b', '/document/paragraph[2]')
   call unequal_pair('a'//LF//LF//'b', 'a', '/document/paragraph[2]')
   call unequal_pair('> a', 'a', '/document/block_quote')
   call unequal_pair('a'//LF//'b', 'a  '//LF//'b', '/document/paragraph/softbreak')
   call unequal_pair('- x'//LF//LF//'+ *a*'//LF//'+ b', '- x'//LF//LF//'+ **a**'//LF//'+ b', &
                     '/document/list[2]/item[1]/paragraph/emph')
   call unequal_pair('- a'//LF//LF//'  > *c*', '- a'//LF//LF//'  > *d*', &
                     '/document/list/item/block_quote/paragraph/emph/text')
   call unequal_pair('[a](/u)', '[b](/u)', '/document/paragraph/link/text')
   call unequal_pair('a *b* c *d*', 'a *b* c *e*', '/document/paragraph/emph[2]/text')
   endsubroutine test_content_pairs

   subroutine test_by_hand()
   !< Trees built by hand: text runs, attributes, extension kinds, style slots, spans, detached nodes, empty documents.
   type(markdown_document)   :: a    !< First document.
   type(markdown_document)   :: b    !< Second document.
   type(markdown_document)   :: e    !< An empty document.
   type(markdown_document)   :: f    !< Another one.
   character(:), allocatable :: path !< Path of the difference.
   character(:), allocatable :: what !< The difference.
   integer(I4P)              :: n    !< A node.

   ! empty documents
   call check(e%equal(f, path=path) .and. len(path) == 0, 'by hand: two empty documents are equal')
   call a%initialize
   call check(.not. e%equal(a, path=path, difference=what), 'by hand: empty and initialised differ')
   call check(path == '/document' .and. index(what, 'only in the second') > 0, 'by hand: empty: '//what)
   call check(.not. a%equal(e, path=path) .and. path == '/document', 'by hand: initialised and empty: '//path)
   ! a run of texts is one text; an empty text is nothing; a detached node is not in the tree; spans and style slots
   call paragraph_of(a, ['ab'], n)
   call a%set_int_slot(a%first_child(a%root()), 1_I4P, 42_I4P)
   call paragraph_of(b, ['a', ' ', 'b'], n) ! 'a', '', 'b': the texts are trimmed
   n = b%new_node(NODE_TEXT)
   call b%set_literal(n, 'not in the tree')
   call b%set_span(b%root(), 3_I4P, 9_I4P)
   call check(a%equal(b, path=path), 'by hand: a text run equals one text, at '//path)
   n = b%new_node(NODE_TEXT)
   call b%append_child(b%first_child(b%root()), n)
   call check(a%equal(b, path=path), 'by hand: a trailing empty text is nothing, at '//path)
   call b%set_literal(n, 'c')
   call check(.not. a%equal(b, path=path, difference=what), 'by hand: a non-empty text joins the run')
   call check(path == '/document/paragraph/text' .and. index(what, '"ab" vs "abc"') > 0, 'by hand: run: '//what)
   ! emphasis character: style
   call emph_of(a, iachar('*'))
   call emph_of(b, iachar('_'))
   call check(a%equal(b, path=path), 'by hand: emphasis characters are style, at '//path)
   n = b%new_node(NODE_TEXT)
   call b%append_child(b%first_child(b%root()), n)
   call check(a%equal(b, path=path), 'by hand: an empty text after an emphasis is nothing, at '//path)
   ! attributes: a set, in any order
   call heading_of(a)
   call heading_of(b)
   call a%set_attribute(a%first_child(a%root()), 'id', 'x')
   call a%set_attribute(a%first_child(a%root()), 'class', 'y')
   call b%set_attribute(b%first_child(b%root()), 'class', 'y')
   call b%set_attribute(b%first_child(b%root()), 'id', 'x')
   call check(a%equal(b, path=path), 'by hand: attributes in any order, at '//path)
   call b%set_attribute(b%first_child(b%root()), 'id', 'z')
   call check(.not. a%equal(b, path=path, difference=what), 'by hand: an attribute value differs')
   call check(path == '/document/heading' .and. index(what, 'attribute "id"') > 0, 'by hand: attribute: '//what)
   call b%remove_attribute(b%first_child(b%root()), 'id')
   call check(.not. a%equal(b, difference=what) .and. index(what, 'only in the first') > 0, 'by hand: '//what)
   call b%set_attribute(b%first_child(b%root()), 'id', 'x')
   call b%set_attribute(b%first_child(b%root()), 'lang', 'it')
   call check(.not. a%equal(b, difference=what) .and. index(what, 'only in the second') > 0, 'by hand: '//what)
   ! extension kinds: by name, every slot
   call math_of(a, 1000_I4P, 'math')
   call math_of(b, 1001_I4P, 'math')
   call check(a%equal(b, path=path), 'by hand: an extension kind by its name, not its number, at '//path)
   call b%set_int_slot(b%first_child(b%first_child(b%root())), 2_I4P, 7_I4P)
   call check(.not. a%equal(b, path=path, difference=what), 'by hand: an extension integer slot differs')
   call check(path == '/document/paragraph/math' .and. index(what, 'integer slot 2') > 0, 'by hand: slot: '//what)
   call math_of(b, 1000_I4P, 'mark')
   call check(.not. a%equal(b, path=path, difference=what), 'by hand: extension kinds of different names')
   call check(path == '/document/paragraph/math' .and. index(what, 'kind math vs mark') > 0, 'by hand: kind: '//what)
   endsubroutine test_by_hand

   subroutine test_deep()
   !< 10^5 nested block quotes, then 10^5 nested lists: compared whole, the difference at the deepest node.
   type(markdown_document)   :: a       !< First document.
   type(markdown_document)   :: b       !< Second document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: path    !< Path of the difference.
   integer                   :: depth   !< Nesting.

   ! a variable, not a named constant: nvfortran 26.9 fails (internal error) on a repeat() of a long constant length
   ! folded in a contained procedure (reproducer notes/nvfortran-ilm-long-constant-string.f90)
   depth = 100000
   call a%load(repeat('>', depth)//' deep'//LF)
   call options%parse(a)
   b = a
   call check(a%equal(b, path=path), 'deep quotes: equal to the copy, at '//path(1:min(80, len(path))))
   call b%load(repeat('>', depth)//' deer'//LF)
   call options%parse(b)
   call check(.not. a%equal(b, path=path), 'deep quotes: unequal')
   call check(count_steps(path, '/block_quote') == depth .and. ends_with(path, '/block_quote/paragraph/text'), &
              'deep quotes: the path holds every block quote')
   print '(A,I0,A,I0,A)', 'deep equality: ', depth, ' nested block quotes, a path of ', len(path), ' bytes'
   call a%load(repeat('- ', depth)//'x'//LF)
   call options%parse(a)
   call b%load(repeat('- ', depth)//'y'//LF)
   call options%parse(b)
   call check(a%equal(a), 'deep lists: equal to itself')
   call check(.not. a%equal(b, path=path), 'deep lists: unequal')
   call check(count_steps(path, '/list/item') == depth .and. ends_with(path, '/item/paragraph/text'), &
              'deep lists: the path holds every list and item')
   print '(A,I0,A)', 'deep equality: ', depth, ' nested lists'
   endsubroutine test_deep

   subroutine equal_pair(x, y, label)
   !< Parse two sources and check they are equal.
   character(*), intent(in)  :: x     !< First source.
   character(*), intent(in)  :: y     !< Second source.
   character(*), intent(in)  :: label !< What the pair shows.
   type(markdown_document)   :: a     !< First document.
   type(markdown_document)   :: b     !< Second document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: what  !< The difference.

   call a%load(x)
   call options%parse(a)
   call b%load(y)
   call options%parse(b)
   call check(a%equal(b, difference=what) .and. b%equal(a), 'style pair, '//label//': '//what)
   endsubroutine equal_pair

   subroutine unequal_pair(x, y, expected)
   !< Parse two sources and check they differ at the expected path.
   character(*), intent(in)  :: x        !< First source.
   character(*), intent(in)  :: y        !< Second source.
   character(*), intent(in)  :: expected !< Path of the first difference.
   type(markdown_document)   :: a        !< First document.
   type(markdown_document)   :: b        !< Second document.
   type(markdown_options)    :: options  !< Options.
   character(:), allocatable :: path     !< Path of the difference.
   character(:), allocatable :: what     !< The difference.

   call a%load(x)
   call options%parse(a)
   call b%load(y)
   call options%parse(b)
   call check(.not. a%equal(b, path=path, difference=what), 'content pair '//expected//': equal')
   call check(path == expected .and. len(path) == len(expected), 'content pair '//expected//': '//what)
   call check(.not. b%equal(a), 'content pair '//expected//': equal, reversed')
   endsubroutine unequal_pair

   subroutine paragraph_of(doc, texts, p)
   !< Make a document holding one paragraph of text nodes (each text trimmed).
   type(markdown_document), intent(inout) :: doc      !< Document.
   character(*),            intent(in)    :: texts(:) !< Texts.
   integer(I4P),            intent(out)   :: p        !< The paragraph.
   integer(I4P)                           :: t        !< A text.
   integer                                :: k        !< Counter.

   call doc%initialize
   p = doc%new_node(NODE_PARAGRAPH)
   call doc%append_child(doc%root(), p)
   do k=1, size(texts)
      t = doc%new_node(NODE_TEXT)
      call doc%set_literal(t, trim(texts(k)))
      call doc%append_child(p, t)
   enddo
   endsubroutine paragraph_of

   subroutine emph_of(doc, char_code)
   !< Make a document holding a paragraph with an emphasis of a recorded character.
   type(markdown_document), intent(inout) :: doc       !< Document.
   integer,                 intent(in)    :: char_code !< Emphasis character.
   integer(I4P)                           :: p         !< The paragraph.
   integer(I4P)                           :: m         !< The emphasis.
   integer(I4P)                           :: t         !< Its text.

   call paragraph_of(doc, ['x'], p)
   m = doc%new_node(NODE_EMPH)
   call doc%set_int_slot(m, SLOT_EMPHASIS_CHAR, int(char_code, I4P))
   call doc%append_child(p, m)
   t = doc%new_node(NODE_TEXT)
   call doc%set_literal(t, 'y')
   call doc%append_child(m, t)
   endsubroutine emph_of

   subroutine heading_of(doc)
   !< Make a document holding a level-2 heading.
   type(markdown_document), intent(inout) :: doc !< Document.
   integer(I4P)                           :: h   !< The heading.

   call doc%initialize
   h = doc%new_node(NODE_HEADING)
   call doc%set_heading_level(h, 2_I4P)
   call doc%append_child(doc%root(), h)
   endsubroutine heading_of

   subroutine math_of(doc, kind_number, name)
   !< Make a document holding a paragraph with a node of an extension kind registered under a number and a name.
   type(markdown_document), intent(inout) :: doc         !< Document.
   integer(I4P),            intent(in)    :: kind_number !< Kind number.
   character(*),            intent(in)    :: name        !< Kind name.
   integer(I4P)                           :: p           !< The paragraph.
   integer(I4P)                           :: m           !< The extension node.

   call doc%destroy
   call doc%register_kind(kind_number, name, .true.) ! the first edit of an empty document creates its root
   p = doc%new_node(NODE_PARAGRAPH)
   call doc%append_child(doc%root(), p)
   m = doc%new_node(kind_number)
   call doc%set_literal(m, 'x^2')
   call doc%append_child(p, m)
   endsubroutine math_of

   pure function count_steps(path, step) result(steps)
   !< The number of occurrences of a step in a path.
   character(*), intent(in) :: path  !< Path.
   character(*), intent(in) :: step  !< Step.
   integer                  :: steps !< Occurrences.
   integer                  :: i     !< Position.
   integer                  :: j     !< Found at.

   steps = 0
   i = 1
   do
      j = index(path(i:), step)
      if (j == 0) exit
      steps = steps + 1
      i = i + j + len(step) - 1
   enddo
   endfunction count_steps

   pure function ends_with(text, tail) result(is)
   !< True if a text ends with a tail.
   character(*), intent(in) :: text !< Text.
   character(*), intent(in) :: tail !< Tail.
   logical                  :: is   !< Result.

   is = .false.
   if (len(text) >= len(tail)) is = text(len(text)-len(tail)+1:) == tail
   endfunction ends_with

   function index_lines(path) result(lines)
   !< The number of lines of a text file (0 if it cannot be read).
   character(*), intent(in) :: path   !< File.
   integer                  :: lines  !< Lines.
   character(1024)          :: record !< A line.
   integer                  :: u      !< Unit.
   integer                  :: ios    !< I/O status.

   lines = 0
   open(newunit=u, file=path, action='read', status='old', iostat=ios)
   if (ios /= 0) return
   do
      read(u, '(A)', iostat=ios) record
      if (ios /= 0) exit
      lines = lines + 1
   enddo
   close(u)
   endfunction index_lines

   pure function str(i) result(s)
   !< An integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction str

   subroutine check(condition, label)
   !< Count a failed check and print it.
   logical,      intent(in) :: condition !< What must hold.
   character(*), intent(in) :: label     !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL '//label
   endif
   endsubroutine check
endprogram baruch_test_equal
