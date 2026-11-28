!< BARUCH test: the tree writer, read back to the same tree, in the recorded and in the canonical style.
program baruch_test_roundtrip
!< BARUCH test: the tree writer, read back to the same tree, in the recorded and in the canonical style.
!<
!< The gate of plan step 3.2. Each of the 652 CommonMark examples (`src/tests/spec/commonmark-0.31.2`, read from the
!< project's root, where `scripts/run_tests.sh` and `fpm test` run) is parsed, written back (recorded style, then
!< canonical style) and parsed again: the trees must be equal (`markdown_document%equal`); the counts and the recorded
!< style's byte-identical rate (examples with no tab and no trailing space) are printed. Then: the canonical style
!< exactly, the escaping of texts that would read back as something else, trees built by hand (no recorded style, no
!< source), the sinks (`character`, `string`, formatted and stream units, files) and their errors, extensions
!< writing their nodes (and a hook failing), `escape_markdown`, and a 10^5-deep nesting written with no recursion.
use baruch
use baruch_test_extensions, only : note_extension, percent_extension, xvar_extension, mark_extension, &
                                   title_extension, math_extension, KIND_MATH
use baruch_writer,          only : escape_markdown
use penf,                   only : I4P
use stringifor,             only : string
implicit none
character, parameter :: LF = achar(10) !< Line feed.
character, parameter :: BS = achar(92) !< Backslash (not a literal: nvfortran reads it as an escape).
integer              :: failures       !< Failed checks.

failures = 0
call test_fixtures
call test_canonical
call test_escaping
call test_by_hand
call test_sinks
call test_extensions
call test_escaper
call test_deep
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_roundtrip: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_roundtrip: ok'

contains
   subroutine test_fixtures()
   !< Every example of the CommonMark fixtures, written in both styles, reads back to the same tree.
   character(*), parameter   :: DIR = 'src/tests/spec/commonmark-0.31.2/' !< Fixtures, from the project's root.
   type(markdown_document)   :: doc       !< Document.
   type(markdown_options)    :: options   !< Options.
   character(:), allocatable :: errmsg    !< Message.
   character(:), allocatable :: source    !< Its source.
   character(:), allocatable :: markdown  !< Written back.
   character(:), allocatable :: what      !< A difference.
   character(4)              :: stem      !< Example number on four digits.
   integer(I4P)              :: stat      !< Status.
   integer                   :: k         !< Counter.
   integer                   :: good(2)   !< Examples reading back equal, recorded and canonical.
   integer                   :: plain     !< Examples with no tab and no trailing space.
   integer                   :: same      !< Of them, written back byte for byte.
   integer                   :: m         !< Style.

   good = 0
   plain = 0
   same = 0
   do k=1, 652
      write(stem, '(I4.4)') k
      call doc%load_file(DIR//'examples/'//stem//'.md', stat=stat, errmsg=errmsg)
      if (stat == BARUCH_OK) call options%parse(doc, stat=stat, errmsg=errmsg)
      if (stat /= BARUCH_OK) then
         call check(.false., 'fixtures: example '//stem//': '//errmsg)
         cycle
      endif
      source = doc%source()
      do m=1, 2
         call options%write_markdown(doc, markdown, canonical=m == 2, stat=stat, errmsg=errmsg)
         call check(stat == BARUCH_OK, 'fixtures: example '//stem//': written with status '//str(stat))
         if (reads_back(doc, markdown, what)) then
            good(m) = good(m) + 1
         else
            call check(.false., 'fixtures: example '//stem//merge(' canonical', ' recorded ', m == 2)//': '//what)
         endif
         if (m == 1 .and. index(source, achar(9)) == 0 .and. index(source, ' '//LF) == 0) then
            plain = plain + 1
            if (markdown == source .and. len(markdown) == len(source)) same = same + 1
         endif
      enddo
   enddo
   print '(A,I0,A,I0,A,I0,A,I0,A,F5.1,A)', 'round trip: recorded ', good(1), '/652, canonical ', good(2), &
      '/652; byte-identical (recorded, no tab, no trailing space) ', same, '/', plain, ' (', 100.0 * same / plain, '%)'
   call check(all(good == 652), 'fixtures: every example reads back equal in both styles')
   endsubroutine test_fixtures

   subroutine test_canonical()
   !< The canonical style, exactly; the recorded one, unchanged; lists, emphasis and blocks that would merge.
   character(*), parameter :: SOURCE = 'Title'//LF//'====='//LF//LF//'* one'//LF//'* two'//LF//LF//'3) x'//LF//  &
                                       '4) y'//LF//LF//'~~~~ fortran'//LF//'print *, 1'//LF//'~~~~'//LF//LF//   &
                                       '    indented'//LF//LF//'___'//LF//LF//'_em_ __strong__ [a][r] <https://e.x>'// &
                                       ' and  '//LF//'break'//BS//LF//'end'//LF//LF//'[r]: /u "t"'//LF
   character(*), parameter :: CANONICAL = '# Title'//LF//LF//'- one'//LF//'- two'//LF//LF//'3. x'//LF//'4. y'//LF// &
                                          LF//'```fortran'//LF//'print *, 1'//LF//'```'//LF//LF//'```'//LF//     &
                                          'indented'//LF//'```'//LF//LF//'***'//LF//LF//'*em* **strong** [a](/u "t") '// &
                                          '<https://e.x> and'//BS//LF//'break'//BS//LF//'end'//LF//LF//'[r]: /u "t"'//LF

   call check(written(SOURCE, .false.) == SOURCE, 'canonical: the recorded style writes the source back: '// &
              written(SOURCE, .false.))
   call check(written(SOURCE, .true.) == CANONICAL, 'canonical: '//written(SOURCE, .true.))
   ! two lists in a row stay two lists; strong around emphasis and emphasis around emphasis stay so
   call check(written('- a'//LF//'+ b'//LF, .true.) == '- a'//LF//LF//'* b'//LF, 'canonical: lists in a row: '// &
              written('- a'//LF//'+ b'//LF, .true.))
   call check(written('1. a'//LF//'1) b'//LF, .true.) == '1. a'//LF//LF//'1) b'//LF, 'canonical: ordered in a row: '// &
              written('1. a'//LF//'1) b'//LF, .true.))
   call check(written('__*x*__'//LF, .true.) == '**_x_**'//LF, 'canonical: strong(emph): '// &
              written('__*x*__'//LF, .true.))
   call check(written('_*x*_'//LF, .true.) == '*_x_*'//LF, 'canonical: emph(emph): '//written('_*x*_'//LF, .true.))
   call check(written('*a*_b_'//LF, .true.) == '*a*_b_'//LF, 'canonical: emphases in a row: '// &
              written('*a*_b_'//LF, .true.))
   ! a code block after a list is fenced (indented, it would join the last item)
   call check(written('- a'//LF//LF//'<!-- -->'//LF//LF//'    code'//LF, .false.) == &
              '- a'//LF//LF//'<!-- -->'//LF//LF//'    code'//LF, 'canonical: code after an HTML block stays indented')
   endsubroutine test_canonical

   subroutine test_escaping()
   !< Texts that would read back as something else, in a tree built by hand: escaped where needed, and only there.
   call text_written('# not a heading', BS//'# not a heading')
   call text_written('1. not a list', '1'//BS//'. not a list')
   call text_written('- not a list', BS//'- not a list')
   call text_written('> not a quote', BS//'> not a quote')
   call text_written('*not emphasis*', BS//'*not emphasis*')
   call text_written('snake_case_name', 'snake_case_name')
   call text_written('a * b', 'a * b')
   call text_written('[not a link](x)', BS//'[not a link](x)')
   call text_written('[not a link] (x)', '[not a link] (x)')
   call text_written('&amp; not an entity', BS//'&amp; not an entity')
   call text_written('AT&T', 'AT&T')
   call text_written('<div> not HTML', BS//'<div> not HTML')
   call text_written('3 < 4', '3 < 4')
   call text_written('a ` tick', 'a ` tick')
   call text_written('`not code`', BS//'`not code`')
   call text_written(' leading space', '&#32;leading space')
   call text_written('trailing space ', 'trailing space&#32;')
   call text_written('back'//BS//'slash', 'back'//BS//'slash')
   call text_written('back'//BS//'*slash', 'back'//BS//BS//'*slash')
   call text_written('~~~ not a fence', BS//'~~~ not a fence')
   call text_written('***', BS//'***')
   ! code spans: a backtick string the content does not hold, padded when needed
   call code_written('a`b', '``a`b``')
   call code_written('`a', '`` `a ``')
   call code_written(' a ', '`  a  `')
   endsubroutine test_escaping

   subroutine test_by_hand()
   !< A tree built by hand (no source, no recorded style) is written in the canonical style and reads back equal.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   character(:), allocatable :: markdown !< Written.
   character(:), allocatable :: what     !< A difference.
   integer(I4P)              :: h, p, l, i, c, t, e, k !< Nodes.

   call doc%initialize
   h = add(doc, doc%root(), NODE_HEADING)
   call doc%set_heading_level(h, 2_I4P)
   t = add(doc, h, NODE_TEXT, 'Edited #')
   l = add(doc, doc%root(), NODE_LIST)
   call doc%set_list_type(l, LIST_ORDERED)
   call doc%set_list_start(l, 7_I4P)
   call doc%set_list_tight(l, .true.)
   i = add(doc, l, NODE_ITEM)
   p = add(doc, i, NODE_PARAGRAPH)
   t = add(doc, p, NODE_TEXT, 'see ')
   k = add(doc, p, NODE_LINK)
   call doc%set_destination(k, '/a b')
   call doc%set_title(k, 'say "hi"')
   t = add(doc, k, NODE_TEXT, 'the [docs]')
   e = add(doc, p, NODE_EMPH)
   t = add(doc, e, NODE_TEXT, ' spaced ')
   i = add(doc, l, NODE_ITEM)
   c = add(doc, i, NODE_CODE_BLOCK)
   call doc%set_info(c, 'f90 `x`')
   call doc%set_literal(c, '```'//LF//'end'//LF)
   p = add(doc, doc%root(), NODE_PARAGRAPH)
   t = add(doc, p, NODE_TEXT, 'one')
   t = add(doc, p, NODE_SOFTBREAK)
   t = add(doc, p, NODE_TEXT, '=====')
   call options%write_markdown(doc, markdown)
   call check(markdown == '## Edited '//BS//'#'//LF//LF//'7. see [the [docs]](</a b> "say '//BS// &
              '"hi'//BS//'"")*&#32;spaced&#32;*'//LF// &
              '8. ~~~f90 `x`'//LF//'   ```'//LF//'   end'//LF//'   ~~~'//LF//LF//'one'//LF//BS//'====='//LF, &
              'by hand: '//markdown)
   call check(reads_back(doc, markdown, what), 'by hand: reads back equal: '//what)
   endsubroutine test_by_hand

   subroutine test_sinks()
   !< Into a `character`, a `string`, a formatted unit, a stream unit and a file; errors through `stat`/`errmsg`.
   character(*), parameter   :: FILE_NAME = 'baruch_test_roundtrip.tmp.md' !< A file written and removed.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   type(string)              :: text     !< Into a string.
   character(:), allocatable :: markdown !< Into a character.
   character(:), allocatable :: errmsg   !< Message.
   character(64)             :: line     !< A line read back.
   character(3)              :: bytes    !< Bytes read back.
   integer(I4P)              :: stat     !< Status.
   integer                   :: u        !< Unit.

   call doc%load('# A'//LF//LF//'b'//LF)
   call options%parse(doc)
   call options%write_markdown(doc, markdown, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK .and. len(errmsg) == 0 .and. markdown == '# A'//LF//LF//'b'//LF, 'sinks: character')
   call options%write_markdown(doc, text, canonical=.true., stat=stat)
   call check(stat == BARUCH_OK .and. text%chars() == markdown, 'sinks: string')
   open(newunit=u, status='scratch', form='formatted', action='readwrite')
   call options%write_markdown(doc, u, stat=stat)
   rewind(u)
   read(u, '(A)') line
   call check(stat == BARUCH_OK .and. trim(line) == '# A', 'sinks: formatted unit, first record "'//trim(line)//'"')
   close(u)
   open(newunit=u, status='scratch', access='stream', form='unformatted', action='readwrite')
   call options%write_markdown(doc, u, stat=stat)
   read(u, pos=1) bytes
   call check(stat == BARUCH_OK .and. bytes == '# A', 'sinks: stream unit')
   close(u)
   call options%write_markdown(doc, u, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. index(errmsg, 'not open') > 0, 'sinks: a closed unit: '//errmsg)
   call options%write_markdown_file(doc, FILE_NAME, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'sinks: file: '//errmsg)
   call doc%load_file(FILE_NAME, stat=stat)
   call check(stat == BARUCH_OK .and. doc%source() == markdown, 'sinks: the file holds the Markdown')
   open(newunit=u, file=FILE_NAME, status='old')
   close(u, status='delete')
   call options%write_markdown_file(doc, 'no/such/directory/x.md', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. index(errmsg, 'write_markdown_file') > 0, 'sinks: a bad path: '//errmsg)
   endsubroutine test_sinks

   subroutine test_extensions()
   !< Extensions write their nodes through their hooks: a container, a leaf taking lines, inlines, a delimiter, a
   !< block of inline content; a hook failing stops the writer with its status.
   character(*), parameter   :: SOURCE = '!> %%one'//LF//'!> %%two'//LF//'!>'//LF//'!> see %x_1% and ==marked=='// &
                                         LF//LF//'@@ *title*'//LF//LF//'a 50'//BS//'% '//BS//'='//BS//'= b'//LF !< The source.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   type(math_extension)      :: math     !< Inline math, built by hand.
   character(:), allocatable :: markdown !< Written.
   character(:), allocatable :: errmsg   !< Message.
   character(:), allocatable :: what     !< A difference.
   integer(I4P)              :: stat     !< Status.
   integer(I4P)              :: p, m     !< Nodes.

   call options%register(note_extension())
   call options%register(percent_extension())
   call options%register(xvar_extension())
   call options%register(mark_extension())
   call options%register(title_extension())
   call doc%load(SOURCE)
   call options%parse(doc)
   call options%write_markdown(doc, markdown, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'extensions: written: '//errmsg)
   call check(markdown == SOURCE, 'extensions: written back as read: '//markdown)
   call check(reads_back(doc, markdown, what, options), 'extensions: read back equal: '//what)
   call options%write_markdown(doc, markdown, canonical=.true.)
   call check(reads_back(doc, markdown, what, options), 'extensions: canonical, read back equal: '//what)
   ! a hook failing
   math%mode = 3_I4P
   call options%register(math)
   call doc%initialize
   call options%register_kinds(doc)
   p = add(doc, doc%root(), NODE_PARAGRAPH)
   m = add(doc, p, KIND_MATH, 'x^2')
   call options%write_markdown(doc, markdown, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. index(errmsg, 'extension "math"') > 0 .and. &
              index(errmsg, 'refuses') > 0, 'extensions: a failing hook: '//errmsg)
   ! disabled, its node is written transparently: the literal as text
   call options%disable('math')
   call options%write_markdown(doc, markdown, stat=stat)
   call check(stat == BARUCH_OK .and. markdown == 'x^2'//LF, 'extensions: no hook, the literal: '//markdown)
   endsubroutine test_extensions

   subroutine test_escaper()
   !< `escape_markdown`: whatever surrounds it, a text reads back as one text.
   character(*), parameter   :: TEXTS(*) = [character(24) :: '*a* [b](c) `d` <e>', '# f &amp; g', '1. h_i_ '//BS//'j', &
                                            '![k] l'//BS, '- m', '___', '[n]: o']
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: text    !< A text.
   integer                   :: k       !< Counter.
   integer(I4P)              :: t       !< Its node.

   do k=1, size(TEXTS)
      text = trim(TEXTS(k))
      call doc%load(escape_markdown(text, line_start=.true.)//' x *y*'//LF)
      call options%parse(doc)
      t = doc%first_child(doc%first_child(doc%root()))
      call check(doc%node_kind(t) == NODE_TEXT .and. doc%literal(t) == text//' x ', 'escaper: "'//text//'" written "'// &
                 escape_markdown(text, line_start=.true.)//'"')
   enddo
   endsubroutine test_escaper

   subroutine test_deep()
   !< 10^5 nested block quotes and 10^4 nested lists, written with no recursion, read back equal.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   character(:), allocatable :: markdown !< Written.
   character(:), allocatable :: what     !< A difference.
   integer                   :: depth    !< Nesting.

   ! a variable, not a named constant: nvfortran 26.9 fails (internal error) on a repeat() of a long constant length
   ! folded in a contained procedure (reproducer notes/nvfortran-ilm-long-constant-string.f90)
   depth = 100000
   call doc%load(repeat('>', depth)//' deep'//LF)
   call options%parse(doc)
   call options%write_markdown(doc, markdown)
   call check(markdown == repeat('> ', depth)//'deep'//LF, 'deep quotes: written')
   call check(reads_back(doc, markdown, what), 'deep quotes: read back equal')
   depth = 10000
   call doc%load(repeat('- ', depth)//'x'//LF//repeat('  ', depth)//'y'//LF)
   call options%parse(doc)
   call options%write_markdown(doc, markdown, canonical=.true.)
   call check(reads_back(doc, markdown, what), 'deep lists: read back equal: '//what(1:min(80, len(what))))
   print '(A)', 'deep: 100000 nested block quotes and 10000 nested lists written and read back'
   endsubroutine test_deep

   ! helpers
   function written(source, canonical) result(markdown)
   !< Parse a source and write it back.
   character(*), intent(in)  :: source    !< Source.
   logical,      intent(in)  :: canonical !< Canonical style.
   character(:), allocatable :: markdown  !< Written.
   type(markdown_document)   :: doc       !< Document.
   type(markdown_options)    :: options   !< Options.
   character(:), allocatable :: what      !< A difference.

   call doc%load(source)
   call options%parse(doc)
   call options%write_markdown(doc, markdown, canonical=canonical)
   call check(reads_back(doc, markdown, what), 'written: "'//source//'" reads back: '//what)
   endfunction written

   subroutine text_written(literal, expected)
   !< A paragraph of one text, built by hand, is written as expected and reads back equal.
   character(*), intent(in)  :: literal  !< The text.
   character(*), intent(in)  :: expected !< Its Markdown.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   character(:), allocatable :: markdown !< Written.
   character(:), allocatable :: what     !< A difference.
   integer(I4P)              :: p, t     !< Nodes.

   call doc%initialize
   p = add(doc, doc%root(), NODE_PARAGRAPH)
   t = add(doc, p, NODE_TEXT, literal)
   call options%write_markdown(doc, markdown)
   call check(markdown == expected//LF, 'escaping: "'//literal//'" written "'//markdown(1:len(markdown)-1)// &
              '", expected "'//expected//'"')
   call check(reads_back(doc, markdown, what), 'escaping: "'//literal//'" reads back: '//what)
   endsubroutine text_written

   subroutine code_written(literal, expected)
   !< A code span built by hand is written as expected and reads back equal.
   character(*), intent(in)  :: literal  !< Its content.
   character(*), intent(in)  :: expected !< Its Markdown.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   character(:), allocatable :: markdown !< Written.
   character(:), allocatable :: what     !< A difference.
   integer(I4P)              :: p, t     !< Nodes.

   call doc%initialize
   p = add(doc, doc%root(), NODE_PARAGRAPH)
   t = add(doc, p, NODE_CODE, literal)
   call options%write_markdown(doc, markdown)
   call check(markdown == expected//LF, 'code span: "'//literal//'" written "'//markdown//'"')
   call check(reads_back(doc, markdown, what), 'code span: "'//literal//'" reads back: '//what)
   endsubroutine code_written

   function add(doc, parent, kind, literal) result(node)
   !< Append a new node of a kind to a parent, with a literal.
   type(markdown_document), intent(inout)        :: doc     !< Document.
   integer(I4P),            intent(in)           :: parent  !< Parent.
   integer(I4P),            intent(in)           :: kind    !< Kind.
   character(*),            intent(in), optional :: literal !< Literal.
   integer(I4P)                                  :: node    !< New node.

   node = doc%new_node(kind)
   call doc%append_child(parent, node)
   if (present(literal)) call doc%set_literal(node, literal)
   endfunction add

   function reads_back(doc, markdown, what, options) result(same)
   !< Return true if Markdown parses to a document equal to `doc`; `what` the difference otherwise.
   type(markdown_document),   intent(in)           :: doc      !< Document.
   character(*),              intent(in)           :: markdown !< Markdown.
   character(:), allocatable, intent(out)          :: what     !< Difference.
   type(markdown_options),    intent(in), optional :: options  !< Options of the parse (default: none).
   logical                                         :: same     !< Result.
   type(markdown_document)                         :: again    !< The Markdown parsed.
   type(markdown_options)                          :: none     !< No extension.

   call again%load(markdown)
   if (present(options)) then
      call options%parse(again)
   else
      call none%parse(again)
   endif
   same = doc%equal(again, difference=what)
   endfunction reads_back

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
endprogram baruch_test_roundtrip
