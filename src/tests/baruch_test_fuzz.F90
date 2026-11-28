!< BARUCH test: mutation fuzzing of the reader and random-tree fuzzing of the writer, deterministic given a seed.
program baruch_test_fuzz
!< BARUCH test: mutation fuzzing of the reader and random-tree fuzzing of the writer, deterministic given a seed.
!<
!< The gate of plan step 3.3 (reference page docs/reference/conformance.md, "Fuzzing"). Two fuzzers:
!<
!< - **mutations of the reader**: an example of the CommonMark fixtures (`src/tests/spec/commonmark-0.31.2`, read from
!<   the project's root), mutated 1 to 8 times (bytes inserted, deleted, replaced; spans duplicated or spliced from
!<   another example; Markdown-significant tokens, line endings, tabs, ill-formed UTF-8 and U+0000 injected), is
!<   loaded, parsed, rendered as HTML, walked and written back as Markdown in the recorded and in the canonical style;
!<   nothing may fail, every output must be well-formed UTF-8 (StringiFor `is_utf8`), and each Markdown written must
!<   parse to a tree equal to the first (`markdown_document%equal`);
!< - **random trees of the writer**: a document built with the document API (every core kind, nested, texts with
!<   special characters and Unicode, links with odd destinations and titles, code with backticks, random recorded
!<   style slots), written in both styles, read back and compared the same way. A tree is drawn among the trees
!<   Markdown can express ([Writer](/reference/writer), "Trees that cannot be written"): `writable` below.
!<
!< Every draw comes from a xoshiro128** generator held in a variable (no module state, no `random_number`), seeded
!< from the seed, the fuzzer and the iteration by integer arithmetic that never overflows: iteration `i` is the same
!< input with every compiler, and can be replayed alone (`--start i --mutations 1`). The run prints an FNV-1a digest of
!< every input it made, the same for every compiler.
!<
!< A failing input is shrunk (bytes, or nodes and text, removed while the same check still fails), printed (bytes
!< escaped, or the tree dumped with the Markdown written from it) and, with `--out`, saved. Inputs the fuzzers once
!< found, fixed since, are checked first (`test_regressions`).
!<
!< With no argument (as `scripts/run_tests.sh` and `fpm test` run it): 10^4 mutations and 10^4 trees with the seed
!< 20261127. `scripts/fuzz.sh` runs longer campaigns:
!<
!<    baruch_test_fuzz [--seed S] [--mutations N] [--trees N] [--start I] [--progress K] [--trace FILE] [--out DIR]
!<    baruch_test_fuzz --dump FILE
!<    baruch_test_fuzz --check FILE
!<
!< `--progress K` prints a line every `K` iterations, `--trace FILE` writes each input to `FILE` before checking it (a
!< crash leaves the culprit there), `--out DIR` saves the shrunk failing inputs, `--dump FILE` prints the tree of a
!< Markdown file, `--check FILE` checks one file as a mutant is checked. The exit status is 1 when an input fails, 2 on
!< a misuse.
use, intrinsic :: iso_fortran_env, only : output_unit
use baruch
use penf,       only : I4P, I8P
use stringifor, only : string
implicit none
character,    parameter :: LF  = achar(10) !< Line feed.
character,    parameter :: CR  = achar(13) !< Carriage return.
character,    parameter :: TAB = achar(9)  !< Tab.
character,    parameter :: BS  = achar(92) !< Backslash (not a literal: nvfortran reads it as an escape).
character,    parameter :: QT  = achar(34) !< Double quote.
character,    parameter :: AP  = achar(39) !< Apostrophe.
character,    parameter :: US  = achar(31) !< Separator of the pooled lists.
character(*), parameter :: DIR = 'src/tests/spec/commonmark-0.31.2/examples/' !< Fixtures, from the project's root.
integer,      parameter :: EXAMPLES = 652                !< Examples of the fixtures.
integer(I8P), parameter :: M32 = 4294967295_I8P          !< 2^32 - 1.
integer(I8P), parameter :: GOLD = 2654435769_I8P         !< 2^32 / golden ratio (0x9E3779B9).
integer(I8P), parameter :: MIX1 = 2246822507_I8P         !< MurmurHash3 fmix32 multiplier (0x85EBCA6B).
integer(I8P), parameter :: MIX2 = 3266489909_I8P         !< MurmurHash3 fmix32 multiplier (0xC2B2AE35).
integer(I8P), parameter :: FNV_BASIS = 2166136261_I8P    !< FNV-1a 32-bit offset basis.
integer(I8P), parameter :: FNV_PRIME = 16777619_I8P      !< FNV-1a 32-bit prime.
integer(I8P), parameter :: SEED_DEFAULT = 20261127_I8P   !< The seed of the CI run.
integer,      parameter :: RUNS_DEFAULT = 10000          !< Mutations and trees of the CI run.
integer,      parameter :: STREAM_MUTATION = 1           !< Stream of the mutation fuzzer.
integer,      parameter :: STREAM_TREE = 2               !< Stream of the tree fuzzer.
integer,      parameter :: SHRINK_BUDGET = 4000          !< Checks a shrink may spend.
! what failed
integer,      parameter :: F_NONE = 0, F_LOAD = 1, F_PARSE = 2, F_HTML = 3, F_HTML_UTF8 = 4, F_WALK = 5, F_WRITE = 6, &
                           F_WRITE_UTF8 = 7, F_REPARSE = 8, F_RECORDED = 9, F_CANONICAL = 10
character(*), parameter :: F_NAMES(0:10) = [character(24) :: 'none', 'load', 'parse', 'render_html', &
                                            'HTML not UTF-8', 'walk', 'write_markdown', 'Markdown not UTF-8', &
                                            'parse of the Markdown', 'round trip, recorded', 'round trip, canonical']

type :: rng_t
   !< xoshiro128** (Blackman, Vigna), its four 32-bit words held in 64-bit integers: no arithmetic overflows.
   integer(I8P) :: s(4) = 0_I8P !< State.
endtype rng_t

type :: pool_t
   !< A list of byte strings in one buffer.
   character(:), allocatable :: chars    !< The strings, one after the other.
   integer,      allocatable :: first(:) !< First byte of each.
   integer,      allocatable :: last(:)  !< Last byte of each.
endtype pool_t

integer                   :: failures     !< Failed inputs.
integer                   :: reported     !< Failures reported in full.
integer(I8P)              :: seed         !< Seed.
integer                   :: mutations    !< Mutations to run.
integer                   :: trees        !< Trees to run.
integer                   :: start        !< First iteration.
integer                   :: progress     !< Print a line every so many iterations (0: never).
integer                   :: reports_max  !< Failures reported in full; the others are counted.
integer                   :: rejected     !< Trees drawn that Markdown cannot express, drawn again.
integer(I8P)              :: digest       !< FNV-1a of every input made.
character(:), allocatable :: trace        !< File each input is written to before its check (empty: none).
character(:), allocatable :: out_dir      !< Directory of the failing inputs (empty: none).
character(256)            :: bytes        !< Every byte: `bytes(i+1:i+1)` is `char(i)`.
type(pool_t)              :: fixtures     !< The examples.
type(pool_t)              :: tokens       !< What a mutation injects.
type(pool_t)              :: pieces       !< What texts are made of.
type(pool_t)              :: url_pieces   !< What destinations are made of.
type(pool_t)              :: html_inlines !< Raw inline HTML.
type(pool_t)              :: html_blocks  !< HTML blocks.
type(pool_t)              :: labels       !< Reference labels.

failures = 0
reported = 0
rejected = 0
digest = FNV_BASIS
call read_arguments
call build_tables
call test_regressions
call fuzz_mutations
call fuzz_trees
print '(A,I0,A,I0,A,I0,A,I0,A,I0,A,Z8.8,A,I0,A)', 'fuzz: seed ', seed, ', mutations ', start, '-', &
   start + mutations - 1, ', trees ', trees, ' (', rejected, ' redrawn), digest ', digest, ', ', failures, ' failure(s)'
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_fuzz: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_fuzz: ok'

contains
   ! the two fuzzers
   subroutine fuzz_mutations()
   !< Mutate the examples, check every mutant.
   type(rng_t)               :: r      !< Generator.
   character(:), allocatable :: s      !< The mutant.
   character(:), allocatable :: what   !< What failed.
   integer                   :: i      !< Iteration.
   integer                   :: k      !< Example.
   integer                   :: code   !< Check result.

   do i=start, start + mutations - 1
      call rng_seed(r, seed, STREAM_MUTATION, i)
      k = 1 + uniform(r, EXAMPLES)
      s = item(fixtures, k)
      call mutate(r, s)
      call fold(digest, s)
      if (len(trace) > 0) call write_bytes(trace, s)
      code = check_source(s, what)
      if (code /= F_NONE) call report_source('mutation '//str(i)//' (example '//str(k)//')', s, code, what)
      if (progress > 0) then
         if (mod(i - start + 1, progress) == 0) then
            print '(A,I0,A,I0)', 'mutations: ', i - start + 1, ' done, failures ', failures
            flush(output_unit)
         endif
      endif
   enddo
   endsubroutine fuzz_mutations

   subroutine fuzz_trees()
   !< Draw random trees, check every one.
   type(rng_t)               :: r    !< Generator.
   type(markdown_document)   :: doc  !< The tree.
   character(:), allocatable :: what !< What failed.
   integer                   :: i    !< Iteration.
   integer                   :: code !< Check result.

   do i=start, start + trees - 1
      call rng_seed(r, seed, STREAM_TREE, i)
      do
         call draw_document(r, doc)
         if (writable(doc, what)) exit
         rejected = rejected + 1
      enddo
      what = dump(doc)
      call fold(digest, what)
      if (len(trace) > 0) call write_bytes(trace, what)
      code = check_doc(doc, what)
      if (code /= F_NONE) call report_tree('tree '//str(i), doc, code, what)
      if (progress > 0) then
         if (mod(i - start + 1, progress) == 0) then
            print '(A,I0,A,I0)', 'trees: ', i - start + 1, ' done, failures ', failures
            flush(output_unit)
         endif
      endif
   enddo
   endsubroutine fuzz_trees

   subroutine test_regressions()
   !< Inputs the fuzzers found failing (shrunk), fixed since: each must now pass every check. Sources from the mutation
   !< fuzzer (`check_source`), trees from the tree fuzzer, built here (`check_doc`); the reference page lists the bugs.
   character(:), allocatable :: what  !< What failed.
   character(:), allocatable :: list  !< The sources.
   integer                   :: code  !< Check result.
   integer                   :: k     !< Counter.
   type(pool_t)              :: cases !< The sources.

   ! sources: emphasis runs, raw HTML, brackets and links, definitions, lists and their markers, code, references
   list = '_o*_o*___'//US//'&#42;v*i*'//US//'**(*G**'//by([149])//'**'//US//'***_* b*r*'//US//                     &
          '**-*****r******b**'//US//'******f*b***f*o*'//US//'__"_*_'//by([192])//'o_'//by([175])//'__'//US//       &
          '**;*"* b*)**'//US//'**-'//CR//'**-*'//LF//'-**'//US//'*'//by([128])//'**?'//LF//'  *o*****'//US//       &
          by([255])//'* '//BS//LF//BS//'i*'//US//BS//' '//BS//LF//BS//US//                                          &
          '>'//LF//'2. <!A'//LF//')'//US//'<!--'//LF//LF//US//'<!A'//LF//LF//US//'<?'//LF//LF//US//                 &
          '-   o'//LF//'  <div'//US//'- '//LF//'-'//LF//'   <!A'//US//'<a f='//BS//'">'//CR//'">'//US//           &
          '[*]:*]: t'//US//'['//LF//']:'//LF//']:/'//US//'[|]: <!--~4'//US//'[](l &quot;<a>&quot;)'//US//          &
          '![[]()'//BS//']()'//US//BS//'![[[]]()]()'//US//'![[[]()](])]()'//US//'[](<<!- >)'//US//                  &
          '[](('//BS//')<v>)'//US//'[](<o'//BS//'>.)'//US//'[](&#10;)'//US//                                        &
          '[F]:'//by([189])//LF//'<l>'//US//'[o]:>'//LF//'    <td>'//US//                                           &
          '-'//CR//'  -     -'//US//'* &#42;*'//US//'-'//TAB//'b'//LF//LF//'  r'//US//'-'//TAB//'a'//LF//'  -'//US// &
          '-  ```'//LF//'  c'//US//'- '//LF//'  >a'//LF//'  >'//LF//'  `'//US//'2. >r'//LF//'   3)'//US//          &
          '* 1. h'//LF//'  *'//US//'~~~&#10;'//US//'-  <?'//CR//'  -'//US//' -  <div>'//LF//'   -'//US//        &
          ' -     o'//LF//'  <r>'//US//'* * - - *'//US//BS//'[]('//BS//BS//')'//US//                            &
          '[F]::'//LF//'<n>'//LF//AP//LF//'-'
   call pool_from(list, cases)
   do k=1, size(cases%first)
      code = check_source(item(cases, k), what)
      if (code /= F_NONE) call report_source('regression '//str(k), item(cases, k), code, what)
   enddo
   call test_regression_trees
   endsubroutine test_regressions

   subroutine test_regression_trees()
   !< Trees the tree fuzzer found failing (shrunk), fixed since: each must now be written so that it reads back.
   type(markdown_document)   :: doc  !< A tree.
   character(:), allocatable :: what !< What failed.
   integer(I4P)              :: p, n, l, i, e !< Nodes.
   integer                   :: k    !< Case.
   integer                   :: code !< Check result.

   do k=1, 16
      call doc%initialize
      select case(k)
      case(1) ! a letter before an emphasis of punctuation: the letter as a reference
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         call add_text(doc, p, 'l')
         e = add(doc, p, NODE_EMPH)
         n = add(doc, e, NODE_HTML_INLINE, '<!X y>')
      case(2) ! a letter after strong of punctuation
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         e = add(doc, p, NODE_STRONG)
         n = add(doc, e, NODE_LINK)
         call add_text(doc, p, 'F')
      case(3) ! `_` within a word
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         call add_text(doc, p, '1')
         e = add(doc, p, NODE_EMPH)
         call doc%set_int_slot(e, SLOT_EMPHASIS_CHAR, int(iachar('_'), I4P))
         n = add(doc, e, NODE_CODE, 't')
      case(4) ! an emphasis first in an emphasis, a letter after it: the inner one takes `_`
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         e = add(doc, p, NODE_EMPH)
         n = add(doc, e, NODE_EMPH)
         call add_text(doc, n, '6')
         call add_text(doc, e, 'D')
      case(5) ! an indented code block after a paragraph in a tight list: fenced
         l = add(doc, doc%root(), NODE_LIST)
         call doc%set_list_type(l, LIST_BULLET)
         call doc%set_list_tight(l, .true.)
         i = add(doc, l, NODE_ITEM)
         p = add(doc, i, NODE_PARAGRAPH)
         call add_text(doc, p, '2')
         n = add(doc, i, NODE_CODE_BLOCK, 'f'//LF)
         call doc%set_int_slot(n, SLOT_CODE_FORM, CODE_INDENTED)
      case(6) ! a heading of level 2 whose link title spans lines: setext
         p = add(doc, doc%root(), NODE_HEADING)
         call doc%set_heading_level(p, 2_I4P)
         n = add(doc, p, NODE_LINK)
         call doc%set_destination(n, 'u')
         call doc%set_title(n, 'a'//LF//'  b')
      case(7) ! empty items behind bullets: not a thematic break
         l = add(doc, doc%root(), NODE_LIST)
         call doc%set_list_type(l, LIST_BULLET)
         call doc%set_list_tight(l, .true.)
         i = add(doc, l, NODE_ITEM)
         l = add(doc, i, NODE_LIST)
         call doc%set_list_type(l, LIST_BULLET)
         call doc%set_list_tight(l, .true.)
         i = add(doc, l, NODE_ITEM)
         l = add(doc, i, NODE_LIST)
         call doc%set_list_type(l, LIST_BULLET)
         call doc%set_list_tight(l, .true.)
         i = add(doc, l, NODE_ITEM)
      case(8) ! an info string starting with the fence character
         n = add(doc, doc%root(), NODE_CODE_BLOCK, '')
         call doc%set_info(n, '~~~')
         call doc%set_int_slot(n, SLOT_CODE_FORM, CODE_FENCED)
         call doc%set_int_slot(n, SLOT_CODE_FENCE_CHAR, int(iachar('~'), I4P))
      case(9) ! a backtick in an image's description and in its destination
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         n = add(doc, p, NODE_IMAGE)
         call doc%set_destination(n, '`b')
         call add_text(doc, n, '`')
      case(10) ! texts of backtick strings, the later ones escaped
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         call add_text(doc, p, '`')
         call add_text(doc, p, 'rN```/')
         call add_text(doc, p, BS//'```')
      case(11) ! a backslash before a character written as a reference
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         e = add(doc, p, NODE_STRONG)
         n = add(doc, e, NODE_EMPH)
         call doc%set_int_slot(n, SLOT_EMPHASIS_CHAR, int(iachar('_'), I4P))
         call add_text(doc, n, 'C'//BS//by([194, 160]))
      case(12) ! a text of `*` before an emphasis whose opener can close: escaped
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         call add_text(doc, p, '**')
         e = add(doc, p, NODE_STRONG)
         call doc%set_int_slot(e, SLOT_EMPHASIS_CHAR, int(iachar('_'), I4P))
         n = add(doc, e, NODE_EMPH)
         call add_text(doc, n, 'j')
         e = add(doc, p, NODE_STRONG)
         call add_text(doc, e, 'E')
         n = add(doc, e, NODE_STRONG)
         call add_text(doc, n, 'k')
      case(13) ! a definition with a line ending in its title, then a paragraph, in a quote of a tight list
         l = add(doc, doc%root(), NODE_LIST)
         call doc%set_list_type(l, LIST_BULLET)
         call doc%set_list_tight(l, .true.)
         i = add(doc, l, NODE_ITEM)
         e = add(doc, i, NODE_BLOCK_QUOTE)
         p = add(doc, e, NODE_PARAGRAPH)
         call add_text(doc, p, 'a')
         p = add(doc, i, NODE_PARAGRAPH)
         call add_text(doc, p, '`')
      case(15) ! a letter, an empty text, an emphasis of `_` first in an emphasis: the opener needs a reference too
         p = add(doc, doc%root(), NODE_PARAGRAPH)
         call add_text(doc, p, 'm')
         e = add(doc, p, NODE_EMPH)
         call add_text(doc, e, 'W')
         call add_text(doc, e, '')
         n = add(doc, e, NODE_EMPH)
         call doc%set_int_slot(n, SLOT_EMPHASIS_CHAR, int(iachar('_'), I4P))
         l = add(doc, n, NODE_LINK)
         call doc%set_destination(l, 'u')
      case(16) ! all the delimiters the other character than first chosen (the canonical style)
         l = add(doc, doc%root(), NODE_PARAGRAPH)
         e = add(doc, l, NODE_STRONG)
         call add_text(doc, e, 'p')
         e = add(doc, l, NODE_STRONG)
         n = add(doc, e, NODE_STRONG)
         call add_text(doc, n, '2z1. *'//by([194, 160]))
         call add_text(doc, e, by([226, 128, 131])//'&')
         n = add(doc, l, NODE_HTML_INLINE, '<?p x?>')
      case(14) ! a setext heading starting like a definition
         p = add(doc, doc%root(), NODE_HEADING)
         call doc%set_heading_level(p, 1_I4P)
         call add_text(doc, p, '[a]: b')
         n = add(doc, p, NODE_SOFTBREAK)
         call add_text(doc, p, 'c')
      endselect
      code = check_doc(doc, what)
      if (code /= F_NONE) call report_tree('regression tree '//str(k), doc, code, what)
   enddo
   endsubroutine test_regression_trees

   ! the checks
   function check_source(source, what) result(code)
   !< Load and parse a source, then check its tree (`check_doc`); return what failed, `F_NONE` if nothing.
   character(*),              intent(in)  :: source  !< Source, any bytes.
   character(:), allocatable, intent(out) :: what    !< What failed, empty if nothing.
   integer                                :: code    !< What failed.
   type(markdown_document)                :: doc     !< Document.
   type(markdown_options)                 :: options !< No extension.
   integer(I4P)                           :: stat    !< Status.
   character(:), allocatable              :: errmsg  !< Message.

   what = ''
   code = F_LOAD
   call doc%load(source, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) then
      what = errmsg
      return
   endif
   code = F_PARSE
   call options%parse(doc, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) then
      what = errmsg
      return
   endif
   code = check_doc(doc, what)
   endfunction check_source

   function check_doc(doc, what, recorded, canonical) result(code)
   !< Render a tree as HTML, walk it, write it in both styles, read each back: nothing fails, every output is UTF-8,
   !< each read back equals the tree. Return what failed, `F_NONE` if nothing; the Markdown written on request.
   type(markdown_document),   intent(in)            :: doc       !< The tree.
   character(:), allocatable, intent(out)           :: what      !< What failed, empty if nothing.
   character(:), allocatable, intent(out), optional :: recorded  !< Markdown, recorded style.
   character(:), allocatable, intent(out), optional :: canonical !< Markdown, canonical style.
   integer                                          :: code      !< What failed.
   type(markdown_options)                           :: options   !< No extension.
   type(markdown_document)                          :: again     !< The Markdown read back.
   character(:), allocatable                        :: html      !< HTML.
   character(:), allocatable                        :: markdown  !< Markdown.
   character(:), allocatable                        :: errmsg    !< Message.
   integer(I4P)                                     :: stat      !< Status.
   integer(I4P)                                     :: node      !< Walk: node.
   integer(I4P)                                     :: event     !< Walk: event.
   integer                                          :: events    !< Walk: events.
   integer                                          :: m         !< Style.

   what = ''
   if (present(recorded)) recorded = ''
   if (present(canonical)) canonical = ''
   code = F_HTML
   call options%render_html(doc, html, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) then
      what = errmsg
      return
   endif
   code = F_HTML_UTF8
   if (.not. is_utf8(html)) return
   code = F_WALK
   node = 0_I4P
   event = EVENT_NONE
   events = 0
   do
      call options%walk(doc, node, event, stat=stat, errmsg=errmsg)
      if (stat /= BARUCH_OK) then
         what = errmsg
         return
      endif
      if (node == 0_I4P) exit
      events = events + 1
      if (events > 2 * doc%last_node() + 2) then
         what = 'more events than twice the nodes'
         return
      endif
   enddo
   do m=1, 2
      code = F_WRITE
      call options%write_markdown(doc, markdown, canonical=m == 2, stat=stat, errmsg=errmsg)
      if (m == 1 .and. present(recorded)) recorded = markdown
      if (m == 2 .and. present(canonical)) canonical = markdown
      if (stat /= BARUCH_OK) then
         what = errmsg
         return
      endif
      code = F_WRITE_UTF8
      if (.not. is_utf8(markdown)) return
      code = F_REPARSE
      call again%load(markdown, stat=stat, errmsg=errmsg)
      if (stat == BARUCH_OK) call options%parse(again, stat=stat, errmsg=errmsg)
      if (stat /= BARUCH_OK) then
         what = errmsg
         return
      endif
      code = merge(F_CANONICAL, F_RECORDED, m == 2)
      if (.not. doc%equal(again, difference=what)) return
   enddo
   code = F_NONE
   what = ''
   endfunction check_doc

   function is_utf8(text) result(is)
   !< Return true if a text is well-formed UTF-8 (StringiFor).
   character(*), intent(in) :: text !< Text.
   logical                  :: is   !< Result.
   type(string)             :: s    !< The text as a string.

   s = text
   is = s%is_utf8()
   endfunction is_utf8

   ! failures: shrink, print, save
   subroutine report_source(label, source, code, what)
   !< Count a failing source; shrink it, print it and save it (the first `--reports` failures).
   character(*), intent(in)  :: label  !< Which input.
   character(*), intent(in)  :: source !< The source.
   integer,      intent(in)  :: code   !< What failed.
   character(*), intent(in)  :: what   !< Details.
   character(:), allocatable :: small  !< Shrunk.
   character(:), allocatable :: detail !< Its details.
   character(:), allocatable :: recorded, canonical !< Markdown written from it.
   type(markdown_document)   :: doc    !< Its tree.
   type(markdown_options)    :: options !< No extension.
   integer                   :: c      !< Its check.

   failures = failures + 1
   if (reported >= reports_max) return
   reported = reported + 1
   small = source
   call shrink_source(small, code)
   c = check_source(small, detail)
   print '(A)', 'FAIL '//label//': '//trim(F_NAMES(code))//': '//what
   print '(A,I0,A,I0,A)', '  input, shrunk from ', len(source), ' to ', len(small), ' bytes: "'//escaped(small)//'"'
   print '(A)', '  shrunk: '//trim(F_NAMES(c))//': '//detail
   call doc%load(small)
   call options%parse(doc)
   call options%write_markdown(doc, recorded)
   call options%write_markdown(doc, canonical, canonical=.true.)
   print '(A)', '  recorded:  "'//escaped(recorded)//'"'
   print '(A)', '  canonical: "'//escaped(canonical)//'"'
   if (len(out_dir) > 0) then
      call write_bytes(out_dir//'/'//file_name(label)//'.md', source)
      call write_bytes(out_dir//'/'//file_name(label)//'.shrunk.md', small)
   endif
   endsubroutine report_source

   subroutine report_tree(label, doc, code, what)
   !< Count a failing tree; shrink it, print it and save it (the first `--reports` failures).
   character(*),            intent(in) :: label  !< Which input.
   type(markdown_document), intent(in) :: doc    !< The tree.
   integer,                 intent(in) :: code   !< What failed.
   character(*),            intent(in) :: what   !< Details.
   type(markdown_document)             :: small  !< Shrunk.
   character(:), allocatable           :: detail !< Its details.
   character(:), allocatable           :: recorded, canonical !< Markdown written from it.
   type(markdown_options)              :: options !< No extension.
   integer                             :: c      !< Its check.

   failures = failures + 1
   if (reported >= reports_max) return
   reported = reported + 1
   small = doc
   call shrink_tree(small, code)
   c = check_doc(small, detail)
   call options%write_markdown(small, recorded)
   call options%write_markdown(small, canonical, canonical=.true.)
   print '(A)', 'FAIL '//label//': '//trim(F_NAMES(code))//': '//what
   print '(A)', '  shrunk: '//trim(F_NAMES(c))//': '//detail
   print '(A)', '  tree:'
   print '(A)', dump(small)
   print '(A)', '  recorded:  "'//escaped(recorded)//'"'
   print '(A)', '  canonical: "'//escaped(canonical)//'"'
   if (len(out_dir) > 0) then
      call write_bytes(out_dir//'/'//file_name(label)//'.tree.txt', dump(doc))
      call write_bytes(out_dir//'/'//file_name(label)//'.shrunk.tree.txt', dump(small))
      call write_bytes(out_dir//'/'//file_name(label)//'.shrunk.recorded.md', recorded)
      call write_bytes(out_dir//'/'//file_name(label)//'.shrunk.canonical.md', canonical)
   endif
   endsubroutine report_tree

   subroutine shrink_source(source, code)
   !< Remove spans of a source, halving their length down to one byte, while the same check still fails.
   character(:), allocatable, intent(inout) :: source !< Source.
   integer,                   intent(in)    :: code   !< What fails.
   character(:), allocatable                :: trial  !< A shorter source.
   character(:), allocatable                :: what   !< Details.
   integer                                  :: chunk  !< Bytes removed at once.
   integer                                  :: p      !< First byte removed.
   integer                                  :: budget !< Checks left.

   budget = SHRINK_BUDGET
   chunk = max(1, len(source) / 2)
   do
      p = 1
      do while (p <= len(source) .and. budget > 0)
         trial = source(1:p-1)//source(min(p + chunk, len(source) + 1):)
         budget = budget - 1
         if (check_source(trial, what) == code) then
            source = trial
         else
            p = p + chunk
         endif
      enddo
      if (chunk == 1 .or. budget <= 0) exit
      chunk = chunk / 2
   enddo
   endsubroutine shrink_source

   subroutine shrink_tree(doc, code)
   !< Unlink nodes and shorten texts of a tree, while it stays writable and the same check still fails.
   type(markdown_document), intent(inout) :: doc     !< The tree.
   integer,                 intent(in)    :: code    !< What fails.
   type(markdown_document)                :: trial   !< A smaller tree.
   character(:), allocatable              :: what    !< Details.
   character(:), allocatable              :: literal !< A literal.
   integer(I4P)                           :: n       !< Node.
   integer(I4P)                           :: k       !< Its kind.
   integer                                :: budget  !< Checks left.
   integer                                :: h       !< Half.
   logical                                :: changed !< A pass changed the tree.

   budget = SHRINK_BUDGET
   changed = .true.
   do while (changed .and. budget > 0)
      changed = .false.
      do n=doc%last_node(), 2_I4P, -1_I4P
         if (budget <= 0) exit
         if (.not. doc%is_node(n)) cycle
         trial = doc
         call trial%unlink(n)
         if (.not. writable(trial, what)) cycle
         budget = budget - 1
         if (check_doc(trial, what) == code) then
            doc = trial
            changed = .true.
         endif
      enddo
      do n=2_I4P, doc%last_node()
         if (budget <= 0) exit
         if (.not. doc%is_node(n)) cycle
         k = doc%node_kind(n)
         if (k /= NODE_TEXT .and. k /= NODE_CODE .and. k /= NODE_CODE_BLOCK) cycle
         literal = doc%literal(n)
         do h=1, 2
            if (len(literal) < 2) exit
            trial = doc
            if (h == 1) call trial%set_literal(n, literal(1:len(literal)/2))
            if (h == 2) call trial%set_literal(n, literal(len(literal)/2+1:))
            if (.not. writable(trial, what)) cycle
            budget = budget - 1
            if (check_doc(trial, what) == code) then
               doc = trial
               literal = doc%literal(n)
               changed = .true.
            endif
         enddo
      enddo
   enddo
   endsubroutine shrink_tree

   ! the mutations
   subroutine mutate(r, s)
   !< Apply 1 to 8 random mutations to a source.
   type(rng_t),               intent(inout) :: r  !< Generator.
   character(:), allocatable, intent(inout) :: s  !< Source.
   integer                                  :: m  !< Mutation.
   integer                                  :: p  !< Position, 1 to len(s) + 1.
   integer                                  :: l  !< Length of a span.
   integer                                  :: q  !< Another position.
   character(:), allocatable                :: other !< Another example.

   do m=1, 1 + uniform(r, 8)
      p = 1 + uniform(r, len(s) + 1)
      select case(uniform(r, 12))
      case(0, 1, 2) ! a token
         s = s(1:p-1)//item(tokens, 1 + uniform(r, size(tokens%first)))//s(p:)
      case(3) ! any byte
         q = 1 + uniform(r, 256)
         s = s(1:p-1)//bytes(q:q)//s(p:)
      case(4, 5) ! delete a span
         if (len(s) == 0) cycle
         p = min(p, len(s))
         l = 1 + uniform(r, min(16, len(s) - p + 1))
         s = s(1:p-1)//s(p+l:)
      case(6) ! replace a byte by a token's first byte or any byte
         if (len(s) == 0) cycle
         p = min(p, len(s))
         if (uniform(r, 2) == 0) then
            q = 1 + uniform(r, size(tokens%first))
            q = tokens%first(q)
            if (q <= len(tokens%chars)) s(p:p) = tokens%chars(q:q)
         else
            q = 1 + uniform(r, 256)
            s(p:p) = bytes(q:q)
         endif
      case(7, 8) ! duplicate a span, elsewhere
         if (len(s) == 0) cycle
         p = min(p, len(s))
         l = 1 + uniform(r, min(32, len(s) - p + 1))
         q = 1 + uniform(r, len(s) + 1)
         s = s(1:q-1)//s(p:p+l-1)//s(q:)
      case(9) ! splice a span of another example
         other = item(fixtures, 1 + uniform(r, EXAMPLES))
         if (len(other) == 0) cycle
         q = 1 + uniform(r, len(other))
         l = 1 + uniform(r, min(48, len(other) - q + 1))
         s = s(1:p-1)//other(q:q+l-1)//s(p:)
      case(10) ! a line ending
         select case(uniform(r, 3))
         case(0)
            s = s(1:p-1)//LF//s(p:)
         case(1)
            s = s(1:p-1)//CR//s(p:)
         case default
            s = s(1:p-1)//CR//LF//s(p:)
         endselect
      case default ! repeat a token
         q = 1 + uniform(r, size(tokens%first))
         l = 2 + uniform(r, 6)
         other = ''
         do while (l > 0)
            other = other//item(tokens, q)
            l = l - 1
         enddo
         s = s(1:p-1)//other//s(p:)
      endselect
   enddo
   endsubroutine mutate

   ! the random trees
   subroutine draw_document(r, doc)
   !< Draw a random document: 1 to 6 blocks, nested up to 4 levels.
   type(rng_t),             intent(inout) :: r   !< Generator.
   type(markdown_document), intent(inout) :: doc !< The tree.

   call doc%initialize
   call draw_blocks(r, doc, doc%root(), 1 + uniform(r, 6), 0, .false.)
   endsubroutine draw_document

   recursive subroutine draw_blocks(r, doc, parent, count, depth, tight)
   !< Append random blocks to a container; in an item of a tight list, only blocks that can follow each other there.
   type(rng_t),             intent(inout) :: r      !< Generator.
   type(markdown_document), intent(inout) :: doc    !< The tree.
   integer(I4P),            intent(in)    :: parent !< Container.
   integer,                 intent(in)    :: count  !< Blocks.
   integer,                 intent(in)    :: depth  !< Nesting of the container.
   logical,                 intent(in)    :: tight  !< The container is an item of a tight list.
   integer(I4P)                           :: b      !< A block.
   integer(I4P)                           :: prev   !< The block before.
   integer                                :: k      !< Counter.
   integer                                :: attempt !< Attempts at a block that can follow.

   prev = 0_I4P
   do k=1, count
      do attempt=1, 4
         b = draw_block(r, doc, parent, depth)
         if (.not. tight .or. prev == 0_I4P) exit
         if (pair_ok(doc, prev, b)) exit
         call doc%unlink(b)
         b = 0_I4P
      enddo
      if (b == 0_I4P) exit
      prev = b
   enddo
   endsubroutine draw_blocks

   recursive function draw_block(r, doc, parent, depth) result(b)
   !< Append a random block to a container.
   type(rng_t),             intent(inout) :: r      !< Generator.
   type(markdown_document), intent(inout) :: doc    !< The tree.
   integer(I4P),            intent(in)    :: parent !< Container.
   integer,                 intent(in)    :: depth  !< Nesting of the container.
   integer(I4P)                           :: b      !< The block.
   integer(I4P)                           :: i      !< An item.
   integer                                :: k      !< Counter.
   integer                                :: w      !< Weight drawn.
   logical                                :: tight  !< A tight list.
   integer(I4P), parameter :: STARTS(6) = [0_I4P, 1_I4P, 2_I4P, 9_I4P, 10_I4P, 123456789_I4P] !< Start numbers.
   character(*), parameter :: BULLETS = '-+*'  !< Bullets.
   character(*), parameter :: BREAKS = '*-_'   !< Thematic break characters.

   w = uniform(r, 100)
   if (depth >= 3 .and. w >= 68) w = 0
   select case(w)
   case(30:39)
      b = add(doc, parent, NODE_HEADING)
      call doc%set_heading_level(b, int(1 + uniform(r, 6), I4P))
      call doc%set_int_slot(b, SLOT_HEADING_FORM, int(1 + uniform(r, 2), I4P))
      call doc%set_int_slot(b, SLOT_HEADING_CLOSING, int(uniform(r, 3), I4P))
      call draw_inlines(r, doc, b, uniform(r, 5), 0, 0, .false., doc%heading_level(b) > 2)
   case(40:46)
      b = add(doc, parent, NODE_THEMATIC_BREAK)
      k = 1 + uniform(r, 3)
      call doc%set_int_slot(b, SLOT_BREAK_CHAR, int(iachar(BREAKS(k:k)), I4P))
   case(47:56)
      b = add(doc, parent, NODE_CODE_BLOCK)
      call doc%set_int_slot(b, SLOT_CODE_FORM, int(1 + uniform(r, 2), I4P))
      call doc%set_int_slot(b, SLOT_CODE_FENCE_CHAR, int(iachar(merge('`', '~', uniform(r, 2) == 0)), I4P))
      call doc%set_int_slot(b, SLOT_CODE_FENCE_LENGTH, int(3 + uniform(r, 3), I4P))
      if (uniform(r, 2) == 0) call doc%set_info(b, draw_info(r))
      call doc%set_literal(b, draw_code_lines(r))
   case(57:62)
      b = add(doc, parent, NODE_HTML_BLOCK)
      call doc%set_literal(b, item(html_blocks, 1 + uniform(r, size(html_blocks%first))))
   case(63:67)
      b = add(doc, parent, NODE_LINK_REFERENCE_DEFINITION)
      call doc%set_label(b, item(labels, 1 + uniform(r, size(labels%first))))
      call doc%set_destination(b, draw_url(r))
      if (uniform(r, 2) == 0) call doc%set_title(b, draw_title(r))
   case(68:81)
      b = add(doc, parent, NODE_LIST)
      tight = uniform(r, 2) == 0
      if (uniform(r, 2) == 0) then
         call doc%set_list_type(b, LIST_BULLET)
         k = 1 + uniform(r, 3)
         call doc%set_int_slot(b, SLOT_LIST_MARKER, int(iachar(BULLETS(k:k)), I4P))
      else
         call doc%set_list_type(b, LIST_ORDERED)
         call doc%set_list_start(b, STARTS(1 + uniform(r, size(STARTS))))
         call doc%set_int_slot(b, SLOT_LIST_MARKER, int(iachar(merge('.', ')', uniform(r, 2) == 0)), I4P))
      endif
      do k=1, 1 + uniform(r, 4)
         i = add(doc, b, NODE_ITEM)
         call draw_blocks(r, doc, i, uniform(r, 4), depth + 1, tight)
      enddo
      ! a loose list needs two items, or an item of two blocks, to read back loose (not definitions: blank lines
      ! around them do not count, as in cmark, where they leave no block)
      if (.not. tight) tight = .not. reads_loose(doc, b)
      call doc%set_list_tight(b, tight)
   case(82:91)
      b = add(doc, parent, NODE_BLOCK_QUOTE)
      call draw_blocks(r, doc, b, uniform(r, 4), depth + 1, .false.)
   case default
      b = add(doc, parent, NODE_PARAGRAPH)
      call draw_inlines(r, doc, b, 1 + uniform(r, 6), 0, 0, .false., .false.)
   endselect
   endfunction draw_block

   recursive subroutine draw_inlines(r, doc, parent, count, depth, emphs, in_link, no_breaks)
   !< Append random inlines to a paragraph, a heading or an inline container: no break at the edges of a paragraph, a
   !< heading or an emphasis, no two breaks in a row, no raw HTML first in a paragraph or a heading (it would open an
   !< HTML block), no link inside a link.
   type(rng_t),             intent(inout) :: r         !< Generator.
   type(markdown_document), intent(inout) :: doc       !< The tree.
   integer(I4P),            intent(in)    :: parent    !< Container.
   integer,                 intent(in)    :: count     !< Inlines.
   integer,                 intent(in)    :: depth     !< Nesting of inline containers.
   integer,                 intent(in)    :: emphs     !< Emphases around, inside the innermost link's text.
   logical,                 intent(in)    :: in_link   !< Inside a link's text.
   logical,                 intent(in)    :: no_breaks !< No line break (a heading of level 3 to 6).
   integer(I4P)                           :: kind      !< Kind of the parent.
   integer(I4P)                           :: n         !< An inline.
   integer                                :: k         !< Counter.
   integer                                :: w         !< Weight drawn.
   logical                                :: edges     !< No break at the edges.
   logical                                :: after_break !< The inline before is a break (or none: the start).
   logical                                :: after_code  !< The inline before is a code span.
   logical                                :: has_emph    !< An emphasis was drawn in this container.
   character(:), allocatable              :: piece       !< A text drawn.
   integer                                :: m           !< A draw.
   logical                                :: block     !< The parent is a paragraph or a heading.

   kind = doc%node_kind(parent)
   block = kind == NODE_PARAGRAPH .or. kind == NODE_HEADING
   edges = block .or. kind == NODE_EMPH .or. kind == NODE_STRONG
   after_break = edges
   after_code = .false.
   has_emph = .false.
   do k=1, count
      w = uniform(r, 100)
      if (depth >= 3 .and. w >= 67) w = 0
      if ((w >= 40 .and. w <= 52) .and. (no_breaks .or. after_break .or. (edges .and. k == count))) w = 0
      if ((w >= 61 .and. w <= 66) .and. block .and. k == 1) w = 0
      if ((w >= 53 .and. w <= 60) .and. after_code) w = 0
      if ((w >= 85 .and. w <= 93) .and. in_link) w = 0
      if ((w >= 67 .and. w <= 84) .and. (emphs >= 2 .or. (emphs == 1 .and. has_emph))) w = 0
      select case(w)
      case(40:47)
         n = add(doc, parent, NODE_SOFTBREAK)
         after_break = .true.
         cycle
      case(48:52)
         n = add(doc, parent, NODE_LINEBREAK)
         call doc%set_int_slot(n, SLOT_LINEBREAK_FORM, int(1 + uniform(r, 2), I4P))
         call doc%set_int_slot(n, SLOT_LINEBREAK_SPACES, int(2 + uniform(r, 3), I4P))
         after_break = .true.
         cycle
      case(53:60)
         m = 1 + uniform(r, 4)
         piece = draw_text(r, m)
         n = add(doc, parent, NODE_CODE, piece)
         if (len(doc%literal(n)) == 0) call doc%set_literal(n, 'x')
         call doc%set_int_slot(n, SLOT_CODE_TICKS, int(uniform(r, 4), I4P))
         after_break = .false.
         after_code = .true.
         cycle
      case(61:66)
         n = add(doc, parent, NODE_HTML_INLINE, item(html_inlines, 1 + uniform(r, size(html_inlines%first))))
      case(67:76, 77:84)
         n = add(doc, parent, merge(NODE_EMPH, NODE_STRONG, w <= 76))
         call doc%set_int_slot(n, SLOT_EMPHASIS_CHAR, int(iachar(merge('*', '_', uniform(r, 2) == 0)), I4P))
         has_emph = .true.
         call draw_inlines(r, doc, n, 1 + uniform(r, 3), depth + 1, emphs + 1, in_link, no_breaks)
      case(85:93, 94:99)
         n = add(doc, parent, merge(NODE_LINK, NODE_IMAGE, w <= 93))
         call draw_link(r, doc, n, no_breaks)
         m = uniform(r, 8) ! (a draw of its own: `.and.` may skip an operand)
         if (m == 0 .and. doc%node_kind(n) == NODE_LINK) then ! an autolink
            if (uniform(r, 2) == 0) then
               m = uniform(r, 3)
               piece = draw_text(r, m, url_safe=.true.)
               call doc%set_destination(n, 'https://e.x/'//piece)
               call add_text(doc, n, doc%destination(n))
            else
               call doc%set_destination(n, 'mailto:a.b@e.x')
               call add_text(doc, n, 'a.b@e.x')
            endif
            call doc%set_int_slot(n, SLOT_LINK_FORM, LINK_AUTOLINK)
         else
            call draw_inlines(r, doc, n, uniform(r, 4), depth + 1, 0, in_link .or. w <= 93, no_breaks)
         endif
      case default
         ! an empty text now and then, never first: it is no node (Equality, text runs)
         m = uniform(r, 20)
         if (k > 1 .and. m == 0) then
            n = add(doc, parent, NODE_TEXT, '')
            cycle
         endif
         m = 1 + uniform(r, 5)
         piece = draw_text(r, m)
         n = add(doc, parent, NODE_TEXT, piece)
         if (len(doc%literal(n)) == 0) call doc%set_literal(n, 'x')
      endselect
      after_break = .false.
      after_code = .false.
   enddo
   endsubroutine draw_inlines

   subroutine draw_link(r, doc, n, one_line)
   !< Draw a link's or an image's destination, title, form and label.
   type(rng_t),             intent(inout) :: r        !< Generator.
   type(markdown_document), intent(inout) :: doc      !< The tree.
   integer(I4P),            intent(in)    :: n        !< Link or image.
   logical,                 intent(in)    :: one_line !< No line ending in the title (a heading of level 3 to 6).
   character(:), allocatable              :: title    !< Title.
   integer                                :: k        !< Position.

   call doc%set_destination(n, draw_url(r))
   if (uniform(r, 2) == 0) then
      title = draw_title(r)
      if (one_line) then
         do k=1, len(title)
            if (title(k:k) == LF) title(k:k) = ' '
         enddo
      endif
      call doc%set_title(n, title)
   endif
   call doc%set_int_slot(n, SLOT_LINK_FORM, int(1 + uniform(r, 4), I4P))
   call doc%set_label(n, item(labels, 1 + uniform(r, size(labels%first))))
   endsubroutine draw_link

   function draw_text(r, count, url_safe) result(text)
   !< Draw a text of `count` pieces (characters, Unicode, Markdown tokens): no line ending.
   type(rng_t),  intent(inout)        :: r        !< Generator.
   integer,      intent(in)           :: count    !< Pieces.
   logical,      intent(in), optional :: url_safe !< Letters and digits only.
   character(:), allocatable          :: text     !< Text.
   character(*), parameter            :: SAFE = 'abcxyz019' !< Letters and digits.
   integer                            :: k        !< Counter.
   integer                            :: q        !< A choice.

   text = ''
   do k=1, count
      if (present(url_safe)) then
         q = 1 + uniform(r, len(SAFE))
         text = text//SAFE(q:q)
      else
         text = text//item(pieces, 1 + uniform(r, size(pieces%first)))
      endif
   enddo
   endfunction draw_text

   function draw_code_lines(r) result(text)
   !< Draw the literal of a code block: 0 to 3 lines, each ending with a line feed. (One draw per statement: the order
   !< in which a statement's function references are evaluated is the processor's, and every compiler must draw the
   !< same.)
   type(rng_t), intent(inout) :: r     !< Generator.
   character(:), allocatable  :: text  !< Literal.
   character(:), allocatable  :: piece !< A piece.
   integer                    :: k     !< Counter.
   integer                    :: q, m  !< Draws.
   ! substrings, not repeat(): nvfortran 26.9 gives bytes of an earlier string for `repeat(' ', q)` and for
   ! `repeat(merge('`', '~', c), m)` here (q, m variables); not reduced (notes/nvfortran-repeat-stale-bytes.md)
   character(*), parameter    :: SPACES = '     '  !< Indentation drawn.
   character(*), parameter    :: TICKS  = '`````'  !< A fence-like line drawn.
   character(*), parameter    :: TILDES = '~~~~~'  !< A fence-like line drawn.

   text = ''
   do k=1, uniform(r, 4)
      select case(uniform(r, 6))
      case(0)
         q = uniform(r, 2)
         m = 3 + uniform(r, 3)
         if (q == 0) then
            text = text//TICKS(1:m)//LF
         else
            text = text//TILDES(1:m)//LF
         endif
      case(1)
         q = uniform(r, 6)
         m = uniform(r, 4)
         piece = draw_text(r, m)
         text = text//SPACES(1:q)//piece//LF
      case default
         m = uniform(r, 6)
         piece = draw_text(r, m)
         text = text//piece//LF
      endselect
   enddo
   endfunction draw_code_lines

   function draw_info(r) result(text)
   !< Draw a code block's info string: no white space at its ends (the parser strips it).
   type(rng_t), intent(inout) :: r    !< Generator.
   character(:), allocatable  :: text !< Info string.
   integer                    :: m    !< Pieces.

   m = 1 + uniform(r, 4)
   text = draw_text(r, m)
   text = strip(text)
   endfunction draw_info

   function draw_url(r) result(text)
   !< Draw a destination: URL pieces, white space, brackets, parentheses, escapes, references, Unicode.
   type(rng_t), intent(inout) :: r    !< Generator.
   character(:), allocatable  :: text !< Destination.
   integer                    :: k    !< Counter.

   text = ''
   do k=1, uniform(r, 6)
      text = text//item(url_pieces, 1 + uniform(r, size(url_pieces%first)))
   enddo
   endfunction draw_url

   function draw_title(r) result(text)
   !< Draw a title: text pieces, quotes, parentheses, now and then a line feed (not two in a row: a blank line).
   type(rng_t), intent(inout) :: r    !< Generator.
   character(:), allocatable  :: text !< Title.
   integer                    :: k    !< Counter.

   text = ''
   do k=1, uniform(r, 6)
      select case(uniform(r, 10))
      case(0)
         text = text//QT
      case(1)
         text = text//AP
      case(2)
         text = text//merge('(', ')', uniform(r, 2) == 0)
      case(3)
         if (len(text) > 0) then
            if (text(len(text):len(text)) /= LF) text = text//LF
         endif
      case default
         text = text//item(pieces, 1 + uniform(r, size(pieces%first)))
      endselect
   enddo
   endfunction draw_title

   ! what Markdown can express
   function writable(doc, why) result(ok)
   !< Return true if a tree is one Markdown can express (Writer, "Trees that cannot be written"): no empty paragraph or
   !< emphasis, no break at the edges of a paragraph, a heading or an emphasis, no two breaks in a row, no break in a
   !< heading of level 3 to 6, no raw HTML first in a paragraph or a heading, no link in a link, code spans not empty
   !< and on one line, texts on one line, fenced literals ending with a line ending, info strings with no white space
   !< at their ends, loose lists that read back loose, and in a tight list's items only blocks that follow each other
   !< with no blank line (`pair_ok`).
   type(markdown_document),   intent(in)  :: doc  !< The tree.
   character(:), allocatable, intent(out) :: why  !< The first reason it is not, empty if it is.
   logical                                :: ok   !< Result.
   integer(I4P)                           :: n    !< Node.
   integer(I4P)                           :: c    !< Child.
   integer(I4P)                           :: kind !< Kind.
   integer(I4P)                           :: a    !< Ancestor.
   integer(I4P)                           :: first, last, prev !< Effective children: first, last, the one before.
   integer                                :: count !< Effective children.
   character(:), allocatable              :: s    !< A string.

   ok = .false.
   why = ''
   do n=2_I4P, doc%last_node()
      if (.not. doc%is_node(n)) cycle
      kind = doc%node_kind(n)
      select case(kind)
      case(NODE_TEXT)
         s = doc%literal(n)
         if (scan(s, LF//CR) > 0) then
            why = 'a text holds a line ending'
            return
         endif
      case(NODE_CODE)
         s = doc%literal(n)
         if (len(s) == 0 .or. scan(s, LF//CR) > 0) then
            why = 'a code span is empty or holds a line ending'
            return
         endif
      case(NODE_CODE_BLOCK)
         s = doc%literal(n)
         if (len(s) > 0) then
            if (s(len(s):len(s)) /= LF) then
               why = 'a code block does not end with a line ending'
               return
            endif
         endif
         s = doc%info(n)
         if (scan(s, LF//CR) > 0 .or. s /= strip(s)) then
            why = 'an info string holds a line ending or white space at an end'
            return
         endif
      case(NODE_LIST)
         if (doc%children_number(n) == 0_I4P) then
            why = 'a list with no item'
            return
         endif
         if (.not. doc%list_tight(n)) then
            if (.not. reads_loose(doc, n)) then
               why = 'a loose list with no blank line a block of its own ends (definitions leave none)'
               return
            endif
         endif
         if (doc%list_tight(n)) then
            c = doc%first_child(n)
            do while (c /= 0_I4P)
               if (trailing_definition(doc, c, doc%next_sibling(c) /= 0_I4P)) then
                  why = 'a tight list''s item, or a list in it, ending with a definition after a blank line'
                  return
               endif
               prev = doc%first_child(c)
               if (prev /= 0_I4P) then
                  a = doc%next_sibling(prev)
                  do while (a /= 0_I4P)
                     if (.not. pair_ok(doc, prev, a)) then
                        why = 'two blocks of a tight list''s item that need a blank line between them'
                        return
                     endif
                     prev = a
                     a = doc%next_sibling(a)
                  enddo
               endif
               c = doc%next_sibling(c)
            enddo
         endif
      case(NODE_PARAGRAPH, NODE_HEADING, NODE_EMPH, NODE_STRONG, NODE_LINK, NODE_IMAGE)
         count = 0
         first = 0_I4P
         last = 0_I4P
         prev = 0_I4P
         c = doc%first_child(n)
         do while (c /= 0_I4P)
            if (.not. is_empty_text(doc, c)) then
               count = count + 1
               if (first == 0_I4P) first = c
               last = c
               if (is_break(doc, c) .and. prev /= 0_I4P) then
                  if (is_break(doc, prev)) then
                     why = 'two breaks in a row'
                     return
                  endif
               endif
               if (doc%node_kind(c) == NODE_CODE .and. prev /= 0_I4P) then
                  if (doc%node_kind(prev) == NODE_CODE) then
                     why = 'two code spans in a row (their backtick strings would be one)'
                     return
                  endif
               endif
               if (is_break(doc, c) .and. kind == NODE_HEADING) then
                  if (doc%heading_level(n) > 2_I4P) then
                     why = 'a break in a heading of level 3 to 6'
                     return
                  endif
               endif
               prev = c
            endif
            c = doc%next_sibling(c)
         enddo
         if (kind == NODE_LINK .or. kind == NODE_IMAGE) then
            if (kind == NODE_LINK) then
               a = doc%parent(n)
               do while (a /= 0_I4P)
                  if (doc%node_kind(a) == NODE_LINK) then
                     why = 'a link in a link'
                     return
                  endif
                  a = doc%parent(a)
               enddo
            endif
            cycle
         endif
         if (count == 0 .and. kind /= NODE_HEADING) then
            why = 'an empty '//doc%kind_name(kind)
            return
         endif
         if (count == 0) cycle
         if (is_break(doc, first) .or. is_break(doc, last)) then
            why = 'a break at an edge of a '//doc%kind_name(kind)
            return
         endif
         if (kind == NODE_EMPH .or. kind == NODE_STRONG) then
            count = 0
            a = doc%parent(n)
            do while (a /= 0_I4P)
               if (doc%node_kind(a) /= NODE_EMPH .and. doc%node_kind(a) /= NODE_STRONG) exit
               count = count + 1
               a = doc%parent(a)
            enddo
            if (count >= 2) then
               why = 'emphases nested three deep'
               return
            endif
            count = 0
            c = doc%first_child(n)
            do while (c /= 0_I4P)
               if (doc%node_kind(c) == NODE_EMPH .or. doc%node_kind(c) == NODE_STRONG) count = count + 1
               c = doc%next_sibling(c)
            enddo
            if (count >= 2) then
               why = 'an emphasis holding two emphases'
               return
            endif
         endif
         if ((kind == NODE_PARAGRAPH .or. kind == NODE_HEADING) .and. doc%node_kind(first) == NODE_HTML_INLINE) then
            why = 'raw HTML first in a '//doc%kind_name(kind)
            return
         endif
         if ((kind == NODE_PARAGRAPH .or. kind == NODE_HEADING) .and. doc%node_kind(first) == NODE_LINK) then
            if (bracket_inside(doc, first)) then
               why = 'a link first in a paragraph or a heading, a ] in its code or raw HTML (a definition, read back)'
               return
            endif
         endif
      endselect
      if (spans_lines(doc, n)) then ! a heading of level 3 to 6 is one line
         a = doc%parent(n)
         do while (a /= 0_I4P)
            if (doc%node_kind(a) == NODE_HEADING) then
               if (doc%heading_level(a) > 2_I4P) then
                  why = 'a heading of level 3 to 6 spanning lines'
                  return
               endif
            endif
            a = doc%parent(a)
         enddo
      endif
   enddo
   ok = .true.
   endfunction writable

   function pair_ok(doc, a, b) result(ok)
   !< Return true if block `b` can follow block `a` in an item of a tight list (no blank line between them): `b` must
   !< not be read as a lazy continuation of a paragraph left open by `a` (a paragraph, a definition, an HTML block of
   !< type 7, a list that cannot interrupt a paragraph, a heading spanning lines), must not need a blank line to end `a`
   !< (an HTML block of type 6 or 7), and two block quotes would be one.
   type(markdown_document), intent(in) :: doc !< The tree.
   integer(I4P),            intent(in) :: a   !< The block before.
   integer(I4P),            intent(in) :: b   !< The block after.
   logical                             :: ok  !< Result.
   integer(I4P)                        :: kb  !< Kind of `b`.

   ok = .false.
   kb = doc%node_kind(b)
   if (doc%node_kind(a) == NODE_HTML_BLOCK) then
      if (html_block_type(doc%literal(a)) >= 6) return
   endif
   if (doc%node_kind(a) == NODE_BLOCK_QUOTE .and. kb == NODE_BLOCK_QUOTE) return
   if (open_paragraph(doc, a)) then
      select case(kb)
      case(NODE_PARAGRAPH, NODE_LINK_REFERENCE_DEFINITION) ! (after a quote, the writer closes it with an empty `>`)
         if (doc%node_kind(a) /= NODE_BLOCK_QUOTE) return
      case(NODE_HEADING) ! underlined, it would continue the paragraph
         if (heading_spans_lines(doc, b)) return
      case(NODE_HTML_BLOCK)
         if (html_block_type(doc%literal(b)) == 7) return
      case(NODE_LIST) ! a list can interrupt a paragraph of its own container only
         if (doc%node_kind(a) == NODE_PARAGRAPH .or. doc%node_kind(a) == NODE_LINK_REFERENCE_DEFINITION) then
            if (doc%children_number(doc%first_child(b)) == 0_I4P) return
            if (doc%list_type(b) == LIST_ORDERED .and. doc%list_start(b) /= 1_I4P) return
         endif
      endselect
   endif
   ok = .true.
   endfunction pair_ok

   function open_paragraph(doc, a) result(open)
   !< Return true if a block ends with a paragraph: itself, or the last block of its last item or quote, downwards (a
   !< definition there too: it is read from a paragraph).
   type(markdown_document), intent(in) :: doc  !< The tree.
   integer(I4P),            intent(in) :: a    !< The block.
   logical                             :: open !< Result.
   integer(I4P)                        :: n    !< Node.

   open = .false.
   n = a
   do while (n /= 0_I4P)
      select case(doc%node_kind(n))
      case(NODE_PARAGRAPH)
         open = .true.
         return
      case(NODE_LINK_REFERENCE_DEFINITION) ! read from a paragraph, open as one
         open = n /= a
         return
      case(NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM)
         n = doc%last_child(n)
      case default
         return
      endselect
   enddo
   endfunction open_paragraph

   function trailing_definition(doc, item, more) result(trailing)
   !< Return true if an item of a tight list, or a list in it followed by another block (`more`: the item is not the
   !< last), ends with a definition after a blank line: after a block left with an open paragraph or an HTML block, or
   !< in an item of a loose list. The definition leaves no block (as in cmark), so the blank line ends the item or the
   !< list, which makes the tight list loose.
   type(markdown_document), intent(in) :: doc      !< The tree.
   integer(I4P),            intent(in) :: item     !< Item.
   logical,                 intent(in) :: more     !< Another item follows.
   logical                             :: trailing !< Result.
   integer(I4P)                        :: c        !< A block of the item.

   trailing = .false.
   if (more) trailing = blank_then_definition(doc, item)
   if (trailing) return
   c = doc%first_child(item)
   do while (c /= 0_I4P)
      if (doc%node_kind(c) == NODE_LIST .and. (more .or. doc%next_sibling(c) /= 0_I4P)) then
         trailing = blank_then_definition(doc, c)
         if (trailing) return
      endif
      c = doc%next_sibling(c)
   enddo
   endfunction trailing_definition

   function blank_then_definition(doc, top) result(blank)
   !< Return true if a list or an item ends (down its last lists and items) with a definition after a blank line.
   type(markdown_document), intent(in) :: doc   !< The tree.
   integer(I4P),            intent(in) :: top   !< List or item.
   logical                             :: blank !< Result.
   integer(I4P)                        :: n     !< A list or an item.
   integer(I4P)                        :: d     !< Its last block.
   integer(I4P)                        :: p     !< The block before.

   blank = .false.
   n = top
   do while (n /= 0_I4P)
      d = doc%last_child(n)
      if (d == 0_I4P) return
      if (doc%node_kind(d) == NODE_LINK_REFERENCE_DEFINITION) then
         p = doc%prev_sibling(d)
         if (p == 0_I4P) return
         blank = open_paragraph(doc, p) .or. doc%node_kind(p) == NODE_HTML_BLOCK
         if (doc%node_kind(n) == NODE_ITEM) blank = blank .or. .not. doc%list_tight(doc%parent(n))
         return
      endif
      if (doc%node_kind(d) /= NODE_LIST .and. doc%node_kind(d) /= NODE_ITEM) return
      n = d
   enddo
   endfunction blank_then_definition

   function bracket_inside(doc, link) result(inside)
   !< Return true if a link's text holds a code span or raw HTML with a `]`: written first in a paragraph, its `[` and
   !< that `]` are a definition's label (definitions are read before code spans and raw HTML), escapes cannot help.
   type(markdown_document), intent(in) :: doc    !< The tree.
   integer(I4P),            intent(in) :: link   !< Link.
   logical                             :: inside !< Result.
   integer(I4P)                        :: n      !< A node of its text.

   inside = .false.
   n = doc%first_child(link)
   do while (n /= 0_I4P .and. n /= link)
      if (doc%node_kind(n) == NODE_CODE .or. doc%node_kind(n) == NODE_HTML_INLINE) then
         inside = index(doc%literal(n), ']') > 0
         if (inside) return
      endif
      if (doc%first_child(n) /= 0_I4P) then
         n = doc%first_child(n)
      else
         do while (n /= link)
            if (doc%next_sibling(n) /= 0_I4P) exit
            n = doc%parent(n)
         enddo
         if (n == link) exit
         n = doc%next_sibling(n)
      endif
   enddo
   endfunction bracket_inside

   function reads_loose(doc, list) result(loose)
   !< Return true if a list written loose (blank lines between its items and between the blocks of its items) reads
   !< back loose: a blank line after an item that is not the last, or between two blocks of an item, ends a block of
   !< its own. A definition leaves no block (as in cmark): a blank line after a definition, down the last blocks of
   !< lists and items, ends nothing; definitions between two blocks are skipped.
   type(markdown_document), intent(in) :: doc   !< The tree.
   integer(I4P),            intent(in) :: list  !< List.
   logical                             :: loose !< Result.
   integer(I4P)                        :: item  !< An item.
   integer(I4P)                        :: c     !< A block of it.
   integer(I4P)                        :: prev  !< The block before it, not a definition.

   loose = .false.
   item = doc%first_child(list)
   do while (item /= 0_I4P)
      if (doc%next_sibling(item) /= 0_I4P .and. .not. ends_with_definition(doc, item)) then
         loose = .true.
         return
      endif
      prev = 0_I4P
      c = doc%first_child(item)
      do while (c /= 0_I4P)
         if (doc%node_kind(c) /= NODE_LINK_REFERENCE_DEFINITION) then
            if (prev /= 0_I4P) then
               if (.not. ends_with_definition(doc, prev)) then
                  loose = .true.
                  return
               endif
            endif
            prev = c
         endif
         c = doc%next_sibling(c)
      enddo
      item = doc%next_sibling(item)
   enddo
   endfunction reads_loose

   function ends_with_definition(doc, block) result(ends)
   !< Return true if a block is, or ends down the last blocks of its lists and items with, a definition.
   type(markdown_document), intent(in) :: doc   !< The tree.
   integer(I4P),            intent(in) :: block !< Block.
   logical                             :: ends  !< Result.
   integer(I4P)                        :: n     !< Current block.

   n = block
   do while (doc%node_kind(n) == NODE_LIST .or. doc%node_kind(n) == NODE_ITEM)
      if (doc%last_child(n) == 0_I4P) exit
      n = doc%last_child(n)
   enddo
   ends = doc%node_kind(n) == NODE_LINK_REFERENCE_DEFINITION
   endfunction ends_with_definition

   function spans_lines(doc, n) result(spans)
   !< Return true for an inline node that spans lines: a break, a title or raw HTML holding a line ending.
   type(markdown_document), intent(in) :: doc   !< The tree.
   integer(I4P),            intent(in) :: n     !< Node.
   logical                             :: spans !< Result.

   select case(doc%node_kind(n))
   case(NODE_SOFTBREAK, NODE_LINEBREAK)
      spans = .true.
   case(NODE_LINK, NODE_IMAGE)
      spans = scan(doc%title(n), LF//CR) > 0
   case(NODE_HTML_INLINE)
      spans = scan(doc%literal(n), LF//CR) > 0
   case default
      spans = .false.
   endselect
   endfunction spans_lines

   function heading_spans_lines(doc, h) result(spans)
   !< Return true for a heading whose content spans lines (written setext, it cannot follow a paragraph directly).
   type(markdown_document), intent(in) :: doc   !< The tree.
   integer(I4P),            intent(in) :: h     !< Heading.
   logical                             :: spans !< Result.
   integer(I4P)                        :: n     !< A node of its content.

   spans = .false.
   n = doc%first_child(h)
   do while (n /= 0_I4P)
      spans = spans_lines(doc, n)
      if (spans) return
      if (doc%first_child(n) /= 0_I4P) then
         n = doc%first_child(n)
      else
         do while (n /= h)
            if (doc%next_sibling(n) /= 0_I4P) exit
            n = doc%parent(n)
         enddo
         if (n == h) exit
         n = doc%next_sibling(n)
      endif
   enddo
   endfunction heading_spans_lines

   pure function html_block_type(literal) result(t)
   !< Return the type (1 to 7) of the HTML blocks this test draws, by their start.
   character(*), intent(in) :: literal !< Literal.
   integer                  :: t       !< Type.

   t = 7
   if (index(literal, '<pre') == 1 .or. index(literal, '<script') == 1) then
      t = 1
   elseif (index(literal, '<!--') == 1) then
      t = 2
   elseif (index(literal, '<?') == 1) then
      t = 3
   elseif (index(literal, '<![CDATA[') == 1) then
      t = 5
   elseif (index(literal, '<!') == 1) then
      t = 4
   elseif (index(literal, '<div') == 1 .or. index(literal, '<table') == 1 .or. index(literal, '</p') == 1) then
      t = 6
   endif
   endfunction html_block_type

   logical function is_break(doc, n)
   !< Return true for a soft or a hard line break.
   type(markdown_document), intent(in) :: doc !< The tree.
   integer(I4P),            intent(in) :: n   !< Node.

   is_break = doc%node_kind(n) == NODE_SOFTBREAK .or. doc%node_kind(n) == NODE_LINEBREAK
   endfunction is_break

   logical function is_empty_text(doc, n)
   !< Return true for a text node with an empty literal (no node, for equality).
   type(markdown_document), intent(in) :: doc !< The tree.
   integer(I4P),            intent(in) :: n   !< Node.

   is_empty_text = .false.
   if (doc%node_kind(n) == NODE_TEXT) is_empty_text = len(doc%literal(n)) == 0
   endfunction is_empty_text

   ! trees: building and dumping
   function add(doc, parent, kind, literal) result(node)
   !< Append a new node of a kind to a parent, with a literal.
   type(markdown_document), intent(inout)        :: doc     !< The tree.
   integer(I4P),            intent(in)           :: parent  !< Parent.
   integer(I4P),            intent(in)           :: kind    !< Kind.
   character(*),            intent(in), optional :: literal !< Literal.
   integer(I4P)                                  :: node    !< New node.

   node = doc%new_node(kind)
   call doc%append_child(parent, node)
   if (present(literal)) call doc%set_literal(node, literal)
   endfunction add

   subroutine add_text(doc, parent, literal)
   !< Append a text to a parent.
   type(markdown_document), intent(inout) :: doc     !< The tree.
   integer(I4P),            intent(in)    :: parent  !< Parent.
   character(*),            intent(in)    :: literal !< Literal.
   integer(I4P)                           :: node    !< New node.

   node = add(doc, parent, NODE_TEXT, literal)
   endsubroutine add_text

   function dump(doc) result(text)
   !< A tree, one node per line, indented by depth: kind, semantic and style fields, strings escaped.
   type(markdown_document), intent(in) :: doc   !< The tree.
   character(:), allocatable           :: text  !< Dump.
   character(:), allocatable           :: line  !< One node.
   integer(I4P)                        :: n     !< Node.
   integer(I4P)                        :: k     !< Its kind.
   integer                             :: depth !< Its depth.

   text = ''
   n = doc%root()
   if (n == 0_I4P) return
   depth = 0
   do
      k = doc%node_kind(n)
      line = repeat('  ', depth)//doc%kind_name(k)
      select case(k)
      case(NODE_HEADING)
         line = line//' level='//str(doc%heading_level(n))//' form='//str(doc%int_slot(n, SLOT_HEADING_FORM))// &
                ' closing='//str(doc%int_slot(n, SLOT_HEADING_CLOSING))
      case(NODE_LIST)
         line = line//' type='//str(doc%list_type(n))//' start='//str(doc%list_start(n))//' tight='// &
                merge('T', 'F', doc%list_tight(n))//' marker='//str(doc%int_slot(n, SLOT_LIST_MARKER))
      case(NODE_CODE_BLOCK)
         line = line//' form='//str(doc%int_slot(n, SLOT_CODE_FORM))//' fence='// &
                str(doc%int_slot(n, SLOT_CODE_FENCE_CHAR))//'x'//str(doc%int_slot(n, SLOT_CODE_FENCE_LENGTH))// &
                ' info="'//escaped(doc%info(n))//'" literal="'//escaped(doc%literal(n))//'"'
      case(NODE_THEMATIC_BREAK, NODE_EMPH, NODE_STRONG)
         line = line//' char='//str(doc%int_slot(n, 1_I4P))
      case(NODE_LINEBREAK)
         line = line//' form='//str(doc%int_slot(n, SLOT_LINEBREAK_FORM))//' spaces='// &
                str(doc%int_slot(n, SLOT_LINEBREAK_SPACES))
      case(NODE_TEXT, NODE_HTML_INLINE, NODE_HTML_BLOCK)
         line = line//' "'//escaped(doc%literal(n))//'"'
      case(NODE_CODE)
         line = line//' ticks='//str(doc%int_slot(n, SLOT_CODE_TICKS))//' "'//escaped(doc%literal(n))//'"'
      case(NODE_LINK, NODE_IMAGE, NODE_LINK_REFERENCE_DEFINITION)
         line = line//' form='//str(doc%int_slot(n, SLOT_LINK_FORM))//' label="'//escaped(doc%label(n))// &
                '" destination="'//escaped(doc%destination(n))//'" title="'//escaped(doc%title(n))//'"'
      endselect
      text = text//line//LF
      if (doc%first_child(n) /= 0_I4P) then
         n = doc%first_child(n)
         depth = depth + 1
      else
         do while (n /= doc%root())
            if (doc%next_sibling(n) /= 0_I4P) exit
            n = doc%parent(n)
            depth = depth - 1
         enddo
         if (n == doc%root()) exit
         n = doc%next_sibling(n)
      endif
   enddo
   endfunction dump

   ! the generator
   subroutine rng_seed(r, seed, stream, i)
   !< Seed a generator from a seed, a stream and an iteration (each iteration its own sequence).
   type(rng_t),  intent(out) :: r      !< Generator.
   integer(I8P), intent(in)  :: seed   !< Seed, >= 0.
   integer,      intent(in)  :: stream !< Stream.
   integer,      intent(in)  :: i      !< Iteration, >= 0.
   integer(I8P)              :: z      !< Running value.
   integer(I8P)              :: hi     !< High word of the seed.
   integer                   :: k      !< Counter.

   hi = iand(ishft(seed, -32), M32)
   z = ieor(iand(seed, M32), mul32(int(i, I8P), GOLD))
   do k=1, 4
      z = iand(z + GOLD + int(stream, I8P), M32)
      r%s(k) = fmix32(ieor(fmix32(z), iand(hi + int(k, I8P), M32)))
   enddo
   if (all(r%s == 0_I8P)) r%s(1) = 1_I8P
   endsubroutine rng_seed

   function next32(r) result(x)
   !< Return the next 32-bit value of a generator (xoshiro128**).
   type(rng_t), intent(inout) :: r !< Generator.
   integer(I8P)               :: x !< Value, 0 to 2^32 - 1.
   integer(I8P)               :: t !< Temporary.

   x = iand(rotl(iand(r%s(2) * 5_I8P, M32), 7) * 9_I8P, M32)
   t = iand(ishft(r%s(2), 9), M32)
   r%s(3) = ieor(r%s(3), r%s(1))
   r%s(4) = ieor(r%s(4), r%s(2))
   r%s(2) = ieor(r%s(2), r%s(3))
   r%s(1) = ieor(r%s(1), r%s(4))
   r%s(3) = ieor(r%s(3), t)
   r%s(4) = rotl(r%s(4), 11)
   endfunction next32

   function uniform(r, n) result(k)
   !< Return a value from 0 to `n - 1` (0 for `n` < 2).
   type(rng_t), intent(inout) :: r !< Generator.
   integer,     intent(in)    :: n !< Values, < 2^31.
   integer                    :: k !< Value.

   k = int(ishft(next32(r) * int(max(n, 1), I8P), -32))
   endfunction uniform

   pure function rotl(x, k) result(y)
   !< Rotate a 32-bit value left.
   integer(I8P), intent(in) :: x !< Value, 0 to 2^32 - 1.
   integer,      intent(in) :: k !< Bits, 1 to 31.
   integer(I8P)             :: y !< Rotated.

   y = iand(ior(ishft(x, k), ishft(x, k - 32)), M32)
   endfunction rotl

   pure function mul32(a, b) result(p)
   !< Return a * b modulo 2^32 with no overflow: the low half of `a` times `b`, plus the high half's product shifted.
   integer(I8P), intent(in) :: a, b !< Factors, 0 to 2^32 - 1.
   integer(I8P)             :: p    !< Product.

   p = iand(iand(a, 65535_I8P) * b + ishft(iand(ishft(a, -16) * b, 65535_I8P), 16), M32)
   endfunction mul32

   pure function fmix32(h0) result(h)
   !< MurmurHash3's finaliser: a 32-bit value mixed.
   integer(I8P), intent(in) :: h0 !< Value.
   integer(I8P)             :: h  !< Mixed.

   h = ieor(h0, ishft(h0, -16))
   h = mul32(h, MIX1)
   h = ieor(h, ishft(h, -13))
   h = mul32(h, MIX2)
   h = ieor(h, ishft(h, -16))
   endfunction fmix32

   subroutine fold(h, text)
   !< Fold a text into an FNV-1a hash (and its length, so that concatenations differ).
   integer(I8P), intent(inout) :: h    !< Hash.
   character(*), intent(in)    :: text !< Text.
   integer                     :: k    !< Byte.

   do k=1, len(text)
      h = mul32(ieor(h, int(iand(ichar(text(k:k)), 255), I8P)), FNV_PRIME)
   enddo
   h = mul32(ieor(h, int(iand(len(text), 255), I8P)), FNV_PRIME)
   endsubroutine fold

   ! tables
   subroutine build_tables()
   !< The byte table, the pools, the fixtures.
   character(:), allocatable :: list !< A list being built.
   integer                   :: k    !< Counter.

   do k=0, 255
      bytes(k+1:k+1) = char(k)
   enddo
   ! injected by the mutations: Markdown-significant bytes and tokens, line endings, tabs, ill-formed UTF-8 (a lone
   ! continuation byte, an overlong form, a truncated sequence, a surrogate, bytes that never start one), U+0000
   list = '*'//US//'_'//US//'['//US//']'//US//'('//US//')'//US//'<'//US//'>'//US//'!'//US//'#'//US//'-'//US// &
          '+'//US//'`'//US//'```'//US//'~~~'//US//BS//US//'&'//US//LF//US//TAB//US//' '//US//'    '//US//    &
          '**'//US//'__'//US//']('//US//'!['//US//'<a>'//US//'</a>'//US//'<!--'//US//'-->'//US//'<?'//US//   &
          '<![CDATA['//US//'<div>'//US//'&amp;'//US//'&#'//US//'&#x'//US//'&#42;'//US//'1.'//US//'1)'//US//  &
          '- '//US//'> '//US//'***'//US//'---'//US//'==='//US//QT//US//AP//US//':'//US//']: '//US//            &
          '<http://a.b>'//US//'<a@b.c>'//US//CR//US//CR//LF//US//LF//LF//US//BS//LF//US//'  '//LF//US//       &
          by([0])//US//by([128])//US//by([192, 175])//US//by([228, 189])//US//by([237, 161, 128])//US//       &
          by([245])//US//by([255])//US//by([240, 160, 128])
   call pool_from(list, tokens)
   ! texts: ASCII graphic characters, white space, Unicode (letters, punctuation, white space, U+FFFD, a character
   ! whose case folding is two), Markdown tokens
   list = ''
   do k=33, 126
      list = list//bytes(k+1:k+1)//US
   enddo
   list = list//'a'//US//'b'//US//'word'//US//'Word'//US//' '//US//' '//US//'  '//US//TAB//US//               &
          by([195, 169])//US//by([228, 184, 173])//US//by([240, 159, 152, 128])//US//by([194, 161])//US//    &
          by([194, 171])//US//by([194, 160])//US//by([226, 128, 131])//US//by([239, 191, 189])//US//          &
          by([225, 186, 158])//US//                                                                           &
          '&amp;'//US//'&#42;'//US//'&#x2A;'//US//'&copy;'//US//'&'//US//'***'//US//'- '//US//'1. '//US//      &
          '2) '//US//'# '//US//'> '//US//'<div>'//US//'<a>'//US//'</a>'//US//'<!--'//US//'`'//US//'``'//US//   &
          BS//US//BS//'*'//US//'http://a.b'//US//'a@b.c'//US//'[a]'//US//'[x](y)'//US//'![i](j)'//US//'    '// &
          US//'==='//US//'---'//US//']: '//US//'|'//US//'~~~'//US//'```'
   call pool_from(list, pieces)
   ! destinations
   list = 'a'//US//'b'//US//'/'//US//'http://e.x'//US//':'//US//'.'//US//'?q=1'//US//'#f'//US//'%20'//US//  &
          ' '//US//TAB//US//'('//US//')'//US//'<'//US//'>'//US//BS//US//'&amp;'//US//'&'//US//QT//US//AP//US// &
          '*'//US//'_'//US//'['//US//']'//US//by([195, 169])//US//'`'//US//'mailto:'//US//'&#32;'
   call pool_from(list, url_pieces)
   ! raw inline HTML
   list = '<a>'//US//'</a>'//US//'<b class="x">'//US//'<span id='//AP//'y'//AP//'>'//US//'<!-- c -->'//US// &
          '<?p x?>'//US//'<!X y>'//US//'<![CDATA[z]]>'//US//'<br/>'//US//'<x-y z>'
   call pool_from(list, html_inlines)
   ! HTML blocks of the seven types, as the parser keeps them (their lines, each ending with a line feed)
   list = '<pre>'//LF//'x *y*'//LF//'</pre>'//LF//US//'<script>'//LF//'var a = 1;'//LF//'</script>'//LF//US// &
          '<!-- comment -->'//LF//US//'<!--'//LF//'multi'//LF//'-->'//LF//US//'<?php echo 1; ?>'//LF//US//      &
          '<!DOCTYPE html>'//LF//US//'<![CDATA['//LF//'x'//LF//']]>'//LF//US//'<div>'//LF//'*not* emphasis'//  &
          LF//'</div>'//LF//US//'<table><tr><td>'//LF//'x'//LF//'</td></tr></table>'//LF//US//'<div class="a">'// &
          LF//US//'<a href="u">'//LF//US//'<custom-tag>'//LF//US//'</b>'//LF
   call pool_from(list, html_blocks)
   ! labels: case, Unicode case folding (U+1E9E and ss), white space inside, emphasis characters
   list = 'a'//US//'A'//US//'foo'//US//'Foo Bar'//US//'foo  bar'//US//'x1'//US//by([195, 132, 195, 150])//US// &
          by([195, 164, 195, 182])//US//by([225, 186, 158])//US//'ss'//US//'a*b*'//US//'c_d'
   call pool_from(list, labels)
   call load_fixtures
   endsubroutine build_tables

   function by(values) result(s)
   !< The bytes of a list of values (0 to 255).
   integer, intent(in)        :: values(:) !< Values.
   character(:), allocatable  :: s         !< Bytes.
   character(len=size(values)) :: buf      !< Buffer.
   integer                    :: k         !< Counter.

   do k=1, size(values)
      buf(k:k) = bytes(values(k)+1:values(k)+1)
   enddo
   s = buf
   endfunction by

   subroutine load_fixtures()
   !< Read the examples into one pool.
   character(:), allocatable :: all !< Every example.
   character(:), allocatable :: one !< One.
   character(4)              :: stem !< Example number on four digits.
   integer                   :: k   !< Counter.

   allocate(fixtures%first(EXAMPLES), fixtures%last(EXAMPLES))
   all = ''
   do k=1, EXAMPLES
      write(stem, '(I4.4)') k
      one = read_bytes(DIR//stem//'.md')
      fixtures%first(k) = len(all) + 1
      all = all//one
      fixtures%last(k) = len(all)
   enddo
   fixtures%chars = all
   endsubroutine load_fixtures

   subroutine pool_from(list, pool)
   !< Make a pool of the items of a list separated by `US` (an empty list: no item).
   character(*), intent(in)  :: list !< Items.
   type(pool_t), intent(out) :: pool !< Pool.
   integer                   :: n    !< Items.
   integer                   :: k    !< Counter.
   integer                   :: p    !< Position.

   pool%chars = list
   if (len(list) == 0) then
      allocate(pool%first(0), pool%last(0))
      return
   endif
   n = 1
   do k=1, len(list)
      if (list(k:k) == US) n = n + 1
   enddo
   allocate(pool%first(n), pool%last(n))
   n = 1
   pool%first(1) = 1
   do p=1, len(list)
      if (list(p:p) == US) then
         pool%last(n) = p - 1
         n = n + 1
         pool%first(n) = p + 1
      endif
   enddo
   pool%last(n) = len(list)
   endsubroutine pool_from

   function item(pool, k) result(s)
   !< Return the k-th item of a pool.
   type(pool_t), intent(in)  :: pool !< Pool.
   integer,      intent(in)  :: k    !< Item.
   character(:), allocatable :: s    !< Item.

   s = pool%chars(pool%first(k):pool%last(k))
   endfunction item

   ! helpers
   function escaped(text) result(e)
   !< A text with the bytes outside printable ASCII escaped (`\n`, `\r`, `\t`, `\xNN`) and the backslash doubled.
   character(*), intent(in)  :: text !< Text.
   character(:), allocatable :: e    !< Escaped.
   character(*), parameter   :: HEX = '0123456789abcdef' !< Hex digits.
   character(len=4*len(text)) :: buf !< Buffer.
   integer                   :: o    !< Bytes in the buffer.
   integer                   :: k    !< Counter.
   integer                   :: b    !< A byte.

   o = 0
   do k=1, len(text)
      b = iand(ichar(text(k:k)), 255)
      select case(b)
      case(92)
         buf(o+1:o+2) = BS//BS
         o = o + 2
      case(32:91, 93:126)
         buf(o+1:o+1) = text(k:k)
         o = o + 1
      case(10)
         buf(o+1:o+2) = BS//'n'
         o = o + 2
      case(13)
         buf(o+1:o+2) = BS//'r'
         o = o + 2
      case(9)
         buf(o+1:o+2) = BS//'t'
         o = o + 2
      case default
         buf(o+1:o+4) = BS//'x'//HEX(b/16+1:b/16+1)//HEX(mod(b, 16)+1:mod(b, 16)+1)
         o = o + 4
      endselect
   enddo
   e = buf(1:o)
   endfunction escaped

   pure function strip(text) result(s)
   !< A text without the spaces and tabs at its ends.
   character(*), intent(in)  :: text !< Text.
   character(:), allocatable :: s    !< Stripped.
   integer                   :: i, j !< Bounds.

   i = 1
   j = len(text)
   do while (i <= j)
      if (text(i:i) /= ' ' .and. text(i:i) /= TAB) exit
      i = i + 1
   enddo
   do while (j >= i)
      if (text(j:j) /= ' ' .and. text(j:j) /= TAB) exit
      j = j - 1
   enddo
   s = text(i:j)
   endfunction strip

   function file_name(label) result(name)
   !< A label as a file name: spaces and parentheses to dashes, the seed in front.
   character(*), intent(in)  :: label !< Label.
   character(:), allocatable :: name  !< File name.
   integer                   :: k     !< Counter.

   name = 'seed'//str(seed)//'-'//label
   do k=1, len(name)
      if (name(k:k) == ' ' .or. name(k:k) == '(' .or. name(k:k) == ')') name(k:k) = '-'
   enddo
   endfunction file_name

   function read_bytes(path) result(text)
   !< Read a whole file, byte for byte (empty if it cannot be read: the check then fails on it).
   character(*), intent(in)  :: path !< File.
   character(:), allocatable :: text !< Its bytes.
   integer                   :: u    !< Unit.
   integer                   :: n    !< Size.
   integer                   :: ios  !< I/O status.

   text = ''
   open(newunit=u, file=path, access='stream', form='unformatted', status='old', action='read', iostat=ios)
   if (ios /= 0) then
      print '(A)', 'FAIL cannot read '//path
      failures = failures + 1
      return
   endif
   inquire(unit=u, size=n)
   deallocate(text)
   allocate(character(n) :: text)
   if (n > 0) read(u, iostat=ios) text
   close(u)
   endfunction read_bytes

   subroutine write_bytes(path, text)
   !< Write a text into a file, byte for byte, replacing it.
   character(*), intent(in) :: path !< File.
   character(*), intent(in) :: text !< Text.
   integer                  :: u    !< Unit.
   integer                  :: ios  !< I/O status.

   open(newunit=u, file=path, access='stream', form='unformatted', status='replace', action='write', iostat=ios)
   if (ios /= 0) return
   write(u, iostat=ios) text
   close(u)
   endsubroutine write_bytes

   function str(i) result(s)
   !< An integer as text.
   class(*), intent(in)      :: i   !< Integer, of any kind.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   select type(i)
   type is(integer(I4P))
      write(buf, '(I0)') i
   type is(integer(I8P))
      write(buf, '(I0)') i
   class default
      buf = '?'
   endselect
   s = trim(buf)
   endfunction str

   subroutine read_arguments()
   !< Read the command line.
   character(:), allocatable :: arg !< An argument.
   character(:), allocatable :: val !< Its value.
   integer                   :: a   !< Argument counter.
   integer                   :: ios !< I/O status.
   type(markdown_document)   :: doc !< A tree to dump.
   type(markdown_options)    :: options !< No extension.
   character(:), allocatable :: what    !< What failed.
   integer                   :: code    !< Check result.

   seed = SEED_DEFAULT
   mutations = RUNS_DEFAULT
   trees = RUNS_DEFAULT
   start = 1
   progress = 0
   reports_max = 20
   trace = ''
   out_dir = ''
   a = 0
   do while (a < command_argument_count())
      a = a + 1
      arg = argument(a)
      if (a == command_argument_count()) call usage(arg//' needs a value')
      a = a + 1
      val = argument(a)
      select case(arg)
      case('--seed')
         read(val, *, iostat=ios) seed
         if (ios /= 0 .or. seed < 0_I8P) call usage('--seed needs an integer >= 0')
      case('--mutations')
         read(val, *, iostat=ios) mutations
         if (ios /= 0 .or. mutations < 0) call usage('--mutations needs an integer >= 0')
      case('--trees')
         read(val, *, iostat=ios) trees
         if (ios /= 0 .or. trees < 0) call usage('--trees needs an integer >= 0')
      case('--start')
         read(val, *, iostat=ios) start
         if (ios /= 0 .or. start < 0) call usage('--start needs an integer >= 0')
      case('--progress')
         read(val, *, iostat=ios) progress
         if (ios /= 0 .or. progress < 0) call usage('--progress needs an integer >= 0')
      case('--reports')
         read(val, *, iostat=ios) reports_max
         if (ios /= 0 .or. reports_max < 0) call usage('--reports needs an integer >= 0')
      case('--trace')
         trace = val
      case('--out')
         out_dir = val
      case('--dump')
         call doc%load(read_bytes(val))
         call options%parse(doc)
         print '(A)', dump(doc)
         stop
      case('--check')
         code = check_source(read_bytes(val), what)
         if (code /= F_NONE) then
            call report_source('file '//val, read_bytes(val), code, what)
            error stop 1
         endif
         print '(A)', 'baruch_test_fuzz: '//val//' passes every check'
         stop
      case default
         call usage('unknown argument '//arg)
      endselect
   enddo
   endsubroutine read_arguments

   function argument(a) result(arg)
   !< Return a command-line argument.
   integer, intent(in)       :: a   !< Its index.
   character(:), allocatable :: arg !< The argument.
   integer                   :: l   !< Its length.

   call get_command_argument(a, length=l)
   allocate(character(l) :: arg)
   if (l > 0) call get_command_argument(a, value=arg)
   endfunction argument

   subroutine usage(message)
   !< Stop on a misuse of the command line.
   character(*), intent(in) :: message !< What is wrong.

   print '(A)', 'baruch_test_fuzz: '//message
   print '(A)', 'usage: baruch_test_fuzz [--seed S] [--mutations N] [--trees N] [--start I] [--progress K] '// &
                '[--reports N] [--trace FILE] [--out DIR] | --dump FILE | --check FILE'
   error stop 2
   endsubroutine usage
endprogram baruch_test_fuzz
