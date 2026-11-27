!< BARUCH test: cmark's pathological inputs (and a few more) parse and render correctly, in linear time.
program baruch_test_pathological
!< BARUCH test: cmark's pathological inputs (and a few more) parse and render correctly, in linear time.
!<
!< The gate of plan step 2.6 (reference page docs/reference/conformance.md, "Pathological inputs"). The cases are those
!< of cmark's `test/pathological_tests.py` (<https://github.com/commonmark/cmark>, BSD-2-Clause, file at commit
!< `64efa3b3b3d35f2ffb604b57a8a9c89047cb420b`, SHA-256 `4f1ba575...c48a4f`): the same generators, each scaled by a
!< size parameter `k`, and the same checks, the regular expression of each case written as the equivalent test on the
!< HTML (most are a repeated literal searched for; three are anchored patterns, matched here by hand). A few cases are
!< BARUCH's own (the constructs whose scans could rescan the subject: unclosed processing instructions, CDATA sections
!< and declarations, attribute values, `&#`, `[\`, `[]`, link destinations with nested parentheses or an open title,
!< many reference definitions).
!<
!< With no argument (as `scripts/run_tests.sh` runs it) every case is built once at a small size, parsed, rendered and
!< checked: a correctness test, no timing. With `--timing` (as `scripts/pathological.sh` runs it):
!<
!<    baruch_test_pathological --timing [--n N] [--max-ratio R] [--case NAME]
!<
!< each case is built at `n` and `10n` (`n` = 10^4 by default; a case slow at 10^4 uses a smaller documented `n`, and
!< a case whose input grows as the square of its parameter takes the parameter that makes the input 10 times larger),
!< loaded, parsed and rendered three times at each size; the best wall time of each (`system_clock`) and their ratio
!< are printed in a table. With `--max-ratio`, a case over `R` is measured again, up to 3 attempts (the last one
!< printed, with its number): a load from another process stretches one measurement, a super-linear case fails them
!< all. The exit status is 1 when a check fails or, with `--max-ratio`, a ratio still exceeds `R`.
use baruch
use penf, only : I4P, I8P, R8P
implicit none
character, parameter    :: LF = achar(10)                          !< Line feed.
character, parameter    :: BS = achar(92)                          !< Backslash.
character, parameter    :: QT = achar(34)                          !< Double quote.
character(*), parameter :: FFFD = char(239)//char(191)//char(189)  !< U+FFFD in UTF-8.
integer, parameter      :: CHECK_CONTAINS = 1                      !< The HTML contains the expected text.
integer, parameter      :: CHECK_BACKTICKS = 2                     !< `^<p>[e`]*</p>\n$`.
integer, parameter      :: CHECK_TAGS_X = 3                        !< `^(<\w+>\n?)+x(</\w+>\n)+$`.
integer, parameter      :: CHECK_QUOTED_P = 4                      !< `^(<\w+>\n)+<p>.*</p>\n(</\w+>\n)+$`.
integer, parameter      :: SCALE_LINEAR = 1                        !< The input grows as k.
integer, parameter      :: SCALE_SQUARE = 2                        !< The input grows as k^2.
integer, parameter      :: NCASES = 35                             !< Number of cases.
integer, parameter      :: NAME_LEN = 48                           !< Width of a case name.
integer                 :: failures                                !< Failed checks.
logical                 :: timing                                  !< Measure (or only check).
integer                 :: n                                       !< Base size.
real(R8P)               :: max_ratio                               !< Largest ratio allowed, <= 0 for none.
character(:), allocatable :: only                                  !< The one case to run, empty for all.
type :: text
   character(:), allocatable :: s !< A string (an array of strings of their own lengths).
endtype text

failures = 0
call read_arguments
if (timing) then
   call run_timing
else
   call run_checks
endif
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_pathological: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_pathological: ok'

contains
   subroutine read_arguments()
   !< Read the command line.
   character(:), allocatable :: arg  !< An argument.
   character(:), allocatable :: val  !< Its value.
   integer                   :: a    !< Argument counter.
   integer                   :: ios  !< I/O status.

   timing = .false.
   n = 10000
   max_ratio = 0._R8P
   only = ''
   a = 0
   do while (a < command_argument_count())
      a = a + 1
      arg = argument(a)
      select case(arg)
      case('--timing')
         timing = .true.
      case('--n', '--max-ratio', '--case')
         if (a == command_argument_count()) call usage(arg//' needs a value')
         a = a + 1
         val = argument(a)
         select case(arg)
         case('--n')
            read(val, *, iostat=ios) n
            if (ios /= 0 .or. n < 10) call usage('--n needs an integer >= 10')
         case('--max-ratio')
            read(val, *, iostat=ios) max_ratio
            if (ios /= 0) call usage('--max-ratio needs a number')
         case('--case')
            only = val
         endselect
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

   print '(A)', 'baruch_test_pathological: '//message
   print '(A)', 'usage: baruch_test_pathological [--timing [--n N] [--max-ratio R] [--case NAME]]'
   error stop 2
   endsubroutine usage

   subroutine run_checks()
   !< Build every case at a small size, parse, render and check it.
   integer                   :: c     !< Case.
   integer                   :: k     !< Size parameter.
   integer                   :: ck    !< Check kind.
   character(:), allocatable :: input !< Markdown.
   character(:), allocatable :: want  !< Expected text.
   character(:), allocatable :: html  !< HTML.

   do c = 1, NCASES
      k = case_k(c, 1000)
      call make_case(c, k, input, want, ck)
      call parse_render(input, html)
      call check(check_html(ck, want, html), trim(case_name(c))//' (k = '//itoa(k)//')')
   enddo
   endsubroutine run_checks

   subroutine run_timing()
   !< Build every case at n and 10n, time it (best of 3), check it, print the table. With a gate (`--max-ratio`), a case
   !< over it is measured again, up to 3 attempts, and its last attempt is printed: another process loading the machine
   !< can stretch one measurement, while a super-linear case fails every attempt.
   integer, parameter        :: ATTEMPTS = 3 !< Measurements of a case over the gate.
   integer                   :: c        !< Case.
   integer                   :: s        !< Size: 1 for n, 2 for 10n.
   integer                   :: r        !< Repetition.
   integer                   :: a        !< Attempt.
   integer                   :: nc       !< The case's n.
   integer                   :: k        !< Size parameter.
   integer                   :: bytes(2) !< Input sizes.
   integer                   :: ck(2)    !< Check kinds.
   real(R8P)                 :: t(2)     !< Best times (s).
   real(R8P)                 :: ratio    !< t(10n)/t(n).
   logical                   :: ok       !< Checks passed.
   logical                   :: found    !< The case asked for exists.
   integer(I8P)              :: t0, t1, rate !< Clock.
   type(text)                :: input(2) !< Markdown at n and 10n.
   type(text)                :: want(2)  !< Expected texts.
   character(:), allocatable :: html     !< HTML.
   character(:), allocatable :: verdict  !< ok, BAD, and the attempts.

   print '(A)', '| case | n | bytes(n) | t(n) ms | t(10n) ms | ratio | check |'
   print '(A)', '|---|---:|---:|---:|---:|---:|---|'
   found = .false.
   do c = 1, NCASES
      if (len(only) > 0) then
         if (trim(case_name(c)) /= only) cycle
      endif
      found = .true.
      nc = n / case_divisor(c)
      do s = 1, 2
         k = case_k(c, nc * 10**(s - 1))
         call make_case(c, k, input(s)%s, want(s)%s, ck(s))
         bytes(s) = len(input(s)%s)
      enddo
      do a = 1, ATTEMPTS
         ok = .true.
         do s = 1, 2
            t(s) = huge(1._R8P)
            do r = 1, 3
               call system_clock(t0, rate)
               call parse_render(input(s)%s, html)
               call system_clock(t1)
               t(s) = min(t(s), real(t1 - t0, R8P) / real(rate, R8P))
            enddo
            ok = ok .and. check_html(ck(s), want(s)%s, html)
         enddo
         ratio = t(2) / max(t(1), 1.e-9_R8P)
         if (.not. ok .or. max_ratio <= 0._R8P .or. ratio <= max_ratio) exit
      enddo
      a = min(a, ATTEMPTS)
      verdict = 'ok'
      if (.not. ok .or. (max_ratio > 0._R8P .and. ratio > max_ratio)) then
         verdict = 'BAD'
         failures = failures + 1
      endif
      if (a > 1) verdict = verdict//', attempt '//itoa(a)
      print '(A)', '| '//trim(case_name(c))//' | '//itoa(nc)//' | '//itoa(bytes(1))//' | '//fixed(1000 * t(1))// &
                   ' | '//fixed(1000 * t(2))//' | '//fixed(ratio)//' | '//verdict//' |'
   enddo
   if (.not. found) call usage('no case '//only)
   endsubroutine run_timing

   pure function fixed(x) result(s)
   !< Return a number with two decimals (a leading 0 on every compiler).
   real(R8P), intent(in)     :: x   !< Number.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(F24.2)') x
   s = trim(adjustl(buf))
   endfunction fixed

   subroutine parse_render(input, html)
   !< Load, parse and render a source (the timed work).
   character(*),              intent(in)  :: input   !< Markdown.
   character(:), allocatable, intent(out) :: html    !< HTML.
   type(markdown_document)                :: doc     !< Document.
   type(markdown_options)                 :: options !< Options.
   integer(I4P)                           :: stat    !< Status.
   character(:), allocatable              :: errmsg  !< Message.

   call doc%load(input)
   call options%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'parse: '//errmsg)
   call options%render_html(doc, html, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'render: '//errmsg)
   endsubroutine parse_render

   function case_name(c) result(name)
   !< Return the name of a case (cmark's, or BARUCH's own marked so).
   integer, intent(in)  :: c    !< Case.
   character(NAME_LEN)  :: name !< Name.

   select case(c)
   case(1);  name = 'nested strong emph'
   case(2);  name = 'many emph closers with no openers'
   case(3);  name = 'many emph openers with no closers'
   case(4);  name = 'many link closers with no openers'
   case(5);  name = 'many link openers with no closers'
   case(6);  name = 'mismatched openers and closers'
   case(7);  name = 'issue #389'
   case(8);  name = 'openers and closers multiple of 3'
   case(9);  name = 'link openers and emph closers'
   case(10); name = 'pattern [ (]( repeated'
   case(11); name = 'pattern ![[]() repeated'
   case(12); name = 'hard link/emph case'
   case(13); name = 'nested brackets'
   case(14); name = 'nested block quotes'
   case(15); name = 'deeply nested lists'
   case(16); name = 'U+0000 in input'
   case(17); name = 'backticks'
   case(18); name = 'unclosed links A'
   case(19); name = 'unclosed links B'
   case(20); name = 'unclosed <!--'
   case(21); name = 'empty lines in deeply nested lists'
   case(22); name = 'empty lines in deeply nested lists in blockquote'
   case(23); name = 'emph in deep blockquote'
   case(24); name = 'reference collisions'
   case(25); name = 'nested inlines'
   case(26); name = 'many references'
   case(27); name = '+ unclosed <?'
   case(28); name = '+ unclosed <![CDATA['
   case(29); name = '+ unclosed <!A'
   case(30); name = '+ unclosed attribute values'
   case(31); name = '+ many &#'
   case(32); name = '+ pattern ['//BS//' repeated'
   case(33); name = '+ pattern [] repeated'
   case(34); name = '+ link destinations with open parentheses'
   case(35); name = '+ link destinations with open titles'
   case default; name = '?'
   endselect
   endfunction case_name

   pure function case_divisor(c) result(d)
   !< Return the divisor of the base size of a case: 1, but 10 for `backticks`, whose input grows as k^2: at n = 10^4
   !< it is 3.1 MB and 31 MB at 10n, past the last-level cache, where a plain paragraph of that size (no backtick)
   !< shows the same ratio (about 19: memory, not the algorithm); at n = 10^3 its 10n input is cmark's own (2500
   !< strings, 3.1 MB).
   integer, intent(in) :: c !< Case.
   integer             :: d !< Divisor.

   d = 1
   if (c == 17) d = 10
   endfunction case_divisor

   pure function case_scale(c) result(s)
   !< Return how the input of a case grows with its parameter.
   integer, intent(in) :: c !< Case.
   integer             :: s !< SCALE_LINEAR or SCALE_SQUARE.

   select case(c)
   case(15, 17)
      s = SCALE_SQUARE
   case default
      s = SCALE_LINEAR
   endselect
   endfunction case_scale

   pure function case_k(c, size) result(k)
   !< Return the parameter of a case for a size: the size itself, or for an input growing as the square of the
   !< parameter its square root, scaled to cmark's value at 10^4 (500 nested lists, 2500 backtick strings).
   integer, intent(in) :: c    !< Case.
   integer, intent(in) :: size !< Size.
   integer             :: k    !< Parameter.

   if (case_scale(c) == SCALE_SQUARE) then
      if (c == 15) then
         k = nint(sqrt(25._R8P * size))
      else
         k = nint(sqrt(625._R8P * size))
      endif
   else
      k = size
   endif
   if (c == 25) k = 2 * (k / 2) ! even: strong emphasis only
   endfunction case_k

   subroutine make_case(c, k, input, want, ck)
   !< Build the input of a case at a parameter, the text the HTML must hold and the kind of check. Both are built by
   !< appending to a buffer on the heap: a large character expression (`repeat(...)//...`) is a temporary that ifx
   !< puts on the stack (a 4 MB one overflows the default 8 MB stack).
   integer,                   intent(in)  :: c     !< Case.
   integer,                   intent(in)  :: k     !< Parameter.
   character(:), allocatable, intent(out) :: input !< Markdown.
   character(:), allocatable, intent(out) :: want  !< Expected text.
   integer,                   intent(out) :: ck    !< Check kind.
   character(:), allocatable              :: bi    !< Input builder.
   character(:), allocatable              :: bw    !< Expected text builder.
   character(:), allocatable              :: bad   !< The key every paragraph uses (reference collisions).
   integer                                :: ui    !< Bytes used in the input builder.
   integer                                :: uw    !< Bytes used in the expected text builder.
   integer                                :: i     !< Counter.
   logical                                :: same  !< The expected text is the input.

   ck = CHECK_CONTAINS
   bi = ''
   bw = ''
   ui = 0
   uw = 0
   same = .false.
   select case(c)
   case(1) ! nested strong emph
      call put(bi, ui, '*a **a ', k); call put(bi, ui, 'b'); call put(bi, ui, ' a** a*', k)
      call put(bw, uw, '<em>a <strong>a ', k); call put(bw, uw, 'b'); call put(bw, uw, ' a</strong> a</em>', k)
   case(2) ! many emph closers with no openers
      call put(bi, ui, 'a_ ', k)
      call put(bw, uw, 'a_ ', k - 1); call put(bw, uw, 'a_')
   case(3) ! many emph openers with no closers
      call put(bi, ui, '_a ', k)
      call put(bw, uw, '_a ', k - 1); call put(bw, uw, '_a')
   case(4) ! many link closers with no openers
      call put(bi, ui, 'a]', k)
      same = .true.
   case(5) ! many link openers with no closers
      call put(bi, ui, '[a', k)
      same = .true.
   case(6) ! mismatched openers and closers
      call put(bi, ui, '*a_ ', k)
      call put(bw, uw, '*a_ ', k - 1); call put(bw, uw, '*a_')
   case(7) ! issue #389
      call put(bi, ui, '*a ', k); call put(bi, ui, '_a*_ ', k)
      call put(bw, uw, '<em>a ', k); call put(bw, uw, '_a</em>_ ', k - 1); call put(bw, uw, '_a</em>_')
   case(8) ! openers and closers multiple of 3
      call put(bi, ui, 'a**b'); call put(bi, ui, 'c* ', k)
      call put(bw, uw, 'a**b'); call put(bw, uw, 'c* ', k - 1); call put(bw, uw, 'c*')
   case(9) ! link openers and emph closers
      call put(bi, ui, '[ a_', k)
      same = .true.
   case(10) ! pattern [ (]( repeated
      call put(bi, ui, '[ (](', k)
      same = .true.
   case(11) ! pattern ![[]() repeated
      call put(bi, ui, '![[]()', k)
      call put(bw, uw, '![<a href='//QT//QT//'></a>', k)
   case(12) ! hard link/emph case: cmark's one paragraph, here k of them
      call put(bi, ui, '**x [a*b**c*](d)'//LF//LF, k)
      call put(bw, uw, '<p>**x <a href='//QT//'d'//QT//'>a<em>b**c</em></a></p>'//LF, k)
   case(13) ! nested brackets
      call put(bi, ui, '[', k); call put(bi, ui, 'a'); call put(bi, ui, ']', k)
      same = .true.
   case(14) ! nested block quotes
      call put(bi, ui, '> ', k); call put(bi, ui, 'a')
      call put(bw, uw, '<blockquote>'//LF, k)
   case(15) ! deeply nested lists: k levels, the input grows as k^2
      do i = 0, k - 1
         call put(bi, ui, '  ', i); call put(bi, ui, '* a'//LF)
      enddo
      call put(bw, uw, '<ul>'//LF); call put(bw, uw, '<li>a'//LF//'<ul>'//LF, k - 1)
      call put(bw, uw, '<li>a</li>'//LF//'</ul>'//LF); call put(bw, uw, '</li>'//LF//'</ul>'//LF, k - 1)
   case(16) ! U+0000 in input: cmark's one line, here k times
      call put(bi, ui, 'abc'//achar(0)//'de'//achar(0), k)
      call put(bw, uw, 'abc'//FFFD//'de'//FFFD, k)
   case(17) ! backticks: k strings of 1 to k backticks, the input grows as k^2
      do i = 1, k - 1
         call put(bi, ui, 'e'); call put(bi, ui, '`', i)
      enddo
      ck = CHECK_BACKTICKS
   case(18) ! unclosed links A
      call put(bi, ui, '[a](<b', k)
      call put(bw, uw, '[a](&lt;b', k)
   case(19) ! unclosed links B
      call put(bi, ui, '[a](b', k)
      same = .true.
   case(20) ! unclosed <!--
      call put(bi, ui, '</'); call put(bi, ui, '<!--', k)
      call put(bw, uw, '&lt;/'); call put(bw, uw, '&lt;!--', k)
   case(21) ! empty lines in deeply nested lists
      call put(bi, ui, '- ', k); call put(bi, ui, 'x'); call put(bi, ui, LF, k)
      ck = CHECK_TAGS_X
   case(22) ! empty lines in deeply nested lists in blockquote
      call put(bi, ui, '> '); call put(bi, ui, '- ', k); call put(bi, ui, 'x'//LF); call put(bi, ui, '>'//LF, k)
      ck = CHECK_TAGS_X
   case(23) ! emph in deep blockquote
      call put(bi, ui, '>', k); call put(bi, ui, 'a*', k)
      ck = CHECK_QUOTED_P
   case(24) ! reference collisions: k keys colliding in cmark's hash table, k - 1 definitions
      call collisions(k, bad, bi, ui)
      call put(bw, uw, '<p>['//bad//']</p>'//LF, k - 1)
   case(25) ! nested inlines (cmark checks its CommonMark writer; here the HTML: k/2 nested strong)
      call put(bi, ui, '*', k); call put(bi, ui, 'a'); call put(bi, ui, '*', k)
      call put(bw, uw, '<p>'); call put(bw, uw, '<strong>', k / 2); call put(bw, uw, 'a')
      call put(bw, uw, '</strong>', k / 2); call put(bw, uw, '</p>'//LF)
   case(26) ! many references (commented out in cmark): k definitions, k uses of an undefined label
      do i = 1, k
         call put(bi, ui, '['//itoa(i)//']: u'//LF)
      enddo
      call put(bi, ui, '[0] ', k)
      call put(bw, uw, '[0] ', k - 1)
   case(27) ! unclosed processing instructions (after a letter: no HTML block)
      call put(bi, ui, 'x'); call put(bi, ui, '<?', k)
      call put(bw, uw, 'x'); call put(bw, uw, '&lt;?', k)
   case(28) ! unclosed CDATA sections
      call put(bi, ui, 'x'); call put(bi, ui, '<![CDATA[', k)
      call put(bw, uw, 'x'); call put(bw, uw, '&lt;![CDATA[', k)
   case(29) ! unclosed declarations
      call put(bi, ui, 'x'); call put(bi, ui, '<!A', k)
      call put(bw, uw, 'x'); call put(bw, uw, '&lt;!A', k)
   case(30) ! unclosed attribute values
      call put(bi, ui, 'x'); call put(bi, ui, '<a b='//"'", k)
      call put(bw, uw, 'x'); call put(bw, uw, '&lt;a b='//"'", k)
   case(31) ! many &#
      call put(bi, ui, '&#', k)
      call put(bw, uw, '&amp;#', k)
   case(32) ! pattern [\ repeated: one bracket, k - 1 escaped brackets, a backslash at the end
      call put(bi, ui, '['//BS, k)
      call put(bw, uw, '[', k); call put(bw, uw, BS//'</p>')
   case(33) ! pattern [] repeated
      call put(bi, ui, '[]', k)
      same = .true.
   case(34) ! link destinations with open parentheses
      call put(bi, ui, '[a](b(', k)
      same = .true.
   case(35) ! link destinations with open titles
      call put(bi, ui, '[a](b '//QT, k)
      call put(bw, uw, '[a](b &quot;', k)
   endselect
   allocate(character(ui) :: input)
   input(1:ui) = bi(1:ui)
   if (same) then
      want = input
   else
      allocate(character(uw) :: want)
      want(1:uw) = bw(1:uw)
   endif
   endsubroutine make_case

   subroutine collisions(k, bad, buf, used)
   !< cmark's `hash_collisions`: the keys `x0`, `x1`, ... whose cmark hash is 0 modulo 16 (its reference map size);
   !< the first is the label every paragraph uses (never defined), each next one a definition and a paragraph.
   integer,                   intent(in)    :: k     !< Number of colliding keys.
   character(:), allocatable, intent(out)   :: bad   !< The first key.
   character(:), allocatable, intent(inout) :: buf   !< Builder of the Markdown.
   integer,                   intent(inout) :: used  !< Bytes used.
   character(:), allocatable                :: key   !< A key.
   integer                                  :: found !< Keys found.
   integer                                  :: i     !< Candidate.

   found = 0
   i = -1
   bad = ''
   do while (found < k)
      i = i + 1
      key = 'x'//itoa(i)
      if (.not. cmark_badhash(key)) cycle
      found = found + 1
      if (found == 1) then
         bad = key
      else
         call put(buf, used, '['//key//']: /url'//LF//LF//'['//bad//']'//LF//LF)
      endif
   enddo
   endsubroutine collisions

   pure function cmark_badhash(key) result(bad)
   !< True when cmark's reference hash of a key (32 bits) is 0 modulo 16.
   character(*), intent(in) :: key !< Key.
   logical                  :: bad !< Colliding.
   integer(I8P), parameter  :: MASK = 4294967295_I8P !< 2^32 - 1.
   integer(I8P)             :: h   !< Hash.
   integer                  :: i   !< Counter.

   h = 0_I8P
   do i = 1, len(key)
      h = iand(int(ichar(key(i:i)), I8P) + iand(ishft(h, 6), MASK) + iand(ishft(h, 16), MASK) - h, MASK)
   enddo
   bad = iand(h, 15_I8P) == 0_I8P
   endfunction cmark_badhash

   function check_html(ck, want, html) result(ok)
   !< Check the HTML of a case as cmark's regular expression does.
   integer,      intent(in) :: ck   !< Check kind.
   character(*), intent(in) :: want !< Expected text (CHECK_CONTAINS).
   character(*), intent(in) :: html !< HTML.
   logical                  :: ok   !< Passed.

   select case(ck)
   case(CHECK_CONTAINS)
      ok = index(html, want) > 0
   case(CHECK_BACKTICKS)
      ok = backticks_only(html)
   case(CHECK_TAGS_X)
      ok = tags_x(html)
   case(CHECK_QUOTED_P)
      ok = quoted_paragraph(html)
   case default
      ok = .false.
   endselect
   endfunction check_html

   pure function backticks_only(html) result(ok)
   !< `^<p>[e`]*</p>\n$`.
   character(*), intent(in) :: html !< HTML.
   logical                  :: ok   !< Matched.

   ok = .false.
   if (len(html) < 8) return
   if (html(1:3) /= '<p>' .or. html(len(html) - 4:) /= '</p>'//LF) return
   ok = verify(html(4:len(html) - 5), 'e`') == 0
   endfunction backticks_only

   pure function tags_x(html) result(ok)
   !< `^(<\w+>\n?)+x(</\w+>\n)+$`.
   character(*), intent(in) :: html  !< HTML.
   logical                  :: ok    !< Matched.
   integer                  :: i     !< Cursor.
   integer                  :: j     !< End of a tag.
   integer                  :: count !< Tags matched.

   ok = .false.
   i = 1
   count = 0
   do
      j = open_tag_end(html, i)
      if (j == 0) exit
      i = j + 1
      count = count + 1
      if (i <= len(html)) then
         if (html(i:i) == LF) i = i + 1
      endif
   enddo
   if (count == 0 .or. i > len(html)) return
   if (html(i:i) /= 'x') return
   ok = closing_tags(html, i + 1)
   endfunction tags_x

   pure function quoted_paragraph(html) result(ok)
   !< `^(<\w+>\n)+<p>.*</p>\n(</\w+>\n)+$` (`.` not matching a line feed).
   character(*), intent(in) :: html  !< HTML.
   logical                  :: ok    !< Matched.
   integer                  :: i     !< Cursor.
   integer                  :: j     !< End of a tag or of the line.
   integer                  :: count !< Tags matched.

   ok = .false.
   i = 1
   count = 0
   do
      j = open_tag_end(html, i)
      if (j == 0) exit
      if (j + 1 > len(html)) exit
      if (html(j + 1:j + 1) /= LF) exit
      i = j + 2
      count = count + 1
   enddo
   if (count == 0 .or. i + 2 > len(html)) return
   if (html(i:i + 2) /= '<p>') return
   j = index(html(i:), LF)
   if (j == 0) return
   j = i + j - 1 ! the line feed ending the paragraph's line
   if (j - 4 < i + 3) return
   if (html(j - 4:j - 1) /= '</p>') return
   ok = closing_tags(html, j + 1)
   endfunction quoted_paragraph

   pure function open_tag_end(html, i) result(j)
   !< Return the position of the `>` of a tag `<\w+>` starting at `i`, 0 if none starts there.
   character(*), intent(in) :: html !< HTML.
   integer,      intent(in) :: i    !< Start.
   integer                  :: j    !< End.

   j = 0
   if (i > len(html)) return
   if (html(i:i) /= '<') return
   j = word_end(html, i + 1)
   if (j == i + 1 .or. j > len(html)) then
      j = 0
   elseif (html(j:j) /= '>') then
      j = 0
   endif
   endfunction open_tag_end

   pure function closing_tags(html, start) result(ok)
   !< `(</\w+>\n)+$` from `start`.
   character(*), intent(in) :: html  !< HTML.
   integer,      intent(in) :: start !< Start.
   logical                  :: ok    !< Matched.
   integer                  :: i     !< Cursor.
   integer                  :: j     !< End of a word.
   integer                  :: count !< Tags matched.

   ok = .false.
   i = start
   count = 0
   do while (i <= len(html))
      if (i + 1 > len(html)) return
      if (html(i:i + 1) /= '</') return
      j = word_end(html, i + 2)
      if (j == i + 2 .or. j + 1 > len(html)) return
      if (html(j:j + 1) /= '>'//LF) return
      i = j + 2
      count = count + 1
   enddo
   ok = count > 0
   endfunction closing_tags

   pure function word_end(html, i) result(j)
   !< Return the first position from `i` that is not a word character (`\w`, ASCII).
   character(*), intent(in) :: html !< HTML.
   integer,      intent(in) :: i    !< Start.
   integer                  :: j    !< End.

   j = i
   do while (j <= len(html))
      select case(html(j:j))
      case('a':'z', 'A':'Z', '0':'9', '_')
         j = j + 1
      case default
         exit
      endselect
   enddo
   endfunction word_end

   subroutine put(buf, used, text, times)
   !< Append text to a builder, `times` times (default once), doubling its capacity when full.
   character(:), allocatable, intent(inout) :: buf   !< Builder.
   integer,                   intent(inout) :: used  !< Bytes used.
   character(*),              intent(in)    :: text  !< Text.
   integer,      optional,    intent(in)    :: times !< Repetitions.
   character(:), allocatable                :: tmp   !< Larger builder.
   integer                                  :: n     !< Repetitions.
   integer                                  :: i     !< Counter.

   n = 1
   if (present(times)) n = times
   if (n <= 0 .or. len(text) == 0) return
   if (used + n * len(text) > len(buf)) then
      allocate(character(max(2 * len(buf), used + n * len(text), 1024)) :: tmp)
      tmp(1:used) = buf(1:used)
      buf = tmp ! not move_alloc: nvfortran 26.9 keeps the old length (szaghi/BARUCH#2 B1)
   endif
   do i = 1, n
      buf(used + 1:used + len(text)) = text
      used = used + len(text)
   enddo
   endsubroutine put

   pure function itoa(i) result(s)
   !< Return an integer as text.
   integer, intent(in)       :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(12)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction itoa

   subroutine check(condition, label)
   !< Count a failed check and print it.
   logical,      intent(in) :: condition !< What must hold.
   character(*), intent(in) :: label     !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL '//label
   endif
   endsubroutine check
endprogram baruch_test_pathological
