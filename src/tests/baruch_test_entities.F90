!< BARUCH test: the HTML5 named character references, every name decoded as WHATWG's entities.json says.
program baruch_test_entities
!< BARUCH test: the HTML5 named character references, every name decoded as WHATWG's entities.json says.
!<
!< The gate of plan step 2.2. Every line of `src/tests/data/entities.txt` (written by `scripts/gen_entities.py` from
!< the pinned `entities.json`: the name as the JSON lists it, with `&`, and the UTF-8 bytes of its `characters`, in
!< hexadecimal) is looked up in the generated table (`entity_lookup`, the name without `&`) and decoded
!< (`entity_utf8`): the bytes must be the expected ones, and every entry of the table must be found once. Strings that
!< are not names (another case, a missing or doubled `;`, a blank, a byte above ASCII, too long) are not found. The
!< data file is read from `src/tests/data/`, relative to the working directory: run it from the repository root (as
!< `scripts/run_tests.sh` and `fpm test` do).
use penf,            only : I4P
use baruch_entities, only : ENTITY_COUNT, ENTITY_NAME_MAX, ENTITY_UTF8_MAX, entity_lookup, entity_utf8
implicit none
character(*), parameter :: DATA_FILE = 'src/tests/data/entities.txt' !< Expected data.
character(*), parameter :: LF        = char(10)                      !< Line feed.
character(*), parameter :: TAB       = char(9)                       !< Tab.
character(*), parameter :: DIGITS    = '0123456789abcdef'            !< Hexadecimal digits.
integer                 :: failures                                  !< Failed checks.

failures = 0
call test_all_names
call test_not_names
call test_examples
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_entities: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_entities: ok'

contains
   subroutine test_all_names()
   !< Every name of the data file decodes to its bytes; every entry of the table is found exactly once.
   character(:), allocatable :: data          !< The data file.
   character(:), allocatable :: record        !< One line of it.
   character(:), allocatable :: got           !< Bytes decoded.
   integer(I4P)              :: entity        !< Index of a name.
   integer(I4P)              :: found(ENTITY_COUNT) !< Times each entry is found.
   integer                   :: i, j, t       !< Bounds of a record and of its tab.
   integer                   :: names, passed !< Counters.
   integer                   :: longest       !< Longest value, in bytes.
   integer(I4P)              :: stat          !< Status.

   call read_whole(DATA_FILE, data, stat)
   call check(stat == 0 .and. len(data) > 0, 'cannot read '//DATA_FILE)
   if (stat /= 0) return
   found = 0
   names = 0
   passed = 0
   longest = 0
   i = 1
   do while (i <= len(data))
      j = index(data(i:), LF) + i - 1
      if (j < i) j = len(data) + 1
      record = data(i:j-1)
      i = j + 1
      if (len(record) == 0) cycle
      names = names + 1
      t = index(record, TAB)
      entity = entity_lookup(record(2:t-1))
      got = entity_utf8(entity)
      longest = max(longest, len(got))
      if (entity > 0 .and. hex(got) == record(t+1:) .and. len(got) == (len(record) - t) / 2) then
         passed = passed + 1
         found(entity) = found(entity) + 1
      elseif (names - passed <= 5) then
         print '(A,I0,A)', 'mismatch: '//record(:t-1)//' (index ', entity, ')'
         print '(A)', '  expected '//record(t+1:)
         print '(A)', '  got      '//hex(got)
      endif
   enddo
   print '(A,I0,A,I0,A,I0,A)', 'entities: ', passed, '/', names, ' names decode as entities.json (', &
                               names - passed, ' differences)'
   call check(names == ENTITY_COUNT, 'the data file and the table hold different numbers of names')
   call check(passed == names, 'not every name decodes to its bytes')
   call check(all(found == 1), 'an entry of the table is not found, or found twice')
   call check(longest == ENTITY_UTF8_MAX, 'ENTITY_UTF8_MAX is not the longest value')
   endsubroutine test_all_names

   subroutine test_not_names()
   !< Strings that are not names are not found; an index out of range decodes to nothing.
   character(ENTITY_NAME_MAX + 1) :: long !< A name one character too long.

   call check(entity_lookup('') == 0, 'the empty string is a name')
   call check(entity_lookup('Amp;') == 0, 'Amp; is a name (names are case-sensitive)')
   call check(entity_lookup('amp;;') == 0, 'amp;; is a name')
   call check(entity_lookup('&amp;') == 0, '&amp; is a name (the name has no &)')
   call check(entity_lookup('amp; ') == 0, 'amp; with a trailing blank is a name')
   call check(entity_lookup(' amp;') == 0, 'amp; with a leading blank is a name')
   call check(entity_lookup('ampx;') == 0, 'ampx; is a name')
   call check(entity_lookup('am') == 0, 'am is a name')
   call check(entity_lookup('ngE') == 0, 'ngE is a name (only ngE; is)')
   call check(entity_lookup('a'//char(195)//char(169)//';') == 0, 'a name with a byte above ASCII is found')
   call check(entity_lookup('amp'//char(0)) == 0, 'a name with a NUL is found')
   long = repeat('a', len(long) - 1)//';'
   call check(entity_lookup(long) == 0, 'a name longer than ENTITY_NAME_MAX is found')
   call check(entity_utf8(0_I4P) == '' .and. len(entity_utf8(0_I4P)) == 0, 'index 0 decodes to something')
   call check(len(entity_utf8(ENTITY_COUNT + 1_I4P)) == 0, 'index ENTITY_COUNT+1 decodes to something')
   call check(len(entity_utf8(-1_I4P)) == 0, 'index -1 decodes to something')
   endsubroutine test_not_names

   subroutine test_examples()
   !< The first and last names, the longest, a value of two code points, a legacy form and its twin.
   call check(decodes('AElig', char(195)//char(134)), 'AElig is not U+00C6')
   call check(decodes('zwnj;', char(226)//char(128)//char(140)), 'zwnj; is not U+200C')
   call check(decodes('CounterClockwiseContourIntegral;', char(226)//char(136)//char(179)), &
              'CounterClockwiseContourIntegral; is not U+2233')
   call check(decodes('ngE;', char(226)//char(137)//char(167)//char(204)//char(184)), &
              'ngE; is not U+2267 U+0338')
   call check(decodes('amp;', '&'), 'amp; is not &')
   call check(decodes('amp', '&'), 'amp (legacy form) is not &')
   call check(entity_lookup('amp') /= entity_lookup('amp;'), 'amp and amp; are the same entry')
   call check(decodes('quot;', '"'), 'quot; is not "')
   call check(decodes('Dcaron;', char(196)//char(142)), 'Dcaron; is not U+010E')
   call check(decodes('HilbertSpace;', char(226)//char(132)//char(139)), &
              'HilbertSpace; is not U+210B')
   call check(decodes('DifferentialD;', char(226)//char(133)//char(134)), &
              'DifferentialD; is not U+2146')
   call check(decodes('ClockwiseContourIntegral;', char(226)//char(136)//char(178)), &
              'ClockwiseContourIntegral; is not U+2232')
   endsubroutine test_examples

   function decodes(name, bytes) result(same)
   !< True if the name decodes to exactly the bytes.
   character(*), intent(in)  :: name  !< Name.
   character(*), intent(in)  :: bytes !< Expected bytes.
   logical                   :: same  !< Result.
   character(:), allocatable :: got   !< Bytes decoded.

   got = entity_utf8(entity_lookup(name))
   same = got == bytes .and. len(got) == len(bytes)
   endfunction decodes

   subroutine read_whole(path, bytes, stat)
   !< Read a file whole.
   character(*),              intent(in)  :: path  !< File.
   character(:), allocatable, intent(out) :: bytes !< Contents.
   integer(I4P),              intent(out) :: stat  !< Status.
   integer                                :: u     !< Unit.
   integer                                :: n     !< Size.
   integer                                :: ios   !< Status.

   bytes = ''
   open(newunit=u, file=path, access='stream', form='unformatted', action='read', status='old', iostat=ios)
   stat = int(ios, I4P)
   if (ios /= 0) return
   inquire(unit=u, size=n)
   deallocate(bytes)
   allocate(character(n) :: bytes)
   read(u, iostat=ios) bytes
   stat = int(ios, I4P)
   close(u)
   endsubroutine read_whole

   pure function hex(bytes) result(h)
   !< Lowercase hexadecimal of bytes.
   character(*), intent(in)  :: bytes !< Bytes.
   character(:), allocatable :: h     !< Hexadecimal.
   integer                   :: k     !< Counter.

   allocate(character(2 * len(bytes)) :: h)
   do k=1, len(bytes)
      h(2*k-1:2*k-1) = DIGITS(ichar(bytes(k:k)) / 16 + 1:ichar(bytes(k:k)) / 16 + 1)
      h(2*k:2*k) = DIGITS(mod(ichar(bytes(k:k)), 16) + 1:mod(ichar(bytes(k:k)), 16) + 1)
   enddo
   endfunction hex

   subroutine check(condition, what)
   !< Count and report a failed check.
   logical,      intent(in) :: condition !< Must be true.
   character(*), intent(in) :: what      !< What is checked.

   if (condition) return
   failures = failures + 1
   print '(A)', 'FAIL '//what
   endsubroutine check
endprogram baruch_test_entities
