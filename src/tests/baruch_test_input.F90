!< BARUCH test: the input, sanitised UTF-8 from any source, its encodings and its lines.
program baruch_test_input
!< BARUCH test: the input, sanitised UTF-8 from any source, its encodings and its lines.
!<
!< The gate of plan step 1.2. Every case of `src/tests/data/input_corpus.txt` (StringiFor's UTF-8 corpus under several
!< encodings, the expected bytes computed by Python's `bytes.decode(codec, 'replace')`, see `scripts/input_corpus.py`)
!< loads byte-identical to the expected UTF-8; U+0000 becomes U+FFFD; the byte order mark goes; a UTF-16LE file with a
!< byte order mark and a windows-1252 file with `encoding=` load as the expected UTF-8; LF, CR and CRLF give the same
!< lines; a missing file and an unsupported encoding give a status and leave the document as it was. The data files are
!< read from `src/tests/data/`, relative to the working directory: run it from the repository root (as
!< `scripts/run_tests.sh` and `fpm test` do).
use baruch
use penf,       only : I4P
use stringifor, only : string
implicit none
character(*), parameter :: DATA_DIR = 'src/tests/data/'                    !< Test data.
character(*), parameter :: FFFD     = char(239)//char(191)//char(189)      !< U+FFFD in UTF-8.
character(*), parameter :: BOM      = char(239)//char(187)//char(191)      !< U+FEFF in UTF-8.
character(*), parameter :: LF       = char(10)                             !< Line feed.
character(*), parameter :: CR       = char(13)                             !< Carriage return.
character(*), parameter :: DIGITS   = '0123456789abcdef'                   !< Hexadecimal digits.
integer                 :: failures                                        !< Failed checks.

failures = 0
call test_corpus
call test_nul_and_bom
call test_files
call test_line_endings
call test_lines_and_slices
call test_sources
call test_errors
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_input: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_input: ok'

contains
   subroutine test_corpus()
   !< Every case of the oracle data byte-identical to Python's replace-decode.
   character(:), allocatable :: data          !< The data file.
   character(:), allocatable :: record        !< One line of it.
   character(:), allocatable :: field(:)      !< Its fields: category, encoding, input, expected (hexadecimal).
   character(:), allocatable :: got           !< Source after the load.
   character(:), allocatable :: expected      !< Expected source.
   type(markdown_document)   :: doc           !< Document.
   integer                   :: i, j          !< Bounds of a record.
   integer                   :: cases, passed !< Counters.
   integer                   :: replaced      !< Cases with a U+FFFD in the result.
   integer(I4P)              :: stat          !< Status.

   call read_whole(DATA_DIR//'input_corpus.txt', data, stat)
   call check(stat == 0 .and. len(data) > 0, 'corpus: cannot read '//DATA_DIR//'input_corpus.txt')
   if (stat /= 0) return
   cases = 0
   passed = 0
   replaced = 0
   i = 1
   do while (i <= len(data))
      j = index(data(i:), LF) + i - 1
      if (j < i) j = len(data) + 1
      record = data(i:j-1)
      i = j + 1
      if (len(record) == 0) cycle
      call split_tabs(record, field)
      cases = cases + 1
      if (trim(field(2)) == '-') then
         call doc%load(unhex(trim(field(3))), stat=stat)
      else
         call doc%load(unhex(trim(field(3))), encoding=trim(field(2)), stat=stat)
      endif
      got = doc%source()
      expected = unhex(trim(field(4)))
      if (stat == BARUCH_OK .and. got == expected .and. len(got) == len(expected)) then
         passed = passed + 1
         if (index(got, FFFD) > 0) replaced = replaced + 1
      elseif (cases - passed <= 5) then
         print '(A)', 'corpus mismatch: '//trim(field(1))//' '//trim(field(2))//' '//trim(field(3))
         print '(A)', '  expected '//trim(field(4))
         print '(A)', '  got      '//hex(got)
      endif
   enddo
   print '(A,I0,A,I0,A,I0,A)', 'corpus: ', passed, '/', cases, ' byte-identical to Python (', replaced, &
                                ' with U+FFFD)'
   call check(cases > 4000 .and. passed == cases, 'corpus: not every case is byte-identical')
   endsubroutine test_corpus

   subroutine test_nul_and_bom()
   !< U+0000 to U+FFFD, the byte order mark removed once, a later U+FEFF kept.
   type(markdown_document) :: doc !< Document.

   call doc%load('a'//char(0)//'b'//char(0))
   call check(doc%source() == 'a'//FFFD//'b'//FFFD, 'nul: U+0000 is not U+FFFD')
   call doc%load(BOM//'# Title'//LF)
   call check(doc%source() == '# Title'//LF, 'bom: the byte order mark is not removed')
   call doc%load(BOM//BOM//'x')
   call check(doc%source() == BOM//'x', 'bom: more than one byte order mark removed')
   call doc%load('x'//BOM)
   call check(doc%source() == 'x'//BOM, 'bom: a U+FEFF after the start is not kept')
   call doc%load(BOM//'x', encoding='UTF-8')
   call check(doc%source() == 'x', 'bom: the byte order mark is not removed with encoding=UTF-8')
   call doc%load(char(255)//char(254)//'x'//char(0), encoding='latin1')
   call check(doc%source() == char(195)//char(191)//char(195)//char(190)//'x'//FFFD, &
              'bom: FF FE read as Latin-1 is not two letters')
   endsubroutine test_nul_and_bom

   subroutine test_files()
   !< A UTF-16LE file with a byte order mark and CRLF lines, and a windows-1252 file, against the same text in UTF-8.
   type(markdown_document)   :: expected     !< The text in UTF-8.
   type(markdown_document)   :: doc          !< Document.
   integer(I4P)              :: stat         !< Status.
   character(:), allocatable :: errmsg       !< Message.
   integer(I4P)              :: i            !< Line.

   call expected%load_file(DATA_DIR//'encoding_expected.md', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'files: encoding_expected.md: '//errmsg)
   call check(expected%lines_number() == 6_I4P, 'files: encoding_expected.md has not 6 lines')
   ! UTF-16LE, read by its byte order mark, then declared
   call doc%load_file(DATA_DIR//'encoding_utf16le_bom.md', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'files: encoding_utf16le_bom.md: '//errmsg)
   call check(same_lines(doc, expected), 'files: the UTF-16LE file has not the expected lines')
   call check(doc%line_ending(1_I4P) == CR//LF, 'files: the UTF-16LE file has not its CRLF kept')
   call check(doc%source_length() == expected%source_length() + expected%lines_number(), &
              'files: the UTF-16LE file has not one CR more per line')
   call doc%load_file(DATA_DIR//'encoding_utf16le_bom.md', encoding='UTF-16', stat=stat)
   call check(stat == BARUCH_OK .and. same_lines(doc, expected), 'files: the UTF-16LE file with encoding=UTF-16')
   ! windows-1252, declared (name and alias); undeclared, it is ill-formed UTF-8
   call doc%load_file(DATA_DIR//'encoding_windows1252.md', encoding='windows-1252', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'files: encoding_windows1252.md: '//errmsg)
   call check(doc%source() == expected%source(), 'files: the windows-1252 file is not the expected UTF-8')
   call doc%load_file(DATA_DIR//'encoding_windows1252.md', encoding=' CP1252 ', stat=stat)
   call check(stat == BARUCH_OK .and. doc%source() == expected%source(), 'files: the alias CP1252')
   call doc%load_file(DATA_DIR//'encoding_windows1252.md', stat=stat)
   call check(stat == BARUCH_OK .and. index(doc%source(), FFFD) > 0, &
              'files: the windows-1252 file read as UTF-8 has no U+FFFD')
   do i=1_I4P, expected%lines_number()
      call check(doc%line_first(i) > 0_I4P, 'files: a line of the windows-1252 file read as UTF-8 is missing')
   enddo
   endsubroutine test_files

   subroutine test_line_endings()
   !< LF, CR, CRLF and a mix give the same lines; each ending is the one of the source; the last line may have none.
   character(*), parameter   :: LINES(5) = ['# Title', '       ', 'text   ', '  more ', '       '] !< The lines.
   integer,      parameter   :: LENS(5)  = [7, 0, 4, 6, 0]                                        !< Their lengths.
   character(2)              :: ending(4)                                                         !< The endings.
   integer                   :: elen(4)                                                           !< Their lengths.
   type(markdown_document)   :: doc                                                               !< Document.
   character(:), allocatable :: text                                                              !< Source.
   integer                   :: e, k                                                              !< Counters.

   ending = [LF//' ', CR//' ', CR//LF, LF//' ']   ! the 4th case mixes the first three
   elen   = [1, 1, 2, 1]
   do e=1, 4
      text = ''
      do k=1, 5
         if (e < 4) then
            text = text//LINES(k)(1:LENS(k))//ending(e)(1:elen(e))
         else ! a mix: CRLF, LF, CR in turn
            text = text//LINES(k)(1:LENS(k))//ending(mod(k - 1, 3) + 1)(1:elen(mod(k - 1, 3) + 1))
         endif
      enddo
      call doc%load(text)
      call check(doc%lines_number() == 5_I4P, 'line endings: not 5 lines, case '//itoa(e))
      do k=1, 5
         call check(doc%line(int(k, I4P)) == LINES(k)(1:LENS(k)) .and. len(doc%line(int(k, I4P))) == LENS(k), &
                    'line endings: line '//itoa(k)//' differs, case '//itoa(e))
         call check(doc%source_slice(doc%line_first(int(k, I4P)), doc%line_last(int(k, I4P))) == &
                    doc%line(int(k, I4P)), 'line endings: the span of line '//itoa(k)//' differs, case '//itoa(e))
      enddo
      call check(doc%source() == text, 'line endings: the source is not the text as it was, case '//itoa(e))
   enddo
   call doc%load('a'//CR//LF//'b'//CR//'c'//LF//'d')
   call check(doc%line_ending(1_I4P) == CR//LF .and. doc%line_ending(2_I4P) == CR .and. &
              doc%line_ending(3_I4P) == LF .and. doc%line_ending(4_I4P) == '' .and. &
              len(doc%line_ending(4_I4P)) == 0, 'line endings: the endings are not the ones of the source')
   endsubroutine test_line_endings

   subroutine test_lines_and_slices()
   !< The line count at the edges (CommonMark 2.1) and the byte ranges.
   type(markdown_document) :: doc !< Document.

   call doc%load('')
   call check(doc%lines_number() == 0_I4P .and. doc%source_length() == 0_I4P, 'lines: an empty text has a line')
   call doc%load(LF)
   call check(doc%lines_number() == 1_I4P .and. doc%line_last(1_I4P) == 0_I4P, 'lines: LF is not one empty line')
   call doc%load('a')
   call check(doc%lines_number() == 1_I4P .and. doc%line(1_I4P) == 'a', 'lines: "a" is not one line')
   call doc%load('a'//CR)
   call check(doc%lines_number() == 1_I4P .and. doc%line_ending(1_I4P) == CR, 'lines: "a CR" is not one line')
   call doc%load(CR//LF//CR//LF)
   call check(doc%lines_number() == 2_I4P .and. doc%line_first(2_I4P) == 3_I4P .and. &
              doc%line_last(2_I4P) == 2_I4P, 'lines: CRLF CRLF is not two empty lines')
   call doc%load(LF//CR)
   call check(doc%lines_number() == 2_I4P, 'lines: LF CR is not two lines')
   call doc%load('h'//char(195)//char(169)//'llo'//LF)
   call check(doc%line_last(1_I4P) == 6_I4P, 'lines: offsets are not bytes')
   call check(doc%source_slice(2_I4P, 3_I4P) == char(195)//char(169) .and. doc%source_slice(3_I4P, 2_I4P) == '', &
              'lines: source_slice')
   endsubroutine test_lines_and_slices

   subroutine test_sources()
   !< A `string`, an unallocated `string`, the tree reset by a load, initialize keeping the source, copy and destroy.
   type(markdown_document) :: doc  !< Document.
   type(markdown_document) :: copy !< Copy.
   type(string)            :: text !< Text.
   integer(I4P)            :: n    !< Node.
   integer(I4P)            :: stat !< Status.

   text = 'one'//CR//'two'//char(195)
   call doc%load(text, stat=stat)
   call check(stat == BARUCH_OK .and. doc%source() == 'one'//CR//'two'//FFFD .and. doc%lines_number() == 2_I4P, &
              'sources: a string')
   n = doc%new_node(NODE_PARAGRAPH)
   call doc%append_child(doc%root(), n)
   copy = doc
   call check(copy%source() == doc%source() .and. copy%line(2_I4P) == 'two'//FFFD, 'sources: the copy')
   call doc%initialize
   call check(doc%nodes_number() == 1_I4P .and. doc%lines_number() == 2_I4P, 'sources: initialize drops the source')
   call copy%load('x')
   call check(copy%nodes_number() == 1_I4P .and. copy%source() == 'x', 'sources: load does not reset the tree')
   call check(doc%source() == 'one'//CR//'two'//FFFD, 'sources: the copy shares the source')
   call doc%destroy
   call check(doc%source_length() == 0_I4P .and. doc%lines_number() == 0_I4P .and. doc%root() == 0_I4P, &
              'sources: destroy keeps the source')
   block
      type(string) :: empty !< Unallocated.
      call doc%load(empty, stat=stat)
      call check(stat == BARUCH_OK .and. doc%source_length() == 0_I4P .and. doc%root() == 1_I4P, &
                 'sources: an unallocated string')
   endblock
   endsubroutine test_sources

   subroutine test_errors()
   !< A missing file, an unsupported encoding, a line or a range that is not there: a status, the document as it was.
   type(markdown_document)   :: doc    !< Document.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.

   call doc%load('kept')
   call doc%load_file(DATA_DIR//'no_such_file.md', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. len(errmsg) > 0, 'errors: a missing file is not BARUCH_ERR_IO')
   call check(doc%source() == 'kept', 'errors: a missing file changed the document')
   call doc%load_file(DATA_DIR//'no_such_file.md') ! no stat: nothing happens
   call doc%load('x', encoding='EBCDIC', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. index(errmsg, 'EBCDIC') > 0, &
              'errors: an unsupported encoding is not BARUCH_ERR_ARGUMENT')
   call check(doc%source() == 'kept', 'errors: an unsupported encoding changed the document')
   call check(doc%line(2_I4P, stat=stat, errmsg=errmsg) == '' .and. stat == BARUCH_ERR_ARGUMENT .and. &
              len(errmsg) > 0, 'errors: line 2 of a one-line source')
   call check(doc%line_first(0_I4P, stat=stat) == 0_I4P .and. stat == BARUCH_ERR_ARGUMENT, 'errors: line_first(0)')
   call check(doc%line_last(-1_I4P, stat=stat) == 0_I4P .and. stat == BARUCH_ERR_ARGUMENT, 'errors: line_last(-1)')
   call check(doc%line_ending(9_I4P, stat=stat) == '' .and. stat == BARUCH_ERR_ARGUMENT, 'errors: line_ending(9)')
   call check(doc%source_slice(3_I4P, 5_I4P, stat=stat) == '' .and. stat == BARUCH_ERR_ARGUMENT, &
              'errors: source_slice beyond the end')
   call check(doc%source_slice(0_I4P, 1_I4P, stat=stat) == '' .and. stat == BARUCH_ERR_ARGUMENT, &
              'errors: source_slice from 0')
   call check(doc%source_slice(1_I4P, 4_I4P, stat=stat) == 'kept' .and. stat == BARUCH_OK, 'errors: a good slice')
   endsubroutine test_errors

   ! helpers
   function same_lines(a, b) result(same)
   !< True if two documents have the same lines (their endings aside).
   type(markdown_document), intent(in) :: a, b !< Documents.
   logical                             :: same !< Result.
   integer(I4P)                        :: i    !< Line.

   same = a%lines_number() == b%lines_number()
   if (.not. same) return
   do i=1_I4P, a%lines_number()
      same = a%line(i) == b%line(i) .and. len(a%line(i)) == len(b%line(i))
      if (.not. same) return
   enddo
   endfunction same_lines

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

   pure subroutine split_tabs(record, field)
   !< Split a record at its tabs into four fields (blank-padded to the longest).
   character(*),              intent(in)  :: record   !< Record.
   character(:), allocatable, intent(out) :: field(:) !< Fields.
   integer                                :: t(5)     !< Bounds.
   integer                                :: k        !< Counter.

   t(1) = 0
   do k=2, 4
      t(k) = index(record(t(k-1)+1:), char(9)) + t(k-1)
   enddo
   t(5) = len(record) + 1
   allocate(character(len(record)) :: field(4))
   do k=1, 4
      field(k) = record(t(k)+1:t(k+1)-1)
   enddo
   endsubroutine split_tabs

   pure function unhex(h) result(bytes)
   !< Bytes from lowercase hexadecimal.
   character(*), intent(in)  :: h     !< Hexadecimal.
   character(:), allocatable :: bytes !< Bytes.
   integer                   :: k     !< Counter.

   allocate(character(len(h) / 2) :: bytes)
   do k=1, len(h) / 2
      bytes(k:k) = achar(16 * (index(DIGITS, h(2*k-1:2*k-1)) - 1) + index(DIGITS, h(2*k:2*k)) - 1)
   enddo
   endfunction unhex

   pure function hex(bytes) result(h)
   !< Lowercase hexadecimal of bytes.
   character(*), intent(in)  :: bytes !< Bytes.
   character(:), allocatable :: h     !< Hexadecimal.
   integer                   :: k     !< Counter.
   integer                   :: b     !< Byte value.

   allocate(character(2 * len(bytes)) :: h)
   do k=1, len(bytes)
      b = iand(ichar(bytes(k:k)), 255)
      h(2*k-1:2*k) = DIGITS(b/16+1:b/16+1)//DIGITS(mod(b, 16)+1:mod(b, 16)+1)
   enddo
   endfunction hex

   pure function itoa(i) result(s)
   !< An integer as text.
   integer, intent(in)       :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(12)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction itoa

   subroutine check(condition, what)
   !< Count and report a failed check.
   logical,      intent(in) :: condition !< Must be true.
   character(*), intent(in) :: what      !< What is checked.

   if (condition) return
   failures = failures + 1
   print '(A)', 'FAIL '//what
   endsubroutine check
endprogram baruch_test_input
