!< BARUCH conformance driver: parse each Markdown fixture of a list, write the HTML rendered from it (or round-trip it).
program baruch_spec_driver
!< BARUCH conformance driver: parse each Markdown fixture of a list, write the HTML rendered from it (or round-trip it).
!<
!< Run by `scripts/spec.sh` (plan szaghi/BARUCH#1, step 1.4; round trip, step 3.2), never by hand:
!<
!<    baruch_spec_driver [--extension NAME]... [--roundtrip [--canonical]] --list FILE
!<
!< `FILE` holds one example per line, `INPUT<TAB>OUTPUT`: the driver loads `INPUT` (`markdown_document%load_file`),
!< parses it (`markdown_options%parse`, no extension enabled) and writes the rendered HTML to `OUTPUT` (stream access,
!< replaced), byte for byte. `--extension NAME` enables an extension for every example (none is built in before the
!< GFM steps: a name is an error until then). `scripts/spec/compare.py` then compares the outputs with the expected
!< HTML. An example that cannot be read, parsed or written is reported on standard error, gets no output file (so it
!< fails the comparison) and the driver goes on; the exit status is then 1. A misuse stops with status 2.
!<
!< The HTML is the library's: `markdown_options%render_html_file`, a fragment (as the spec's examples are), with the
!< options of the parse.
!<
!< With `--roundtrip` the driver writes no HTML: it parses the example, writes the tree back as Markdown
!< (`markdown_options%write_markdown`, the recorded style, or the canonical one with `--canonical`) into `OUTPUT`,
!< parses that Markdown and compares the two trees (`markdown_document%equal`). When they differ it writes the
!< difference (the path of the first differing node and what differs) into `OUTPUT.fail`, after removing a stale one;
!< `scripts/spec/compare.py --roundtrip` counts the examples with no `.fail` file.
!<
!< With no argument the driver does nothing and exits 0: it lives in `src/tests/spec/`, so the test modes build it into
!< `exe/` with every compiler, and `scripts/run_tests.sh`, which runs every program of `exe/` with no argument, counts
!< it as a test that builds and starts.
use, intrinsic :: iso_fortran_env, only : error_unit
use baruch
use penf,                          only : I4P
implicit none
character(:), allocatable :: arg       !< A command-line argument.
character(:), allocatable :: list_path !< Name of the list file.
character(:), allocatable :: list      !< The list file.
character(:), allocatable :: record    !< One line of it.
character(:), allocatable :: input     !< Markdown file of an example.
character(:), allocatable :: output    !< HTML file of an example.
character(:), allocatable :: errmsg    !< Message.
logical                   :: roundtrip !< Round-trip the examples (no HTML).
logical                   :: canonical !< Write them in the canonical style.
integer                   :: a         !< Argument counter.
integer                   :: i, j, t   !< Bounds of a record and of its tab.
integer                   :: examples  !< Examples run.
integer                   :: failures  !< Examples with an error.
integer(I4P)              :: stat      !< Status.

if (command_argument_count() == 0) stop
list_path = ''
roundtrip = .false.
canonical = .false.
a = 1
do while (a <= command_argument_count())
   call get_argument(a, arg)
   select case(arg)
   case('--list')
      if (a == command_argument_count()) call usage('--list needs a file')
      call get_argument(a + 1, list_path)
      a = a + 2
   case('--roundtrip')
      roundtrip = .true.
      a = a + 1
   case('--canonical')
      canonical = .true.
      a = a + 1
   case('--extension')
      if (a == command_argument_count()) call usage('--extension needs a name')
      call get_argument(a + 1, arg)
      call usage('unknown extension "'//arg//'": none is built in yet') ! the GFM and VitePress steps add theirs
   case default
      call usage('unknown argument "'//arg//'"')
   endselect
enddo
if (len(list_path) == 0) call usage('no --list given')
if (canonical .and. .not. roundtrip) call usage('--canonical needs --roundtrip')
call read_whole(list_path, list, stat, errmsg)
if (stat /= 0) call usage('cannot read the list "'//list_path//'": '//errmsg)

examples = 0
failures = 0
i = 1
do while (i <= len(list))
   j = index(list(i:), char(10)) + i - 1
   if (j < i) j = len(list) + 1
   record = list(i:j-1)
   i = j + 1
   if (len(record) == 0) cycle
   t = index(record, char(9))
   if (t < 2 .or. t == len(record)) then
      write(error_unit, '(A)') 'baruch_spec_driver: not "INPUT<TAB>OUTPUT": '//record
      failures = failures + 1
      cycle
   endif
   input = record(:t-1)
   output = record(t+1:)
   examples = examples + 1
   if (roundtrip) then
      call roundtrip_example(input, output, stat, errmsg)
   else
      call run_example(input, output, stat, errmsg)
   endif
   if (stat /= 0) then
      write(error_unit, '(A)') 'baruch_spec_driver: '//input//': '//errmsg
      failures = failures + 1
   endif
enddo
if (failures > 0) then
   write(error_unit, '(A,I0,A,I0,A)') 'baruch_spec_driver: ', failures, ' error(s) in ', examples, ' example(s)'
   stop 1
endif

contains
   subroutine run_example(input, output, stat, errmsg)
   !< Load, parse and render one example into its HTML file.
   character(*),              intent(in)  :: input   !< Markdown file.
   character(*),              intent(in)  :: output  !< HTML file.
   integer(I4P),              intent(out) :: stat    !< Status.
   character(:), allocatable, intent(out) :: errmsg  !< Message.
   type(markdown_document)                :: doc     !< Document.
   type(markdown_options)                 :: options !< Options: CommonMark, no extension.

   errmsg = ''
   call doc%load_file(input, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%parse(doc, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%render_html_file(doc, output, stat=stat, errmsg=errmsg)
   endsubroutine run_example

   subroutine roundtrip_example(input, output, stat, errmsg)
   !< Load and parse one example, write it as Markdown into its output file, parse that and compare the trees: a
   !< difference goes into `OUTPUT.fail` (a difference is a result, not an error: `stat` stays 0).
   character(*),              intent(in)  :: input      !< Markdown file.
   character(*),              intent(in)  :: output     !< Markdown written.
   integer(I4P),              intent(out) :: stat       !< Status.
   character(:), allocatable, intent(out) :: errmsg     !< Message.
   type(markdown_document)                :: doc        !< Document.
   type(markdown_document)                :: again      !< The written Markdown, parsed.
   type(markdown_options)                 :: options    !< Options: CommonMark, no extension.
   character(:), allocatable              :: markdown   !< Markdown written.
   character(:), allocatable              :: difference !< First difference.
   character(256)                         :: iomsg      !< I/O message.
   integer                                :: u          !< Unit.
   integer                                :: ios        !< I/O status.
   logical                                :: exists     !< A stale failure file exists.

   errmsg = ''
   call doc%load_file(input, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%parse(doc, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%write_markdown(doc, markdown, canonical=canonical, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%write_markdown_file(doc, output, canonical=canonical, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call again%load(markdown, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%parse(again, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   inquire(file=output//'.fail', exist=exists)
   if (exists) then
      open(newunit=u, file=output//'.fail', status='old', iostat=ios)
      if (ios == 0) close(u, status='delete')
   endif
   if (doc%equal(again, difference=difference)) return
   open(newunit=u, file=output//'.fail', access='stream', form='unformatted', action='write', status='replace', &
        iostat=ios, iomsg=iomsg)
   if (ios == 0) then
      write(u, iostat=ios, iomsg=iomsg) difference//char(10)
      close(u)
   endif
   if (ios /= 0) then
      stat = BARUCH_ERR_IO
      errmsg = 'cannot write "'//output//'.fail": '//trim(iomsg)
   endif
   endsubroutine roundtrip_example

   subroutine get_argument(n, arg)
   !< Return a command-line argument, of any length.
   integer,                   intent(in)  :: n   !< Argument number.
   character(:), allocatable, intent(out) :: arg !< Argument.
   integer                                :: l   !< Length.

   call get_command_argument(n, length=l)
   allocate(character(l) :: arg)
   if (l > 0) call get_command_argument(n, value=arg)
   endsubroutine get_argument

   subroutine read_whole(path, bytes, stat, errmsg)
   !< Read a file whole.
   character(*),              intent(in)  :: path   !< File.
   character(:), allocatable, intent(out) :: bytes  !< Contents.
   integer(I4P),              intent(out) :: stat   !< Status.
   character(:), allocatable, intent(out) :: errmsg !< Message.
   character(256)                         :: iomsg  !< I/O message.
   integer                                :: u      !< Unit.
   integer                                :: n      !< Size.
   integer                                :: ios    !< Status.

   bytes = ''
   errmsg = ''
   open(newunit=u, file=path, access='stream', form='unformatted', action='read', status='old', iostat=ios, &
        iomsg=iomsg)
   if (ios == 0) then
      inquire(unit=u, size=n)
      deallocate(bytes)
      allocate(character(max(n, 0)) :: bytes)
      if (n > 0) read(u, iostat=ios, iomsg=iomsg) bytes
      close(u)
   endif
   stat = int(ios, I4P)
   if (ios /= 0) errmsg = trim(iomsg)
   endsubroutine read_whole

   subroutine usage(message)
   !< Report a misuse and stop with status 2 (a plain STOP: no backtrace, the message says it all).
   character(*), intent(in) :: message !< What is wrong.

   write(error_unit, '(A)') 'baruch_spec_driver: '//message
   write(error_unit, '(A)') 'usage: baruch_spec_driver [--extension NAME]... [--roundtrip [--canonical]] --list FILE   '// &
                            '(FILE: one "INPUT<TAB>OUTPUT" per line)'
   stop 2
   endsubroutine usage
endprogram baruch_spec_driver
