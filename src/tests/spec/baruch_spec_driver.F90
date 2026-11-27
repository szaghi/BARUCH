!< BARUCH conformance driver: parse each Markdown fixture of a list, write the HTML rendered from it.
program baruch_spec_driver
!< BARUCH conformance driver: parse each Markdown fixture of a list, write the HTML rendered from it.
!<
!< Run by `scripts/spec.sh` (plan szaghi/BARUCH#1, step 1.4), never by hand:
!<
!<    baruch_spec_driver [--extension NAME]... --list FILE
!<
!< `FILE` holds one example per line, `INPUT<TAB>OUTPUT`: the driver loads `INPUT` (`markdown_document%load_file`),
!< parses it (`markdown_options%parse`, no extension enabled) and writes the rendered HTML to `OUTPUT` (stream access,
!< replaced), byte for byte. `--extension NAME` enables an extension for every example (none is built in before the
!< GFM steps: a name is an error until then). `scripts/spec/compare.py` then compares the outputs with the expected
!< HTML. An example that cannot be read, parsed or written is reported on standard error, gets no output file (so it
!< fails the comparison) and the driver goes on; the exit status is then 1. A misuse stops with status 2.
!<
!< The HTML comes from one procedure, `render_html`. The library's renderer comes with plan step 1.6; until then it
!< calls the test-only block serialiser of `baruch_spec_html` (blocks as cmark writes them, inline content as escaped
!< text), which step 1.6 replaces.
!<
!< With no argument the driver does nothing and exits 0: it lives in `src/tests/spec/`, so the test modes build it into
!< `exe/` with every compiler, and `scripts/run_tests.sh`, which runs every program of `exe/` with no argument, counts
!< it as a test that builds and starts.
use, intrinsic :: iso_fortran_env, only : error_unit
use baruch
use baruch_spec_html,              only : render_blocks_html
use penf,                          only : I4P
implicit none
character(:), allocatable :: arg       !< A command-line argument.
character(:), allocatable :: list_path !< Name of the list file.
character(:), allocatable :: list      !< The list file.
character(:), allocatable :: record    !< One line of it.
character(:), allocatable :: input     !< Markdown file of an example.
character(:), allocatable :: output    !< HTML file of an example.
character(:), allocatable :: errmsg    !< Message.
integer                   :: a         !< Argument counter.
integer                   :: i, j, t   !< Bounds of a record and of its tab.
integer                   :: examples  !< Examples run.
integer                   :: failures  !< Examples with an error.
integer(I4P)              :: stat      !< Status.

if (command_argument_count() == 0) stop
list_path = ''
a = 1
do while (a <= command_argument_count())
   call get_argument(a, arg)
   select case(arg)
   case('--list')
      if (a == command_argument_count()) call usage('--list needs a file')
      call get_argument(a + 1, list_path)
      a = a + 2
   case('--extension')
      if (a == command_argument_count()) call usage('--extension needs a name')
      call get_argument(a + 1, arg)
      call usage('unknown extension "'//arg//'": none is built in yet') ! the GFM and VitePress steps add theirs
   case default
      call usage('unknown argument "'//arg//'"')
   endselect
enddo
if (len(list_path) == 0) call usage('no --list given')
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
   call run_example(input, output, stat, errmsg)
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
   !< Load, parse and render one example; write its HTML.
   character(*),              intent(in)  :: input   !< Markdown file.
   character(*),              intent(in)  :: output  !< HTML file.
   integer(I4P),              intent(out) :: stat    !< Status.
   character(:), allocatable, intent(out) :: errmsg  !< Message.
   type(markdown_document)                :: doc     !< Document.
   type(markdown_options)                 :: options !< Options: CommonMark, no extension.
   character(:), allocatable              :: html    !< Rendered HTML.
   character(256)                         :: iomsg   !< I/O message.
   integer                                :: u       !< Unit.
   integer                                :: ios     !< I/O status.

   errmsg = ''
   call doc%load_file(input, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call options%parse(doc, stat=stat, errmsg=errmsg)
   if (stat /= BARUCH_OK) return
   call render_html(doc, html)
   open(newunit=u, file=output, access='stream', form='unformatted', action='write', status='replace', iostat=ios, &
        iomsg=iomsg)
   if (ios == 0) then
      write(u, iostat=ios, iomsg=iomsg) html
      close(u)
   endif
   if (ios /= 0) then
      stat = BARUCH_ERR_IO
      errmsg = 'cannot write '//output//': '//trim(iomsg)
   endif
   endsubroutine run_example

   subroutine render_html(doc, html)
   !< Render a parsed document as HTML: the one place the driver's output comes from. Until plan step 1.6 (the
   !< library's renderer), the test-only block serialiser.
   type(markdown_document),   intent(in)  :: doc  !< Document, parsed.
   character(:), allocatable, intent(out) :: html !< HTML.

   call render_blocks_html(doc, html)
   endsubroutine render_html

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
   write(error_unit, '(A)') 'usage: baruch_spec_driver [--extension NAME]... --list FILE   (FILE: one "INPUT<TAB>OUTPUT" '// &
                            'per line)'
   stop 2
   endsubroutine usage
endprogram baruch_spec_driver
