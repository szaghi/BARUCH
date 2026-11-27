!< BARUCH, the input: any bytes to sanitised UTF-8, a file read whole, the line index.
module baruch_input
!< BARUCH, the input: any bytes to sanitised UTF-8, a file read whole, the line index.
!<
!< Every byte sequence is a document. [[sanitize]] turns bytes into the UTF-8 the document keeps:
!<
!< 1. the encoding is the one passed (any name or alias StringiFor's `canonical_encoding` knows), else the one a byte
!<    order mark names (UTF-8, UTF-16LE/BE, UTF-32LE/BE), else UTF-8; `UTF-16` and `UTF-32` without an endianness take
!<    it from a byte order mark, big endian without one (RFC 2781);
!< 2. a non-UTF-8 encoding is converted by StringiFor's `to_utf8`, then every ill-formed UTF-8 sequence becomes U+FFFD
!<    (StringiFor's `to_valid_utf8`, maximal subparts, as Python `bytes.decode('utf-8', 'replace')`);
!< 3. one leading U+FEFF, the byte order mark, is removed;
!< 4. every U+0000 becomes U+FFFD (CommonMark 2.3, "insecure characters").
!<
!< Line endings are kept as they are: the buffer is the caller's text, sanitised and nothing else, so a span is a range
!< of bytes of it. [[index_lines]] finds the lines (CommonMark 2.1: a line ends at LF, CR or CRLF, or at the end of the
!< input; the ending is not part of the line). These procedures are BARUCH's own plumbing: a client uses the
!< `markdown_document` methods built on them (`load`, `load_file`, `source`, `line_first`, ...).
use penf,        only : I4P, I8P
use stringifor,  only : bom_encoding, canonical_encoding, string, to_utf8
implicit none
private
public :: index_lines
public :: read_bytes
public :: sanitize
public :: REPLACEMENT

character(*), parameter :: REPLACEMENT = char(239)//char(191)//char(189) !< U+FFFD in UTF-8.
character(*), parameter :: BOM_UTF8    = char(239)//char(187)//char(191) !< U+FEFF in UTF-8, the byte order mark.
character(*), parameter :: LF          = char(10)                        !< Line feed.
character(*), parameter :: CR          = char(13)                        !< Carriage return.

contains
   pure subroutine sanitize(bytes, encoding, text, ierr, msg)
   !< Return the sanitised UTF-8 of `bytes` read in `encoding` (absent or blank: the byte order mark decides, else
   !< UTF-8). An encoding StringiFor does not support gives `ierr /= 0`, a message and an empty text.
   character(*),              intent(in)           :: bytes    !< Input bytes.
   character(*),              intent(in), optional :: encoding !< Encoding name, absent or blank for none.
   character(:), allocatable, intent(out)          :: text     !< Sanitised UTF-8.
   integer(I4P),              intent(out)          :: ierr     !< 0, or 1 for an unsupported encoding.
   character(:), allocatable, intent(out)          :: msg      !< Message, empty on success.
   character(:), allocatable                       :: name     !< Encoding the bytes are read in.
   character(:), allocatable                       :: utf8     !< Converted bytes.
   type(string)                                    :: buffer   !< Buffer.

   ierr = 0_I4P
   msg = ''
   name = ''
   if (present(encoding)) then
      if (len_trim(encoding) > 0) then
         name = canonical_encoding(encoding)
         if (len(name) == 0) then
            ierr = 1_I4P
            msg = 'unsupported encoding "'//trim(adjustl(encoding))//'" (supported: UTF-8, US-ASCII, ISO-8859-1, '// &
                  'windows-1252, UTF-16, UTF-16LE, UTF-16BE, UTF-32, UTF-32LE, UTF-32BE and their aliases)'
            text = ''
            return
         endif
      endif
   endif
   if (len(name) == 0) name = bom_encoding(bytes)
   select case (name)
   case ('')
      name = 'UTF-8'
   case ('UTF-16')
      name = 'UTF-16BE'
      if (len(bytes) >= 2) then
         if (bytes(1:2) == char(255)//char(254)) name = 'UTF-16LE'
      endif
   case ('UTF-32')
      name = 'UTF-32BE'
      if (len(bytes) >= 4) then
         if (bytes(1:4) == char(255)//char(254)//char(0)//char(0)) name = 'UTF-32LE'
      endif
   endselect
   if (name == 'UTF-8') then
      buffer%raw = bytes
   else
      call to_utf8(bytes=bytes, encoding=name, utf8=utf8)
      buffer%raw = utf8
      deallocate(utf8)
   endif
   buffer = buffer%to_valid_utf8()
   if (len(buffer%raw) >= 3) then
      if (buffer%raw(1:3) == BOM_UTF8) buffer%raw = buffer%raw(4:)
   endif
   if (index(buffer%raw, char(0)) > 0) buffer = buffer%replace(old=char(0), new=REPLACEMENT)
   text = buffer%raw
   endsubroutine sanitize

   pure subroutine index_lines(text, first, last)
   !< Return the first and the last byte of each line of `text`, the ending (LF, CR or CRLF) excluded: an empty line has
   !< `last = first - 1`. The input ending with a line ending has no empty line after it; an empty input has no line.
   character(*),              intent(in)  :: text     !< Text.
   integer(I4P), allocatable, intent(out) :: first(:) !< First byte of each line.
   integer(I4P), allocatable, intent(out) :: last(:)  !< Last byte of each line, ending excluded.
   integer(I4P)                           :: lines    !< Lines.
   integer(I4P)                           :: pass     !< 1: count, 2: fill.
   integer(I4P)                           :: i        !< Current byte.
   integer(I4P)                           :: j        !< Line ending.
   integer(I4P)                           :: length   !< Bytes of the text.

   length = len(text, kind=I4P)
   do pass=1_I4P, 2_I4P
      lines = 0_I4P
      i = 1_I4P
      do while (i <= length)
         j = scan(text(i:), CR//LF, kind=I4P)
         lines = lines + 1_I4P
         if (j == 0_I4P) then
            if (pass == 2_I4P) then
               first(lines) = i
               last(lines) = length
            endif
            exit
         endif
         j = i + j - 1_I4P
         if (pass == 2_I4P) then
            first(lines) = i
            last(lines) = j - 1_I4P
         endif
         i = j + 1_I4P
         if (text(j:j) == CR .and. i <= length) then
            if (text(i:i) == LF) i = i + 1_I4P
         endif
      enddo
      if (pass == 1_I4P) allocate(first(lines), last(lines))
   enddo
   endsubroutine index_lines

   subroutine read_bytes(path, bytes, ierr, msg)
   !< Read a whole file as bytes (stream access, a unit of `newunit=`). A file that cannot be opened or read, or that is
   !< larger than `huge(0_I4P)` bytes, gives `ierr /= 0` (the `iostat`, or -1), a message and no bytes.
   character(*),              intent(in)  :: path   !< File name.
   character(:), allocatable, intent(out) :: bytes  !< Contents.
   integer(I4P),              intent(out) :: ierr   !< 0 on success.
   character(:), allocatable, intent(out) :: msg    !< Message, empty on success.
   character(512)                         :: iomsg  !< Message of the processor.
   integer(I8P)                           :: size_  !< Bytes of the file.
   integer                                :: unit_  !< Unit.
   integer                                :: ios    !< I/O status.

   msg = ''
   bytes = ''
   iomsg = ''
   open(newunit=unit_, file=path, access='stream', form='unformatted', action='read', status='old', iostat=ios, &
        iomsg=iomsg)
   if (ios /= 0) then
      ierr = int(ios, I4P)
      msg = 'cannot open "'//path//'": '//trim(iomsg)
      return
   endif
   inquire(unit=unit_, size=size_)
   if (size_ < 0_I8P .or. size_ > int(huge(0_I4P), I8P)) then
      close(unit_)
      ierr = -1_I4P
      msg = 'cannot read "'//path//'": its size is unknown or above '//'2147483647 bytes'
      return
   endif
   deallocate(bytes)
   allocate(character(len=int(size_)) :: bytes)
   ios = 0
   if (size_ > 0_I8P) read(unit_, iostat=ios, iomsg=iomsg) bytes
   close(unit_)
   ierr = int(ios, I4P)
   if (ios /= 0) then
      msg = 'cannot read "'//path//'": '//trim(iomsg)
      bytes = ''
   endif
   endsubroutine read_bytes
endmodule baruch_input
