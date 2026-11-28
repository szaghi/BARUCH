!< BARUCH, the inline scanners: character references, backslash escapes, autolinks, raw HTML (CommonMark 0.31.2, 6).
module baruch_inlines_scan
!< BARUCH, the inline scanners: character references, backslash escapes, autolinks, raw HTML (CommonMark 0.31.2, 6).
!<
!< Pure procedures on a text and a 1-based position in it, used by the inline parser (`baruch_inlines`) and by the
!< block parser for the strings it stores (`unescape_string`: info strings, destinations and titles of link reference
!< definitions). Nothing reads past `len(text)`, nothing keeps state.
!<
!< - **Character references** (spec 2.5, 6.2): `&name;` for the 2125 HTML5 names ending in `;` (`baruch_entities`),
!<   `&#` and 1 to 7 decimal digits, `&#x` (or `&#X`) and 1 to 6 hexadecimal digits, then `;`. A numeric reference to
!<   U+0000, to a surrogate or beyond U+10FFFF stands for U+FFFD. `scan_reference` gives the UTF-8 bytes.
!< - **Backslash escapes** (spec 2.4): a backslash before an ASCII punctuation character stands for that character.
!<   `unescape_string` processes both in one pass, left to right (a reference's bytes are never read again, so
!<   `\&amp;` is `&amp;` and `&#92;*` is `\*`, as commonmark.js and the spec read them).
!< - **Autolinks** (spec 6.5): `scan_autolink_uri` (a scheme of 2 to 32 characters, `:`, no white space, `<` or `>`)
!<   and `scan_autolink_email` (the spec's email address), each `<...>`.
!< - **Raw HTML** (spec 6.6): open and closing tags (`baruch_blocks_scan`), comments, processing instructions,
!<   declarations, CDATA sections. The four whose end is a fixed string take the position to search from and answer
!<   whether the end exists at all from there: the inline parser remembers a failed search, so that a subject full of
!<   `<!--` is read in linear time (cmark's flags).
use penf,               only : I4P
use stringifor,         only : string
use baruch_blocks_scan, only : BACKSLASH, is_ascii_punctuation, is_ascii_letter, is_ascii_digit, scan_open_tag, &
                               scan_closing_tag
use baruch_entities,    only : entity_lookup, entity_utf8, ENTITY_NAME_MAX
implicit none
private
public :: REPLACEMENT_UTF8
public :: is_ascii_alnum, is_ascii_space, classify, code_content
public :: utf8_encode, scan_reference, unescape_string, decode_references
public :: scan_autolink_uri, scan_autolink_email
public :: scan_html_comment, scan_processing_instruction, scan_declaration, scan_cdata, scan_html_tag

character(*), parameter :: REPLACEMENT_UTF8 = char(239)//char(191)//char(189) !< U+FFFD in UTF-8.
character(*), parameter :: EMAIL_LOCAL = & !< The characters of an email address's local part, letters and digits aside.
   ".!#$%&'*+/=?^_`{|}~-"
integer(I4P), parameter :: DECIMAL_DIGITS_MAX = 7_I4P !< Digits of a decimal reference.
integer(I4P), parameter :: HEX_DIGITS_MAX = 6_I4P     !< Digits of a hexadecimal reference.
integer(I4P), parameter :: SCHEME_MIN = 2_I4P         !< Characters of an autolink's scheme, at least.
integer(I4P), parameter :: SCHEME_MAX = 32_I4P        !< And at most.
integer(I4P), parameter :: DOMAIN_LABEL_MAX = 63_I4P  !< Characters of an email domain's label, at most.

contains
   ! characters
   elemental function is_ascii_alnum(c) result(is)
   !< True for an ASCII letter or digit.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = is_ascii_letter(c) .or. is_ascii_digit(c)
   endfunction is_ascii_alnum

   elemental function is_ascii_space(c) result(is)
   !< True for an ASCII white space byte: space, tab, line feed, line tabulation, form feed, carriage return.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = c == ' ' .or. (iachar(c) >= 9 .and. iachar(c) <= 13)
   endfunction is_ascii_space

   subroutine classify(text, i, space, punct)
   !< Whether the UTF-8 character at `i` is Unicode White_Space and whether it is Unicode punctuation (General_Category
   !< P or S), by StringiFor's `is_space(unicode=.true.)` and `is_punct(unicode=.true.)`; no character (`i` 0, the
   !< start or the end of the subject) counts as white space.
   character(*), intent(in)  :: text  !< Text, well-formed UTF-8.
   integer(I4P), intent(in)  :: i     !< First byte of the character, 0 for none.
   logical,      intent(out) :: space !< White space.
   logical,      intent(out) :: punct !< Punctuation.
   type(string)              :: u     !< The character.
   integer(I4P)              :: b     !< Its lead byte.
   integer(I4P)              :: n     !< Its bytes.

   space = .true.
   punct = .false.
   if (i < 1_I4P) return
   b = iachar(text(i:i))
   if (b < 128_I4P) then
      space = text(i:i) == ' ' .or. (b >= 9_I4P .and. b <= 13_I4P)
      punct = is_ascii_punctuation(text(i:i))
      return
   elseif (b >= 240_I4P) then
      n = 4_I4P
   elseif (b >= 224_I4P) then
      n = 3_I4P
   else
      n = 2_I4P
   endif
   u = text(i:min(len(text, kind=I4P), i + n - 1_I4P))
   space = u%is_space(unicode=.true.)
   punct = u%is_punct(unicode=.true.)
   endsubroutine classify

   pure function code_content(raw) result(content)
   !< Return a code span's content: line endings as spaces, then one space removed at both ends when there is one at
   !< both ends and the content is not only spaces.
   character(*), intent(in)  :: raw     !< Bytes between the backtick strings.
   character(:), allocatable :: content !< Content.
   integer(I4P)              :: k       !< Position.
   integer(I4P)              :: n       !< Length.

   content = raw
   n = len(content, kind=I4P)
   do k=1, n
      if (content(k:k) == achar(10)) content(k:k) = ' '
   enddo
   if (n >= 2_I4P) then
      if (content(1:1) == ' ' .and. content(n:n) == ' ' .and. verify(content, ' ') > 0) content = content(2:n-1)
   endif
   endfunction code_content

   pure function utf8_encode(code) result(bytes)
   !< Return the UTF-8 bytes of a code point; U+FFFD for one that is not a Unicode scalar value (negative, a
   !< surrogate, beyond U+10FFFF). The bytes are written one by one into a fixed buffer: nvfortran 26.9 gives a
   !< deferred-length result assigned `achar(i)` (or `char(i)`, or a concatenation of them) the length `i`
   !< (szaghi/BARUCH#2, reproducer notes/nvfortran-achar-deferred-length.f90).
   integer(I4P), intent(in)  :: code  !< Code point.
   character(:), allocatable :: bytes !< UTF-8.
   character(4)              :: buf   !< The bytes.
   integer(I4P)              :: n     !< Bytes used.

   if (code < 0_I4P .or. (code >= 55296_I4P .and. code <= 57343_I4P) .or. code > 1114111_I4P) then
      buf(1:3) = REPLACEMENT_UTF8
      n = 3_I4P
   elseif (code < 128_I4P) then
      buf(1:1) = achar(code)
      n = 1_I4P
   elseif (code < 2048_I4P) then
      buf(1:1) = char(192 + code / 64)
      buf(2:2) = char(128 + mod(code, 64_I4P))
      n = 2_I4P
   elseif (code < 65536_I4P) then
      buf(1:1) = char(224 + code / 4096)
      buf(2:2) = char(128 + mod(code / 64, 64_I4P))
      buf(3:3) = char(128 + mod(code, 64_I4P))
      n = 3_I4P
   else
      buf(1:1) = char(240 + code / 262144)
      buf(2:2) = char(128 + mod(code / 4096, 64_I4P))
      buf(3:3) = char(128 + mod(code / 64, 64_I4P))
      buf(4:4) = char(128 + mod(code, 64_I4P))
      n = 4_I4P
   endif
   bytes = buf(1:n)
   endfunction utf8_encode

   ! character references and backslash escapes
   pure subroutine scan_reference(text, i, bytes, length)
   !< A character reference at `text(i:)` (`text(i:i)` is `&`): its UTF-8 `bytes` and its `length` in `text`, 0 (and
   !< no bytes) for none.
   character(*),              intent(in)  :: text   !< Text.
   integer(I4P),              intent(in)  :: i      !< Position of `&`.
   character(:), allocatable, intent(out) :: bytes  !< UTF-8 of the reference.
   integer(I4P),              intent(out) :: length !< Bytes of the reference, 0 for none.
   integer(I4P)                           :: n      !< Length of the text.
   integer(I4P)                           :: k      !< Position.
   integer(I4P)                           :: first  !< First digit.
   integer(I4P)                           :: code   !< Code point.
   integer(I4P)                           :: digit  !< Value of a digit.
   integer(I4P)                           :: most   !< Digits allowed.
   integer(I4P)                           :: base   !< 10 or 16.
   integer(I4P)                           :: entity !< Index of a name.

   length = 0_I4P
   bytes = ''
   n = len(text, kind=I4P)
   if (i + 2_I4P > n) return ! the shortest reference has three bytes after `&`... `&#1;`, `&lt;`: four in all
   if (text(i+1:i+1) == '#') then
      k = i + 2_I4P
      base = 10_I4P
      most = DECIMAL_DIGITS_MAX
      if (text(k:k) == 'x' .or. text(k:k) == 'X') then
         base = 16_I4P
         most = HEX_DIGITS_MAX
         k = k + 1_I4P
      endif
      first = k
      code = 0_I4P
      do while (k <= n .and. k - first < most + 1_I4P)
         digit = digit_value(text(k:k), base)
         if (digit < 0_I4P) exit
         if (k - first < most) code = code * base + digit
         k = k + 1_I4P
      enddo
      if (k == first .or. k - first > most .or. k > n) return
      if (text(k:k) /= ';') return
      if (code == 0_I4P) code = 65533_I4P
      bytes = utf8_encode(code)
      length = k - i + 1_I4P
   else
      k = i + 1_I4P
      do while (k <= n .and. k - i <= ENTITY_NAME_MAX)
         if (.not. is_ascii_alnum(text(k:k))) exit
         k = k + 1_I4P
      enddo
      if (k == i + 1_I4P .or. k > n) return
      if (text(k:k) /= ';') return
      entity = entity_lookup(text(i+1:k))
      if (entity == 0_I4P) return
      bytes = entity_utf8(entity)
      length = k - i + 1_I4P
   endif
   endsubroutine scan_reference

   pure function unescape_string(s) result(u)
   !< Return `s` with its backslash escapes and its character references replaced by what they stand for, in one pass
   !< (a backslash not before ASCII punctuation, an `&` not starting a reference, stay).
   character(*), intent(in)  :: s !< Text.
   character(:), allocatable :: u !< Processed text.

   u = unescape(s, .true.)
   endfunction unescape_string

   pure function decode_references(s) result(u)
   !< Return `s` with its character references replaced by what they stand for; backslashes stay (an autolink has no
   !< escapes, spec 6.5).
   character(*), intent(in)  :: s !< Text.
   character(:), allocatable :: u !< Processed text.

   u = unescape(s, .false.)
   endfunction decode_references

   ! autolinks
   pure function scan_autolink_uri(text) result(length)
   !< A URI autolink at the start of `text` (`<`, a scheme, `:`, then no ASCII control, space, `<` or `>`, then `>`).
   !< Return its bytes, `<` and `>` included, 0 for none.
   character(*), intent(in) :: text   !< Text, starting at `<`.
   integer(I4P)             :: length !< Bytes of the autolink.
   integer(I4P)             :: n      !< Length of the text.
   integer(I4P)             :: k      !< Position.
   integer                  :: c      !< Byte value.

   length = 0_I4P
   n = len(text, kind=I4P)
   if (n < 4_I4P) return
   if (text(1:1) /= '<' .or. .not. is_ascii_letter(text(2:2))) return
   k = 3_I4P
   do while (k <= n .and. k - 2_I4P < SCHEME_MAX)
      if (.not. (is_ascii_alnum(text(k:k)) .or. text(k:k) == '+' .or. text(k:k) == '.' .or. text(k:k) == '-')) exit
      k = k + 1_I4P
   enddo
   if (k - 2_I4P < SCHEME_MIN .or. k > n) return
   if (text(k:k) /= ':') return
   k = k + 1_I4P
   do while (k <= n)
      c = iachar(text(k:k))
      if (c <= 32 .or. c == 127 .or. text(k:k) == '<' .or. text(k:k) == '>') exit
      k = k + 1_I4P
   enddo
   if (k > n) return
   if (text(k:k) /= '>') return
   length = k
   endfunction scan_autolink_uri

   pure function scan_autolink_email(text) result(length)
   !< An email autolink at the start of `text`: `<`, one or more of letters, digits and ``.!#$%&'*+/=?^_`{|}~-``, `@`,
   !< labels of 1 to 63 letters, digits and `-` (not first, not last) separated by `.`, then `>`. Return its bytes, `<`
   !< and `>` included, 0 for none.
   character(*), intent(in) :: text   !< Text, starting at `<`.
   integer(I4P)             :: length !< Bytes of the autolink.
   integer(I4P)             :: n      !< Length of the text.
   integer(I4P)             :: k      !< Position.
   integer(I4P)             :: first  !< First byte of a label.

   length = 0_I4P
   n = len(text, kind=I4P)
   if (n < 4_I4P .or. text(1:1) /= '<') return
   k = 2_I4P
   do while (k <= n)
      if (.not. (is_ascii_alnum(text(k:k)) .or. index(EMAIL_LOCAL, text(k:k)) > 0)) exit
      k = k + 1_I4P
   enddo
   if (k == 2_I4P .or. k > n) return
   if (text(k:k) /= '@') return
   do ! a label, then `.` and another, or `>`
      k = k + 1_I4P
      first = k
      do while (k <= n)
         if (.not. (is_ascii_alnum(text(k:k)) .or. text(k:k) == '-')) exit
         k = k + 1_I4P
      enddo
      if (k == first .or. k - first > DOMAIN_LABEL_MAX .or. k > n) return
      if (text(first:first) == '-' .or. text(k-1:k-1) == '-') return
      if (text(k:k) == '>') exit
      if (text(k:k) /= '.') return
   enddo
   length = k
   endfunction scan_autolink_email

   ! raw HTML
   pure function scan_html_tag(text) result(length)
   !< An open or a closing tag at the start of `text` (spec 6.6). Return its bytes, 0 for none.
   character(*), intent(in) :: text   !< Text, starting at `<`.
   integer(I4P)             :: length !< Bytes of the tag.

   length = scan_open_tag(text)
   if (length == 0_I4P) length = scan_closing_tag(text)
   endfunction scan_html_tag

   pure subroutine scan_html_comment(text, length, no_end)
   !< An HTML comment at the start of `text` (spec 6.6, 0.31.2): `<!-->`, `<!--->`, or `<!--`, text not holding `-->`,
   !< `-->`. `length` is its bytes, 0 for none; `no_end` is true when `text` holds no `-->` after `<!--` (no comment
   !< can end in it, nor in any text that is a suffix of it).
   character(*), intent(in)  :: text   !< Text, starting at `<!--`.
   integer(I4P), intent(out) :: length !< Bytes of the comment.
   logical,      intent(out) :: no_end !< No `-->` in the text.
   integer(I4P)              :: e      !< Position of `-->` after the opener.

   length = 0_I4P
   no_end = .false.
   if (len(text) < 5) then
      no_end = .true.
      return
   endif
   if (text(1:5) == '<!-->') then
      length = 5_I4P
   elseif (len(text) >= 6) then
      if (text(1:6) == '<!--->') length = 6_I4P
   endif
   if (length > 0_I4P) return
   e = index(text(5:), '-->', kind=I4P)
   if (e == 0_I4P) then
      no_end = .true.
   else
      length = e + 6_I4P
   endif
   endsubroutine scan_html_comment

   pure subroutine scan_processing_instruction(text, length, no_end)
   !< A processing instruction at the start of `text`: `<?`, text not holding `?>`, `?>`. `length` is its bytes, 0 for
   !< none; `no_end` is true when `text` holds no `?>` after `<?`.
   character(*), intent(in)  :: text   !< Text, starting at `<?`.
   integer(I4P), intent(out) :: length !< Bytes of the instruction.
   logical,      intent(out) :: no_end !< No `?>` in the text.

   call scan_until(text, 3_I4P, '?>', length, no_end)
   endsubroutine scan_processing_instruction

   pure subroutine scan_declaration(text, length, no_end)
   !< A declaration at the start of `text`: `<!`, an ASCII letter, text not holding `>`, `>`. `length` is its bytes, 0
   !< for none; `no_end` is true when `text` holds no `>` after `<!`.
   character(*), intent(in)  :: text   !< Text, starting at `<!` and a letter.
   integer(I4P), intent(out) :: length !< Bytes of the declaration.
   logical,      intent(out) :: no_end !< No `>` in the text.

   call scan_until(text, 3_I4P, '>', length, no_end)
   endsubroutine scan_declaration

   pure subroutine scan_cdata(text, length, no_end)
   !< A CDATA section at the start of `text`: `<![CDATA[`, text not holding `]]>`, `]]>`. `length` is its bytes, 0 for
   !< none; `no_end` is true when `text` holds no `]]>` after the opener.
   character(*), intent(in)  :: text   !< Text, starting at `<![CDATA[`.
   integer(I4P), intent(out) :: length !< Bytes of the section.
   logical,      intent(out) :: no_end !< No `]]>` in the text.

   call scan_until(text, 10_I4P, ']]>', length, no_end)
   endsubroutine scan_cdata

   ! private
   pure function unescape(s, escapes) result(u)
   !< Return `s` with its character references and, if `escapes`, its backslash escapes replaced, in one pass.
   character(*), intent(in)  :: s       !< Text.
   logical,      intent(in)  :: escapes !< Process the backslash escapes too.
   character(:), allocatable :: u       !< Processed text.
   character(:), allocatable :: bytes   !< A reference's bytes.
   integer(I4P)              :: i       !< Position in `s`.
   integer(I4P)              :: j       !< Bytes of `u` written.
   integer(I4P)              :: length  !< Bytes of a reference.

   if (scan(s, BACKSLASH//'&') == 0) then
      u = s
      return
   endif
   allocate(character(2 * len(s)) :: u) ! a reference's bytes are fewer than twice its own
   i = 1_I4P
   j = 0_I4P
   do while (i <= len(s, kind=I4P))
      if (s(i:i) == BACKSLASH .and. escapes .and. i < len(s, kind=I4P)) then
         if (is_ascii_punctuation(s(i+1:i+1))) i = i + 1_I4P
      elseif (s(i:i) == '&') then
         call scan_reference(s, i, bytes, length)
         if (length > 0_I4P) then
            u(j+1:j+len(bytes)) = bytes
            j = j + len(bytes, kind=I4P)
            i = i + length
            cycle
         endif
      endif
      u(j+1:j+1) = s(i:i)
      j = j + 1_I4P
      i = i + 1_I4P
   enddo
   u = u(1:j)
   endfunction unescape

   pure subroutine scan_until(text, from, closer, length, no_end)
   !< The bytes of `text` up to the first `closer` at or after `from`, the closer included; 0 and `no_end` if none.
   character(*), intent(in)  :: text   !< Text.
   integer(I4P), intent(in)  :: from   !< First position searched.
   character(*), intent(in)  :: closer !< End string.
   integer(I4P), intent(out) :: length !< Bytes up to the closer's end, 0 for none.
   logical,      intent(out) :: no_end !< No closer.
   integer(I4P)              :: e      !< Position of the closer from `from`.

   length = 0_I4P
   no_end = .true.
   if (from > len(text, kind=I4P)) return
   e = index(text(from:), closer, kind=I4P)
   if (e == 0_I4P) return
   no_end = .false.
   length = from + e - 2_I4P + len(closer, kind=I4P)
   endsubroutine scan_until

   elemental function digit_value(c, base) result(v)
   !< The value of a decimal (`base` 10) or hexadecimal (16) digit, -1 for any other byte.
   character,    intent(in) :: c    !< Character.
   integer(I4P), intent(in) :: base !< 10 or 16.
   integer(I4P)             :: v    !< Value.

   v = -1_I4P
   if (is_ascii_digit(c)) then
      v = iachar(c) - iachar('0')
   elseif (base == 16_I4P) then
      if (c >= 'a' .and. c <= 'f') v = iachar(c) - iachar('a') + 10
      if (c >= 'A' .and. c <= 'F') v = iachar(c) - iachar('A') + 10
   endif
   endfunction digit_value
endmodule baruch_inlines_scan
