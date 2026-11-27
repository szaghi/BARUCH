!< BARUCH test: the dependencies are built with the library and work under the compiler of the build mode.
program baruch_test_toolchain
!< BARUCH test: the dependencies are built with the library and work under the compiler of the build mode.
!<
!< StringiFor's Unicode White_Space (used by CommonMark's delimiter-run rules) is checked on U+00A0 NO-BREAK SPACE, a
!< two-byte UTF-8 sequence that ASCII-only whitespace does not match.
use baruch
use penf, only : I4P
use stringifor, only : string
implicit none
type(string) :: text

if (storage_size(1_I4P) /= 32) error stop 'I4P is not a 32-bit integer'
text = '# Heading'
if (.not. text%start_with(prefix='# ')) error stop 'string%start_with failed'
text = char(194)//char(160)
if (.not. text%is_space(unicode=.true.)) error stop 'U+00A0 is not Unicode White_Space'
if (text%is_space()) error stop 'U+00A0 is ASCII whitespace'
print '(A)', 'baruch_test_toolchain: ok'
endprogram baruch_test_toolchain
