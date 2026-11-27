program hello
!< Quick start: a Markdown text read into a document and written back as HTML.
use baruch
implicit none
type(markdown_document)   :: doc     ! the document: its source and its tree
type(markdown_options)    :: options ! what a parse does: here CommonMark alone
character(:), allocatable :: html    ! the HTML

call doc%load('# Hello, BARUCH'//new_line('a')//new_line('a')// &
              'A Markdown reader in pure Fortran:'//new_line('a')//new_line('a')// &
              '- blocks parsed'//new_line('a')//'- HTML written'//new_line('a'))
call options%parse(doc)
call options%render_html(doc, html)
write(*, '(A)', advance='no') html
endprogram hello
