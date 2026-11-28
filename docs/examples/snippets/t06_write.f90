program t06_write
!< Tutorial chapter 6: edit a parsed document and write it back as Markdown, in its own style and in the canonical
!< one, then read what was written back to the same tree.
use baruch
implicit none
character, parameter      :: LF = achar(10)  !< Line feed.
type(markdown_document)   :: doc        !< The document.
type(markdown_document)   :: again      !< What was written, read back.
type(markdown_options)    :: options    !< CommonMark, no extension.
character(:), allocatable :: source     !< The Markdown read.
character(:), allocatable :: markdown   !< The Markdown written.
character(:), allocatable :: difference !< Where two trees differ.
integer                   :: heading    !< The heading.
integer                   :: list       !< The list.
integer                   :: item       !< A new item.
integer                   :: para       !< Its paragraph.
integer                   :: text       !< Its text.

source = 'Solver notes'//LF//                            &
         '============'//LF//                            &
         LF//                                            &
         'Set the _time step_ in [the input][input]:'//LF// &
         LF//                                            &
         '* `dt = 1e-3`'//LF//                           &
         '* `cfl = 0.8`'//LF//                           &
         LF//                                            &
         '~~~ ini'//LF//                                 &
         '[solver]'//LF//                                &
         'dt = 1e-3'//LF//                               &
         '~~~'//LF//                                     &
         LF//                                            &
         '[input]: https://example.org/solver.ini'//LF
call doc%load(source)
call options%parse(doc)
call options%write_markdown(doc, markdown)
print '(A,L1)', 'written back byte for byte: ', markdown == source

heading = doc%first_child(doc%root())
call doc%set_literal(doc%first_child(heading), 'Solver notes, revised')
list = doc%next_sibling(doc%next_sibling(heading))
item = doc%new_node(NODE_ITEM)
para = doc%new_node(NODE_PARAGRAPH)
text = doc%new_node(NODE_TEXT)
call doc%set_literal(text, '*not* emphasis, [not] a link, 3 < 4')
call doc%append_child(para, text)
call doc%append_child(item, para)
call doc%append_child(list, item)

call options%write_markdown(doc, markdown)
write(*, '(A)', advance='no') markdown

call options%write_markdown(doc, markdown, canonical=.true.)
print '(A)', '----- canonical'
write(*, '(A)', advance='no') markdown

call again%load(markdown)
call options%parse(again)
print '(A,L1)', 'reads back to the same tree: ', doc%equal(again, difference=difference)
endprogram t06_write
