function mark_name(self) result(name)
class(mark_extension), intent(in) :: self !< Extension.
character(:), allocatable         :: name !< Name.

name = 'mark'
endfunction mark_name

subroutine mark_register_kinds(self, doc, stat, errmsg)
class(mark_extension),     intent(in)    :: self   !< Extension.
type(markdown_document),   intent(inout) :: doc    !< Document.
integer(I4P),              intent(inout) :: stat   !< Status.
character(:), allocatable, intent(inout) :: errmsg !< Message.

call doc%register_kind(NODE_MARK, 'mark', inline=.true., stat=stat, errmsg=errmsg)
endsubroutine mark_register_kinds

function mark_delimiter_chars(self) result(chars)
class(mark_extension), intent(in) :: self  !< Extension.
character(:), allocatable         :: chars !< Its delimiter characters.

chars = '='
endfunction mark_delimiter_chars

subroutine mark_delimiter_run(self, doc, text, first, length, left_flanking, right_flanking, before_punct, &
                              after_punct, can_open, can_close)
class(mark_extension),   intent(in)  :: self           !< Extension.
type(markdown_document), intent(in)  :: doc            !< Document.
character(*),            intent(in)  :: text           !< The block's inline content.
integer(I4P),            intent(in)  :: first          !< First byte of the run.
integer(I4P),            intent(in)  :: length         !< Its characters.
logical,                 intent(in)  :: left_flanking  !< Left-flanking (CommonMark 6.2).
logical,                 intent(in)  :: right_flanking !< Right-flanking.
logical,                 intent(in)  :: before_punct   !< Punctuation before it.
logical,                 intent(in)  :: after_punct    !< Punctuation after it.
logical,                 intent(out) :: can_open       !< It can open.
logical,                 intent(out) :: can_close      !< It can close.

can_open = left_flanking .and. length <= 2 ! `=` or `==`; `===` is text
can_close = right_flanking .and. length <= 2
endsubroutine mark_delimiter_run

subroutine mark_delimiter_match(self, doc, delimiter, opener_length, closer_length, opener_original, &
                                closer_original, opener_can_close, closer_can_open, used, node, stat, errmsg)
class(mark_extension),     intent(in)    :: self             !< Extension.
type(markdown_document),   intent(inout) :: doc              !< Document.
character,                 intent(in)    :: delimiter        !< The character, `=`.
integer(I4P),              intent(in)    :: opener_length    !< Characters left in the opener.
integer(I4P),              intent(in)    :: closer_length    !< Characters left in the closer.
integer(I4P),              intent(in)    :: opener_original  !< The opener as read.
integer(I4P),              intent(in)    :: closer_original  !< The closer as read.
logical,                   intent(in)    :: opener_can_close !< The opener can also close.
logical,                   intent(in)    :: closer_can_open  !< The closer can also open.
integer(I4P),              intent(out)   :: used             !< Characters taken from each, 0: no match.
integer(I4P),              intent(out)   :: node             !< The node holding what is between.
integer(I4P),              intent(inout) :: stat             !< Status.
character(:), allocatable, intent(inout) :: errmsg           !< Message.

used = 0
node = 0
if (opener_length /= closer_length) return ! `==a=` marks nothing
used = opener_length
node = doc%new_node(NODE_MARK, stat=stat, errmsg=errmsg)
endsubroutine mark_delimiter_match

subroutine mark_write_html(self, doc, node, entering, html, handled, stat, errmsg)
class(mark_extension),     intent(in)    :: self     !< Extension.
type(markdown_document),   intent(in)    :: doc      !< Document.
integer(I4P),              intent(in)    :: node     !< Node.
logical,                   intent(in)    :: entering !< Entering it, else leaving it.
character(:), allocatable, intent(out)   :: html     !< Its HTML.
logical,                   intent(out)   :: handled  !< The node is mine.
integer(I4P),              intent(inout) :: stat     !< Status.
character(:), allocatable, intent(inout) :: errmsg   !< Message.

html = ''
handled = doc%node_kind(node) == NODE_MARK
if (.not. handled) return
if (entering) then
   html = '<mark>'
else
   html = '</mark>'
endif
endsubroutine mark_write_html

subroutine mark_write_markdown(self, doc, node, entering, markdown, handled, stat, errmsg)
class(mark_extension),     intent(in)    :: self     !< Extension.
type(markdown_document),   intent(in)    :: doc      !< Document.
integer(I4P),              intent(in)    :: node     !< Node.
logical,                   intent(in)    :: entering !< Entering it, else leaving it.
character(:), allocatable, intent(out)   :: markdown !< Its Markdown.
logical,                   intent(out)   :: handled  !< The node is mine.
integer(I4P),              intent(inout) :: stat     !< Status.
character(:), allocatable, intent(inout) :: errmsg   !< Message.

markdown = ''
handled = doc%node_kind(node) == NODE_MARK
if (handled) markdown = '=='
endsubroutine mark_write_markdown
