!< BARUCH test: the extension interface, its block hooks, the registry in the options, the hook contract.
program baruch_test_extension_contract
!< BARUCH test: the extension interface, its block hooks, the registry in the options, the hook contract.
!<
!< The gate of plan step 1.3: a test extension claiming the lines that start with `%%` puts its blocks in the tree with
!< its registered kind; a container extension holds blocks, refuses what `can_contain` refuses, and keeps a lazy
!< paragraph line; a block-start hook reads the open paragraph's lines and takes the last one; every way of breaking
!< the hook contract gives `stat = BARUCH_ERR_CONTRACT` and a message, and the parse ends (no loop); the options
!< register, enable, disable and copy extensions by name, and a kind clash between two extensions is a status.
!<
!< The gate of plan step 2.5: an inline trigger (`%name%`) gives nodes of its kind, inside emphasis, links and headings,
!< escapes keeping the core's meaning; a delimiter character (`=`) takes part in the delimiter algorithm, the extension
!< deciding per run and per match; a block of an extension whose content is inline is inline-parsed; `post_process`
!< runs after the inlines and the tree is compacted after it; every way of breaking the inline contract is a status.
!< The extensions are in `baruch_test_extensions`, written through the public interface only.
use baruch
use baruch_test_extensions
use penf, only : I4P
implicit none
character, parameter :: LF = achar(10) !< Line feed.
character, parameter :: BS = achar(92) !< Backslash (not a literal: nvfortran reads it as an escape).
integer              :: failures       !< Failed checks.

failures = 0
call test_percent
call test_disabled
call test_note
call test_caption
call test_contract
call test_options
call test_long
call test_inline_trigger
call test_delimiter
call test_inline_block
call test_inline_contract
call test_inline_long
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_extension_contract: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_extension_contract: ok'

contains
   subroutine test_percent()
   !< `%%` lines become one block of the registered kind, between two paragraphs; spans and literal exact.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: opts     !< Options.
   type(percent_extension)   :: percent  !< Extension.
   integer(I4P)              :: n(3)     !< Children of the root.
   integer(I4P)              :: stat     !< Status.
   character(:), allocatable :: errmsg   !< Message.
   character(:), allocatable :: markdown !< Markdown of the block.
   logical                   :: handled  !< Written by the extension.

   call opts%register(percent)
   call doc%load('Intro text'//LF//'%% one'//LF//'  %%two'//LF//'after'//LF)
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'percent: parse stat '//errmsg)
   call check(doc%children_number(doc%root()) == 3, 'percent: 3 blocks under the root')
   n(1) = doc%first_child(doc%root())
   n(2) = doc%next_sibling(n(1))
   n(3) = doc%next_sibling(n(2))
   call check(doc%node_kind(n(1)) == NODE_PARAGRAPH, 'percent: a paragraph first')
   call check(doc%node_kind(n(2)) == KIND_PERCENT, 'percent: the block has the registered kind')
   call check(doc%kind_name(doc%node_kind(n(2))) == 'percent_block', 'percent: the kind name')
   call check(doc%node_kind(n(3)) == NODE_PARAGRAPH, 'percent: a paragraph last')
   call check(doc%literal(n(2)) == ' one'//LF//'two', 'percent: literal "'//doc%literal(n(2))//'"')
   call check(doc%source_slice(doc%span_first(n(2)), doc%span_last(n(2))) == '%% one'//LF//'  %%two', &
              'percent: span "'//doc%source_slice(doc%span_first(n(2)), doc%span_last(n(2)))//'"')
   call check(doc%literal(doc%first_child(n(1))) == 'Intro text', 'percent: first paragraph text')
   call check(doc%literal(doc%first_child(n(3))) == 'after', 'percent: last paragraph text')
   call check(doc%span_first(doc%root()) == 1 .and. doc%span_last(doc%root()) == doc%source_length(), 'percent: root span')
   call percent%write_markdown(doc, n(2), .true., markdown, handled, stat, errmsg)
   call check(handled .and. markdown == '%% one'//LF//'%%two'//LF, 'percent: written back "'//markdown//'"')
   call percent%write_markdown(doc, n(1), .true., markdown, handled, stat, errmsg)
   call check(.not. handled, 'percent: a paragraph is not its to write')
   ! the same options, parsed again: the same tree (no state kept between parses)
   call opts%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%nodes_number() == 6, 'percent: second parse, 6 nodes')
   endsubroutine test_percent

   subroutine test_disabled()
   !< A disabled extension's syntax is paragraph text; an empty options object parses paragraphs only.
   type(markdown_document) :: doc     !< Document.
   type(markdown_options)  :: opts    !< Options.
   type(markdown_options)  :: none    !< No extension.
   type(percent_extension) :: percent !< Extension.
   integer(I4P)            :: stat    !< Status.

   call opts%register(percent)
   call opts%disable('percent', stat=stat)
   call check(stat == BARUCH_OK .and. .not. opts%is_enabled('percent'), 'disabled: disable')
   call doc%load('Intro text'//LF//'%% one'//LF//'after')
   call opts%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%children_number(doc%root()) == 1, 'disabled: one paragraph')
   call check(doc%children_number(doc%first_child(doc%root())) == 5, &
              'disabled: the paragraph holds the three lines, two soft breaks between them')
   call check(doc%literal(doc%last_child(doc%first_child(doc%root()))) == 'after', 'disabled: the third line')
   call check(.not. doc%is_known_kind(KIND_PERCENT), 'disabled: its kind is not registered')
   call doc%load('a'//LF//'b'//LF//LF//'  c  '//LF//LF)
   call none%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%children_number(doc%root()) == 2, 'none: two paragraphs')
   call check(doc%literal(doc%first_child(doc%last_child(doc%root()))) == 'c', 'none: white space stripped')
   call doc%load('')
   call none%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%nodes_number() == 1, 'none: empty source, the root alone')
   endsubroutine test_disabled

   subroutine test_note()
   !< A container: its children, a lazy paragraph line, a child it refuses.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: opts    !< Options.
   integer(I4P)              :: note    !< Note.
   integer(I4P)              :: c       !< Child.
   integer(I4P)              :: stat    !< Status.
   character(:), allocatable :: errmsg  !< Message.

   call opts%register(percent_extension())
   call opts%register(note_extension())
   call doc%load('!> %% a'//LF//'!>%% b'//LF//'!> text'//LF//'lazy'//LF//LF//'out')
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'note: parse stat '//errmsg)
   call check(doc%children_number(doc%root()) == 2, 'note: a note and a paragraph')
   note = doc%first_child(doc%root())
   call check(doc%node_kind(note) == KIND_NOTE .and. doc%children_number(note) == 2, 'note: two children')
   c = doc%first_child(note)
   call check(doc%node_kind(c) == KIND_PERCENT .and. doc%literal(c) == ' a'//LF//' b', 'note: the percent block')
   c = doc%next_sibling(c)
   call check(doc%node_kind(c) == NODE_PARAGRAPH, 'note: a paragraph in it')
   call check(doc%children_number(c) == 3 .and. doc%literal(doc%last_child(c)) == 'lazy', &
              'note: the lazy line joins the paragraph')
   call check(doc%source_slice(doc%span_first(note), doc%span_last(note)) == &
              '!> %% a'//LF//'!>%% b'//LF//'!> text'//LF//'lazy', 'note: its span covers the lazy line')
   ! a note cannot hold a note: the outer one closes, the inner one opens in the root
   call doc%load('!> !> x')
   call opts%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%children_number(doc%root()) == 2, 'note: two notes side by side')
   call check(doc%children_number(doc%first_child(doc%root())) == 0, 'note: the first one is empty')
   call check(doc%node_kind(doc%first_child(doc%last_child(doc%root()))) == NODE_PARAGRAPH, &
              'note: the second holds the paragraph')
   endsubroutine test_note

   subroutine test_caption()
   !< The block-start hook reads the open paragraph's lines and takes the last one.
   type(markdown_document)   :: doc    !< Document.
   type(markdown_options)    :: opts   !< Options.
   integer(I4P)              :: n      !< Node.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.

   call opts%register(caption_extension())
   call doc%load('first line'//LF//'  second line'//LF//'^^^'//LF//'next')
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'caption: parse stat '//errmsg)
   call check(doc%children_number(doc%root()) == 3, 'caption: paragraph, caption, paragraph')
   n = doc%first_child(doc%root())
   call check(doc%literal(doc%first_child(n)) == 'first line', 'caption: the paragraph keeps its first line')
   call check(doc%source_slice(doc%span_first(n), doc%span_last(n)) == 'first line', 'caption: paragraph span')
   n = doc%next_sibling(n)
   call check(doc%node_kind(n) == KIND_CAPTION .and. doc%literal(n) == 'second line', 'caption: the caption')
   ! a paragraph left with no line goes
   call doc%load('only'//LF//'^^^')
   call opts%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%children_number(doc%root()) == 1, 'caption: the caption alone')
   call check(doc%node_kind(doc%first_child(doc%root())) == KIND_CAPTION .and. doc%nodes_number() == 2, &
              'caption: the paragraph removed')
   ! no paragraph open: not a caption
   call doc%load('^^^')
   call opts%parse(doc, stat=stat)
   call check(doc%node_kind(doc%first_child(doc%root())) == NODE_PARAGRAPH, 'caption: needs a paragraph')
   endsubroutine test_caption

   subroutine test_contract()
   !< Every way of breaking the contract is a status and a message, and the parse ends.
   type(markdown_document)   :: doc    !< Document.
   type(markdown_options)    :: opts   !< Options.
   integer(I4P)              :: mode   !< Way of breaking it.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.
   character(*), parameter   :: input = 'para'//LF//'!! x'//LF//'!! y'//LF !< Input.

   do mode=1, 6
      block
         type(markdown_options) :: fresh !< A fresh options object for each mode.

         call fresh%register(broken_extension(mode=mode))
         call doc%load(input)
         call fresh%parse(doc, stat=stat, errmsg=errmsg)
         call check(stat == BARUCH_ERR_CONTRACT, 'contract mode '//char(48 + mode)//': stat')
         call check(index(errmsg, 'broke the hook contract') > 0 .and. index(errmsg, '"broken"') > 0, &
                    'contract mode '//char(48 + mode)//': message "'//errmsg//'"')
         print '(A)', 'contract mode '//char(48 + mode)//': '//errmsg
      endblock
   enddo
   ! without stat: the documented default, the parse ends all the same
   call opts%register(broken_extension(mode=1_I4P))
   call opts%parse(doc)
   call check(doc%is_node(doc%root()), 'contract: no stat, the document is still a tree')
   endsubroutine test_contract

   subroutine test_options()
   !< Register, names, enable, disable, copy; a kind clash between two extensions.
   type(markdown_options)    :: opts   !< Options.
   type(markdown_options)    :: copy   !< Its copy.
   type(markdown_document)   :: doc    !< Document.
   integer(I4P)              :: k      !< Counter.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.
   character(:), allocatable :: name   !< An extension's name.

   call check(opts%extensions_number() == 0, 'options: empty')
   call opts%register(percent_extension())
   call opts%register(note_extension(), enabled=.false.)
   call opts%register(caption_extension())
   call opts%register(percent_extension(), stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT, 'options: a name registered twice')
   call check(opts%extensions_number() == 3, 'options: 3 registered')
   call check(opts%extension_name(2) == 'note', 'options: the second is "note"')
   call check(opts%is_registered('note') .and. .not. opts%is_enabled('note'), 'options: note registered, disabled')
   call check(.not. opts%is_registered('not') .and. .not. opts%is_registered('notes'), 'options: exact names')
   call opts%enable('note')
   call check(opts%is_enabled('note'), 'options: note enabled')
   call opts%enable('nothing', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. len(errmsg) > 0, 'options: enable an unknown name')
   name = opts%extension_name(9, stat=stat)
   call check(name == '' .and. stat == BARUCH_ERR_ARGUMENT, 'options: no extension 9')
   ! more than the initial slots: the registry grows
   do k=1, 9
      call opts%register(broken_extension(mode=k), enabled=.false., stat=stat)
   enddo
   call check(opts%extensions_number() == 4, 'options: one "broken" only (names are unique)')
   ! a copy is independent
   copy = opts
   call copy%disable('percent')
   call check(opts%is_enabled('percent') .and. .not. copy%is_enabled('percent'), 'options: the copy is independent')
   call doc%load('%% x')
   call copy%parse(doc, stat=stat)
   call check(doc%node_kind(doc%first_child(doc%root())) == NODE_PARAGRAPH, 'options: the copy parses without it')
   call opts%parse(doc, stat=stat)
   call check(doc%node_kind(doc%first_child(doc%root())) == KIND_PERCENT, 'options: the original with it')
   ! two extensions with one kind number
   call opts%register(clash_extension())
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. index(errmsg, '"clash"') > 0, 'options: kind clash "'//errmsg//'"')
   call opts%register_kinds(doc, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'options: register_kinds reports the clash too')
   endsubroutine test_options

   subroutine test_long()
   !< A long block and many blocks: linear, exact.
   type(markdown_document)   :: doc    !< Document.
   type(markdown_options)    :: opts   !< Options.
   character(:), allocatable :: text   !< Source.
   integer(I4P)              :: k      !< Counter.
   integer(I4P)              :: stat   !< Status.

   call opts%register(percent_extension())
   call opts%register(note_extension())
   ! 2000 blocks: a percent line, a paragraph line, a blank line
   text = ''
   do k=1, 2000
      text = text//'%%'//LF//'p'//LF//LF
   enddo
   call doc%load(text)
   call opts%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%children_number(doc%root()) == 4000, 'long: 4000 blocks')
   ! 2000 nested notes cannot nest (can_contain): 2000 notes side by side, the last with a paragraph
   text = repeat('!> ', 2000)//'deep'
   call doc%load(text)
   call opts%parse(doc, stat=stat)
   call check(stat == BARUCH_OK .and. doc%children_number(doc%root()) == 2000, 'long: 2000 notes')
   endsubroutine test_long

   subroutine test_inline_trigger()
   !< `%name%` nodes: kind, literal, span, where they can appear; what is not one stays text; post-process.
   type(markdown_document)   :: doc    !< Document.
   type(markdown_options)    :: opts   !< Options.
   integer(I4P)              :: p      !< Paragraph.
   integer(I4P)              :: n      !< Node.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.
   character(:), allocatable :: html   !< HTML.

   call opts%register(xvar_extension())
   call doc%load('Hi %who%, %a b% and 100%.'//LF)
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'xvar: parse stat '//errmsg)
   p = doc%first_child(doc%root())
   call check(doc%children_number(p) == 3, 'xvar: text, xvar, text')
   n = doc%first_child(p)
   call check(doc%node_kind(n) == NODE_TEXT .and. doc%literal(n) == 'Hi ', 'xvar: text before')
   n = doc%next_sibling(n)
   call check(doc%node_kind(n) == KIND_XVAR .and. doc%kind_name(KIND_XVAR) == 'xvar', 'xvar: its kind')
   call check(doc%literal(n) == 'who', 'xvar: literal "'//doc%literal(n)//'"')
   call check(doc%source_slice(doc%span_first(n), doc%span_last(n)) == '%who%', 'xvar: span')
   call check(doc%attribute(n, 'index') == '1', 'xvar: numbered by post_process')
   n = doc%next_sibling(n)
   call check(doc%literal(n) == ', %a b% and 100%.', 'xvar: not mine is text, merged "'//doc%literal(n)//'"')
   call opts%render_html(doc, html)
   call check(html == '<p>Hi <var>who</var>, %a b% and 100%.</p>'//LF, 'xvar: html "'//html//'"')
   ! escaped, in emphasis, in a link, in a heading, across lines, numbered in document order, dropped by post_process
   call doc%load('# T %h%'//LF//LF//BS//'%e% *%x%* [%y%](u) %drop% a'//LF//'  %z%'//LF)
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'xvar: second parse stat '//errmsg)
   call opts%render_html(doc, html)
   call check(html == '<h1>T <var>h</var></h1>'//LF//'<p>%e% <em><var>x</var></em> <a href="u"><var>y</var></a>  a'// &
              LF//'<var>z</var></p>'//LF, 'xvar: html "'//html//'"')
   n = doc%last_child(doc%last_child(doc%root()))
   call check(doc%node_kind(n) == KIND_XVAR .and. doc%attribute(n, 'index') == '4', 'xvar: the last is the fourth')
   call check(doc%source_slice(doc%span_first(n), doc%span_last(n)) == '%z%', 'xvar: span on the second line')
   call check(doc%nodes_number() == doc%last_node(), 'xvar: compacted after post_process')
   ! disabled: text
   call opts%disable('xvar')
   call opts%parse(doc, stat=stat)
   call opts%render_html(doc, html)
   call check(index(html, '<var>') == 0 .and. index(html, '%drop%') > 0, 'xvar: disabled, text')
   endsubroutine test_inline_trigger

   subroutine test_delimiter()
   !< `=` runs through the delimiter algorithm: the extension decides per run and per match.
   type(markdown_document)   :: doc    !< Document.
   type(markdown_options)    :: opts   !< Options.
   integer(I4P)              :: p      !< Paragraph.
   integer(I4P)              :: n      !< Node.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.
   character(:), allocatable :: html   !< HTML.
   integer(I4P)              :: k      !< Case.
   character(*), parameter   :: cases(2, 9) = reshape([character(48) ::                                    &
      '==hi== and =one=',                 '<p><mark>hi</mark> and <mark>one</mark></p>',               &
      '===no=== x',                       '<p>===no=== x</p>',                                         &
      '==a= b',                           '<p>==a= b</p>',                                             &
      '==*a*==',                          '<p><mark><em>a</em></mark></p>',                            &
      '*==a*==',                          '<p><em>==a</em>==</p>',                                     &
      '[==a==](u) =b',                    '<p><a href="u"><mark>a</mark></a> =b</p>',                  &
      'a == b == c',                      '<p>a == b == c</p>',                                        &
      BS//'==a==',                        '<p>==a==</p>',                                              &
      '=a ==b== c=',                      '<p><mark>a <mark>b</mark> c</mark></p>'], [2, 9])

   call opts%register(mark_extension())
   call doc%load('x ==hi== y'//LF)
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'mark: parse stat '//errmsg)
   p = doc%first_child(doc%root())
   n = doc%next_sibling(doc%first_child(p))
   call check(doc%node_kind(n) == KIND_MARK .and. doc%kind_name(KIND_MARK) == 'mark', 'mark: its kind')
   call check(doc%source_slice(doc%span_first(n), doc%span_last(n)) == '==hi==', 'mark: span')
   call check(doc%children_number(n) == 1 .and. doc%literal(doc%first_child(n)) == 'hi', 'mark: its content')
   call check(doc%children_number(p) == 3, 'mark: text, mark, text')
   do k=1, size(cases, 2)
      call doc%load(trim(cases(1, k)))
      call opts%parse(doc, stat=stat, errmsg=errmsg)
      call opts%render_html(doc, html)
      call check(stat == BARUCH_OK .and. html == trim(cases(2, k))//LF, &
                 'mark: "'//trim(cases(1, k))//'" gives "'//html//'"')
   enddo
   ! two extensions declaring one delimiter
   call opts%register(broken_inline_extension(mode=8_I4P))
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. index(errmsg, '"mark" and "broken_inline"') > 0, &
              'mark: a delimiter declared twice "'//errmsg//'"')
   endsubroutine test_delimiter

   subroutine test_inline_block()
   !< A block of an extension whose content is inline: read as a paragraph, the other extensions' inlines included.
   type(markdown_document)   :: doc    !< Document.
   type(markdown_options)    :: opts   !< Options.
   integer(I4P)              :: t      !< Title.
   integer(I4P)              :: n      !< Node.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.

   call opts%register(title_extension())
   call opts%register(xvar_extension())
   call doc%load('para'//LF//'@@ a *b* %c%'//LF)
   call opts%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'title: parse stat '//errmsg)
   t = doc%last_child(doc%root())
   call check(doc%node_kind(t) == KIND_TITLE .and. doc%children_number(t) == 4, 'title: four inline children')
   n = doc%first_child(t)
   call check(doc%literal(n) == 'a ', 'title: text')
   n = doc%next_sibling(n)
   call check(doc%node_kind(n) == NODE_EMPH, 'title: emphasis')
   call check(doc%source_slice(doc%span_first(n), doc%span_last(n)) == '*b*', 'title: emphasis span')
   n = doc%last_child(t)
   call check(doc%node_kind(n) == KIND_XVAR .and. doc%literal(n) == 'c', 'title: an xvar')
   call check(doc%source_slice(doc%span_first(n), doc%span_last(n)) == '%c%', 'title: xvar span')
   endsubroutine test_inline_block

   subroutine test_inline_contract()
   !< Every way of breaking the inline contract is a status and a message, and the parse ends; a failing hook too.
   type(markdown_document)   :: doc    !< Document.
   integer(I4P)              :: mode   !< Way of breaking it.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.

   do mode=1, 12
      block
         type(markdown_options) :: fresh !< A fresh options object for each mode.

         call fresh%register(broken_inline_extension(mode=mode))
         call doc%load('a %b% ==c== d'//LF)
         call fresh%parse(doc, stat=stat, errmsg=errmsg)
         if (mode <= 10) then
            call check(stat == BARUCH_ERR_CONTRACT, 'inline contract mode '//itoa(mode)//': stat')
            call check(index(errmsg, 'broke the hook contract') > 0 .and. index(errmsg, '"broken_inline"') > 0, &
                       'inline contract mode '//itoa(mode)//': message "'//errmsg//'"')
         else
            call check(stat == BARUCH_ERR_ARGUMENT .and. index(errmsg, 'failed: asked to fail') > 0, &
                       'inline contract mode '//itoa(mode)//': "'//errmsg//'"')
         endif
         call check(doc%is_node(doc%root()), 'inline contract mode '//itoa(mode)//': still a tree')
         print '(A)', 'inline contract mode '//itoa(mode)//': '//errmsg
      endblock
   enddo
   endsubroutine test_inline_contract

   subroutine test_inline_long()
   !< Many inline nodes of both kinds in one paragraph: exact counts.
   type(markdown_document)   :: doc  !< Document.
   type(markdown_options)    :: opts !< Options.
   character(:), allocatable :: text !< Source.
   integer(I4P)              :: k    !< Counter.
   integer(I4P)              :: n    !< Node.
   integer(I4P)              :: xv   !< Xvars.
   integer(I4P)              :: mk   !< Marks.
   integer(I4P)              :: stat !< Status.

   call opts%register(xvar_extension())
   call opts%register(mark_extension())
   text = ''
   do k=1, 5000
      text = text//'%v% ==m== = '
   enddo
   call doc%load(text)
   call opts%parse(doc, stat=stat)
   xv = 0
   mk = 0
   n = doc%first_child(doc%first_child(doc%root()))
   do while (n /= 0)
      if (doc%node_kind(n) == KIND_XVAR) xv = xv + 1
      if (doc%node_kind(n) == KIND_MARK) mk = mk + 1
      n = doc%next_sibling(n)
   enddo
   call check(stat == BARUCH_OK .and. xv == 5000 .and. mk == 5000, 'long: 5000 xvars and 5000 marks')
   endsubroutine test_inline_long

   pure function itoa(i) result(s)
   !< An integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(12)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction itoa

   subroutine check(condition, what)
   !< Count a failed check and say which.
   logical,      intent(in) :: condition !< Condition.
   character(*), intent(in) :: what      !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL: '//what
   endif
   endsubroutine check
endprogram baruch_test_extension_contract
