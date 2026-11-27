!< BARUCH test: the extension interface, its block hooks, the registry in the options, the hook contract.
program baruch_test_extension_contract
!< BARUCH test: the extension interface, its block hooks, the registry in the options, the hook contract.
!<
!< The gate of plan step 1.3: a test extension claiming the lines that start with `%%` puts its blocks in the tree with
!< its registered kind; a container extension holds blocks, refuses what `can_contain` refuses, and keeps a lazy
!< paragraph line; a block-start hook reads the open paragraph's lines and takes the last one; every way of breaking
!< the hook contract gives `stat = BARUCH_ERR_CONTRACT` and a message, and the parse ends (no loop); the options
!< register, enable, disable and copy extensions by name, and a kind clash between two extensions is a status. The
!< extensions are in `baruch_test_extensions`, written through the public interface only.
use baruch
use baruch_test_extensions
use penf, only : I4P
implicit none
character, parameter :: LF = achar(10) !< Line feed.
integer              :: failures       !< Failed checks.

failures = 0
call test_percent
call test_disabled
call test_note
call test_caption
call test_contract
call test_options
call test_long
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
   call check(doc%literal(doc%first_child(doc%first_child(doc%root()))) == 'Intro text'//LF//'%% one'//LF//'after', &
              'disabled: the paragraph holds the three lines')
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
   call check(doc%literal(doc%first_child(c)) == 'text'//LF//'lazy', 'note: the lazy line joins the paragraph')
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
   call check(opts%extension_name(9, stat=stat) == '' .and. stat == BARUCH_ERR_ARGUMENT, 'options: no extension 9')
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
