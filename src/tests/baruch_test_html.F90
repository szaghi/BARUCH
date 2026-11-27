!< BARUCH test: the HTML renderer, its output exactly, its sinks, the extensions' hooks, deep nesting.
program baruch_test_html
!< BARUCH test: the HTML renderer, its output exactly, its sinks, the extensions' hooks, deep nesting.
!<
!< The gate of plan step 1.6 beside the conformance suite (`scripts/spec.sh commonmark`, which renders through the same
!< renderer): every block kind written as cmark writes it, every inline kind of a tree built by hand (escaping,
!< `escape_href`, an image's alt text), the standalone document and its title, the four sinks (`character`, `string`,
!< a unit formatted or unformatted stream, a file) giving the same bytes, their errors as statuses, the extensions'
!< `write_html` hooks (an extension's container, a core kind written by an extension, an extension node with no hook,
!< a hook failing, a disabled extension), and a 10^5-deep nesting of block quotes and of lists rendered with no
!< recursion (the test runs under `ulimit -s 8192`).
use baruch
use baruch_test_extensions, only : percent_extension, note_extension, math_extension, rule_extension, KIND_MATH, &
                                   KIND_NOTE, KIND_PERCENT
use penf,                   only : I4P
use stringifor,             only : string
implicit none
character, parameter :: LF = achar(10) !< Line feed.
integer              :: failures       !< Failed checks.

failures = 0
call test_blocks
call test_inlines
call test_standalone
call test_sinks
call test_extension_defaults
call test_deep
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_html: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_html: ok'

contains
   function blocks_source() result(source)
   !< A document of every block kind, and of the test extensions' blocks.
   character(:), allocatable :: source !< Markdown.

   source = '# Title & 2 > 1'//LF//LF//'a < b & "c"'//LF//'second line'//LF//LF//'- one'//LF//'- two'//LF//LF// &
            '3. three'//LF//LF//'4. four'//LF//LF//'```fortran x'//LF//'x = 1 < 2'//LF//'```'//LF//LF//       &
            '```language-c'//LF//'int'//LF//'```'//LF//LF//'<div>'//LF//'raw'//LF//'</div>'//LF//LF//'***'//LF// &
            LF//'> quoted'//LF//LF//'[ref]: /url'//LF//LF//'!> noted'//LF//LF//'%% kept & safe'//LF
   endfunction blocks_source

   function blocks_html(extensions) result(html)
   !< The HTML of `blocks_source`, as cmark writes it (checked with markdown-it-py 3.0.0, CommonMark mode), with or
   !< without the note, percent and rule extensions.
   logical, intent(in)       :: extensions !< The extensions enabled.
   character(:), allocatable :: html       !< HTML.

   html = '<h1>Title &amp; 2 &gt; 1</h1>'//LF//'<p>a &lt; b &amp; &quot;c&quot;'//LF//'second line</p>'//LF// &
          '<ul>'//LF//'<li>one</li>'//LF//'<li>two</li>'//LF//'</ul>'//LF//'<ol start="3">'//LF//'<li>'//LF//  &
          '<p>three</p>'//LF//'</li>'//LF//'<li>'//LF//'<p>four</p>'//LF//'</li>'//LF//'</ol>'//LF//            &
          '<pre><code class="language-fortran">x = 1 &lt; 2'//LF//'</code></pre>'//LF//                           &
          '<pre><code class="language-language-c">int'//LF//'</code></pre>'//LF//'<div>'//LF//'raw'//LF//'</div>'//LF
   if (extensions) then
      html = html//'<hr class="rule" />'//LF//'<blockquote>'//LF//'<p>quoted</p>'//LF//'</blockquote>'//LF// &
             '<aside class="note">'//LF//'<p>noted</p>'//LF//'</aside>'//LF//' kept &amp; safe'//LF
   else
      html = html//'<hr />'//LF//'<blockquote>'//LF//'<p>quoted</p>'//LF//'</blockquote>'//LF// &
             '<p>!&gt; noted</p>'//LF//'<p>%% kept &amp; safe</p>'//LF
   endif
   endfunction blocks_html

   subroutine test_blocks()
   !< Every block kind, CommonMark alone and with extensions: an extension's container written by its hook, a core kind
   !< written by an extension, an extension leaf with no hook written as its escaped literal; a disabled extension.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: core    !< No extension.
   type(markdown_options)    :: options !< The note, percent and rule extensions.
   type(note_extension)      :: note    !< Extension.
   type(percent_extension)   :: percent !< Extension.
   type(rule_extension)      :: rule    !< Extension.
   character(:), allocatable :: html    !< HTML.
   integer(I4P)              :: stat    !< Status.
   character(:), allocatable :: errmsg  !< Message.

   call doc%load(blocks_source())
   call core%parse(doc)
   call core%render_html(doc, html, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'blocks: stat '//errmsg)
   call check(html == blocks_html(.false.), 'blocks: CommonMark'//LF//html)
   call options%register(note)
   call options%register(percent)
   call options%register(rule)
   call options%parse(doc)
   call options%render_html(doc, html)
   call check(html == blocks_html(.true.), 'blocks: with extensions'//LF//html)
   call options%disable('rule')
   call options%render_html(doc, html)
   call check(index(html, '<hr />'//LF) > 0 .and. index(html, 'rule') == 0, 'blocks: a disabled extension writes nothing')
   endsubroutine test_blocks

   subroutine test_inlines()
   !< Every inline kind, built by hand: escaping, destinations, titles, an image's alt text, an inline extension node
   !< with its hook, with none, and with a hook failing.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< The math extension.
   type(markdown_options)    :: core    !< No extension.
   type(markdown_options)    :: failing !< The math extension failing.
   type(math_extension)      :: math    !< Extension.
   character(:), allocatable :: html    !< HTML.
   character(:), allocatable :: expected !< Expected HTML, before the math.
   integer(I4P)              :: stat    !< Status.
   character(:), allocatable :: errmsg  !< Message.
   integer(I4P)              :: p, n, s !< Nodes.

   call options%register(math)
   call doc%initialize()
   call options%register_kinds(doc)
   p = add(doc, doc%root(), NODE_PARAGRAPH)
   n = add(doc, p, NODE_TEXT, 'a&')
   n = add(doc, p, NODE_EMPH)
   n = add(doc, n, NODE_TEXT, 'b')
   s = add(doc, p, NODE_STRONG)
   s = add(doc, s, NODE_LINK)
   call doc%set_destination(s, '/u?x=1&y=[2] '//achar(195)//achar(169))
   call doc%set_title(s, 'T "q"')
   n = add(doc, s, NODE_CODE, '<c>')
   n = add(doc, p, NODE_SOFTBREAK)
   n = add(doc, p, NODE_LINEBREAK)
   n = add(doc, p, NODE_HTML_INLINE, '<i>')
   s = add(doc, p, NODE_IMAGE)
   call doc%set_destination(s, '/i')
   call doc%set_title(s, 't')
   n = add(doc, s, NODE_TEXT, 'd')
   n = add(doc, s, NODE_EMPH)
   n = add(doc, n, NODE_TEXT, 'e')
   n = add(doc, s, NODE_SOFTBREAK)
   n = add(doc, s, NODE_CODE, 'f')
   n = add(doc, p, KIND_MATH, 'x<2')
   expected = '<p>a&amp;<em>b</em><strong><a href="/u?x=1&amp;y=%5B2%5D%20%C3%A9" title="T &quot;q&quot;">'// &
              '<code>&lt;c&gt;</code></a></strong>'//LF//'<br />'//LF//'<i><img src="/i" alt="de f" title="t" />'
   call options%render_html(doc, html)
   call check(html == expected//'<span class="math">x<2</span></p>'//LF, 'inlines: with the math hook'//LF//html)
   call core%render_html(doc, html)
   call check(html == expected//'x&lt;2</p>'//LF, 'inlines: an extension node with no hook, its literal'//LF//html)
   math%mode = 2_I4P
   call failing%register(math)
   call failing%render_html(doc, html, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. index(errmsg, 'extension "math": math: asked to fail') > 0, &
              'inlines: a hook failing gives its status "'//errmsg//'"')
   call check(html == expected, 'inlines: what was rendered before the failure is kept')
   endsubroutine test_inlines

   subroutine test_standalone()
   !< A standalone document: its title given, from the first heading, or empty.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: html    !< HTML.
   character(:), allocatable :: head    !< Expected head, before the title.
   character(:), allocatable :: body    !< Expected body, from the closing title tag.

   head = '<!DOCTYPE html>'//LF//'<html>'//LF//'<head>'//LF//'<meta charset="utf-8">'//LF//'<title>'
   body = '</title>'//LF//'</head>'//LF//'<body>'//LF//'<p>text</p>'//LF//'<h2>A &lt; b &amp;'//LF//'c</h2>'//LF// &
          '</body>'//LF//'</html>'//LF
   call doc%load('text'//LF//LF//'A < b &amp;'//LF//'c'//LF//'---'//LF)
   call options%parse(doc)
   call options%render_html(doc, html, standalone=.true.)
   call check(html == head//'A &lt; b &amp; c'//body, 'standalone: titled by the first heading'//LF//html)
   call options%render_html(doc, html, standalone=.true., title='x & y')
   call check(html == head//'x &amp; y'//body, 'standalone: the title given'//LF//html)
   call options%render_html(doc, html, standalone=.false., title='ignored')
   call check(html == '<p>text</p>'//LF//'<h2>A &lt; b &amp;'//LF//'c</h2>'//LF, 'standalone: a fragment'//LF//html)
   call doc%load('no heading'//LF)
   call options%parse(doc)
   call options%render_html(doc, html, standalone=.true.)
   call check(index(html, '<title></title>') > 0, 'standalone: no heading, an empty title')
   endsubroutine test_standalone

   subroutine test_sinks()
   !< The same bytes into a `character`, a `string`, a file, a formatted unit and an unformatted stream unit; the
   !< sinks' errors.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_document)   :: empty   !< Uninitialised document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: html    !< HTML, the reference.
   character(:), allocatable :: back    !< HTML read back.
   type(string)              :: text    !< HTML as a string.
   integer(I4P)              :: stat    !< Status.
   character(:), allocatable :: errmsg  !< Message.
   integer                   :: u       !< Unit.
   character(*), parameter   :: TMP = 'baruch_test_html.tmp' !< Scratch file, in the working directory.

   call doc%load(blocks_source())
   call options%parse(doc)
   call options%render_html(doc, html)
   call options%render_html(doc, text, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK .and. text%chars() == html, 'sinks: string')
   call options%render_html_file(doc, TMP, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK .and. read_back(TMP) == html, 'sinks: file')
   open(newunit=u, file=TMP, form='formatted', action='write', status='replace')
   call options%render_html(doc, u, stat=stat, errmsg=errmsg)
   close(u)
   call check(stat == BARUCH_OK .and. read_back(TMP) == html, 'sinks: formatted unit "'//errmsg//'"')
   open(newunit=u, file=TMP, access='stream', form='unformatted', action='write', status='replace')
   call options%render_html(doc, u, standalone=.true., stat=stat, errmsg=errmsg)
   close(u)
   call options%render_html(doc, html, standalone=.true.)
   call check(stat == BARUCH_OK .and. read_back(TMP) == html, 'sinks: unformatted stream unit')
   ! errors
   open(newunit=u, file=TMP, action='read', status='old')
   call options%render_html(doc, u, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. index(errmsg, 'reading') > 0, 'sinks: a unit open for reading "'//errmsg//'"')
   close(u)
   call options%render_html(doc, u, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. index(errmsg, 'not open') > 0, 'sinks: a unit not open "'//errmsg//'"')
   open(newunit=u, file=TMP, form='unformatted', access='sequential', action='write', status='replace')
   call options%render_html(doc, u, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. index(errmsg, 'not stream') > 0, &
              'sinks: an unformatted sequential unit "'//errmsg//'"')
   close(u, status='delete')
   call options%render_html_file(doc, 'no/such/directory/page.html', stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_IO .and. len(errmsg) > 0, 'sinks: a file that cannot be written "'//errmsg//'"')
   call options%render_html(empty, back, stat=stat)
   call check(stat == BARUCH_OK .and. len(back) == 0, 'sinks: an empty document, no HTML')
   endsubroutine test_sinks

   subroutine test_extension_defaults()
   !< Extension nodes no extension writes: a container rendered transparently, a leaf as its escaped literal.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< No extension.
   character(:), allocatable :: html    !< HTML.
   integer(I4P)              :: n, p    !< Nodes.

   call doc%initialize()
   call doc%register_kind(KIND_NOTE, 'note', inline=.false.)
   call doc%register_kind(KIND_PERCENT, 'percent_block', inline=.false.)
   n = add(doc, doc%root(), KIND_NOTE)
   p = add(doc, n, NODE_PARAGRAPH)
   p = add(doc, p, NODE_TEXT, 'x')
   n = add(doc, doc%root(), KIND_PERCENT, 'a<b')
   p = add(doc, doc%root(), NODE_THEMATIC_BREAK)
   call options%render_html(doc, html)
   call check(html == '<p>x</p>'//LF//'a&lt;b'//LF//'<hr />'//LF, 'defaults: transparent container, literal leaf'// &
              LF//html)
   endsubroutine test_extension_defaults

   subroutine test_deep()
   !< 10^5 nested block quotes, then 10^5 nested lists, rendered whole with no recursion.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: source  !< Markdown.
   character(:), allocatable :: html    !< HTML.
   integer                   :: depth   !< Nesting.

   ! a variable, not a named constant: nvfortran 26.9 fails (internal error) on a repeat() of a long constant length
   ! folded in a contained procedure (reproducer notes/nvfortran-ilm-long-constant-string.f90)
   depth = 100000
   source = repeat('>', depth)//' deep'//LF
   call doc%load(source)
   call options%parse(doc)
   call options%render_html(doc, html)
   print '(A,I0,A,I0,A)', 'deep render: ', depth, ' nested block quotes, ', len(html), ' bytes'
   call check(html == repeat('<blockquote>'//LF, depth)//'<p>deep</p>'//LF//repeat('</blockquote>'//LF, depth), &
              'deep: block quotes')
   source = repeat('- ', depth)//'x'//LF
   call doc%load(source)
   call options%parse(doc)
   call options%render_html(doc, html)
   print '(A,I0,A,I0,A)', 'deep render: ', depth, ' nested lists, ', len(html), ' bytes'
   call check(html == '<ul>'//LF//'<li>'//repeat(LF//'<ul>'//LF//'<li>', depth - 1)//'x'// &
                      repeat('</li>'//LF//'</ul>'//LF, depth), 'deep: lists')
   endsubroutine test_deep

   function add(doc, parent, kind, literal) result(n)
   !< Append a new node of a kind to a parent, with a literal if given.
   type(markdown_document), intent(inout)        :: doc     !< Document.
   integer(I4P),            intent(in)           :: parent  !< Parent.
   integer(I4P),            intent(in)           :: kind    !< Kind.
   character(*),            intent(in), optional :: literal !< Literal.
   integer(I4P)                                  :: n       !< New node.

   n = doc%new_node(kind)
   call doc%append_child(parent, n)
   if (present(literal)) call doc%set_literal(n, literal)
   endfunction add

   function read_back(path) result(bytes)
   !< A file's bytes.
   character(*), intent(in)  :: path  !< File.
   character(:), allocatable :: bytes !< Its bytes.
   integer                   :: u     !< Unit.
   integer                   :: n     !< Size.
   integer                   :: ios   !< I/O status.

   bytes = ''
   open(newunit=u, file=path, access='stream', form='unformatted', action='read', status='old', iostat=ios)
   if (ios /= 0) return
   inquire(unit=u, size=n)
   deallocate(bytes)
   allocate(character(max(n, 0)) :: bytes)
   if (n > 0) read(u, iostat=ios) bytes
   close(u)
   endfunction read_back

   subroutine check(condition, label)
   !< Count a failed check and print it.
   logical,      intent(in) :: condition !< What must hold.
   character(*), intent(in) :: label     !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL '//label
   endif
   endsubroutine check
endprogram baruch_test_html
