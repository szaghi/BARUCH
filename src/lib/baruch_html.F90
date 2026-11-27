!< BARUCH, the HTML renderer: a document's tree as HTML, as cmark writes it; an HTML fragment, or a standalone page.
module baruch_html
!< BARUCH, the HTML renderer: a document's tree as HTML, as cmark writes it; an HTML fragment, or a standalone page.
!<
!< The renderer is a loop over the events of the walk (`baruch_walk`): no recursion, so the depth of the tree does not
!< matter. Each event is first offered to the enabled extensions' `write_html` hooks, in registration order, for every
!< node (core kinds too: an extension can change how a heading or a list item is written); the first that handles it
!< writes the HTML of that event. Otherwise the core writes it, byte for byte what cmark 0.31's `html.c` writes (with
!< raw HTML allowed: `--unsafe`):
!<
!< - blocks: `<blockquote>`, `<ul>` and `<ol>` (`start="N"` when N is not 1), `<li>`, `<p>` (not in a tight list),
!<   `<h1>` to `<h6>`, `<pre><code class="language-X">` (X the first word of the info string), `<hr />`, an HTML block
!<   verbatim, nothing for a link reference definition; each block starts on a new line;
!< - inlines: text HTML-escaped, a soft break as LF, a hard break as `<br />`, `<code>`, raw inline HTML verbatim,
!<   `<em>`, `<strong>`, `<a href="..." title="...">`, `<img src="..." alt="..." title="..." />` (the alt text is the
!<   image's content as plain text). Destinations are written as cmark does (`escape_href`): the URL-safe characters
!<   kept, `&` and `'` as character references, every other byte percent-encoded.
!<
!< A parsed paragraph or heading holds the inline nodes of `baruch_inlines`: text, code spans, raw HTML, line breaks,
!< emphasis, links and images; a tree built by hand with them is written the same way.
!<
!< An extension node no enabled extension writes is rendered **transparently**: its children are rendered as if it were
!< not there; one with no children writes its literal, HTML-escaped (a block one on its own line). Nothing is dropped
!< silently and no markup is invented.
!<
!< The output is an HTML **fragment** (what goes inside `<body>`: no wrapper, no style sheet, no script, so that a page
!< can embed it) or, with `standalone`, a whole HTML5 document: doctype, `<meta charset="utf-8">`, a `<title>` (the
!< `title` given, else the plain text of the first heading, else empty), the fragment as the body. Line endings are LF.
use penf,               only : I4P
use baruch_document,    only : markdown_document
use baruch_extension,   only : extension_slot
use baruch_kinds_nodes, only : NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK, NODE_HTML_BLOCK, &
                               NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION,      &
                               NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH,      &
                               NODE_STRONG, NODE_LINK, NODE_IMAGE, LIST_BULLET
use baruch_walk,        only : walk_step, EVENT_NONE, EVENT_ENTER_BLOCK, EVENT_LEAVE_BLOCK, EVENT_ENTER_SPAN, &
                               EVENT_LEAVE_SPAN, EVENT_TEXT
use baruch_status
implicit none
private
public :: render_html_document
public :: escape_html, escape_href

character, parameter :: LF = achar(10) !< Line feed.

type :: html_buffer
   !< Output buffer, grown by doubling.
   character(:), allocatable :: chars        !< Bytes, `chars(1:used)` written.
   integer(I4P)              :: used = 0_I4P !< Bytes written.
   contains
      procedure, pass(self) :: put !< Append bytes.
      procedure, pass(self) :: cr  !< Append a line feed unless the output is empty or ends with one.
endtype html_buffer

contains
   subroutine render_html_document(doc, slots, html, standalone, title, ierr, msg)
   !< Render a document as HTML with the enabled extensions of `slots`: a fragment, or a standalone page. `ierr` (0 on
   !< success) and `msg` report an extension hook failing or breaking the walk's contract; `html` then holds what was
   !< rendered before it.
   type(markdown_document),   intent(in)    :: doc        !< Document.
   type(extension_slot),      intent(in)    :: slots(:)   !< Extensions of the options.
   character(:), allocatable, intent(out)   :: html       !< HTML.
   logical,                   intent(in)    :: standalone !< A whole HTML document (else a fragment).
   character(*),              intent(in)    :: title      !< Title of a standalone document (empty: the first heading).
   integer(I4P),              intent(out)   :: ierr       !< Status.
   character(:), allocatable, intent(inout) :: msg        !< Message, set with a nonzero `ierr`.
   type(html_buffer)                        :: out        !< Output.

   ierr = BARUCH_OK
   allocate(character(max(1024_I4P, 2_I4P * doc%source_length())) :: out%chars)
   if (standalone) then
      call out%put('<!DOCTYPE html>'//LF//'<html>'//LF//'<head>'//LF//'<meta charset="utf-8">'//LF//'<title>')
      if (len(title) > 0) then
         call out%put(escape_html(title))
      else
         call out%put(escape_html(first_heading_text(doc, slots)))
      endif
      call out%put('</title>'//LF//'</head>'//LF//'<body>'//LF)
   endif
   call render_tree(doc, slots, out, ierr, msg)
   if (standalone) then
      call out%cr()
      call out%put('</body>'//LF//'</html>'//LF)
   endif
   html = out%chars(1:out%used)
   endsubroutine render_html_document

   pure function escape_html(text) result(escaped)
   !< Return a text HTML-escaped as cmark does: `&`, `<`, `>`, `"` as character references, every other byte kept.
   character(*), intent(in)  :: text    !< Text.
   character(:), allocatable :: escaped !< Escaped text.
   integer(I4P)              :: i       !< Position in `text`.
   integer(I4P)              :: n       !< Length of `escaped`.
   integer                   :: pass    !< 1: measure, 2: write.

   n = 0_I4P
   do pass=1, 2
      if (pass == 2) then
         allocate(character(n) :: escaped)
         n = 0_I4P
      endif
      do i=1, len(text, kind=I4P)
         select case(text(i:i))
         case('&')
            if (pass == 2) escaped(n+1:n+5) = '&amp;'
            n = n + 5_I4P
         case('<')
            if (pass == 2) escaped(n+1:n+4) = '&lt;'
            n = n + 4_I4P
         case('>')
            if (pass == 2) escaped(n+1:n+4) = '&gt;'
            n = n + 4_I4P
         case('"')
            if (pass == 2) escaped(n+1:n+6) = '&quot;'
            n = n + 6_I4P
         case default
            if (pass == 2) escaped(n+1:n+1) = text(i:i)
            n = n + 1_I4P
         endselect
      enddo
   enddo
   endfunction escape_html

   pure function escape_href(url) result(escaped)
   !< Return a destination escaped for an `href` or `src` attribute as cmark does (houdini's `escape_href`): letters,
   !< digits and `-_.+!*(),%#@?=;:/$~` kept, `&` as `&amp;`, `'` as `&#x27;`, every other byte as `%XX` (upper case).
   character(*), intent(in)  :: url     !< Destination.
   character(:), allocatable :: escaped !< Escaped destination.
   character(*), parameter   :: SAFE = '-_.+!*(),%#@?=;:/$~' !< Punctuation kept as it is.
   character(*), parameter   :: HEX = '0123456789ABCDEF'     !< Hexadecimal digits.
   integer(I4P)              :: i                            !< Position in `url`.
   integer(I4P)              :: n                            !< Length of `escaped`.
   integer(I4P)              :: c                            !< Byte value.
   integer                   :: pass                         !< 1: measure, 2: write.

   n = 0_I4P
   do pass=1, 2
      if (pass == 2) then
         allocate(character(n) :: escaped)
         n = 0_I4P
      endif
      do i=1, len(url, kind=I4P)
         if (is_alnum(url(i:i)) .or. index(SAFE, url(i:i)) > 0) then
            if (pass == 2) escaped(n+1:n+1) = url(i:i)
            n = n + 1_I4P
         elseif (url(i:i) == '&') then
            if (pass == 2) escaped(n+1:n+5) = '&amp;'
            n = n + 5_I4P
         elseif (url(i:i) == "'") then
            if (pass == 2) escaped(n+1:n+6) = '&#x27;'
            n = n + 6_I4P
         else
            if (pass == 2) then
               c = iachar(url(i:i), kind=I4P)
               escaped(n+1:n+3) = '%'//HEX(c/16+1:c/16+1)//HEX(mod(c, 16_I4P)+1:mod(c, 16_I4P)+1)
            endif
            n = n + 3_I4P
         endif
      enddo
   enddo
   endfunction escape_href

   ! private
   subroutine render_tree(doc, slots, out, ierr, msg)
   !< Render the whole tree into the buffer, event by event.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   type(html_buffer),         intent(inout) :: out      !< Output.
   integer(I4P),              intent(out)   :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< HTML of an extension's hook.
   integer(I4P)                             :: node     !< Current node.
   integer(I4P)                             :: event    !< Current event.
   integer(I4P)                             :: plain    !< The image whose alt text is being written, 0 if none.
   integer(I4P)                             :: s        !< Counter.
   logical                                  :: entering !< The event enters its node (or is a text).
   logical                                  :: handled  !< An extension wrote the event.

   ierr = BARUCH_OK
   node = 0_I4P
   event = EVENT_NONE
   plain = 0_I4P
   do
      call walk_step(doc, slots, doc%root(), node, event, ierr, msg)
      if (ierr /= BARUCH_OK .or. node == 0_I4P) exit
      if (plain /= 0_I4P) then ! inside an image: its content as plain text, the alt attribute
         if (node /= plain) then
            if (event == EVENT_TEXT) call out%put(plain_text(doc, node))
            cycle
         endif
         plain = 0_I4P
      endif
      entering = event /= EVENT_LEAVE_BLOCK .and. event /= EVENT_LEAVE_SPAN
      handled = .false.
      do s=1, size(slots, kind=I4P)
         if (.not. slots(s)%enabled) cycle
         call slots(s)%ext%write_html(doc, node, entering, piece, handled, ierr, msg)
         if (ierr /= BARUCH_OK) then
            msg = 'markdown_options%render_html: extension "'//slots(s)%ext%name()//'": '//msg
            return
         endif
         if (handled) exit
      enddo
      if (handled) then
         if (allocated(piece)) call out%put(piece)
         cycle
      endif
      call render_core(doc, node, event, out, plain)
   enddo
   endsubroutine render_tree

   subroutine render_core(doc, n, event, out, plain)
   !< Write the HTML of one event of a node, as cmark does.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: n     !< Node.
   integer(I4P),            intent(in)    :: event !< Event.
   type(html_buffer),       intent(inout) :: out   !< Output.
   integer(I4P),            intent(inout) :: plain !< The image whose alt text starts, 0 if none.
   logical                                :: enter !< Entering (or a text).

   enter = event /= EVENT_LEAVE_BLOCK .and. event /= EVENT_LEAVE_SPAN
   select case(doc%node_kind(n))
   case(NODE_DOCUMENT, NODE_LINK_REFERENCE_DEFINITION)
   case(NODE_BLOCK_QUOTE)
      call out%cr()
      if (enter) then
         call out%put('<blockquote>'//LF)
      else
         call out%put('</blockquote>'//LF)
      endif
   case(NODE_LIST)
      if (enter) then
         call out%cr()
         if (doc%list_type(n) == LIST_BULLET) then
            call out%put('<ul>'//LF)
         elseif (doc%list_start(n) == 1_I4P) then
            call out%put('<ol>'//LF)
         else
            call out%put('<ol start="'//istr(doc%list_start(n))//'">'//LF)
         endif
      elseif (doc%list_type(n) == LIST_BULLET) then
         call out%put('</ul>'//LF)
      else
         call out%put('</ol>'//LF)
      endif
   case(NODE_ITEM)
      if (enter) then
         call out%cr()
         call out%put('<li>')
      else
         call out%put('</li>'//LF)
      endif
   case(NODE_HEADING)
      if (enter) then
         call out%cr()
         call out%put('<h'//istr(heading_level(doc, n))//'>')
      else
         call out%put('</h'//istr(heading_level(doc, n))//'>'//LF)
      endif
   case(NODE_PARAGRAPH)
      if (.not. in_tight_list(doc, n)) then
         if (enter) then
            call out%cr()
            call out%put('<p>')
         else
            call out%put('</p>'//LF)
         endif
      endif
   case(NODE_CODE_BLOCK)
      if (enter) call code_block(doc, n, out)
   case(NODE_HTML_BLOCK)
      if (enter) then
         call out%cr()
         call out%put(doc%literal(n))
         call out%cr()
      endif
   case(NODE_THEMATIC_BREAK)
      if (enter) then
         call out%cr()
         call out%put('<hr />'//LF)
      endif
   case(NODE_TEXT)
      call out%put(escape_html(doc%literal(n)))
   case(NODE_SOFTBREAK)
      call out%put(LF)
   case(NODE_LINEBREAK)
      call out%put('<br />'//LF)
   case(NODE_CODE)
      call out%put('<code>'//escape_html(doc%literal(n))//'</code>')
   case(NODE_HTML_INLINE)
      call out%put(doc%literal(n))
   case(NODE_EMPH)
      if (enter) then
         call out%put('<em>')
      else
         call out%put('</em>')
      endif
   case(NODE_STRONG)
      if (enter) then
         call out%put('<strong>')
      else
         call out%put('</strong>')
      endif
   case(NODE_LINK)
      if (enter) then
         call out%put('<a href="'//escape_href(doc%destination(n))//'"')
         if (len(doc%title(n)) > 0) call out%put(' title="'//escape_html(doc%title(n))//'"')
         call out%put('>')
      else
         call out%put('</a>')
      endif
   case(NODE_IMAGE)
      if (enter) then
         call out%put('<img src="'//escape_href(doc%destination(n))//'" alt="')
         plain = n
      else
         if (len(doc%title(n)) > 0) call out%put('" title="'//escape_html(doc%title(n)))
         call out%put('" />')
      endif
   case default ! an extension kind no extension writes: transparent
      if (doc%first_child(n) /= 0_I4P) then
         if (event == EVENT_ENTER_BLOCK .or. event == EVENT_LEAVE_BLOCK) call out%cr()
      elseif (enter) then
         if (event == EVENT_ENTER_BLOCK) call out%cr()
         call out%put(escape_html(doc%literal(n)))
         if (event == EVENT_ENTER_BLOCK) call out%cr()
      endif
   endselect
   endsubroutine render_core

   subroutine code_block(doc, n, out)
   !< Write a code block: `<pre><code>`, with `class="language-X"` when it has an info string (X its first word).
   type(markdown_document), intent(in)    :: doc  !< Document.
   integer(I4P),            intent(in)    :: n    !< Code block.
   type(html_buffer),       intent(inout) :: out  !< Output.
   character(:), allocatable              :: info !< Info string.
   integer                                :: e    !< End of its first word.

   call out%cr()
   info = doc%info(n)
   if (len(info) == 0) then
      call out%put('<pre><code>')
   else
      e = scan(info, ' '//achar(9)//LF//achar(11)//achar(12)//achar(13))
      if (e == 0) e = len(info) + 1
      call out%put('<pre><code class="language-'//escape_html(info(1:e-1))//'">')
   endif
   call out%put(escape_html(doc%literal(n))//'</code></pre>'//LF)
   endsubroutine code_block

   function plain_text(doc, n) result(text)
   !< The plain text of an inline leaf, as cmark writes it inside an image's alt attribute: a literal HTML-escaped, a
   !< line break as a space.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Node.
   character(:), allocatable           :: text !< Text.

   select case(doc%node_kind(n))
   case(NODE_SOFTBREAK, NODE_LINEBREAK)
      text = ' '
   case default
      text = escape_html(doc%literal(n))
   endselect
   endfunction plain_text

   function first_heading_text(doc, slots) result(text)
   !< The plain text of the first heading of a document (empty if none): its texts, code spans and line breaks, in
   !< document order.
   type(markdown_document), intent(in) :: doc      !< Document.
   type(extension_slot),    intent(in) :: slots(:) !< Extensions of the options.
   character(:), allocatable           :: text     !< Text.
   character(:), allocatable           :: msg      !< Message of the walk.
   integer(I4P)                        :: node     !< Current node.
   integer(I4P)                        :: event    !< Current event.
   integer(I4P)                        :: ierr     !< Status of the walk.
   integer(I4P)                        :: heading  !< The heading, once found.

   text = ''
   msg = ''
   node = 0_I4P
   event = EVENT_NONE
   heading = 0_I4P
   do
      call walk_step(doc, slots, doc%root(), node, event, ierr, msg)
      if (ierr /= BARUCH_OK .or. node == 0_I4P) exit
      if (heading == 0_I4P) then
         if (event == EVENT_ENTER_BLOCK .and. doc%node_kind(node) == NODE_HEADING) heading = node
      elseif (node == heading) then
         exit
      elseif (event == EVENT_TEXT) then
         select case(doc%node_kind(node))
         case(NODE_SOFTBREAK, NODE_LINEBREAK)
            text = text//' '
         case(NODE_HTML_INLINE)
         case default
            text = text//doc%literal(node)
         endselect
      endif
   enddo
   endfunction first_heading_text

   function in_tight_list(doc, n) result(tight)
   !< True for a paragraph whose grandparent is a tight list (its `<p>` is not written).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: n     !< Paragraph.
   logical                             :: tight !< Result.
   integer(I4P)                        :: g     !< Grandparent.

   tight = .false.
   if (doc%parent(n) == 0_I4P) return
   g = doc%parent(doc%parent(n))
   if (g == 0_I4P) return
   if (doc%node_kind(g) == NODE_LIST) tight = doc%list_tight(g)
   endfunction in_tight_list

   function heading_level(doc, n) result(level)
   !< The level of a heading, 1 to 6 (a heading built by hand with no level is written as level 1).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: n     !< Heading.
   integer(I4P)                        :: level !< Level.

   level = max(1_I4P, min(6_I4P, doc%heading_level(n)))
   endfunction heading_level

   pure function is_alnum(c) result(is)
   !< True for an ASCII letter or digit.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. (c >= '0' .and. c <= '9')
   endfunction is_alnum

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr

   subroutine put(self, bytes)
   !< Append bytes, the buffer doubled when full.
   class(html_buffer), intent(inout) :: self  !< Buffer.
   character(*),       intent(in)    :: bytes !< Bytes.
   character(:), allocatable         :: grown !< Grown buffer.
   integer(I4P)                      :: need  !< Bytes needed.

   need = self%used + len(bytes, kind=I4P)
   if (need > len(self%chars, kind=I4P)) then
      allocate(character(max(need, 2_I4P * len(self%chars, kind=I4P))) :: grown)
      grown(1:self%used) = self%chars(1:self%used)
      self%chars = grown ! not move_alloc: nvfortran 26.9 loses a deferred length through it (LOG, step 1.1)
   endif
   self%chars(self%used+1:need) = bytes
   self%used = need
   endsubroutine put

   subroutine cr(self)
   !< Append a line feed unless the output is empty or already ends with one.
   class(html_buffer), intent(inout) :: self !< Buffer.

   if (self%used == 0_I4P) return
   if (self%chars(self%used:self%used) /= LF) call self%put(LF)
   endsubroutine cr
endmodule baruch_html
