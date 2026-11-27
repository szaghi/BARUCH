!< BARUCH conformance driver, test code only: a minimal HTML serialiser of the block tree, as cmark's renderer does.
module baruch_spec_html
!< BARUCH conformance driver, test code only: a minimal HTML serialiser of the block tree, as cmark's renderer does.
!<
!< Not the library's renderer and not public API: plan step 1.5 needs the HTML of the block subset before the public
!< renderer of step 1.6 exists, so the driver carries this one, and step 1.6 replaces it. It writes what cmark's
!< `html.c` writes for the blocks (`<blockquote>`, `<ul>`/`<ol start="N">`, `<li>`, `<p>` but in a tight list,
!< `<hN>`, `<pre><code class="language-X">`, `<hr />`, HTML blocks verbatim, nothing for a link reference
!< definition) and the inline content as it stands after the block phase: the text nodes HTML-escaped (`&`, `<`, `>`,
!< `"`). The tree is walked by its links, with no recursion: depth does not matter.
use penf,   only : I4P
use baruch, only : markdown_document, NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK, &
                   NODE_HTML_BLOCK, NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK, NODE_TEXT, LIST_BULLET
implicit none
private
public :: render_blocks_html

type :: html_buffer
   !< Output buffer, grown by doubling.
   character(:), allocatable :: chars           !< Bytes, `chars(1:used)` written.
   integer(I4P)              :: used = 0_I4P    !< Bytes written.
   contains
      procedure, pass(self) :: put  !< Append bytes.
      procedure, pass(self) :: cr   !< Append a line feed unless the output is empty or ends with one.
endtype html_buffer

character, parameter :: LF = achar(10) !< Line feed.

contains
   subroutine render_blocks_html(doc, html)
   !< Render a parsed document's blocks as HTML.
   type(markdown_document),   intent(in)  :: doc      !< Document, parsed.
   character(:), allocatable, intent(out) :: html     !< HTML.
   type(html_buffer)                      :: out      !< Output.
   integer(I4P)                           :: n        !< Current node.
   logical                                :: entering !< Entering `n` (else leaving it).

   allocate(character(1024) :: out%chars)
   n = doc%root()
   entering = .true.
   do while (n /= 0_I4P)
      if (entering) then
         call enter(doc, n, out)
         if (doc%first_child(n) /= 0_I4P .and. has_block_children(doc%node_kind(n))) then
            n = doc%first_child(n)
            cycle
         endif
      endif
      call leave(doc, n, out)
      if (n == doc%root()) exit
      if (doc%next_sibling(n) /= 0_I4P) then
         n = doc%next_sibling(n)
         entering = .true.
      else
         n = doc%parent(n)
         entering = .false.
      endif
   enddo
   html = out%chars(1:out%used)
   endsubroutine render_blocks_html

   ! private
   pure function has_block_children(kind) result(has)
   !< True for the kinds whose children are walked (containers, and the blocks holding inline content).
   integer(I4P), intent(in) :: kind !< Kind.
   logical                  :: has  !< Result.

   select case(kind)
   case(NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_PARAGRAPH, NODE_HEADING)
      has = .true.
   case default
      has = .false.
   endselect
   endfunction has_block_children

   subroutine enter(doc, n, out)
   !< Write what a node opens with (a leaf: all of it).
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: n     !< Node.
   type(html_buffer),       intent(inout) :: out   !< Output.
   character(:), allocatable              :: info  !< Info string.
   integer                                :: e     !< End of its first word.

   select case(doc%node_kind(n))
   case(NODE_BLOCK_QUOTE)
      call out%cr()
      call out%put('<blockquote>'//LF)
   case(NODE_LIST)
      call out%cr()
      if (doc%list_type(n) == LIST_BULLET) then
         call out%put('<ul>'//LF)
      elseif (doc%list_start(n) == 1_I4P) then
         call out%put('<ol>'//LF)
      else
         call out%put('<ol start="'//istr(doc%list_start(n))//'">'//LF)
      endif
   case(NODE_ITEM)
      call out%cr()
      call out%put('<li>')
   case(NODE_HEADING)
      call out%cr()
      call out%put('<h'//istr(doc%heading_level(n))//'>')
   case(NODE_PARAGRAPH)
      if (.not. in_tight_list(doc, n)) then
         call out%cr()
         call out%put('<p>')
      endif
   case(NODE_CODE_BLOCK)
      call out%cr()
      info = doc%info(n)
      if (len(info) == 0) then
         call out%put('<pre><code>')
      else
         e = scan(info, ' '//achar(9)//LF)
         if (e == 0) e = len(info) + 1
         call out%put('<pre><code class="language-'//escaped(info(1:e-1))//'">')
      endif
      call out%put(escaped(doc%literal(n))//'</code></pre>'//LF)
   case(NODE_HTML_BLOCK)
      call out%cr()
      call out%put(doc%literal(n))
      call out%cr()
   case(NODE_THEMATIC_BREAK)
      call out%cr()
      call out%put('<hr />'//LF)
   case(NODE_TEXT)
      call out%put(escaped(doc%literal(n)))
   endselect
   endsubroutine enter

   subroutine leave(doc, n, out)
   !< Write what a node closes with.
   type(markdown_document), intent(in)    :: doc !< Document.
   integer(I4P),            intent(in)    :: n   !< Node.
   type(html_buffer),       intent(inout) :: out !< Output.

   select case(doc%node_kind(n))
   case(NODE_BLOCK_QUOTE)
      call out%cr()
      call out%put('</blockquote>'//LF)
   case(NODE_LIST)
      if (doc%list_type(n) == LIST_BULLET) then
         call out%put('</ul>'//LF)
      else
         call out%put('</ol>'//LF)
      endif
   case(NODE_ITEM)
      call out%put('</li>'//LF)
   case(NODE_HEADING)
      call out%put('</h'//istr(doc%heading_level(n))//'>'//LF)
   case(NODE_PARAGRAPH)
      if (.not. in_tight_list(doc, n)) call out%put('</p>'//LF)
   endselect
   endsubroutine leave

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

   pure function escaped(text) result(e)
   !< Return a text HTML-escaped: `&`, `<`, `>`, `"` as character references.
   character(*), intent(in)  :: text !< Text.
   character(:), allocatable :: e    !< Escaped text.
   integer                   :: i    !< Position in `text`.
   integer                   :: n    !< Length of `e`.

   n = 0
   do i=1, len(text)
      select case(text(i:i))
      case('&')
         n = n + 5
      case('<', '>')
         n = n + 4
      case('"')
         n = n + 6
      case default
         n = n + 1
      endselect
   enddo
   allocate(character(n) :: e)
   n = 0
   do i=1, len(text)
      select case(text(i:i))
      case('&')
         e(n+1:n+5) = '&amp;'
         n = n + 5
      case('<')
         e(n+1:n+4) = '&lt;'
         n = n + 4
      case('>')
         e(n+1:n+4) = '&gt;'
         n = n + 4
      case('"')
         e(n+1:n+6) = '&quot;'
         n = n + 6
      case default
         e(n+1:n+1) = text(i:i)
         n = n + 1
      endselect
   enddo
   endfunction escaped

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

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr
endmodule baruch_spec_html
