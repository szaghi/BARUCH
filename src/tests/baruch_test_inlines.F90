!< BARUCH test: the inline parser, the nodes it builds, their literals and spans, the strings of the blocks it decodes.
program baruch_test_inlines
!< BARUCH test: the inline parser, the nodes it builds, their literals and spans, the strings of the blocks it decodes.
!<
!< The gate of plan step 2.3 beside the conformance suite (`scripts/spec.sh commonmark`, which checks the HTML): here
!< the tree. Backslash escapes, entity and numeric character references (invalid code points as U+FFFD), code spans
!< (stripping, unmatched backtick strings), URI and email autolinks (`LINK_AUTOLINK`, destinations), the five forms
!< of raw inline HTML, hard and soft line breaks, text merged into one node per run (delimiter runs and brackets that
!< open nothing included), spans that point at the source through container markers, character references and
!< backslash escapes in info strings, destinations and titles (one pass), and a parsed tree free of tombstones.
use baruch
use penf, only : I4P
implicit none
character, parameter :: LF = achar(10)  !< Line feed.
character, parameter :: BS = achar(92)  !< Backslash (not a literal: nvfortran reads it as an escape).
character(*), parameter :: FFFD = char(239)//char(191)//char(189) !< U+FFFD in UTF-8.
integer              :: failures        !< Failed checks.

failures = 0
call test_text
call test_references
call test_code_spans
call test_autolinks
call test_raw_html
call test_breaks
call test_spans
call test_block_strings
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_inlines: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_inlines: ok'

contains
   subroutine parse(doc, source)
   !< Load a source and parse it with no extension; check the parse succeeds and leaves no tombstone.
   type(markdown_document), intent(inout) :: doc     !< Document.
   character(*),            intent(in)    :: source  !< Markdown.
   type(markdown_options)                 :: options !< Options.
   integer(I4P)                           :: stat    !< Status.
   character(:), allocatable              :: errmsg  !< Message.

   call doc%load(source)
   call options%parse(doc, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'parse: stat '//errmsg)
   call check(doc%nodes_number() == doc%last_node(), 'parse: no tombstone left')
   endsubroutine parse

   function shape_of(doc, block) result(text)
   !< Return the children of a block as `kind:literal` items joined by `|` (a link: `link=destination>its text`).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: block !< Block.
   character(:), allocatable           :: text  !< Description.
   integer(I4P)                        :: c     !< Child.

   text = ''
   c = doc%first_child(block)
   do while (c /= 0_I4P)
      if (len(text) > 0) text = text//'|'
      select case(doc%node_kind(c))
      case(NODE_TEXT, NODE_CODE, NODE_HTML_INLINE)
         text = text//doc%kind_name(doc%node_kind(c))//':'//doc%literal(c)
      case(NODE_LINK)
         text = text//'link='//doc%destination(c)//'>'//doc%literal(doc%first_child(c))
      case default
         text = text//doc%kind_name(doc%node_kind(c))
      endselect
      c = doc%next_sibling(c)
   enddo
   endfunction shape_of

   function first_block(doc) result(n)
   !< Return the first block of the document that is not a link reference definition.
   type(markdown_document), intent(in) :: doc !< Document.
   integer(I4P)                        :: n   !< Block.

   n = doc%first_child(doc%root())
   do while (n /= 0_I4P)
      if (doc%node_kind(n) /= NODE_LINK_REFERENCE_DEFINITION) exit
      n = doc%next_sibling(n)
   enddo
   endfunction first_block

   function span_of(doc, n) result(text)
   !< Return the source bytes of a node's span.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Node.
   character(:), allocatable           :: text !< Bytes.

   text = doc%source_slice(doc%span_first(n), doc%span_last(n))
   endfunction span_of

   subroutine expect(source, expected, label)
   !< Parse a one-block source and check the shape of its block.
   character(*), intent(in) :: source   !< Markdown.
   character(*), intent(in) :: expected !< Expected shape.
   character(*), intent(in) :: label    !< What is checked.
   type(markdown_document)  :: doc      !< Document.

   call parse(doc, source)
   call check(shape_of(doc, first_block(doc)) == expected, label//': "'//shape_of(doc, first_block(doc))//'"')
   endsubroutine expect

   subroutine test_text()
   !< Backslash escapes, and text merged: one node per run, delimiter runs and brackets that open nothing included.
   call expect('a'//BS//'*b'//BS//BS//' '//BS//'q '//BS//'_', 'text:a*b'//BS//' '//BS//'q _', 'text: escapes')
   call expect('a * b _ c [d] ![e] f]', 'text:a * b _ c [d] ![e] f]', 'text: runs and brackets merged into one')
   call expect('end'//BS, 'text:end'//BS, 'text: a backslash at the end')
   call expect('# A '//BS//'# b', 'text:A # b', 'text: a heading')
   endsubroutine test_text

   subroutine test_references()
   !< Named, decimal and hexadecimal references; what is not one stays text; invalid code points become U+FFFD.
   call expect('&copy; &#35; &#X22; &ngE;', 'text:'//char(194)//char(169)//' # " '//char(226)//char(137)//char(167)// &
               char(204)//char(184), 'references: named, decimal, hexadecimal, two code points')
   call expect('&#0; &#xD800; &#1114112;', 'text:'//FFFD//' '//FFFD//' '//FFFD, 'references: invalid code points')
   call expect('&copy &bogus; &#12345678; &#xabcdef0; &#; &', 'text:&copy &bogus; &#12345678; &#xabcdef0; &#; &', &
               'references: not references')
   call expect(BS//'&amp; &#92;*', 'text:&amp; '//BS//'*', 'references: one pass with the escapes')
   endsubroutine test_references

   subroutine test_code_spans()
   !< Code spans: stripping, line endings, backtick strings of other lengths inside, unmatched openers.
   call expect('a `` b`c `` d', 'text:a |code:b`c|text: d', 'code: one space stripped at both ends')
   call expect('`  `', 'code:  ', 'code: only spaces, kept')
   call expect('`a'//LF//'b `', 'code:a b ', 'code: a line ending as a space, one end only: no strip')
   call expect('```a`` b', 'text:```a`` b', 'code: no closer of the same length')
   call expect('`a'//BS//'`b`', 'code:a'//BS//'|text:b`', 'code: no escape inside')
   endsubroutine test_code_spans

   subroutine test_autolinks()
   !< URI and email autolinks: destination, text, the form recorded; what is not one.
   type(markdown_document) :: doc  !< Document.
   integer(I4P)            :: link !< The link.

   call expect('<https://a.b/c?d=1&amp;e>', 'link=https://a.b/c?d=1&e>https://a.b/c?d=1&e', 'autolinks: URI')
   call expect('<made-up:x'//BS//'[>', 'link=made-up:x'//BS//'[>made-up:x'//BS//'[', 'autolinks: no escape inside')
   call expect('<me.too+x@a-b.example>', 'link=mailto:me.too+x@a-b.example>me.too+x@a-b.example', 'autolinks: email')
   call expect('<a+:c d> <x@-y.z> <x:>', 'text:<a+:c d> <x@-y.z> <x:>', 'autolinks: none')
   call parse(doc, 'go <https://x.y> now')
   link = doc%next_sibling(doc%first_child(first_block(doc)))
   call check(doc%node_kind(link) == NODE_LINK .and. doc%int_slot(link, SLOT_LINK_FORM) == LINK_AUTOLINK, &
              'autolinks: LINK_AUTOLINK recorded')
   call check(span_of(doc, link) == '<https://x.y>' .and. span_of(doc, doc%first_child(link)) == 'https://x.y', &
              'autolinks: spans')
   endsubroutine test_autolinks

   subroutine test_raw_html()
   !< The five forms of raw inline HTML, kept verbatim; a tag across lines.
   call expect('a <b class="c">x</b>', 'text:a |html_inline:<b class="c">|text:x|html_inline:</b>', 'html: tags')
   call expect('a <!-- c -- d --> <!--> <?p x ?>', 'text:a |html_inline:<!-- c -- d -->|text: |html_inline:<!-->'// &
               '|text: |html_inline:<?p x ?>', 'html: comments and an instruction')
   call expect('a <!X y> <![CDATA[ <z> ]]>', 'text:a |html_inline:<!X y>|text: |html_inline:<![CDATA[ <z> ]]>', &
               'html: a declaration and CDATA')
   call expect('x <a'//LF//'  href="y">', 'text:x |html_inline:<a'//LF//'href="y">', 'html: a tag over two lines')
   call expect('a <!-- x <?y <![CDATA[ z', 'text:a <!-- x <?y <![CDATA[ z', 'html: unclosed forms are text')
   endsubroutine test_raw_html

   subroutine test_breaks()
   !< Hard line breaks (spaces, backslash), soft ones, trailing spaces dropped, leading spaces of the next line skipped.
   call expect('a  '//LF//'b'//BS//LF//'c '//LF//'   d', &
               'text:a|linebreak|text:b|linebreak|text:c|softbreak|text:d', 'breaks: hard, hard, soft')
   call expect('`x`   '//LF//'y', 'code:x|linebreak|text:y', 'breaks: after a code span')
   call expect('a'//BS, 'text:a'//BS, 'breaks: not at the end of a block')
   endsubroutine test_breaks

   subroutine test_spans()
   !< Spans through container markers, references, escapes and breaks: every inline node's span inside its block's.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: p   !< Paragraph.
   integer(I4P)            :: c   !< Child.
   integer(I4P)            :: n   !< Node.
   logical                 :: inside !< Every inline span inside its block's.

   call parse(doc, '> a &amp;  '//LF//'> `b` <c@d.e>'//LF//'>   '//BS//'*  ')
   p = doc%first_child(doc%first_child(doc%root()))
   call check(shape_of(doc, p) == 'text:a &|linebreak|code:b|text: |link=mailto:c@d.e>c@d.e|softbreak|text:*', &
              'spans: the shape "'//shape_of(doc, p)//'"')
   c = doc%first_child(p)
   call check(span_of(doc, c) == 'a &amp;', 'spans: text with a reference "'//span_of(doc, c)//'"')
   c = doc%next_sibling(c)
   call check(span_of(doc, c) == '  '//LF, 'spans: the hard break, its spaces and the line ending')
   c = doc%next_sibling(c)
   call check(span_of(doc, c) == '`b`', 'spans: the code span on the second line "'//span_of(doc, c)//'"')
   c = doc%next_sibling(doc%next_sibling(c))
   call check(span_of(doc, c) == '<c@d.e>', 'spans: the autolink "'//span_of(doc, c)//'"')
   c = doc%next_sibling(c)
   call check(span_of(doc, c) == LF, 'spans: the soft break, the line ending')
   c = doc%next_sibling(c)
   call check(span_of(doc, c) == BS//'*', 'spans: the escape on the third line "'//span_of(doc, c)//'"')
   ! every inline node of a larger document inside its parent's span
   call parse(doc, '- a `b`'//LF//'  c <d:e>'//LF//LF//'  > f'//LF//'  > g  '//LF//'  > h'//LF//LF//'Head &#33;'// &
              LF//'---'//LF)
   inside = .true.
   do n=1, doc%last_node()
      if (.not. is_core_inline(doc%node_kind(n))) cycle
      c = doc%parent(n)
      if (doc%span_first(n) < doc%span_first(c) .or. doc%span_last(n) > doc%span_last(c) .or. &
          doc%span_first(n) > doc%span_last(n)) then
         inside = .false.
         print '(A)', 'outside: '//doc%kind_name(doc%node_kind(n))//' "'//span_of(doc, n)//'"'
      endif
   enddo
   call check(inside, 'spans: every inline span inside its parent''s')
   endsubroutine test_spans

   elemental function is_core_inline(kind) result(is)
   !< True for a core inline kind.
   integer(I4P), intent(in) :: kind !< Kind.
   logical                  :: is   !< Result.

   is = kind >= NODE_INLINE_FIRST .and. kind <= NODE_INLINE_LAST
   endfunction is_core_inline

   subroutine test_block_strings()
   !< Info strings, destinations and titles: backslash escapes and character references, in one pass.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: n   !< Node.

   call parse(doc, '``` f&ouml;'//BS//'*&amp;x'//LF//'```'//LF)
   n = doc%first_child(doc%root())
   call check(doc%info(n) == 'f'//char(195)//char(182)//'*&x', 'strings: info string "'//doc%info(n)//'"')
   call parse(doc, '[a]: /f&ouml;'//BS//'* "t'//BS//'&amp; &#34;"'//LF)
   n = doc%first_child(doc%root())
   call check(doc%destination(n) == '/f'//char(195)//char(182)//'*', 'strings: destination "'//doc%destination(n)//'"')
   call check(doc%title(n) == 't&amp; "', 'strings: title "'//doc%title(n)//'"')
   call check(doc%label(n) == 'a', 'strings: the label as written')
   endsubroutine test_block_strings

   subroutine check(condition, label)
   !< Count a failed check and print it.
   logical,      intent(in) :: condition !< What must hold.
   character(*), intent(in) :: label     !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL '//label
   endif
   endsubroutine check
endprogram baruch_test_inlines
