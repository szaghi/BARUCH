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
!< Plan step 2.4: emphasis (the flanking rules with Unicode white space and punctuation, the rule of 3, the character
!< recorded, the spans of what a match leaves), links and images (inline, full, collapsed, shortcut: the form and the
!< label recorded), reference labels normalised (case folding, white space), the first definition winning, links
!< not nesting.
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
call test_emphasis
call test_links
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
   call expect(repeat('`', 1200)//'a'//repeat('`', 3)//'b'//repeat('`', 1200), 'code:a```b', &
               'code: 1200 backticks (no length limit, plan step 2.6)')
   call expect('x'//repeat('`', 90)//' `` '//repeat('`', 90)//'y', 'text:x|code:``|text:y', &
               'code: 90 backticks after a scan that met them (the table grows)')
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

   subroutine test_emphasis()
   !< Emphasis and strong emphasis: both characters, the rule of 3, Unicode punctuation and white space around a run,
   !< the character recorded, the spans of the new nodes and of the delimiters left.
   character(*), parameter :: NBSP = char(194)//char(160)   !< U+00A0, Unicode White_Space.
   character(*), parameter :: LAQUO = char(194)//char(171)  !< U+00AB, Unicode punctuation (Pi).
   character(*), parameter :: RAQUO = char(194)//char(187)  !< U+00BB, Unicode punctuation (Pf).
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: b   !< Block.
   integer(I4P)            :: n   !< Node.

   call expect_tree('*a* _b_ **c** __d__', 'emph*(text:a)|text: |emph_(text:b)|text: |strong*(text:c)|text: |'// &
                    'strong_(text:d)', 'emphasis: both characters, recorded')
   call expect_tree('*foo**bar**baz*', 'emph*(text:foo|strong*(text:bar)|text:baz)', 'emphasis: the rule of 3')
   call expect_tree('***a***', 'emph*(strong*(text:a))', 'emphasis: three characters')
   call expect_tree('**a*', 'text:*|emph*(text:a)', 'emphasis: a character left')
   call expect_tree('foo_bar_ *a _b* c_', 'text:foo_bar_ |emph*(text:a _b)|text: c_', 'emphasis: intraword _, overlap')
   call expect_tree('a*'//LAQUO//'b'//RAQUO//'*c', 'text:a*'//LAQUO//'b'//RAQUO//'*c', &
                    'emphasis: Unicode punctuation around the runs')
   call expect_tree('*'//NBSP//'a*', 'text:*'//NBSP//'a*', 'emphasis: Unicode white space after the run')
   call parse(doc, 'x ***a** b*')
   b = first_block(doc)
   n = doc%next_sibling(doc%first_child(b))
   call check(doc%node_kind(n) == NODE_EMPH .and. span_of(doc, n) == '***a** b*', &
              'emphasis: the span of emphasis "'//span_of(doc, n)//'"')
   n = doc%first_child(n)
   call check(doc%node_kind(n) == NODE_STRONG .and. span_of(doc, n) == '**a**', &
              'emphasis: the span of strong emphasis "'//span_of(doc, n)//'"')
   call check(doc%int_slot(n, SLOT_EMPHASIS_CHAR) == iachar('*'), 'emphasis: SLOT_EMPHASIS_CHAR')
   call parse(doc, '**a*')
   n = doc%first_child(first_block(doc))
   call check(span_of(doc, n) == '*' .and. doc%span_first(n) == 1, 'emphasis: the span of the character left')
   endsubroutine test_emphasis

   subroutine test_links()
   !< Links and images: the four forms, destination, title, label as written, normalised matching, the first definition
   !< winning, no link in a link, spans.
   character(*), parameter :: SHARP_S = char(225)//char(186)//char(158) !< U+1E9E, folds to `ss`.
   type(markdown_document) :: doc !< Document.
   integer(I4P)            :: b   !< Block.
   integer(I4P)            :: n   !< Node.

   call expect_tree('[a *b*](</u v> "t")', 'link[1,,/u v,t](text:a |emph*(text:b))', 'links: inline')
   call expect_tree('[a]()', 'link[1,,,](text:a)', 'links: inline, empty destination')
   call expect_tree('[x][Foo  '//LF//'Bar]'//LF//LF//'[foo bar]: /v "w"', 'link[2,Foo  '//LF//'Bar,/v,w](text:x)', &
                    'links: full, the label normalised')
   call expect_tree('[Foo bar][]'//LF//LF//'[foo bar]: /v', 'link[3,Foo bar,/v,](text:Foo bar)', 'links: collapsed')
   call expect_tree('[Foo bar]'//LF//LF//'[foo bar]: /v', 'link[4,Foo bar,/v,](text:Foo bar)', 'links: shortcut')
   call expect_tree('['//SHARP_S//']'//LF//LF//'[SS]: /s', 'link[4,'//SHARP_S//',/s,](text:'//SHARP_S//')', &
                    'links: Unicode case folding')
   call expect_tree('[a]'//LF//LF//'[A]: /first'//LF//'[a]: /second', 'link[4,a,/first,](text:a)', &
                    'links: the first definition wins')
   call expect_tree('[a][b] [b]'//LF//LF//'[a]: /a', 'text:[a][b] [b]', 'links: an undefined label is text')
   call expect_tree('[a [b](/c)](/d)', 'text:[a |link[1,,/c,](text:b)|text:](/d)', 'links: no link in a link')
   call expect_tree('![a *b*](/i "t")', 'image[1,,/i,t](text:a |emph*(text:b))', 'images: inline')
   call expect_tree('[![a](/i)](/l)', 'link[1,,/l,](image[1,,/i,](text:a))', 'images: in a link')
   call parse(doc, 'x [a](/u) ![b][c] y'//LF//LF//'[c]: /c')
   b = first_block(doc)
   n = doc%next_sibling(doc%first_child(b))
   call check(doc%node_kind(n) == NODE_LINK .and. span_of(doc, n) == '[a](/u)', 'links: span "'//span_of(doc, n)//'"')
   n = doc%next_sibling(doc%next_sibling(n))
   call check(doc%node_kind(n) == NODE_IMAGE .and. span_of(doc, n) == '![b][c]', &
              'images: span "'//span_of(doc, n)//'"')
   call check(doc%int_slot(n, SLOT_LINK_FORM) == LINK_FULL .and. doc%label(n) == 'c', 'images: form and label')
   endsubroutine test_links

   subroutine expect_tree(source, expected, label)
   !< Parse a source and check the subtree of its first block that is not a definition.
   character(*), intent(in) :: source   !< Markdown.
   character(*), intent(in) :: expected !< Expected tree.
   character(*), intent(in) :: label    !< What is checked.
   type(markdown_document)  :: doc      !< Document.

   call parse(doc, source)
   call check(tree_of(doc, first_block(doc)) == expected, label//': "'//tree_of(doc, first_block(doc))//'"')
   endsubroutine expect_tree

   function tree_of(doc, top) result(text)
   !< Return the subtree under `top`, walked by its links: `kind:literal` for a leaf, `kind(children)` for a node with
   !< children; emphasis followed by its character, a link or an image by `[form,label,destination,title]`.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: top  !< Root of the subtree.
   character(:), allocatable           :: text !< Description.
   integer(I4P)                        :: n    !< Node.
   integer(I4P)                        :: k    !< Kind.
   character                           :: c    !< A character slot (fixed length: szaghi/BARUCH#2 B3).

   text = ''
   n = doc%first_child(top)
   do while (n /= 0_I4P)
      k = doc%node_kind(n)
      select case(k)
      case(NODE_TEXT, NODE_CODE, NODE_HTML_INLINE)
         text = text//doc%kind_name(k)//':'//doc%literal(n)
      case(NODE_EMPH, NODE_STRONG)
         c = achar(doc%int_slot(n, SLOT_EMPHASIS_CHAR))
         text = text//doc%kind_name(k)//c
      case(NODE_LINK, NODE_IMAGE)
         c = achar(48 + doc%int_slot(n, SLOT_LINK_FORM))
         text = text//doc%kind_name(k)//'['//c//','//doc%label(n)//','//doc%destination(n)//','//doc%title(n)//']'
      case default
         text = text//doc%kind_name(k)
      endselect
      if (doc%first_child(n) /= 0_I4P) then
         text = text//'('
         n = doc%first_child(n)
         cycle
      endif
      do while (n /= top)
         if (doc%next_sibling(n) /= 0_I4P) then
            text = text//'|'
            n = doc%next_sibling(n)
            exit
         endif
         n = doc%parent(n)
         if (n /= top) text = text//')'
      enddo
      if (n == top) exit
   enddo
   endfunction tree_of

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
