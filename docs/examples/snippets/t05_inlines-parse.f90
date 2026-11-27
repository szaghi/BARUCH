source = '# Run `42` &mdash; done'//LF//                                 &
         LF//                                                          &
         '> Set `dt = 1e-3` in <b>solver.ini</b>, see'//LF//            &
         '> <https://example.org/dt?step=1&amp;max=9>  '//LF//          &
         '> or write to <help@example.org>.'//LF//                      &
         LF//                                                          &
         'Not code: '//BS//'`dt'//BS//'`, not a tag: 3 < 4 &copy;'//BS//LF// &
         'a hard break above, a soft one below'//LF//                  &
         'and a comment: <!-- the end -->'//LF//                        &
         LF//                                                          &
         'Read *the* **solver notes** in [the _wiki_][Notes] or'//LF//  &
         '![the plot](img/r.png "Residuals"); a * alone is text.'//LF// &
         LF//                                                          &
         '[notes]: https://example.org/notes'//LF
print '(A)', source
call doc%load(source)
call options%parse(doc)
