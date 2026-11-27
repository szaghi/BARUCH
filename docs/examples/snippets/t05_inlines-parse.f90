source = '# Run `42` &mdash; done'//LF//                                 &
         LF//                                                          &
         '> Set `dt = 1e-3` in <b>solver.ini</b>, see'//LF//            &
         '> <https://example.org/dt?step=1&amp;max=9>  '//LF//          &
         '> or write to <help@example.org>.'//LF//                      &
         LF//                                                          &
         'Not code: '//BS//'`dt'//BS//'`, not a tag: 3 < 4 &copy;'//BS//LF// &
         'a hard break above, a soft one below'//LF//                  &
         'and a comment: <!-- the end -->'//LF
print '(A)', source
call doc%load(source)
call options%parse(doc)
