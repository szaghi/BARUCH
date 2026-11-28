source = 'Solver notes'//LF//                            &
         '============'//LF//                            &
         LF//                                            &
         'Set the _time step_ in [the input][input]:'//LF// &
         LF//                                            &
         '* `dt = 1e-3`'//LF//                           &
         '* `cfl = 0.8`'//LF//                           &
         LF//                                            &
         '~~~ ini'//LF//                                 &
         '[solver]'//LF//                                &
         'dt = 1e-3'//LF//                               &
         '~~~'//LF//                                     &
         LF//                                            &
         '[input]: https://example.org/solver.ini'//LF
call doc%load(source)
call options%parse(doc)
call options%write_markdown(doc, markdown)
print '(A,L1)', 'written back byte for byte: ', markdown == source
