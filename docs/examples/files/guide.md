Solver guide
============

The solver reads `solver.ini` and writes a report: see the [manual][] and
the [issue tracker](https://github.com/szaghi/BARUCH/issues "Issues").

Running
-------

+ build it:   `fobis build --mode static-gnu`
+ run it with __two__ ranks
+ read the report

1)  Prepare the mesh.
2)  Start the run.

~~~ bash
mpirun -np 2 ./solver solver.ini
~~~

___

Results
-------

The residual drops below 1e-8 after *about* 400 steps.


[manual]: https://example.org/manual
