# Quick start

The reader, the renderer and the writers are not written yet ([About](/guide/)): this page shows how a program uses
the library today, and grows with it.

## A first program

<<< @/examples/snippets/hello.f90

Build the library, then compile the program against it as any client does: the module files on the include path, the
archive (which holds BARUCH and its dependencies) on the link line.

```bash
fobis fetch && fobis build --mode static-gnu
gfortran -Ilib/gnu/static/mod hello.f90 lib/gnu/static/libbaruch.a -o hello
```

<<< @/examples/output/hello.ansi{ansi}

The program and its output come from `docs/examples/src/hello.f90`, built and run by `scripts/docs_examples.sh`: the
examples of these pages are never pasted by hand.

## Tests

```bash
fobis fetch
fobis build --mode tests-gnu && bash scripts/run_tests.sh
fobis rule --ex makecoverage-analysis   # what CI runs: coverage into docs/public/coverage.json
```

`exe/` is shared by the test modes: clean it before switching mode or compiler.

## Documentation

```bash
bash scripts/docs_examples.sh   # rebuild and rerun the examples: docs/examples/snippets and docs/examples/output
fobis rule --ex makedoc         # the API pages (formal) and the site (VitePress) into docs/.vitepress/dist
```
