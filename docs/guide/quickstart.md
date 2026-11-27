# Quick Start

```fortran
use baruch
```

The parser is not written yet; this page follows it.

## Tests

```bash
fobis fetch
fobis build --mode tests-gnu && bash scripts/run_tests.sh
fobis rule --ex makecoverage-analysis   # what CI runs: coverage into docs/public/coverage.json
```

`exe/` is shared by the test modes: clean it before switching mode or compiler.
