# Installation

BARUCH needs a Fortran compiler and its dependencies, StringiFor and PENF (with FACE and BeFoR64, which StringiFor
uses), fetched as sources and compiled with it.

## FoBiS

```bash
git clone https://github.com/szaghi/BARUCH && cd BARUCH
fobis fetch                        # the dependencies into src/third_party, pinned by src/third_party/fobos.lock
fobis build --mode static-gnu      # lib/gnu/static/libbaruch.a (shared-gnu: libbaruch.so)
```

`fobis build --lmodes` lists the modes: static and shared libraries and tests for gfortran (`-gnu`), nvfortran
(`-nvf`) and ifx (`-intel`). The library archive holds the dependencies too: a client links `libbaruch.a` only.

## fpm

```toml
[dependencies]
BARUCH = { git = "https://github.com/szaghi/BARUCH", branch = "main" }
```
