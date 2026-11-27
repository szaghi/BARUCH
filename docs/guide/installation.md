# Installation

BARUCH needs a Fortran compiler and its dependencies, StringiFor and PENF (with FACE and BeFoR64, which StringiFor
uses), fetched as sources and compiled with it.

BARUCH follows its dependencies' **latest default branch**: nothing is pinned (no tag in `fobos` or `fpm.toml`, no
tracked `fobos.lock`). The inline parser (planned, phase 2 of the plan) needs StringiFor **v1.12.3 or later**, the first
release with Unicode punctuation (`is_punct(unicode=.true.)`, the P and S general categories CommonMark's emphasis
rules use) and full case folding (`casefold()`, for matching link labels). An older StringiFor in `src/third_party` is
refreshed by `fobis fetch --update`.

Tested compilers: gfortran 16.2, nvfortran 26.9, ifx 2026.2.

## FoBiS

```bash
git clone https://github.com/szaghi/BARUCH && cd BARUCH
fobis fetch                        # the dependencies into src/third_party, their latest default branch
fobis build --mode static-gnu      # lib/gnu/static/libbaruch.a (shared-gnu: libbaruch.so)
```

`fobis build --lmodes` lists the modes: static and shared libraries and tests for gfortran (`-gnu`), nvfortran
(`-nvf`) and ifx (`-intel`). The library archive holds the dependencies too: a client links `libbaruch.a` only, with
the module files of `lib/gnu/static/mod` on its include path ([Quick start](/guide/quickstart)).

## fpm

```toml
[dependencies]
BARUCH = { git = "https://github.com/szaghi/BARUCH", branch = "main" }
```
