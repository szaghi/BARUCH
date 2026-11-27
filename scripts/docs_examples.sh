#!/usr/bin/env bash
# Build and run the documentation examples, regenerating everything the pages include from them (StringiFor's and
# HERALD's scheme).
#
#   docs/examples/src/*.f90    the example programs (hand-written), with marker comments:
#                                !run ID COMMAND        a run shown in the pages (!run -s: with its exit status)
#                                !region NAME ... !endregion NAME   a part of the program included on its own
#   docs/examples/files/       input files (Markdown documents), copied into the directory where the runs happen
#   docs/examples/snippets/    generated: <program>.f90 without the markers, and <program>-<region>.f90 (dedented)
#   docs/examples/output/      generated: <ID>.ansi, "$ COMMAND" then its output (standard output and error), the
#                              scratch run directory shown as /home/user
#
# The library is built by FoBiS (mode static-gnu: BARUCH and the sources of its dependencies, fetched into
# src/third_party, in one archive) and every example is compiled against it exactly as a user program would be:
# `gfortran -I<mods> prog.f90 libbaruch.a` (with -fcheck=bounds: an example that reads out of bounds fails the build).
# The outputs must not depend on the compiler or the machine: explicit formats, no timings, no sizes. Each run happens
# in a scratch directory with a minimal environment (HOME there, PATH: the examples, /usr/bin, /bin) and no standard
# input. The docs-examples check fails when the committed snippets or outputs differ from the regenerated ones.
#
# Usage: bash scripts/docs_examples.sh      (FC=/path/to/gfortran bash scripts/docs_examples.sh: another gfortran; it
#                                            must be the one fobis uses, or the module files do not match)
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
ex=$root/docs/examples
build=$root/build/docs-examples # git-ignored
lib=$root/lib/gnu/static        # fobis build --mode static-gnu
run_dir=$build/run
home=/home/user                 # how the run directory is shown
fc=${FC:-gfortran}
command -v "$fc" > /dev/null || { echo "docs_examples: compiler '$fc' not found (set FC)" >&2; exit 1; }
fc=$(command -v "$fc")
mkdir -p "$root/build"
# the dependencies are fetched once (a fresh clone, the CI); afterwards the ones in src/third_party are used
if [ ! -d "$root/src/third_party/StringiFor" ]; then
  (cd "$root" && fobis fetch) > "$root/build/docs-examples.log" 2>&1 || {
    cat "$root/build/docs-examples.log"; echo "docs_examples: fobis fetch failed" >&2; exit 1; }
fi
(cd "$root" && fobis build --mode static-gnu --fc "$fc") > "$root/build/docs-examples.log" 2>&1 || {
  cat "$root/build/docs-examples.log"; echo "docs_examples: library build failed" >&2; exit 1; }
rm -rf -- "$build"
mkdir -p "$build/bin" "$build/mod" "$run_dir" "$ex/snippets" "$ex/output"
rm -f -- "$ex"/snippets/*.f90 "$ex"/output/*.ansi
markers='^ *!(run|region|endregion) '
dedent() { awk '{l[NR]=$0; if ($0 ~ /[^ ]/) {match($0, /^ */); if (m == "" || RLENGTH < m) m = RLENGTH}}
                END {for (i = 1; i <= NR; i++) print substr(l[i], m + 1)}' "$1"; }
for src in "$ex"/src/*.f90; do
  name=$(basename "$src" .f90)
  grep -Ev "$markers" "$src" > "$ex/snippets/$name.f90" || true
  for region in $(sed -n 's/^ *!region \([A-Za-z0-9_-]*\).*/\1/p' "$src"); do
    awk -v r="$region" '$1 == "!endregion" && $2 == r {on = 0}
                        on && $0 !~ /^ *!(run|region|endregion) / {print}
                        $1 == "!region" && $2 == r {on = 1}' "$src" > "$build/region.f90"
    dedent "$build/region.f90" > "$ex/snippets/$name-$region.f90"
  done
done
for src in "$ex"/src/*.f90; do
  "$fc" -O2 -fcheck=bounds,do,mem,pointer -I"$lib/mod" -J"$build/mod" -o "$build/bin/$(basename "$src" .f90)" \
        "$src" "$lib/libbaruch.a"
done
if [ -d "$ex/files" ]; then cp -R "$ex/files/." "$run_dir/"; fi
path=$build/bin
run() { # run [-s] ID COMMAND
  local show=0 status=0
  if [ "$1" = -s ]; then show=1; shift; fi
  local id=$1; shift
  local cmd="$*"
  {
    printf '$ %s\n' "$cmd"
    (cd "$run_dir" && env -i HOME="$run_dir" PATH="$path:/usr/bin:/bin" SHELL=/bin/bash LC_ALL=C \
                     LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}" GFORTRAN_UNBUFFERED_PRECONNECTED=y \
                     GFORTRAN_ERROR_BACKTRACE=0 bash -c "$cmd" < /dev/null 2>&1) || status=$?
    if [ $show = 1 ]; then printf '[exit status %d]\n' "$status"; fi
  } | sed -e "s|$run_dir|$home|g" > "$ex/output/$id.ansi"
}
for src in "$ex"/src/*.f90; do
  while IFS= read -r line; do
    line=${line#*!run }
    if [ "${line%% *}" = -s ]; then line=${line#-s }; run -s "${line%% *}" "${line#* }"
    else run "${line%% *}" "${line#* }"; fi
  done < <(grep -E '^ *!run ' "$src" || true)
done
echo "docs_examples: $(ls "$ex"/src/*.f90 | wc -l) programs, $(ls "$ex"/output/*.ansi | wc -l) runs"
