#!/usr/bin/env bash
# Run a conformance suite: build the driver, render every example, compare with the expected HTML (plan
# szaghi/BARUCH#1, step 1.4; reference page docs/reference/conformance.md).
#
# Usage: bash scripts/spec.sh [OPTIONS] SUITE... [OPTIONS]      (options and suites in any order)
#
# SUITE    a suite of scripts/spec/fixtures.py (`python3 -I scripts/spec/fixtures.py --list`), or `all` for every one.
#          A suite is a name bound, in that one registry, to its pinned source, its fixtures directory
#          (src/tests/spec/<dir>/: index.tsv and examples/NNNN.md, NNNN.html) and the extensions it needs. Suites
#          now: commonmark (CommonMark 0.31.2, the core alone). Later steps add theirs to the registry, named by
#          family and feature: gfm-table, gfm-strikethrough, gfm-tasklist, gfm-autolink, gfm-footnote,
#          vp-container, vp-frontmatter, vp-anchor, vp-attrs, vp-import, vp-toc, vp-fence-title, ... and spec.sh runs
#          them with no change here.
#
# Options
#   --mode MODE      fobos mode that compiles the driver (default tests-gnu; tests-nvf, tests-intel, the -debug
#                    ones). The driver alone is built (--target), into build/spec/MODE/ (--build-dir): exe/ and the
#                    other compilers' objects are never touched.
#   --no-build       run the driver already built for MODE.
#   --blocks-only    count only the examples with no inline syntax (the rule: scripts/spec/compare.py), size printed.
#   --failures N     show the Markdown and the normalised diff of the first N failing examples of each suite.
#   --roundtrip      (plan step 3.2) parse, write Markdown, parse again, compare the trees; not available yet.
#   --canonical      (plan step 3.2) with --roundtrip: the writer ignores the recorded style; not available yet.
#   -h, --help       this text.
#
# For each suite the driver (src/tests/spec/baruch_spec_driver.F90) gets one `--extension NAME` per extension of
# the suite and `--list build/spec/MODE/SUITE/list.tsv` (one "INPUT<TAB>OUTPUT" per example); it writes
# build/spec/MODE/SUITE/html/NNNN.html, emptied first. scripts/spec/compare.py then prints passed/total per section
# and overall; the last line of each suite is "SUITE: P/T passed (E exact)".
#
# Exit status: 0 when every suite ran (whatever the count: the count is the result, compared by the plan's gates),
# 1 when the driver reported an error on an example (the counts are printed anyway), 2 on a usage or build error.
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root" || exit 2

usage() { sed -n '2,/^$/{s/^# \{0,1\}//;p}' "${BASH_SOURCE[0]}"; }
die() { printf 'spec.sh: %s\n' "$1" >&2; exit 2; }

mode=tests-gnu
build=1
compare_opts=()
names=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode ) [[ $# -ge 2 ]] || die '--mode needs a fobos mode'; mode=$2; shift 2 ;;
    --no-build ) build=0; shift ;;
    --blocks-only ) compare_opts+=(--blocks-only); shift ;;
    --failures ) [[ $# -ge 2 && "$2" =~ ^[0-9]+$ ]] || die '--failures needs a number'
                 compare_opts+=(--failures "$2"); shift 2 ;;
    --roundtrip | --canonical ) die "$1 is not available yet (plan szaghi/BARUCH#1, step 3.2)" ;;
    -h | --help ) usage; exit 0 ;;
    -* ) die "unknown option $1 (--help)" ;;
    * ) names+=("$1"); shift ;;
  esac
done
[[ ${#names[@]} -gt 0 ]] || die 'no suite given (--help)'

# the registry: NAME<TAB>DIRECTORY<TAB>EXTENSIONS
declare -A suite_dir suite_ext
registry=()
while IFS=$'\t' read -r name dir ext; do
  registry+=("$name"); suite_dir[$name]=$dir; suite_ext[$name]=$ext
done < <(python3 -I scripts/spec/fixtures.py --list) || die 'cannot read the suites of scripts/spec/fixtures.py'
suites=()
for name in "${names[@]}"; do
  if [[ "$name" == all ]]; then
    suites+=("${registry[@]}")
  elif [[ -n "${suite_dir[$name]:-}" ]]; then
    suites+=("$name")
  else
    die "unknown suite $name (suites: ${registry[*]}, all)"
  fi
done

out=build/spec/$mode
driver=$out/baruch_spec_driver
if [[ $build -eq 1 ]]; then
  log=$(fobis build --mode "$mode" --target src/tests/spec/baruch_spec_driver.F90 --build-dir "$out/" 2>&1) || {
    printf '%s\n' "$log" >&2; die "the driver does not build with --mode $mode"; }
fi
[[ -x "$driver" ]] || die "no driver $driver (build it: drop --no-build)"

status=0
for name in "${suites[@]}"; do
  dir=${suite_dir[$name]}
  work=$out/$name
  if ! { rm -rf "$work/html" && mkdir -p "$work/html"; }; then die "cannot create $work/html"; fi
  # one example per line of the index, after its header: the number is the file name on four digits
  tail -n +2 "$dir/index.tsv" | while IFS=$'\t' read -r number _; do
    printf -v stem '%04d' "$((10#$number))"
    printf '%s\t%s\n' "$dir/examples/$stem.md" "$work/html/$stem.html"
  done > "$work/list.tsv"
  extension_opts=()
  if [[ "${suite_ext[$name]}" != - ]]; then
    IFS=, read -r -a exts <<< "${suite_ext[$name]}"
    for e in "${exts[@]}"; do extension_opts+=(--extension "$e"); done
  fi
  if ! "$driver" "${extension_opts[@]}" --list "$work/list.tsv"; then
    printf 'spec.sh: %s: the driver reported errors (above); those examples have no output and fail\n' "$name" >&2
    status=1
  fi
  python3 -I scripts/spec/compare.py --suite "$name" --actual "$work/html" "${compare_opts[@]}" || die 'compare.py failed'
done
exit $status
