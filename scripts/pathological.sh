#!/usr/bin/env bash
# Time the pathological inputs: each case at n and 10n, the ratio of the best wall times gated (plan szaghi/BARUCH#1,
# step 2.6; reference page docs/reference/conformance.md, "Pathological inputs").
#
# Usage: bash scripts/pathological.sh [OPTIONS]
#
# Options
#   --mode MODE       fobos mode that compiles the test (default tests-gnu; tests-nvf, tests-intel). The test alone is
#                     built (--target), into build/pathological/MODE/ (--build-dir): exe/ and the other compilers'
#                     objects are never touched. Time release modes: a -debug mode measures the checks.
#   --no-build        run the test already built for MODE.
#   --n N             base size (default 10000).
#   --max-ratio R     largest t(10n)/t(n) allowed (default 15).
#   --case NAME       one case only (its name as the table prints it).
#   -h, --help        this text.
#
# The test (src/tests/baruch_test_pathological.F90, with --timing) builds every case at n and 10n, loads, parses and
# renders it three times at each size (system_clock, best of 3), checks the HTML as cmark's test does, and prints one
# Markdown table row per case: case | n | bytes(n) | t(n) ms | t(10n) ms | ratio | check. A case over R is measured
# again, up to 3 attempts, the last one printed with its number ("ok, attempt 2"): another process loading the machine
# stretches one measurement, a super-linear case fails them all. The exit status is 0 when every case is correct with
# a ratio <= R, 1 otherwise, 2 on a usage or build error. scripts/run_tests.sh runs the
# same test with no argument: every case once at a small size, checked, not timed (no timing assertion in CI).
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root" || exit 2

usage() { sed -n '2,/^[^#]/{/^#/{s/^# \{0,1\}//;p}}' "${BASH_SOURCE[0]}"; }
die() { printf 'pathological.sh: %s\n' "$1" >&2; exit 2; }

mode=tests-gnu
build=1
opts=(--timing --max-ratio 15)
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode ) [[ $# -ge 2 ]] || die '--mode needs a fobos mode'; mode=$2; shift 2 ;;
    --no-build ) build=0; shift ;;
    --n ) [[ $# -ge 2 && "$2" =~ ^[0-9]+$ ]] || die '--n needs a number'; opts+=(--n "$2"); shift 2 ;;
    --max-ratio ) [[ $# -ge 2 && "$2" =~ ^[0-9]+([.][0-9]+)?$ ]] || die '--max-ratio needs a number'
                  opts+=(--max-ratio "$2"); shift 2 ;;
    --case ) [[ $# -ge 2 ]] || die '--case needs a name'; opts+=(--case "$2"); shift 2 ;;
    -h | --help ) usage; exit 0 ;;
    * ) die "unknown argument $1 (--help)" ;;
  esac
done

out=build/pathological/$mode
test=$out/baruch_test_pathological
if [[ $build -eq 1 ]]; then
  log=$(fobis build --mode "$mode" --target src/tests/baruch_test_pathological.F90 --build-dir "$out/" 2>&1) || {
    printf '%s\n' "$log" >&2; die "the test does not build with --mode $mode"; }
fi
[[ -x "$test" ]] || die "no test $test (build it: drop --no-build)"

printf 'pathological inputs, mode %s, %s\n' "$mode" "$(date -u +%Y-%m-%d)"
# a later --max-ratio overrides the default one: the test reads its arguments in order
"$test" "${opts[@]}"
