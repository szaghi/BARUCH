#!/usr/bin/env bash
# Fuzz the reader and the writer for long: mutations of the CommonMark examples and random trees (plan szaghi/BARUCH#1,
# step 3.3; reference page docs/reference/conformance.md, "Fuzzing").
#
# Usage: bash scripts/fuzz.sh [N] [SEED] [OPTIONS]
#
# N        mutations and trees to run, each (default 1000000).
# SEED     the seed, an integer >= 0 (default: drawn from /dev/urandom). The same seed gives the same inputs with every
#          compiler; the seed is printed first, so that any run can be repeated.
#
# Options
#   --mode MODE   fobos mode that compiles the test (default tests-gnu; tests-nvf, tests-intel, the -debug ones). The
#                 test alone is built (--target), into build/fuzz/MODE/ (--build-dir): exe/ and the other compilers'
#                 objects are never touched.
#   --no-build    run the test already built for MODE.
#   --progress K  a progress line every K iterations of each fuzzer (default 10000).
#   -h, --help    this text.
#
# The test (src/tests/baruch_test_fuzz.F90) checks every input: no load, parse, render, walk or write fails, every
# output is well-formed UTF-8, each Markdown written (recorded and canonical style) reads back to the same tree. A
# failing input is shrunk and printed, and saved with its shrunk form in build/fuzz/MODE/failures-SEED/ (the first 20,
# then counted). A crash (no summary line) is located: the block of iterations after the last progress line is run
# again with --trace, which writes each input to a file before checking it; the crashing input is left in
# failures-SEED/crash-mutation.md (bytes) or crash-tree.txt (the tree dumped). The last line is the test's summary:
# "fuzz: seed S, mutations 1-N, trees N (R redrawn), digest D, F failure(s)".
#
# Exit status: 0 when every input passes, 1 when one fails or the test crashes, 2 on a usage or build error.
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root" || exit 2

usage() { sed -n '2,/^[^#]/{/^#/{s/^# \{0,1\}//;p}}' "${BASH_SOURCE[0]}"; }
die() { printf 'fuzz.sh: %s\n' "$1" >&2; exit 2; }

mode=tests-gnu
build=1
progress=10000
positional=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode ) [[ $# -ge 2 ]] || die '--mode needs a fobos mode'; mode=$2; shift 2 ;;
    --no-build ) build=0; shift ;;
    --progress ) [[ $# -ge 2 && "$2" =~ ^[1-9][0-9]*$ ]] || die '--progress needs a positive integer'
                 progress=$2; shift 2 ;;
    -h | --help ) usage; exit 0 ;;
    -* ) die "unknown option $1 (--help)" ;;
    * ) positional+=("$1"); shift ;;
  esac
done
[[ ${#positional[@]} -le 2 ]] || die 'too many arguments (--help)'
n=${positional[0]:-1000000}
[[ "$n" =~ ^[0-9]+$ ]] || die "N must be an integer >= 0, not $n"
if [[ ${#positional[@]} -ge 2 ]]; then
  seed=${positional[1]}
  [[ "$seed" =~ ^[0-9]+$ ]] || die "SEED must be an integer >= 0, not $seed"
else
  seed=$(od -An -N4 -tu4 /dev/urandom | tr -d ' ')
fi

out=build/fuzz/$mode
test=$out/baruch_test_fuzz
if [[ $build -eq 1 ]]; then
  log=$(fobis build --mode "$mode" --target src/tests/baruch_test_fuzz.F90 --build-dir "$out/" 2>&1) || {
    printf '%s\n' "$log" >&2; die "the test does not build with --mode $mode"; }
fi
[[ -x "$test" ]] || die "no test $test (build it: drop --no-build)"
failures=$out/failures-$seed
mkdir -p "$failures" || die "cannot create $failures"

printf 'fuzz: mode %s, seed %s, %s mutations, %s trees, %s\n' "$mode" "$seed" "$n" "$n" "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
log=$out/fuzz-$seed.log
start_time=$(date +%s)
"$test" --seed "$seed" --mutations "$n" --trees "$n" --progress "$progress" --out "$failures" 2>&1 | tee "$log"
status=${PIPESTATUS[0]}
printf 'fuzz: %s s\n' "$(( $(date +%s) - start_time ))"
grep -q '^fuzz: seed' "$log" && exit "$status"

# a crash: run again the block after the last progress line, each input traced
printf 'fuzz.sh: the test stopped with status %s and no summary: locating the input\n' "$status" >&2
last_m=$(grep -o '^mutations: [0-9]*' "$log" | tail -n 1 | grep -o '[0-9]*$')
last_t=$(grep -o '^trees: [0-9]*' "$log" | tail -n 1 | grep -o '[0-9]*$')
if [[ -z "$last_t" && "${last_m:-0}" -lt "$n" ]]; then
  phase=mutation; first=$(( ${last_m:-0} + 1 )); opts=(--mutations "$progress" --trees 0)
else
  phase=tree; first=$(( ${last_t:-0} + 1 )); opts=(--mutations 0 --trees "$progress")
fi
trace=$failures/crash-$phase$([[ $phase == tree ]] && printf '.txt' || printf '.md')
"$test" --seed "$seed" --start "$first" "${opts[@]}" --trace "$trace" > "$failures/crash.log" 2>&1
printf 'fuzz.sh: the %s being checked when it crashed (iterations from %s): %s, the run: %s\n' "$phase" "$first" \
  "$trace" "$failures/crash.log" >&2
exit 1
