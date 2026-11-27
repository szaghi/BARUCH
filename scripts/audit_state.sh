#!/usr/bin/env bash
# Audit the library for what would make it unsafe in MPI and OpenMP programs (settled design 8 of the plan): mutable
# module state and fixed units. Reused by plan step 4.2.
#
# Counted in every .F90/.f90 under the directory (default src/lib), on the code only (comments and the contents of
# string literals removed, continuation lines joined):
#   save                 a SAVE attribute or statement;
#   initialised          a variable initialised in its declaration (`integer :: n = 0`, `p => null()`), module or
#                        local: both have the SAVE attribute implicitly. Named constants (`parameter`) and the
#                        default values of derived-type components are not variables and are not counted;
#   units                a fixed or preconnected unit: a literal unit number or `*` in an I/O statement, `print`, or a
#                        reference to output_unit, error_unit, input_unit (units come from `newunit=` or the caller);
#   stop                 a STOP or ERROR STOP statement (errors are returned through stat/errmsg).
#
# Usage: bash scripts/audit_state.sh [DIR]   exit status 0 when every count is 0; the offending lines are listed.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
dir=${1:-$root/src/lib}
[ -d "$dir" ] || { echo "audit_state: no directory $dir" >&2; exit 2; }
mapfile -t files < <(find "$dir" -type f \( -name '*.F90' -o -name '*.f90' \) | sort)
[ "${#files[@]}" -gt 0 ] || { echo "audit_state: no Fortran sources in $dir" >&2; exit 2; }

awk '
# the code of a physical line: string contents and the comment removed, lower case
function code(line,    out, i, c, q, n) {
   out = ""; q = ""; n = length(line)
   for (i = 1; i <= n; i++) {
      c = substr(line, i, 1)
      if (q != "") {
         if (c == q) {
            if (substr(line, i + 1, 1) == q) { i++; continue } # a doubled quote inside the string
            q = ""; out = out c
         }
         continue
      }
      if (c == "!") break
      if (c == "\047" || c == "\"") { q = c; out = out c; continue }
      out = out c
   }
   return tolower(out)
}
function report(what, msg) { count[what]++; lines[what] = lines[what] sprintf("  %s:%d: %s\n", FILENAME, start, msg) }
FNR == 1 { stmt = ""; in_type = 0 }
/^[ \t]*#/ { next } # preprocessor
{
   c = code($0)
   if (stmt == "") start = FNR
   sub(/^[ \t]*&/, "", c)
   if (c ~ /&[ \t]*$/) { sub(/&[ \t]*$/, "", c); stmt = stmt c; next }
   stmt = stmt c
   s = stmt; stmt = ""
   if (s ~ /^[ \t]*$/) next
   # derived-type definitions: their components default values are not variables
   if (s ~ /^[ \t]*end[ \t]*type([ \t]|$)/) { in_type = 0; next }
   if (s ~ /^[ \t]*type[ \t]*(,|::)/ || s ~ /^[ \t]*type[ \t]+[a-z]/ && s !~ /^[ \t]*type[ \t]+is[ \t]*\(/) {
      in_type = 1; next
   }
   # save
   if (s ~ /(^|[^a-z0-9_%])save([^a-z0-9_]|$)/) report("save", s)
   # initialised variables
   if (!in_type && s ~ /^[ \t]*(integer|real|complex|logical|character|double[ \t]*precision|type[ \t]*\(|class[ \t]*\(|procedure[ \t]*\()/ \
       && index(s, "::") > 0) {
      attrs = substr(s, 1, index(s, "::") - 1)
      ents = substr(s, index(s, "::") + 2)
      gsub(/==|\/=|<=|>=/, "", ents)
      if (attrs !~ /parameter/ && ents ~ /=/) report("initialised", s)
   }
   # units
   if (s ~ /(^|[^a-z0-9_%])(open|close|read|write|inquire|rewind|backspace|endfile|flush|wait)[ \t]*\([ \t]*(unit[ \t]*=[ \t]*)?([0-9]+|\*)[ \t]*[,)]/ \
       || s ~ /^[ \t]*([0-9]+[ \t]+)?print([ \t]|\*|$)/ || s ~ /(^|[^a-z0-9_%])(output_unit|error_unit|input_unit)([^a-z0-9_]|$)/)
      report("units", s)
   # stop
   if (s ~ /(^|[^a-z0-9_%])(error[ \t]*)?stop([^a-z0-9_]|$)/) report("stop", s)
}
END {
   printf "audit_state: %d files\n", files
   printf "  SAVE attributes or statements:             %d\n", count["save"] + 0
   printf "  initialised variables (implicit SAVE):     %d\n", count["initialised"] + 0
   printf "  fixed or preconnected units:               %d\n", count["units"] + 0
   printf "  stop or error stop:                        %d\n", count["stop"] + 0
   printf "%s%s%s%s", lines["save"], lines["initialised"], lines["units"], lines["stop"]
   exit (count["save"] + count["initialised"] + count["units"] + count["stop"] > 0)
}
' files="${#files[@]}" "${files[@]}"
