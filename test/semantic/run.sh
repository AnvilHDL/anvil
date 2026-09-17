#!/usr/bin/env bash
# Semantic tests of the assertion language against hand-written reference SVA.
# See README.md in this directory.  Usage:  bash run.sh [case ...]
#
# For each case directory <case>/ :
#   dut.anvil               program with assertions (label under test in expect.txt)
#   mutant.anvil            optional: program on which the property must be REFUTED
#   reference.sv            wrapper module ref_top with  property P_ref  and  ref_verdict: assert property (P_ref);
#                           optionally  wire w_ref = ...;  (propositional cases, for the miter)
#   reference_mutant.sv     optional: reference used for the mutant instead of reference.sv
#   expect.txt              key=value: label, bound, instance (default dut), dut, mutant
#
# Levels (one line per case and level):
#   verdict      EBMC verdict of the generated property == verdict of P_ref == expected, on dut and mutant
#   implication  (P_gen implies P_ref) and (P_ref implies P_gen) PROVED up to bound   [temporal bodies]
#   miter        assert property (w_gen == w_ref), k-induction k=1 and BMC            [propositional bodies]
# Status: PASS / FAIL / UNSUPPORTED (compiler does not accept the program yet) / SKIP (level not applicable).
# Exit code 1 if any level FAILs.

set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
OUT="${OUT:-$HERE/_out}"
EBMC="${EBMC:-ebmc}"
if [ -z "${ANVIL:-}" ]; then
  if command -v anvil >/dev/null 2>&1; then ANVIL=anvil
  elif [ -x "$ROOT/_build/default/bin/main.exe" ]; then ANVIL="$ROOT/_build/default/bin/main.exe"
  else echo "no anvil compiler found (set ANVIL=...)" >&2; exit 2; fi
fi

n_pass=0; n_fail=0; n_unsup=0; n_skip=0

report() {  # case level status detail
  printf '%-12s %-12s %-12s %s\n' "$1" "$2" "$3" "$4"
  case "$3" in
    PASS) n_pass=$((n_pass+1));; FAIL) n_fail=$((n_fail+1));;
    UNSUPPORTED) n_unsup=$((n_unsup+1));; SKIP) n_skip=$((n_skip+1));;
  esac
}

# ---------- helpers ----------

# read key from expect.txt (empty if absent)
expect_key() { sed -n "s/^[[:space:]]*$2[[:space:]]*=[[:space:]]*//p" "$1/expect.txt" | head -1 | sed 's/[[:space:]]*#.*$//; s/[[:space:]]*$//'; }

# the compiler connects spawned instances with the implicit named port
# connection shorthand (.clk_i, / .rst_ni), which EBMC 6.0 rejects; make it explicit
patch_ports() { sed -E 's/^([[:space:]]*)\.clk_i,[[:space:]]*$/\1.clk_i(clk_i),/; s/^([[:space:]]*)\.rst_ni[[:space:]]*$/\1.rst_ni(rst_ni)/' "$1" > "$2"; }

# does the program use a temporal operator (N/G/F prefix inside an assertion body)?
uses_temporal() { grep -qE '(\(|&&|\|\||!)[[:space:]]*[NGF][[:space:]]+' "$1"; }

# compile $1 -> $2 (patched). Prints: ok | unsupported | error
compile() {
  local src="$1" dst="$2" raw="$2.raw" err="$2.err"
  if "$ANVIL" "$src" > "$raw" 2> "$err"; then
    patch_ports "$raw" "$dst"; echo ok
  elif uses_temporal "$src" && grep -q 'Syntax error' "$err"; then
    echo unsupported
  else
    echo error
  fi
}

# normalise an EBMC verdict string
norm() {
  case "$1" in
    "PROVED up to bound"*|"PROVED ("*|PROVED) echo PROVED;;
    REFUTED*) echo REFUTED;;
    "") echo MISSING;;
    *) echo "$1";;
  esac
}

# run ebmc on files $2.. with flags in $EBMC_FLAGS; result lines to $1
run_ebmc() {
  local log="$1"; shift
  # shellcheck disable=SC2086
  "$EBMC" "$@" --top ref_top --reset 'ref_top.rst_ni==0' $EBMC_FLAGS > "$log" 2>&1
  return 0
}

# verdict of property whose name ends in .$2 (or equals $2), from log $1
verdict_of() {
  sed -n -E "s/^\[(ref_top\.)?([A-Za-z0-9_\$.]*\.)?$2\] .*: ([A-Z][A-Za-z0-9 ()-]*)$/\3/p" "$1" | head -1
}

# prefix identifiers of an SVA body with $2. (hierarchical path); keywords and
# based literals (8'd1) are left alone
prefix_ids() {
  perl -pe 'BEGIN { $p = shift; $kw = q{always|s_always|nexttime|s_nexttime|eventually|s_eventually|until|s_until|until_with|s_until_with|not|and|or|implies|iff|strong|weak|if|else|throughout|within|intersect|first_match} }
            s/(?<![\w\$\x27.])(?!(?:$kw)(?![\w\$]))([A-Za-z_][\w\$]*)/$p.$1/g' "$1" "$2"
}

# reference with module items injected before the final endmodule
inject() {  # reference.sv  injected-text  -> stdout   (text goes through a file: BSD awk rejects newlines in -v)
  local f; f="$(mktemp)"; printf '%s\n' "$2" > "$f"
  awk -v inj="$f" '/^[[:space:]]*endmodule/{last=NR} {l[NR]=$0}
       END{for(i=1;i<=NR;i++){if(i==last){while((getline x < inj)>0)print x}; print l[i]}}' "$1"
  rm -f "$f"
}

# ---------- one case ----------

run_case() {
  local c="$1" dir="$HERE/$1" o="$OUT/$1"
  mkdir -p "$o"
  if [ ! -f "$dir/expect.txt" ] || [ ! -f "$dir/dut.anvil" ] || [ ! -f "$dir/reference.sv" ]; then
    report "$c" all FAIL "case incomplete (needs dut.anvil, reference.sv, expect.txt)"; return
  fi
  local label bound inst
  label="$(expect_key "$dir" label)"; bound="$(expect_key "$dir" bound)"; inst="$(expect_key "$dir" instance)"
  inst="${inst:-dut}"
  if [ -z "$label" ] || [ -z "$bound" ]; then report "$c" all FAIL "expect.txt needs label= and bound="; return; fi

  # targets: dut, and mutant if present
  local targets="dut"; [ -f "$dir/mutant.anvil" ] && targets="dut mutant"

  # compile all targets first
  local t st unsupported="" comp_fail=""
  for t in $targets; do
    st="$(compile "$dir/$t.anvil" "$o/$t.sv")"
    case "$st" in
      unsupported) unsupported="$unsupported $t";;
      error) comp_fail="$comp_fail $t";;
    esac
  done
  if [ -n "$comp_fail" ]; then
    local why; why="$(grep -v '^Compilation failed' "$o/${comp_fail# }.sv.err" | head -2 | tr '\n' ' ')"
    for lv in verdict implication miter; do report "$c" $lv FAIL "compile error ($comp_fail):$why"; done; return
  fi
  if [ -n "$unsupported" ]; then
    for lv in verdict implication miter; do report "$c" $lv UNSUPPORTED "compiler: unsupported (temporal operator N/G/F does not parse yet;$unsupported)"; done; return
  fi

  # the generated property for the label, per target: kind_<t> (temporal|immediate) and body_<t> (SVA body | wire)
  local kind_dut="" body_dut="" kind_mutant="" body_mutant="" kind="" body=""
  for t in $targets; do
    local line
    line="$(grep -E "^[[:space:]]*$label: assert property \(.*\);[[:space:]]*$" "$o/$t.sv" | head -1)"
    if [ -n "$line" ]; then
      body="$(printf '%s' "$line" | sed -E "s/^[[:space:]]*$label: assert property \((.*)\);[[:space:]]*$/\1/")"
      # every assertion is now one property "rst_ni && <bit> |-> S"; a consequent that is a
      # bare wire is a propositional body: keep its wire for the miter level
      local conseq
      conseq="$(printf '%s' "$body" | sed -E 's/^.*\|-> *//')"
      if printf '%s' "$conseq" | grep -qE '^thread_[0-9]+_wire\$[0-9]+$'; then
        eval "kind_$t=immediate; body_$t=\$conseq"
      else
        eval "kind_$t=temporal; body_$t=\$body"
      fi
    else
      line="$(grep -E "^[[:space:]]*$label: assert \(thread_[0-9]+_wire\\\$[0-9]+\);[[:space:]]*$" "$o/$t.sv" | head -1)"
      if [ -n "$line" ]; then
        body="$(printf '%s' "$line" | sed -E "s/^[[:space:]]*$label: assert \((.*)\);[[:space:]]*$/\1/")"
        eval "kind_$t=immediate; body_$t=\$body"
      else
        for lv in verdict implication miter; do report "$c" $lv FAIL "no 'assert' with label $label in generated SV ($t)"; done; return
      fi
    fi
    printf '%s\n' "$line" > "$o/$t.gen_property.txt"
  done

  local ref_dut="$dir/reference.sv" ref_mutant="$dir/reference.sv" ref=""
  [ -f "$dir/reference_mutant.sv" ] && ref_mutant="$dir/reference_mutant.sv"
  # per-target lookups: sets kind, body, ref for target $1
  pick() { eval "kind=\$kind_$1; body=\$body_$1; ref=\$ref_$1"; }

  # ---- level 1: verdict ----
  local ok=1 detail=""
  for t in $targets; do
    local exp; exp="$(expect_key "$dir" "$t")"
    pick "$t"
    EBMC_FLAGS="--bound $bound" run_ebmc "$o/$t.verdict.log" "$o/$t.sv" "$ref"
    local vg vr; vg="$(norm "$(verdict_of "$o/$t.verdict.log" "$label")")"; vr="$(norm "$(verdict_of "$o/$t.verdict.log" ref_verdict)")"
    detail="$detail$t: gen=$vg ref=$vr (expect $exp); "
    [ "$vg" = "$vr" ] && [ "$vg" = "$exp" ] || ok=0
  done
  if [ $ok = 1 ]; then report "$c" verdict PASS "$detail"; else report "$c" verdict FAIL "$detail"; fi

  # ---- level 2: implication (temporal bodies) ----
  ok=1; detail=""
  local any_temporal=0
  for t in $targets; do pick "$t"; [ "$kind" = temporal ] && any_temporal=1; done
  if [ $any_temporal = 0 ]; then
    report "$c" implication SKIP "immediate check ($body_dut); equivalence is the miter level"
  else
    for t in $targets; do
      pick "$t"
      if [ "$kind" != temporal ]; then detail="$detail$t: immediate, skipped; "; continue; fi
      local pgen; pgen="$(printf '%s' "$body" | prefix_ids "$inst" /dev/stdin)"
      local inj
      inj=$(printf '  // injected by run.sh: the generated property, identifiers prefixed with %s.\n  property P_gen; %s; endproperty\n  gen_implies_ref: assert property (P_gen implies P_ref);\n  ref_implies_gen: assert property (P_ref implies P_gen);' "$inst" "$pgen")
      inject "$ref" "$inj" > "$o/$t.implication.sv"
      EBMC_FLAGS="--bound $bound" run_ebmc "$o/$t.implication.log" "$o/$t.sv" "$o/$t.implication.sv"
      local v1 v2; v1="$(norm "$(verdict_of "$o/$t.implication.log" gen_implies_ref)")"; v2="$(norm "$(verdict_of "$o/$t.implication.log" ref_implies_gen)")"
      detail="$detail$t: gen=>ref $v1, ref=>gen $v2; "
      [ "$v1" = PROVED ] && [ "$v2" = PROVED ] || ok=0
    done
    if [ $ok = 1 ]; then report "$c" implication PASS "$detail"; else report "$c" implication FAIL "$detail"; fi
  fi

  # ---- level 3: miter (propositional bodies) ----
  ok=1; detail=""
  local any_imm=0
  for t in $targets; do pick "$t"; [ "$kind" = immediate ] && any_imm=1; done
  if [ $any_imm = 0 ]; then
    report "$c" miter SKIP "temporal body; equivalence is the implication level"
  else
    for t in $targets; do
      pick "$t"
      if [ "$kind" != immediate ]; then detail="$detail$t: temporal, skipped; "; continue; fi
      if ! grep -qE '^[[:space:]]*wire[[:space:]]+w_ref[[:space:]]*=' "$ref"; then
        detail="$detail$t: no w_ref in reference; "; ok=0; continue
      fi
      local wgen="$inst.$body"
      local inj
      inj=$(printf '  // injected by run.sh: the wire of the generated propositional check against the reference wire\n  miter: assert property (%s == w_ref);' "$wgen")
      inject "$ref" "$inj" > "$o/$t.miter.sv"
      EBMC_FLAGS="--k-induction --bound 1" run_ebmc "$o/$t.miter.kind.log" "$o/$t.sv" "$o/$t.miter.sv"
      EBMC_FLAGS="--bound $bound" run_ebmc "$o/$t.miter.bmc.log" "$o/$t.sv" "$o/$t.miter.sv"
      local vk vb; vk="$(norm "$(verdict_of "$o/$t.miter.kind.log" miter)")"; vb="$(norm "$(verdict_of "$o/$t.miter.bmc.log" miter)")"
      detail="$detail$t: $wgen==w_ref k-ind $vk, bmc $vb; "
      [ "$vk" = PROVED ] && [ "$vb" = PROVED ] || ok=0
    done
    if [ $ok = 1 ]; then report "$c" miter PASS "$detail"; else report "$c" miter FAIL "$detail"; fi
  fi
}

# ---------- main ----------

command -v "$EBMC" >/dev/null 2>&1 || { echo "ebmc not found (set EBMC=...)" >&2; exit 2; }
mkdir -p "$OUT"
if [ $# -gt 0 ]; then cases="$*"; else
  cases="$(cd "$HERE" && for d in */; do d="${d%/}"; [ -f "$d/expect.txt" ] && echo "$d"; done)"
fi
printf '%-12s %-12s %-12s %s\n' CASE LEVEL STATUS DETAIL
for c in $cases; do
  [ -d "$HERE/$c" ] || { report "$c" all FAIL "no such case"; continue; }
  run_case "$c"
done
echo "semantic: $n_pass PASS, $n_fail FAIL, $n_unsup UNSUPPORTED, $n_skip SKIP  (outputs in $OUT)"
[ $n_fail = 0 ]
