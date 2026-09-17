# Semantic tests: generated SVA against a hand-written reference

Kind 3 of the assertion test plan (`FYP/docs/assert-implementation-plan.md`, B.4).
The question answered per test program: *is the SVA the compiler emits for an
`assert` the same property as the one a person would write by hand over the
module's ports and event bits?*  The checker is EBMC 6.0 (`ebmc` on `PATH`).

    bash run.sh            # all cases
    bash run.sh tick3      # one case (or several)

One line per case and level, then a summary; exit status 1 if any level FAILs.
Intermediate files (compiled SV, EBMC logs, injected wrappers) are kept in
`_out/<case>/` (gitignored). Environment: `ANVIL` (compiler command, default
`anvil` on `PATH`, else `_build/default/bin/main.exe`), `EBMC`, `OUT`.

Status per line: `PASS`, `FAIL`, `UNSUPPORTED` (the compiler does not accept
the program yet, e.g. `N`/`G`/`F` do not parse), `SKIP` (the level does not
apply to this kind of body, see below).

## Layout of a case

    <case>/
      dut.anvil             the program, with assertions in the new language
      mutant.anvil          optional: a variant on which the property must be REFUTED
      reference.sv          hand-written wrapper `ref_top` (see below)
      reference_mutant.sv   optional: reference used for the mutant instead of reference.sv
      expect.txt            key = value lines

`expect.txt` keys: `label` (the assertion under test), `bound` (BMC bound),
`instance` (hierarchical path from `ref_top` to the module that holds the
assertion; default `dut`, e.g. `dut._spawn_0` for a spawned process), `dut`
and `mutant` (expected verdict, `PROVED` or `REFUTED`; `PROVED up to bound k`
counts as `PROVED`).

`reference.sv` must contain a module `ref_top` that instantiates the compiled
module as `dut` with explicit port connections, any environment `assume
property` the case needs, and

    property P_ref; <hand translation over dut.<signals>>; endproperty
    ref_verdict: assert property (P_ref);
    wire w_ref = <propositional body over dut.<signals>>;   // propositional cases only

`ref_top` must be the last module of the file: the runner injects further
module items before its final `endmodule`.  Every reference file starts with
the comment `REFERENCE — written by hand, to be reviewed by the project owner`;
the reference is the oracle and is not derived from compiler output.

The runner patches the compiler's implicit port connections (`.clk_i,` /
`.rst_ni`) of spawned instances to `.clk_i(clk_i)` / `.rst_ni(rst_ni)`, which
EBMC rejects otherwise, and runs EBMC with `--top ref_top --reset
'ref_top.rst_ni==0'`.

## The three levels

The generated property for `label` is found in the compiled SV by regex:
either `label: assert property (BODY);` (temporal body, a concurrent
property) or `label: assert (thread_N_wire$k);` (propositional body, an
immediate check inside the thread's `always_ff`, guarded by the event bit;
EBMC applies that guard).

| level | check | engine | applies to |
|---|---|---|---|
| `verdict` | verdict of the generated property = verdict of `P_ref` = expected, on `dut` and on `mutant` | BMC, `--bound` from expect.txt | all |
| `implication` | `assert property (P_gen implies P_ref)` and `(P_ref implies P_gen)` both PROVED, where `P_gen` is BODY with every identifier prefixed by `instance.` | BMC, same bound | temporal bodies (SKIP otherwise) |
| `miter` | `assert property (instance.thread_N_wire$k == w_ref)`, every cycle | `--k-induction --bound 1` (unbounded) and BMC | propositional bodies (SKIP otherwise) |

The verdict level catches gross errors (wrong event bit, wrong operator); the
mutant guards against a reference or a generated property that is vacuously
true.  The implication level catches any trace within the bound on which the
two properties differ.  The miter is exact equivalence of the atom and
connective lowering, proved by 1-induction.

## Limits

- Levels `verdict` and `implication` are bound-relative: `PROVED up to bound
  k` is not a proof, and a lasso counterexample to `s_eventually` needs a bound
  at least the length of the loop (`docs/model-checking-algorithms.md`, 6.2).
  `s_eventually` inside `implies` is checked by lasso BMC only.
- The reference is hand-written and can be wrong; that is why each case has a
  mutant, and why the header asks for review.
- For propositional atoms (`m?`, `m!`) the property is true by construction at
  the event where the recv/send completes, so a mutant can only move or change
  the assertion; such a mutant needs its own translation, `reference_mutant.sv`.
  Behavioural mutants (`tick3`: `cycle 2`; `eventually`: the consumer never
  answers) keep the assertion and share `reference.sv`.
- The mutant must keep the process name, since `reference.sv` instantiates the
  module by name.
- Event-bit indices in a reference are read off the generated SV of the
  program compiled *without* the temporal assertion (assertions add no events);
  each reference file records the `assign _thread_0_events[k] = ...` line it
  relies on.

## Adding a case

1. Create `<case>/dut.anvil` with a labelled assertion; optionally
   `mutant.anvil` (same process name) on which the property is false.
2. Compile `dut.anvil` (`anvil dut.anvil`) and find the event bit at which the
   assertion is placed (the `if (_thread_0_events[k])` guard of a propositional
   check, or the `assign _thread_0_events[k]` of the completion event).
3. Write `reference.sv` by hand from the language definition (`assert.typ`,
   2.3), over `dut.<port>` and `dut._thread_N_events[k]`; add `wire w_ref` for
   propositional bodies; add `reference_mutant.sv` if the mutant changes the
   assertion.
4. Write `expect.txt`; run `bash run.sh <case>`.
