# Survival parity gate

The four survival models in `inst/stan` compute each comparator arm's linear
predictor once per parameter evaluation and reuse it across the arm's
pseudo-individuals, a change suggested by Tim Disher (@timdisher). This
directory holds the evidence that the reuse changes nothing the models
compute, and the gate that keeps it that way. It is excluded from the package
build (`.Rbuildignore`).

* `survival-baseline-6a366a0/`: the four survival models and their eight
  includes as they were at commit `6a366a0`, before the reuse, with
  `MANIFEST.txt` listing their git blob hashes. These are frozen: the gate
  stops if one changes. They live outside `inst/stan` so that `rstantools`
  does not register them as package models.
* `build-armfast-cases.R` and `armfast-fixtures.R`: build 52 Stan data
  fixtures through the public route (`set_ipd()`, `set_agd_surv()`,
  `combine_data()`, `add_integration()`, `mlumr()`) with the sampler replaced
  by a capture, from the source tree this directory belongs to.
* `armfast-parity-gate.R` and `armfast-cmdstan.R`: compile both sides with
  CmdStan and compare, at the same unconstrained points, the log density and
  its gradient (with and without the Jacobian), the constrained parameters and
  every generated quantity, componentwise within `1e-8 + 1e-10 * max(|x|, |y|)`.
  `--mutations` also checks that deliberately wrong models are caught.
* `benchmark/`: the per-evaluation timing, memory and full-fit measurements
  behind the NEWS statement.

The source contract (each production model is the frozen model plus the reuse,
token for token) is `tests/repo/test-survival-armfast-contract.R`.

Run from the package root, in two processes:

```sh
Rscript tests/parity/build-armfast-cases.R /tmp/parity/cases.rds
Rscript tests/parity/armfast-parity-gate.R /tmp/parity/cases.rds /tmp/parity/out \
        --mutations --exe-dir /tmp/parity/exe
```

The gate never loads mlumr: cmdstanr's model methods link their own copy of
TBB, which cannot share a process with the package's. CI runs both steps in the
full-test-suite job.
