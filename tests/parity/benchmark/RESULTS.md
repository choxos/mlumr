# Benchmark results: v0.2.1 armfast against the frozen baseline

These measurements were taken before promotion, with the prototype's benchmark
scripts, which are not part of this tree. The candidate models benchmarked are
byte-identical to the survival models now in `inst/stan`. The baseline was
pinned at `4e650fd`; it differs from `survival-baseline-6a366a0/` only by two
include helpers that no model calls. Ratios below are baseline time over
candidate time, so a ratio of 1.27 means the candidate takes 21 percent less
time (1 - 1/1.27).

Run on 2026-09-15 on an otherwise idle Apple Silicon machine (macOS 26.6.1,
R 4.6.0, CmdStan 2.39.0, cmdstanr 0.9.0; `results/sessionInfo.txt`).
Workloads were built with mlumr 0.1.0.9000 at
`4e650fd2fff20fa013375521207b39898b254750` by
`build-benchmark-workloads.R`: 150 IPD rows and one comparator arm in every
case, simulated with seed 2026 through the public setup route. Both sides
compiled from their own source with the same CmdStan flags; the benchmark
process never loads mlumr. Raw measurements: `results/timing.csv` (10
replicates per side per quantity per workload, alternating blocks),
`results/memory.csv`, `results/fits.csv`; summaries in
`results/timing_summary.csv` and `results/fits_summary.csv`.

## 1. Fixed-point evaluation

Seconds per call at fixed finite unconstrained points in one process after
warm-up, medians of 10 replicates. Ratio is baseline over candidate, so 1.10
means the baseline call took 10 percent longer than the candidate call.

| workload                     | baseline ms per gradient | log density | gradient | generated quantities |
|------------------------------|-------------------------:|------------:|---------:|---------------------:|
| weibull_spfa_n50_int16       |                    0.154 |        1.12 |     1.13 |                 1.00 |
| weibull_spfa_n50_int64       |                    0.448 |        1.09 |     1.07 |                 1.02 |
| weibull_spfa_n50_int256      |                    1.65  |        1.07 |     1.05 |                 1.01 |
| weibull_spfa_n500_int16      |                    1.34  |        1.19 |     1.27 |                 1.00 |
| weibull_spfa_n500_int64      |                    4.51  |        1.08 |     1.11 |                 1.04 |
| weibull_spfa_n500_int256     |                   18.5   |        1.10 |     1.07 |                 0.99 |
| weibull_spfa_n2000_int16     |                    4.85  |        1.14 |     1.17 |                 1.10 |
| weibull_spfa_n2000_int64     |                   18.7   |        1.08 |     1.08 |                 1.07 |
| weibull_spfa_n2000_int256    |                   72.2   |        1.09 |     1.08 |                 1.03 |
| lognormal_spfa_n500_int64    |                    2.95  |        1.14 |     1.14 |                 1.01 |
| gengamma_relaxed_n500_int64  |                    7.90  |        1.11 |     1.06 |                 1.00 |
| mspline_spfa_n500_int64      |                    2.08  |        1.21 |     1.19 |                 1.03 |
| pexp_relaxed_n500_int64      |                    2.16  |        1.23 |     1.18 |                 1.02 |

Reading: every workload's gradient and log-density calls are cheaper on the
candidate: the candidate takes about 5 to 21 percent less time (the
baseline takes 5 to 27 percent longer than the candidate). The gain is largest
where `n_int` is small relative to `n_agd` (`n500_int16`: 1.27), because the
removed per-pseudo-individual matrix-vector products are then the largest
share of the evaluation, and it shrinks as `n_int` grows (`n500_int256`:
1.07), because the `n_agd * n_int` kernel calls that the change leaves in
place dominate. The generated quantities are unchanged in cost within noise
(0.99 to 1.10; they do not run the reverse sweep). The timings are
sub-millisecond on the small workloads; the replicate spread is in
`results/timing.csv`.

## 2. Peak resident memory

One short sampling run of each executable on `weibull_spfa_n500_int64`, peak
resident set size from `/usr/bin/time -l`:

| side      | peak RSS |
|-----------|---------:|
| baseline  |  33.6 MB |
| candidate |  30.3 MB |

The change replaces `n_agd` products `Xq_int[a] * beta_tilde` per
evaluation by `n_agd_rows` of them, so the reverse-mode stack holds fewer
nodes per evaluation; the direction of the difference agrees with that, but
one short run per side is within what two runs of one executable show on
this machine, so no memory effect is claimed.

## 3. Full fits

Two chains, 500 warmup and 500 sampling iterations, `adapt_delta` 0.95,
`max_treedepth` 15, three seeds, identical data and settings on both sides.
ESS is for the population effect `delta_comparator`. No divergences and no
treedepth saturation in any fit.

| workload                 | seed | side      | total s | bulk ESS | tail ESS | bulk ESS/s |
|--------------------------|-----:|-----------|--------:|---------:|---------:|-----------:|
| weibull_spfa_n500_int64  | 2026 | baseline  |   125.3 |     1043 |      824 |       8.32 |
| weibull_spfa_n500_int64  | 2026 | candidate |   145.3 |     1043 |      824 |       7.18 |
| weibull_spfa_n500_int64  | 2027 | baseline  |   164.6 |      620 |      544 |       3.77 |
| weibull_spfa_n500_int64  | 2027 | candidate |   161.5 |      620 |      544 |       3.84 |
| weibull_spfa_n500_int64  | 2028 | baseline  |   179.9 |      947 |      724 |       5.27 |
| weibull_spfa_n500_int64  | 2028 | candidate |   169.7 |      947 |      724 |       5.58 |
| mspline_spfa_n500_int64  | 2026 | baseline  |   817.2 |      680 |     1027 |       0.83 |
| mspline_spfa_n500_int64  | 2026 | candidate |   595.9 |      884 |      945 |       1.48 |
| mspline_spfa_n500_int64  | 2027 | baseline  |   383.3 |      932 |      670 |       2.43 |
| mspline_spfa_n500_int64  | 2027 | candidate |   542.7 |     1030 |      790 |       1.90 |
| mspline_spfa_n500_int64  | 2028 | baseline  |   436.0 |     1064 |      977 |       2.44 |
| mspline_spfa_n500_int64  | 2028 | candidate |   464.7 |     1047 |     1013 |       2.25 |

Reading:

* On the Weibull workload the two sides produced the same trajectories on
  all three seeds (posterior means agree to eleven digits, ESS to the last
  digit), so the wall-time ratio measures the per-evaluation cost inside the
  sampler directly: 1.02 and 1.06 in favor of the candidate on seeds 2027 and
  2028, and 0.86 against it on seed 2026, the first pair run. That spread is
  run-to-run variation on this machine, larger than the 11 percent
  fixed-point gain on the same workload. The full-fit evidence is therefore
  consistent with a small gain and does not measure one.
* On the M-spline workload the trajectories differ between sides (the graph
  change reorders additions, so NUTS paths diverge under one seed), and the
  seed-to-seed variation of one side (383 to 817 s on the baseline) is larger
  than any difference between sides. Median bulk ESS per second is 2.43
  baseline and 1.90 candidate; the means of total time are 546 s and 534 s.
  Three seeds do not separate the sides here.

## What can be said

The predictor reuse makes each log-density and gradient evaluation take about
5 to 21 percent less time across the thirteen workloads, with the size of the gain set
by `n_int` relative to `n_agd` as the mechanism predicts. That is the whole
measured effect. It does not show up as a wall-time or ESS-per-second
difference in six full fits per workload on this machine, where run-to-run
and seed-to-seed variation are larger than the per-evaluation saving. Any
statement outside this file should say "about 5 to 21 percent less time per
evaluation on the benchmark workloads" and should not name a fit-level
speedup.
