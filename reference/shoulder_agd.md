# Shoulder pain: simulated comparator aggregate data (continuous)

Aggregate summary of the comparator treatment (exercise therapy, ET) for
the continuous ML-UMR example; pair with
[`shoulder_ipd`](https://choxos.github.io/mlumr/reference/shoulder_ipd.md).
The two arms are treated as separate single-arm sources to illustrate an
unanchored comparison. Not real patient data.

## Usage

``` r
shoulder_agd
```

## Format

A data frame with 1 row and 10 columns: `study`, `treatment`, `n`,
outcome summary (`y_mean`, `y_se`) and covariate summaries
(`age_mean`/`age_sd`, `sex_prop` (a proportion),
`baseline_vas_mean`/`baseline_vas_sd`).

## Source

Simulated; see
[`shoulder_ipd`](https://choxos.github.io/mlumr/reference/shoulder_ipd.md)
for the modeling basis.
