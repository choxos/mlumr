# Dental caries: simulated comparator aggregate data (count)

Aggregate summary of the comparator treatment (nano-silver fluoride,
NSF) for the count ML-UMR example; pair with
[`caries_ipd`](https://choxos.github.io/mlumr/reference/caries_ipd.md).
Not real patient data.

## Usage

``` r
caries_agd
```

## Format

A data frame with 1 row and 10 columns: `study`, `treatment`, `n`, count
outcome `r` (total dmft) with exposure `E` (n children), and covariate
summaries (`age_mean`/`age_sd`, `gender_prop` (a proportion),
`log_cfu_mean`/`log_cfu_sd`).

## Source

Simulated; see
[`caries_ipd`](https://choxos.github.io/mlumr/reference/caries_ipd.md)
for the modeling basis.
