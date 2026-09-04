# Plaque psoriasis: comparator aggregate data (binary)

Aggregate (arm-level) summary for the comparator arm (FIXTURE,
secukinumab 300 mg), the AgD comparator in the binary ML-UMR example;
pair with
[`psoriasis_ipd`](https://choxos.github.io/mlumr/reference/psoriasis_ipd.md).

## Usage

``` r
psoriasis_agd
```

## Format

A data frame with 1 row and 11 columns: `study`, `treatment`; response
counts `pasi75_r` / `pasi75_n`; and covariate summaries `age_mean` /
`age_sd`, `bsa_mean` / `bsa_sd`, `weight_mean` / `weight_sd`, and
`prevsys_prop` (a proportion).

## Source

The FIXTURE secukinumab-300 arm of
[`multinma::plaque_psoriasis_agd`](https://dmphillippo.github.io/multinma/reference/plaque_psoriasis.html)
(GPL-3; Phillippo, *multinma*, 2024), subset to the columns used in the
binary vignette. See
[`psoriasis_ipd`](https://choxos.github.io/mlumr/reference/psoriasis_ipd.md)
for the original trial sources.
