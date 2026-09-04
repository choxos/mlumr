# Newly diagnosed multiple myeloma: comparator covariate summaries (survival)

Published covariate summaries for the comparator arm (Morgan 2012,
thalidomide), supplying the covariate moments for the AgD integration
points.

## Usage

``` r
ndmm_agd_covs
```

## Format

A data frame with 1 row and 7 columns: `study`, `treatment`, `age_mean`
/ `age_sd`, and the proportions `iss_stage3_prop`,
`response_cr_vgpr_prop`, `male_prop`.

## Source

The Morgan-2012 thalidomide row of
[`multinma::ndmm_agd_covs`](https://dmphillippo.github.io/multinma/reference/ndmm.html)
(GPL-3; Phillippo, *multinma*, 2024), subset to the columns used in the
survival vignette. See
[`ndmm_ipd`](https://choxos.github.io/mlumr/reference/ndmm_ipd.md) for
trial sources.
