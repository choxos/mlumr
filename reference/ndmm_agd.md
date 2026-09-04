# Newly diagnosed multiple myeloma: comparator pseudo-IPD (survival)

Reconstructed Kaplan-Meier pseudo-IPD for the comparator arm (Morgan
2012, thalidomide), the survival AgD comparator; pair with
[`ndmm_ipd`](https://choxos.github.io/mlumr/reference/ndmm_ipd.md) and
[`ndmm_agd_covs`](https://choxos.github.io/mlumr/reference/ndmm_agd_covs.md).

## Usage

``` r
ndmm_agd
```

## Format

A data frame with 408 rows and 5 columns: `study`, `treatment`,
`subject`, `eventtime`, `status`.

## Source

The Morgan-2012 thalidomide arm of
[`multinma::ndmm_agd`](https://dmphillippo.github.io/multinma/reference/ndmm.html)
(GPL-3; Phillippo, *multinma*, 2024); event/censoring times
reconstructed from published Kaplan-Meier curves. See
[`ndmm_ipd`](https://choxos.github.io/mlumr/reference/ndmm_ipd.md) for
trial sources.
