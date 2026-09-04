# Newly diagnosed multiple myeloma: index individual patient data (survival)

Individual patient data for the index arm (McCarthy 2012, lenalidomide),
the survival (progression-free survival) ML-UMR worked example; pair
with [`ndmm_agd`](https://choxos.github.io/mlumr/reference/ndmm_agd.md)
and
[`ndmm_agd_covs`](https://choxos.github.io/mlumr/reference/ndmm_agd_covs.md).

## Usage

``` r
ndmm_ipd
```

## Format

A data frame with 231 rows and 9 columns:

- study, treatment:

  study and treatment labels

- subject:

  row identifier

- age, iss_stage3, response_cr_vgpr, male:

  patient covariates

- eventtime, status:

  progression-free survival time and event indicator (1 = event, 0 =
  censored)

## Source

The McCarthy-2012 lenalidomide arm of
[`multinma::ndmm_ipd`](https://dmphillippo.github.io/multinma/reference/ndmm.html)
(GPL-3; Phillippo, *multinma*, 2024), subset to the columns used in the
survival vignette. multinma provides *simulated* IPD resembling
published newly-diagnosed-multiple-myeloma trials; the vignette compares
lenalidomide (McCarthy et al. 2012) with thalidomide (Morgan et al.
2012). See `data-raw/prepare_multinma_subsets.R`.

## Details

The two source trials are not genuinely disconnected: McCarthy 2012 and
Morgan 2012 both include a placebo arm. Those arms are omitted here
deliberately, so that this pair of datasets poses the unanchored problem
ML-UMR exists for.
[`vignette("survival-outcomes")`](https://choxos.github.io/mlumr/articles/survival-outcomes.md)
puts them back at the end to check the unanchored estimate against the
anchored one.

McCarthy 2012 is the index arm because population overlap, not sample
size, governs how far an unanchored comparison has to extrapolate.
Unanchored MAIC effective sample size against the Morgan 2012
thalidomide arm is 35.8 of 231 (15.5\\ lenalidomide arm, the other
candidate for this comparison. See `data-raw/prepare_multinma_subsets.R`
for the full pair ranking.
