# Plaque psoriasis: index individual patient data (binary)

Individual patient data for the index arm (UNCOVER-2, ixekizumab Q4W),
the binary (PASI 75 response) ML-UMR worked example; pair with
[`psoriasis_agd`](https://choxos.github.io/mlumr/reference/psoriasis_agd.md).

## Usage

``` r
psoriasis_ipd
```

## Format

A data frame with 347 rows and 8 columns:

- study, treatment:

  study and treatment labels

- subject:

  row identifier

- pasi75:

  binary PASI 75 response indicator

- age, bsa, weight, prevsys:

  patient covariates (age in years, body-surface area in percent, weight
  in kg, and previous systemic treatment as a 1/0 indicator)

`weight` is missing for 2 of the 347 rows, as it is in the multinma
source. [`set_ipd`](https://choxos.github.io/mlumr/reference/set_ipd.md)
drops incomplete rows with a warning, so a fit adjusting for weight uses
345 of them.

## Source

The UNCOVER-2 ixekizumab-Q4W arm of
[`multinma::plaque_psoriasis_ipd`](https://dmphillippo.github.io/multinma/reference/plaque_psoriasis.html)
(GPL-3; Phillippo, *multinma*, 2024,
[doi:10.5281/zenodo.3904454](https://doi.org/10.5281/zenodo.3904454) ),
subset to the columns used in the binary vignette. multinma provides
*simulated* IPD resembling the UNCOVER-2 / UNCOVER-3 (ixekizumab;
Griffiths et al. 2015,
[doi:10.1016/s0140-6736(15)60125-8](https://doi.org/10.1016/s0140-6736%2815%2960125-8)
) and secukinumab (Langley et al. 2014,
[doi:10.1056/nejmoa1314258](https://doi.org/10.1056/nejmoa1314258) )
plaque-psoriasis trials. See `data-raw/prepare_multinma_subsets.R`.

## Details

The two source trials are not genuinely disconnected: UNCOVER-2 and
FIXTURE both include placebo and etanercept arms. Those arms are omitted
here deliberately, so that this pair of datasets poses the unanchored
problem ML-UMR exists for.
[`vignette("binary-outcomes")`](https://choxos.github.io/mlumr/articles/binary-outcomes.md)
puts them back at the end to check the unanchored estimate against the
anchored one.
