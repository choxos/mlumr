# Dental caries: simulated index IPD (count)

Simulated individual patient data for the index treatment (silver
diamine fluoride, SDF) in a count-outcome ML-UMR example; pair with
[`caries_agd`](https://choxos.github.io/mlumr/reference/caries_agd.md).
Not real patient data.

## Usage

``` r
caries_ipd
```

## Format

A data frame with 103 rows and 8 columns:

- study:

  study label (`"Ammar 2025"`)

- treatment:

  index treatment label (`"SDF"`)

- subject:

  row identifier

- age:

  age in years

- gender:

  1/0 indicator, as coded in the source trial, which does not record
  which level is which. Only the covariate's distribution matters for
  the adjustment, so the labeling does not affect the estimand.

- log_cfu:

  log baseline salivary *S. mutans* count, `log(CFU/mL + 1)`

- dmft:

  decayed-missing-filled-teeth count (the count outcome)

- exposure:

  Poisson offset, 1 per child. `dmft` is a whole-mouth count with no
  time at risk and no per-tooth denominator, so the rate is "dmft per
  child" and the offset is 1. The column is present because
  [`set_ipd`](https://choxos.github.io/mlumr/reference/set_ipd.md)
  requires `exposure` for the Poisson family; `log(1) = 0`, so it
  contributes nothing to the linear predictor. The comparator's `E` is
  the matching total, one unit per child.

`log_cfu` is bimodal: 9 of the 103 children (8.7\\ detectable baseline
count and so sit at exactly 0, and the rest are concentrated near 10.9
(SD 0.8). The comparator arm declares this covariate as a single normal
distribution (`log_cfu_mean` 11.0, `log_cfu_sd` 0.7) because the source
trial's comparator arm contains no such zeros. Integrating a normal over
the comparator therefore puts almost no weight where the index arm's
zero mode sits, so effects adjusted for `log_cfu` extrapolate over that
part of the covariate space. This is a property of the source trial, not
of the synthesis; treat it as a worked illustration of a covariate whose
arms are not fully overlapping.

## Source

Simulated (not real patient data), generated with the synthpop package
(sequential CART; Nowok, Raab and Dibben 2016,
[doi:10.18637/jss.v074.i11](https://doi.org/10.18637/jss.v074.i11) )
from the silver-diamine-fluoride vs nano-silver-fluoride dental caries
RCT (Ammar et al., *BMC Oral Health* 2025;25:945, CC BY 4.0; data
Synapse syn43185346), preserving the covariate relationships; fidelity
validated with the syntheticdata package. A plausible caries-arresting
SDF effect was imposed for illustration (dmft was a balanced baseline
characteristic in the source trial). See
`data-raw/simulate_external_data.R`.

## Details

The index and comparator arms come from the SAME trial, so both carry
the study label `"Ammar 2025"`. Splitting one randomized trial into a
single-arm IPD source and a single-arm aggregate source is what makes
this an unanchored example whose true answer is still known. Because the
label is shared,
[`combine_data`](https://choxos.github.io/mlumr/reference/combine_data.md)
warns that IPD and AgD come from the same study; that warning is
expected here and is the honest reading of the data. It does not fire
for
[`psoriasis_ipd`](https://choxos.github.io/mlumr/reference/psoriasis_ipd.md)
or [`ndmm_ipd`](https://choxos.github.io/mlumr/reference/ndmm_ipd.md),
whose arms really do come from different trials.
