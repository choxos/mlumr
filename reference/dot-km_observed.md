# Observed Kaplan-Meier step + censoring data for a survival mlumr_data

Each selected cohort is fitted on its own, so a single cohort is a
single curve and nothing depends on the `strata` a multi-curve fit
carries, and the two cohorts stay apart when their treatment labels
coincide.

## Usage

``` r
.km_observed(data, population = c("Index", "Comparator"))
```

## Arguments

- data:

  A survival `mlumr_data`.

- population:

  The cohorts to fit, `"Index"` and/or `"Comparator"`.
