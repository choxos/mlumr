# Shoulder pain: simulated index IPD (continuous)

Simulated individual patient data for the index treatment (arthroscopic
subacromial decompression, ASD) in a continuous-outcome ML-UMR example.
Pair with
[`shoulder_agd`](https://choxos.github.io/mlumr/reference/shoulder_agd.md)
(the comparator). Not real patient data.

## Usage

``` r
shoulder_ipd
```

## Format

A data frame with 147 rows and 7 columns:

- study:

  study label (`"FIMPACT"`)

- treatment:

  index treatment label (`"ASD"`)

- subject:

  row identifier

- age:

  age in years

- sex:

  1 = male, 0 = female

- baseline_vas:

  baseline shoulder pain on activity (VAS 0-100)

- pain_vas_activity:

  shoulder pain on activity at 24 months (VAS 0-100)

## Source

Simulated (not real patient data), generated with the synthpop package
(sequential CART; Nowok, Raab and Dibben 2016,
[doi:10.18637/jss.v074.i11](https://doi.org/10.18637/jss.v074.i11) )
from the FIMPACT 10-year trial (BMJ 2025;391:e086201; dataset CC BY 4.0,
University of Helsinki / Finnish Ministry of Education open-data portal,
[doi:10.23729/fd-d323a34b-f698-3bc6-b38d-9c93aeadbe74](https://doi.org/10.23729/fd-d323a34b-f698-3bc6-b38d-9c93aeadbe74)
), preserving its covariate and covariate-outcome relationships;
fidelity validated with the syntheticdata package. See
`data-raw/simulate_external_data.R`.

## Details

The index and comparator arms come from the SAME trial, so both carry
the study label `"FIMPACT"`. Splitting one randomized trial into a
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
