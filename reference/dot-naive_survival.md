# Naive comparison for survival outcomes

Unadjusted Cox proportional-hazards log hazard ratio comparing the index
IPD against the reconstructed comparator pseudo-IPD, plus Kaplan-Meier
median survival per arm. Because this benchmark is a right-censored Cox
model, left- and interval-censored records (internal status 2/3) are
rejected rather than collapsed to right-censoring.

## Usage

``` r
.naive_survival(data, conf_level, z)
```
