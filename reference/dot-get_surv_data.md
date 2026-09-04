# Parse survival outcome columns into mlumr's internal contract

Maps either a
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) object
or character column names to the internal columns `.time`,
`.start_time`, `.delay_time`, `.status` with status codes `0` =
right-censored, `1` = event, `2` = left-censored, `3` =
interval-censored.

## Usage

``` r
.get_surv_data(
  data,
  Surv = NULL,
  time = NULL,
  status = NULL,
  entry_time = NULL
)
```

## Arguments

- data:

  Source data frame (used for the character-column route).

- Surv:

  An optional
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object.

- time, status, entry_time:

  Character column names (character route). Only right-censoring (status
  `0`/`1`) and optional delayed entry are supported via this route; use
  a `Surv` object for left/interval censoring.

## Value

A data frame with `.time`, `.start_time`, `.delay_time`, `.status`.
