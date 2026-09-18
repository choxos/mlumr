# Say how many parameters had no diagnostic, rather than dropping them

Names the outcome, not a cause: a missing diagnostic is as consistent
with a constant quantity as with stuck chains or non-finite draws.

## Usage

``` r
.report_missing_diagnostics(d, label, what, variables = NULL)
```

## Arguments

- d:

  A
  [`.usable_diagnostic_values()`](https://choxos.github.io/mlumr/reference/dot-usable_diagnostic_values.md)
  result.

- label:

  Diagnostic name for the message.

- what:

  What the diagnostic measures, for the message.

- variables:

  Parameter names in the same order as the column, or `NULL`.

## Value

`NULL`, invisibly.
