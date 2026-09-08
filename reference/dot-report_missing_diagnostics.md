# Say how many parameters had no diagnostic, rather than dropping them

A partly-missing column checked on its present entries alone reads
exactly like a clean one. This follows the tail-ESS block below, which
already counts and reports what it could not check.

## Usage

``` r
.report_missing_diagnostics(d, label, what)
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

## Value

`NULL`, invisibly.
