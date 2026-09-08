# Say how many parameters had no diagnostic, rather than dropping them

A partly-missing column checked on its present entries alone reads
exactly like a clean one. This follows the tail-ESS block below, which
already counts and reports what it could not check.

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

## Details

The message names the OUTCOME and not a cause. A missing diagnostic has
several, and nothing here has looked at the draws to tell them apart: a
quantity that is constant by construction has no Rhat, and neither does
one whose chains are each stuck at a different constant, or whose draws
are not finite. Calling the benign one "the usual reason" turned an
unchecked parameter into a reassurance. Which parameters they were is
something this does know, so it says that instead.
