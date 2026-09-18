# Note that absolute survival predictions transport a study-specific baseline

With one arm per study a study-specific shape and a treatment-specific
shape are aliased, so predicting a treatment in the other population
carries its study's shape across: an assumption the data cannot check.
Once per session, like the marginal-HR note.

## Usage

``` r
.transported_baseline_note(object)
```

## Arguments

- object:

  A fitted `mlumr_fit`.

## Value

`TRUE` invisibly if the note was emitted.
