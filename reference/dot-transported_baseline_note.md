# Note that absolute survival predictions transport a study-specific baseline

Each study contributes exactly one arm, so a study-specific baseline
shape and a treatment-specific baseline shape are perfectly aliased: no
part of the data can say whether a difference in shape belongs to the
treatment or to the study's eligibility, ascertainment, follow-up,
calendar time, supportive care, or unmeasured prognosis. Predicting one
treatment in the other population therefore carries that study's shape
across, which is a structural assumption on top of the covariate
adjustment, not a consequence of it. The contrast estimands are less
exposed than the absolute curves, and the RMST estimands are
collapsible, so say this where the absolute numbers are produced. Once
per session, like the marginal-HR note.

## Usage

``` r
.transported_baseline_note(object)
```

## Arguments

- object:

  A fitted `mlumr_fit`.

## Value

`TRUE` invisibly if the note was emitted.
