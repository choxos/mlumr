# Count draws dropped across a loop and report once

Three callers summarize one profile, treatment or cell at a time.
Letting each call report would give a single bad posterior draw dozens
of identical warnings that name no profile between them, so they tally
here and report once. The worst single quantity is carried through
rather than pooled, since a loss concentrated in one summary is exactly
what a pooled denominator hides.

## Usage

``` r
.draw_tally()
```

## Value

An environment with `affected`, `units`, `worst_dropped`, `worst_n`,
`add()` and `report()`.
