# The interpretation paragraph the LOO/WAIC comparison prints

Kept callable so a test can assert what users actually see rather than
matching source text, which is brittle and, worse, matches comments
about the wording as readily as the wording itself.

## Usage

``` r
.model_comparison_interpretation()
```

## Value

A character vector, one element per printed line.

## Details

The standard error of a difference measures UNCERTAINTY about that
difference, not support for it. An earlier version of this paragraph
presented a large `se_diff` as the threshold for a meaningful
difference, under which an `elpd_diff` of 0.1 alongside an `se_diff` of
3 would have qualified as persuasive. It is the difference read against
its own uncertainty that carries information, and even that is a
heuristic.
