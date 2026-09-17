# Refuse arguments a function has no use for

A function that computes its own answer would otherwise discard whatever
`...` collected, so a misspelled argument would read as a default.

## Usage

``` r
.reject_unused_dots(...)
```
