# Format a diagnostic for a message without turning Inf into a number

`sprintf("%.3f", Inf)` prints "Inf", which is right, but the same format
applied to a very large finite value prints a wall of digits. Handle the
non-finite case by name.

## Usage

``` r
.format_diagnostic(x)
```

## Arguments

- x:

  A single numeric value.

## Value

A single string.
