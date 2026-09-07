# Drop rows with missing setup inputs

Drop rows with missing setup inputs

## Usage

``` r
.drop_missing_rows(
  data,
  required_cols,
  complete = stats::complete.cases(data[, required_cols])
)
```
