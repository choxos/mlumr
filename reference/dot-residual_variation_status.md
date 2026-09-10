# Classify how much residual variation a normal IPD outcome has

The decision core behind
[`.check_normal_residual_variation()`](https://choxos.github.io/mlumr/reference/dot-check_normal_residual_variation.md),
which turns the status into a refusal, a warning or silence. Kept
separate so the mixed-zero log-link case can classify the positive rows
on their own.

## Usage

``` r
.residual_variation_status(X_raw, y, link, center = TRUE)
```

## Arguments

- X_raw:

  Design matrix, intercept included, uncentered.

- y:

  Outcome vector; positive under `link = "log"`.

- link:

  `"identity"` or `"log"`.

- center:

  Whether the model will center the covariates, or fit a QR
  reparameterization of them, which decorrelates the design and removes
  the same offset cancellation. The guard then works on a centered
  design too, so a contrast between rows that centering rounds away is
  one the fitted model loses as well; with neither, the design is only
  scaled, which is exact, and the rounding bound carries whatever
  cancellation the raw offsets impose, as the fitted model then does.

## Value

List with `status` and, where they apply, `n`, `rank`, `ratio` and
`zero_ratio`.

## Details

Statuses, in the order they are decided:

- `"saturated"`: `n <= rank`, the design reproduces any outcome.

- `"constant"`: the outcome is constant with residual degrees of
  freedom, so the intercept alone fits it exactly.

- `"exact"`: every replicate group agrees and there are exactly `rank`
  distinct design rows, so the design reaches every observed value.

- `"unresolved_log"`: `log(y)` is constant although `y` is not, so the
  log-scale total sum of squares is zero and the ratio undefined.

- `"undecidable"`: the computed residual is at or below the rounding an
  exact fit can leave.

- `"near_exact"`: a real residual at most `1e-6` of the total.

- `"positive"`: a real residual.
