# Regenerates mspline-reference.txt, the R splines2 values that
# src/test/survival.test.ts checks the TypeScript M-spline basis against.
#
# Run from this directory with the mlumr package installed:
#   Rscript generate-mspline-reference.R > mspline-reference.txt
#
# The evaluation follows R/mspline.R .eval_basis() in the package: times are
# clamped to the boundary, the integral is extended linearly past the upper
# boundary at the boundary hazard, and is zero at or below zero.

times <- c(0, 0.5, 2, 5, 7.3, 12, 14)
internal <- c(2, 5, 9)
boundary <- c(0, 12)

eval_basis <- function(degree, integral) {
  lower <- boundary[1]
  upper <- boundary[2]
  n_scoef <- length(internal) + degree + 1L
  clamped <- pmin(pmax(times, lower), upper)
  basis <- splines2::mSpline(clamped, knots = internal, degree = degree,
                             Boundary.knots = boundary, intercept = TRUE,
                             integral = integral)
  basis <- matrix(as.numeric(basis), nrow = length(times), ncol = n_scoef)
  if (integral) {
    after_upper <- times > upper
    if (any(after_upper)) {
      haz <- splines2::mSpline(rep(upper, sum(after_upper)), knots = internal,
                               degree = degree, Boundary.knots = boundary,
                               intercept = TRUE, integral = FALSE)
      haz <- matrix(as.numeric(haz), nrow = sum(after_upper), ncol = n_scoef)
      basis[after_upper, ] <- basis[after_upper, ] +
        (times[after_upper] - upper) * haz
    }
    basis[times <= 0, ] <- 0
  }
  basis
}

for (degree in 0:3) {
  n_scoef <- length(internal) + degree + 1L
  cat(sprintf("DEGREE %d n_scoef=%d\n", degree, n_scoef))
  for (part in c("M", "I")) {
    cat(part, ":\n", sep = "")
    basis <- eval_basis(degree, integral = identical(part, "I"))
    for (i in seq_along(times)) {
      cat(sprintf("t=%g %s\n", times[i],
                  paste(sprintf("%.17g", basis[i, ]), collapse = " ")))
    }
  }
}
