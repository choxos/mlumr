# A row with no confidence interval must not be drawn as though it had the
# widest one on the figure.

# Three ordinary rows, one genuine outlier wide enough to trigger clipping, one
# row whose bounds were never supplied, and one whose upper bound is actually
# infinite. Clipping has to be triggered by something else for the bug to
# appear at all, which is what the outlier is for.
.clipping_frame <- function() {
  data.frame(
    label = c("a", "b", "c", "outlier", "missing-CI", "infinite-CI"),
    effect = "HR",
    est = c(1.00, 1.02, 0.98, 1.00, 1.05, 1.03),
    lo = c(0.90, 0.92, 0.88, 0.01, NA, 0),
    hi = c(1.10, 1.12, 1.08, 1000, NA, Inf)
  )
}

# Which rows a layer actually drew, by label.
.drawn <- function(p, i) {
  d <- ggplot2::layer_data(p, i)
  if (!all(c("x", "xend", "y") %in% names(d))) {
    return(character(0))
  }
  labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  labels[d$y]
}

test_that("a row with no interval is drawn as its estimate alone", {
  skip_if_not_installed("ggplot2")
  p <- mlumr_forest(.clipping_frame())

  segments <- .drawn(p, 2)
  expect_false("missing-CI" %in% segments)
  # The ordinary rows and both genuinely wide ones are still drawn.
  expect_true(all(c("a", "b", "c", "outlier", "infinite-CI") %in% segments))

  # No clipping arrow on either side for the row that has no interval.
  arrows <- unlist(lapply(seq(3, length(p$layers)), function(i) .drawn(p, i)))
  expect_false("missing-CI" %in% arrows)
})

test_that("an infinite bound is still clipped and still gets an arrow", {
  skip_if_not_installed("ggplot2")
  p <- mlumr_forest(.clipping_frame())
  arrows <- unlist(lapply(seq(3, length(p$layers)), function(i) .drawn(p, i)))
  # Infinity genuinely runs past the viewport; that is what the arrow says.
  expect_true("infinite-CI" %in% arrows)
  expect_true("outlier" %in% arrows)
})

test_that("the point estimate of an interval-less row is still plotted", {
  skip_if_not_installed("ggplot2")
  p <- mlumr_forest(.clipping_frame())
  # The point layer takes every row, so the estimate is not lost with the
  # interval.
  points <- Filter(function(i) {
    d <- ggplot2::layer_data(p, i)
    "x" %in% names(d) && !"xend" %in% names(d)
  }, seq_along(p$layers))
  expect_gt(length(points), 0)
  d <- ggplot2::layer_data(p, points[[1]])
  expect_equal(nrow(d), 6L)
})

test_that("rows without intervals do not disturb the clipping window", {
  skip_if_not_installed("ggplot2")
  frame <- .clipping_frame()
  with_missing <- mlumr_forest(frame)
  without <- mlumr_forest(frame[frame$label != "missing-CI", , drop = FALSE])
  expect_equal(with_missing$coordinates$limits$x,
               without$coordinates$limits$x)
})
