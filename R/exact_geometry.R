# Exact linear geometry of a design matrix of doubles.
#
# Shared by the normal exact-fit guard in mlumr() and the Poisson
# finite-maximum check in stc(). Both ask questions a floating-point
# factorization cannot answer: whether a column is exactly a combination of
# the others, and whether a row lies exactly in the span of some others. A
# QR at machine precision treats a contrast of 1e-20 as absent; the model
# that receives the raw design does not, and an exact fit through that
# contrast makes its posterior improper.


#' Primes just below 2^26, largest first
#'
#' A segmented sieve over the top of the range, sized to the count asked for.
#' Residues below `2^26` multiply to below `2^52`, which a double holds
#' exactly, and that is the only reason for the bound.
#'
#' @param n_primes How many primes to return.
#' @return A numeric vector of `n_primes` primes in decreasing order.
#' @keywords internal
.primes_below_2_26 <- function(n_primes) {
  top <- 2^26
  limit <- 8192L
  small <- rep(TRUE, limit)
  small[1L] <- FALSE
  for (i in 2:90) {
    if (small[i]) {
      small[seq.int(i * i, limit, by = i)] <- FALSE
    }
  }
  small <- which(small)
  found <- numeric(0)
  width <- 40L * n_primes + 400L
  hi <- top - 1
  while (length(found) < n_primes) {
    lo <- hi - width + 1
    flags <- rep(TRUE, width)
    for (q in small) {
      first <- ceiling(lo / q) * q
      if (first <= hi) {
        flags[seq.int(first - lo + 1, width, by = q)] <- FALSE
      }
    }
    found <- c(found, rev(which(flags)) + lo - 1)
    hi <- lo - 1
  }
  found[seq_len(n_primes)]
}


#' Modular exponentiation, vectorized over the exponent
#' @param base A single non-negative integer below `p`.
#' @param e Non-negative integer exponents.
#' @param p The modulus, below `2^26`.
#' @return `base^e mod p`, one value per exponent.
#' @keywords internal
.mod_pow <- function(base, e, p) {
  result <- rep(1, length(e))
  b <- base %% p
  while (any(e > 0)) {
    odd <- e %% 2 == 1
    result[odd] <- (result[odd] * b) %% p
    e <- e %/% 2
    b <- (b * b) %% p
  }
  result
}


#' Rank of an integer matrix over the integers modulo a prime
#'
#' Plain Gaussian elimination. Entries are integers in `[0, p)` with
#' `p < 2^26`, so every product stays below `2^52` and is exact in a double.
#'
#' @param A The matrix, already reduced modulo `p`.
#' @param p The prime.
#' @return List with `rank` and `pivots`, the columns carrying the pivots.
#' @keywords internal
.mod_rank <- function(A, p) {
  n <- nrow(A)
  k <- ncol(A)
  r <- 0L
  pivots <- integer(0)
  for (j in seq_len(k)) {
    if (r == n) {
      break
    }
    rows <- seq.int(r + 1L, n)
    i <- rows[A[rows, j] != 0]
    if (!length(i)) {
      next
    }
    i <- i[1L]
    r <- r + 1L
    pivots <- c(pivots, j)
    if (i != r) {
      swap <- A[i, ]
      A[i, ] <- A[r, ]
      A[r, ] <- swap
    }
    A[r, ] <- (A[r, ] * .mod_pow(A[r, j], p - 2, p)) %% p
    others <- which(A[, j] != 0)
    others <- others[others != r]
    if (length(others)) {
      A[others, ] <- (A[others, , drop = FALSE] -
                        outer(A[others, j], A[r, ])) %% p
    }
  }
  list(rank = r, pivots = pivots)
}


#' Exact rank of a matrix of doubles
#'
#' Every double is a rational number, `m * 2^e` with an odd integer `m` below
#' `2^53`, so each column can be scaled by a power of two into a column of
#' integers, and the rank of that integer matrix is the exact rank of the
#' input. The rank is found modulo primes: the rank modulo `p` never exceeds
#' the exact rank, and it falls short only when `p` divides every nonzero
#' minor of the exact rank's size. Hadamard's inequality bounds those minors,
#' so once the product of the primes used exceeds the bound some prime sees
#' the full rank, and the largest rank found is the exact one. Residues below
#' `2^26` multiply to below `2^52`, so the whole computation is exact.
#'
#' This is what lets a guard reason about the design the model fits rather
#' than about the one a floating-point factorization resolves: a column that
#' differs from a combination of the others by `1e-20` is independent of
#' them, and a rank computed at machine precision cannot see that. The
#' pivot columns returned are exactly independent, which a numerical pivot
#' set is not guaranteed to be.
#'
#' @param X A matrix of finite doubles.
#' @return List with `rank` and `pivots`, a set of `rank` columns that is
#'   exactly linearly independent.
#' @keywords internal
.exact_rank <- function(X) {
  X <- as.matrix(X)
  n <- nrow(X)
  k <- ncol(X)
  if (n == 0L || k == 0L) {
    return(list(rank = 0L, pivots = integer(0)))
  }
  ax <- abs(X)
  nonzero <- ax > 0
  e <- floor(log2(ax))
  e[!nonzero] <- 0
  # log2() can round across a power of two; put each exponent back where
  # the value lies in [2^e, 2^(e + 1)).
  e <- e + (ax >= 2^(e + 1)) - (ax < 2^e)
  # The mantissa as an integer, scaled in two steps so a subnormal input
  # does not ask for a power of two beyond the representable range.
  a <- pmin(52 - e, 1023)
  m <- ax * 2^a * 2^(52 - e - a)
  m[!nonzero] <- 0
  # Strip trailing zero bits so an integer-valued column stays small.
  repeat {
    even <- nonzero & m %% 2 == 0
    if (!any(even)) {
      break
    }
    m[even] <- m[even] / 2
    e[even] <- e[even] + 1
  }
  # Per column, shift the exponents so the smallest is zero: the column is
  # then the integers m * 2^s, and a column scaling leaves the rank alone.
  s <- matrix(0, n, k)
  bits <- numeric(k)
  for (j in seq_len(k)) {
    nz <- nonzero[, j]
    if (!any(nz)) {
      next
    }
    s[nz, j] <- e[nz, j] - min(e[nz, j])
    bits[j] <- max(floor(log2(m[nz, j])) + 1 + s[nz, j])
  }
  # Hadamard: a minor is at most the product over its columns of sqrt(k)
  # times the column's largest entry, so this many bits bound every minor.
  # Each prime below 2^26 exceeds 2^25.
  total_bits <- sum(bits) + k * log2(k) / 2 + 1
  primes <- .primes_below_2_26(ceiling(total_bits / 25) + 1L)
  negative <- X < 0
  hi <- floor(m / 2^26)
  lo <- m - hi * 2^26
  best <- list(rank = -1L, pivots = integer(0))
  for (p in primes) {
    residue <- ((hi %% p) * (2^26 %% p) + lo) %% p
    residue <- (residue * .mod_pow(2, as.vector(s), p)) %% p
    flip <- negative & residue != 0
    residue[flip] <- p - residue[flip]
    found <- .mod_rank(matrix(residue, n, k), p)
    if (found$rank > best$rank) {
      best <- found
    }
    if (best$rank == min(n, k)) {
      break
    }
  }
  best
}


#' Exact keys for design rows
#'
#' The binary representation of every element, so two rows compare equal
#' exactly when they are the same numbers. `paste()` on doubles keeps fifteen
#' digits and could merge two rows that differ. A negative zero is the same
#' number as a positive one, and every fit treats it so, but `%a` spells the
#' two differently; adding zero to each element folds them together and
#' changes nothing else.
#'
#' @param X Design matrix.
#' @return Character vector, one key per row.
#' @keywords internal
.row_keys <- function(X) {
  apply(X + 0, 1, function(r) paste(sprintf("%a", r), collapse = ","))
}


#' Replicate design rows, and whether their outcomes agree
#'
#' Rows with identical covariates get identical fitted values under any model,
#' so two such rows with different outcomes leave a residual that no fit can
#' remove. That is a structural fact, not a numerical one: it proves the
#' residual sum of squares positive without measuring it. The rows compared
#' are the ones the model fits, so two raw rows that its centering rounds
#' together count as replicates, since the model cannot tell them apart.
#'
#' @param X Design matrix as the model fits it.
#' @param y Outcome vector.
#' @return List with `n_distinct`, the number of distinct design rows, and
#'   `consistent`, `FALSE` if some replicate group carries more than one
#'   outcome value.
#' @keywords internal
.design_replicates <- function(X, y) {
  groups <- split(y, .row_keys(X))
  list(n_distinct = length(groups),
       consistent = all(vapply(groups, function(g) all(g == g[1]),
                               logical(1))))
}


#' Scale a design's predictors by powers of two
#'
#' Division by a power of two is exact in binary, so this changes no
#' distinction between rows and no exact rank: two rows that differ by 1e-20
#' in a column still do afterward. Only the size changes, so that the
#' largest absolute value lands in [1, 2) and the pivot decisions of a
#' numerical factorization do not depend on the predictor's units. R's
#' `dqrdc2` judges a column against its own original norm, so a predictor
#' whose whole range is `2^-60` survives beside an intercept of ones; a rule
#' that judged it against the largest column would drop it. Scaling a column
#' by `c` scales its coefficient by `1 / c`, so the rounding bound
#' `p * eps * |X||b|` is unchanged too.
#'
#' The exception is a column spanning most of the double range, whose
#' smallest entries underflow to zero when the column is scaled to its
#' largest; that is why the bitwise and exact tests read the unscaled rows.
#'
#' @param X Design matrix, intercept first.
#' @return The scaled design.
#' @keywords internal
.scale_design <- function(X) {
  if (ncol(X) > 1L) {
    for (j in 2:ncol(X)) {
      size <- max(abs(X[, j]))
      if (is.finite(size) && size > 0) {
        # log2() of the largest double rounds to 1024, and 2^1024 is Inf,
        # which would zero the column; below the normal range the power
        # underflows to zero instead. Clamp to the representable exponents.
        power <- min(max(floor(log2(size)), -1022), 1023)
        X[, j] <- X[, j] / 2^power
      }
    }
  }
  X
}


#' Whether zero rows can be sent to the boundary while positive rows stay put
#'
#' Under a log link a zero outcome is matched only in the limit where its
#' linear predictor goes to `-Inf`, and a zero count's likelihood keeps
#' rising as its rate falls. Both ask for a direction `d` of the
#' coefficients that leaves every positive row's predictor unchanged,
#' `X_pos d = 0`, and lowers the zero rows'. The normal guard needs every
#' zero row lowered, `X_zero d < 0`, since the residual reaches zero only
#' when all of them do (`strict = TRUE`). The Poisson likelihood needs only
#' some row lowered and none raised, `X_zero d <= 0` with `X_zero d != 0`,
#' since it rises along the ray as long as one rate falls (`strict = FALSE`).
#' A rank deficit in `X_pos` is necessary for either and not sufficient: with
#' positive rows at `x = 0` and zeros at `x = -1` and `x = 1` the one free
#' direction moves the two zero rows in opposite directions.
#'
#' The question is a linear feasibility one, decided exactly for up to two
#' free directions. A zero row that lies EXACTLY in the row space of the
#' positive rows is pinned: every direction that leaves the positive
#' predictors fixed leaves its own fixed too. Whether it does is settled by
#' [.exact_rank()], never by a small computed distance, since a row at
#' `2^-48` off that space is free and a numerical test cannot tell it from
#' one exactly on it. A pinned row makes the strict boundary unreachable and
#' simply drops out of the weak question. The remaining rows' loadings on
#' the free directions are computed numerically, so a row within `1e-8` of
#' the row space, whose loadings are rounding, leaves the answer unknown.
#' The number of free directions the zero rows load on is again taken
#' exactly; for one direction the loadings must share a sign, for two they
#' must lie in an open half-plane, which is a gap of more than pi between
#' consecutive angles, or for the weak question a closed one with some row
#' off its boundary. A gap within rounding of pi is settled exactly when
#' the two rows bounding it point exactly opposite ways, and left unknown
#' otherwise. Beyond two directions the answer is unknown, and the caller
#' refuses conservatively.
#'
#' @param X_pos Scaled design rows of the positive outcomes.
#' @param X_zero Scaled design rows of the zero outcomes.
#' @param raw_pos,raw_zero The same rows unscaled, for the bitwise and exact
#'   tests: scaling a column that spans most of the double range underflows
#'   its smallest entries to zero.
#' @param strict Whether every zero row must be lowered (`TRUE`, the normal
#'   log-link boundary) or only some with none raised (`FALSE`, the Poisson
#'   likelihood's recession direction).
#' @return `"reachable"`, `"unreachable"` or `"unknown"`.
#' @keywords internal
.zero_boundary <- function(X_pos, X_zero, raw_pos = X_pos, raw_zero = X_zero,
                           strict = TRUE) {
  tol <- .Machine$double.eps
  p <- ncol(X_pos)
  r <- .exact_rank(raw_pos)$rank
  if (r == p) {
    return("unreachable")
  }
  pinned <- .row_keys(raw_zero) %in% .row_keys(raw_pos)
  if (r == 0L) {
    row_space <- matrix(0, p, 0L)
    null_basis <- diag(p)
  } else {
    qp <- qr(t(X_pos), tol = tol)
    if (qp$rank != r) {
      # Below the exact rank, the positive rows' own geometry is under the
      # factorization's resolution and no numerical basis of their row
      # space exists. Above it, a row exactly in the span of earlier rows
      # left a rounding residual the factorization kept as a direction,
      # and that direction can sit among the first `r` columns of Q ahead
      # of a genuine one; the row-space basis would then be wrong and the
      # loadings with it. Neither is a geometry to decide from.
      return("unknown")
    }
    complete <- qr.Q(qp, complete = TRUE)
    row_space <- complete[, seq_len(r), drop = FALSE]
    null_basis <- complete[, seq.int(r + 1L, p), drop = FALSE]
  }
  off_space <- X_zero - X_zero %*% row_space %*% t(row_space)
  closeness <- sqrt(rowSums(off_space^2)) / sqrt(rowSums(X_zero^2))
  near <- !pinned & closeness < 1e-8
  for (i in which(near)) {
    if (.exact_rank(rbind(raw_pos, raw_zero[i, ]))$rank == r) {
      pinned[i] <- TRUE
    }
  }
  if (any(near & !pinned)) {
    return("unknown")
  }
  if (strict && any(pinned)) {
    return("unreachable")
  }
  X_zero <- X_zero[!pinned, , drop = FALSE]
  raw_zero <- raw_zero[!pinned, , drop = FALSE]
  if (nrow(X_zero) == 0L) {
    return("unreachable")
  }
  loadings <- X_zero %*% null_basis
  # The number of free directions the zero rows actually load on, taken
  # exactly: a direction no zero row loads on, a constant covariate say,
  # adds a coordinate without adding to the question, and two zero rows
  # loading (-1, 0) and (1, 0) are a one-direction problem with opposite
  # signs, not a two-direction one with a gap of pi.
  k <- .exact_rank(rbind(raw_pos, raw_zero))$rank - r
  if (k == 0L) {
    # Every remaining zero row lies exactly in the positive rows' span
    # after all, so every direction that holds the positive predictors
    # holds theirs too.
    return("unreachable")
  }
  span <- qr(t(loadings), tol = tol)
  if (span$rank != k) {
    # Below the exact count, a direction the zero rows load on is under the
    # factorization's resolution; above it, a rounding residual was kept as
    # a direction and could sit among the first `k` columns of Q ahead of a
    # genuine one. Neither basis is one to read angles from.
    return("unknown")
  }
  loadings <- loadings %*% qr.Q(span)[, seq_len(k), drop = FALSE]
  if (k == 1L) {
    z <- loadings[, 1]
    return(if (all(z < 0) || all(z > 0)) "reachable" else "unreachable")
  }
  if (k == 2L) {
    angles <- atan2(loadings[, 2], loadings[, 1])
    ord <- order(angles)
    angles <- angles[ord]
    gaps <- c(diff(angles), angles[1] + 2 * pi - angles[length(angles)])
    gap <- max(gaps)
    # Above pi the rows lie in an open half-plane and a direction lowers
    # them all; below pi no closed half-plane holds them and none lowers
    # any without raising another. The loadings of a row 1e-8 from the row
    # space carry a relative rounding near 1e-8, so within a band of 1e-6
    # around pi the computed angles cannot say which side the gap is on.
    if (gap > pi + 1e-6) {
      return("reachable")
    }
    if (gap < pi - 1e-6) {
      return("unreachable")
    }
    # Within rounding of pi. Whether the two rows bounding the gap point
    # exactly opposite ways is an exact question: their off-space
    # components are collinear exactly when adding both to the positive
    # rows raises the exact rank by one. If they are not, the gap is on one
    # side of pi or the other and nothing here says which. If they are,
    # every row lies in the closed half-plane they bound: no direction
    # lowers them all (the strict answer), while a direction along the
    # line's normal lowers every row off the line and holds the two on it.
    # Some row is off the line, since the rows span two directions and
    # one line holds only one.
    i <- which.max(gaps)
    a <- ord[i]
    b <- ord[if (i == length(gaps)) 1L else i + 1L]
    if (.exact_rank(rbind(raw_pos, raw_zero[a, ], raw_zero[b, ]))$rank !=
          r + 1L) {
      return("unknown")
    }
    return(if (strict) "unreachable" else "reachable")
  }
  "unknown"
}
