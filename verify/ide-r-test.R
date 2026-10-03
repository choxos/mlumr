# Checks the pure R helpers in src/ide.R that build what the Stan workers
# get: the data JSON (with the model's declared dimensions) and the sampler
# settings (from mlumr()'s arguments). Plain R, no webR; needs mlumr and
# jsonlite. Run from this branch's root: Rscript verify/ide-r-test.R
suppressMessages(library(mlumr))
source("src/ide.R")
ide <- get(".ide", pos = "ide:tools")
ok <- TRUE
check <- function(cond, what) { if (!isTRUE(cond)) { ok <<- FALSE; cat("FAIL:", what, "\n") } }
err <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))

# Data: a length-one vector the model declares as an array stays an array;
# scalars are unboxed; logicals and factors become integers; empty arrays
# stay empty; matrices keep their shape.
j <- jsonlite::fromJSON(ide$stan_data_json(
  list(n = 1L, pred_times = 12, y = c(1, 2), M = matrix(1:2, 1), flag = TRUE, f = factor("b", levels = c("a", "b")),
       empty = numeric(0), extra = 3),
  list(n = 0, pred_times = 1, y = 1, M = 2, flag = 0, f = 1, empty = 1)), simplifyVector = FALSE)
check(identical(j$pred_times, list(12L)) || identical(j$pred_times, list(12)), "length-one vector declared as an array stays [12]")
check(identical(j$n, 1L), "a declared scalar is unboxed")
check(length(j$y) == 2, "a vector keeps its length")
check(length(j$M) == 1 && length(j$M[[1]]) == 2, "a 1 x 2 matrix stays a matrix")
check(identical(j$flag, 1L), "a logical becomes an integer")
check(identical(j$f, list(2L)), "a factor becomes its integer code, as an array")
check(identical(j$empty, list()), "an empty array stays []")
check(identical(j$extra, 3L) || identical(j$extra, 3), "a variable the model does not declare passes through")
check(grepl("must be an array with 2 dimensions", err(ide$stan_data_json(list(M = 5), list(M = 2)))),
      "a vector where a matrix is declared stops with a message")

# Sampler settings.
r0 <- ide$tinystan_request("m", 4, 2000, 1000, 2026, 0.8, 10)
check(identical(r0$req$sampler$metric, 2L), "the default metric is diagonal, as in rstan and CmdStan")
check(identical(r0$req$sampler$delta, 0.8) && identical(r0$req$sampler$max_depth, 10L), "defaults pass through")
check(r0$req$num_samples == 1000 && r0$thin == 1 && is.null(r0$req$inits), "draws, thinning and inits by default")
r1 <- ide$tinystan_request("m", 2, 2000, 1000, 1, 0.8, 10, list(control = list(adapt_delta = 0.99, max_treedepth = 15)))
check(identical(r1$req$sampler$delta, 0.99) && identical(r1$req$sampler$max_depth, 15L), "control is merged")
check(identical(r1$control$adapt_delta, 0.99), "the merged control is reported")
r2 <- ide$tinystan_request("m", 2, 2000, 1000, 1, 0.8, 10,
                           list(control = list(metric = "dense_e", stepsize = 0.1, adapt_window = 30), thin = 2, init = 0,
                                cores = 4))
check(identical(r2$req$sampler$metric, 1L) && identical(r2$req$sampler$stepsize, 0.1) && identical(r2$req$sampler$window, 30),
      "metric, step size and adaptation settings map to TinyStan's")
check(r2$thin == 2 && identical(r2$req$sampler$init_radius, 0), "thin and init = 0")
r3 <- ide$tinystan_request("m", 2, 20, 10, 1, 0.8, 10, list(init = function(chain_id) list(mu = chain_id)))
check(identical(r3$req$inits, c(list('{"mu":1}'), list('{"mu":2}'))) || identical(unlist(r3$req$inits), c('{"mu":1}', '{"mu":2}')),
      "an init function gives one JSON text per chain")
check(grepl("does not support `pars`", err(ide$tinystan_request("m", 2, 20, 10, 1, 0.8, 10, list(pars = "mu")))),
      "an unsupported argument stops with its name")
check(grepl("control setting `foo`", err(ide$tinystan_request("m", 2, 20, 10, 1, 0.8, 10, list(control = list(foo = 1))))),
      "an unsupported control setting stops with its name")
check(grepl("adapt_delta", err(ide$tinystan_request("m", 2, 20, 10, 1, 0.8, 10, list(control = list(adapt_delta = 2))))),
      "control values are validated as mlumr validates them")

# The data viewer's JSON keeps arrays for one column and for one row.
v1 <- jsonlite::fromJSON(ide$view(data.frame(a = 1:3)), simplifyVector = FALSE)
check(is.list(v1$names) && length(v1$names) == 1 && is.list(v1$cols[[1]]) && length(v1$cols[[1]]) == 3 &&
        is.list(v1$classes) && identical(v1$nrow, 3L), "one-column data frame")
v2 <- jsonlite::fromJSON(ide$view(data.frame(a = "12", b = "34")), simplifyVector = FALSE)
check(length(v2$names) == 2 && is.list(v2$cols[[1]]) && identical(v2$cols[[1]][[1]], "12") &&
        identical(v2$cols[[2]][[1]], "34"), "one-row data frame")
cat(if (ok) "ide.R helpers: all checks pass\n" else "ide.R helpers: FAILURES\n")
quit(status = if (ok) 0 else 1)
