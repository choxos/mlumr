# Helpers for the lesson's in-browser mlumr cell.
#
# rstan and cmdstanr cannot run in webR, so the lesson loads mlumr's R sources
# without their Stan backend files. load.R replaces the one function that hands
# prepared data to a backend: instead of sampling, it signals the Stan data back
# to lesson_stan_data(). Calling the public mlumr() therefore runs every
# argument check, warning and preflight a native fit runs, and the browser
# samples exactly the data mlumr() would have passed to its engine.

# One analysis, one set of sampler settings: the mlumr() call that prepares the
# data and the browser run that samples it both read these.
lesson_sampler <- list(chains = 2L, warmup = 500L, samples = 500L, seed = 2026L,
                       adapt_delta = 0.95, max_treedepth = 15L)

lesson_stan_data <- function(dat, model = c("spfa", "relaxed"),
                             prior_intercept = prior_normal(0, 2.5),
                             prior_beta = prior_normal(0, 1)) {
  model <- match.arg(model)
  if (inherits(dat, "mlumr_data") && !identical(dat$family, "binomial")) {
    stop("The browser fit covers binary outcomes only. Other outcome types ",
         "need a native mlumr fit.", call. = FALSE)
  }
  s <- lesson_sampler
  caught <- tryCatch(
    mlumr(dat, model = model, prior_intercept = prior_intercept,
          prior_beta = prior_beta, chains = s$chains,
          iter = s$warmup + s$samples, warmup = s$warmup, seed = s$seed,
          adapt_delta = s$adapt_delta, max_treedepth = s$max_treedepth,
          engine = "rstan", verbose = FALSE),
    lesson_stan_data = function(condition) condition
  )
  if (!inherits(caught, "lesson_stan_data")) {
    stop("mlumr() returned without reaching its Stan backend.", call. = FALSE)
  }
  caught
}

# Everything the Fit button needs, from one R call, so no other cell can change
# `dat` between building the Stan data and running the benchmarks. It never
# throws: errors, warnings and messages come back as data for the page to show.
lesson_prepare_fit <- function(dat, model) {
  notes <- character()
  warns <- character()
  prepared <- tryCatch(
    withCallingHandlers(
      lesson_stan_data(dat, model),
      message = function(m) {
        notes <<- c(notes, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(prepared, "error")) {
    return(lesson_to_json(list(ok = FALSE, error = conditionMessage(prepared),
                               messages = I(notes), warnings = I(warns))))
  }
  stan_file <- file.path(getOption("lesson.stan_dir"),
                         paste0(prepared$model_name, ".stan"))
  lesson_to_json(list(
    ok = TRUE,
    model_name = prepared$model_name,
    stan = lesson_json(prepared$stan_data, stan_file),
    sampler = lesson_sampler,
    messages = I(notes),
    warnings = I(warns),
    benchmarks = list(naive = lesson_benchmark(naive, dat),
                      stc = lesson_benchmark(stc, dat))
  ))
}

# A benchmark keeps its warnings. An estimate drawn as a reference line without
# the warning that came with it would look more trustworthy than it is.
lesson_benchmark <- function(fun, dat) {
  notes <- character()
  warns <- character()
  result <- tryCatch(
    withCallingHandlers(
      fun(dat, link = "logit"),
      message = function(m) {
        notes <<- c(notes, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    return(list(valid = FALSE, error = conditionMessage(result),
                messages = I(notes), warnings = I(warns)))
  }
  values <- c(result$estimate, result$ci_lower, result$ci_upper)
  list(valid = length(values) == 3L && all(is.finite(values)),
       estimate = result$estimate, lower = result$ci_lower,
       upper = result$ci_upper, conf_level = result$conf_level,
       messages = I(notes), warnings = I(warns))
}

lesson_to_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA,
                                null = "null", na = "null"))
}

# Stan JSON: only scalars declared in the Stan data block are unboxed, because
# a length-1 vector or array must stay a JSON array.
lesson_json <- function(stan_data, stan_file) {
  lines <- readLines(stan_file, warn = FALSE)
  start <- grep("^data\\s*\\{", lines)[1]
  end <- start + grep("^\\}", lines[-seq_len(start)])[1]
  block <- lines[(start + 1L):(end - 1L)]
  for (i in rev(grep("^\\s*#include", block))) {
    included <- readLines(file.path(dirname(stan_file), trimws(sub("^\\s*#include", "", block[i]))), warn = FALSE)
    block <- append(block[-i], included, after = i - 1L)
  }
  statements <- trimws(strsplit(paste(sub("//.*$", "", block), collapse = " "), ";", fixed = TRUE)[[1]])
  statements <- statements[nzchar(statements)]
  ident <- "[A-Za-z_][A-Za-z0-9_]*"
  declared <- sub(paste0("^.*[^A-Za-z0-9_](", ident, ")$"), "\\1", statements)
  scalars <- declared[grepl(paste0("^(int|real)\\s*(<[^>]*>)?\\s+", ident, "$"), statements)]
  missing <- setdiff(declared, names(stan_data))
  if (length(missing)) stop("Stan data is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  out <- lapply(names(stan_data), function(name) {
    value <- stan_data[[name]]
    if (name %in% scalars) as.vector(value) else unname(as.array(value))
  })
  names(out) <- names(stan_data)
  as.character(jsonlite::toJSON(out, auto_unbox = TRUE, digits = NA))
}
