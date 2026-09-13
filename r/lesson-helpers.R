# Helpers for the lesson's in-browser mlumr cell.
#
# rstan and cmdstanr cannot run in webR, so the lesson loads mlumr's R sources
# without their four Stan backend files (see load.R). These helpers rebuild the
# exact Stan data that mlumr() hands to its backend for a binomial model, so the
# browser can sample the same compiled Stan program with TinyStan. A native
# check found the result identical() to the data mlumr() itself passes on.

lesson_stan_json <- function(dat, model = c("spfa", "relaxed"),
                             prior_intercept = prior_normal(0, 2.5),
                             prior_beta = prior_normal(0, 1)) {
  model <- match.arg(model)
  if (!inherits(dat, "mlumr_data")) {
    stop("`dat` must be created with combine_data().", call. = FALSE)
  }
  if (!isTRUE(dat$has_integration)) {
    stop("Integration points not found. Run add_integration() first.", call. = FALSE)
  }
  if (!identical(dat$family, "binomial")) {
    stop("The browser fit covers binary outcomes only.", call. = FALSE)
  }
  validate_prior(prior_intercept, "intercept")
  validate_prior(prior_beta, "beta")
  stan_data <- .mlumr_build_stan_data(
    data = dat, family = "binomial", link_info = check_link("binomial", NULL),
    prior_intercept = prior_intercept, prior_beta = prior_beta,
    prior_beta_comparator = NULL, prior_sigma = default_prior_sigma(),
    surv_info = NULL, prior_aux = NULL, prior_aux2 = NULL, prior_smooth = NULL,
    n_knots = 7L, knots = NULL, pred_times = NULL, rmst_horizon = NULL,
    n_rmst_grid = 100L, aux_by = ".study", model = model, center = TRUE,
    qr = FALSE
  )$stan_data
  lesson_json(stan_data, file.path(getOption("lesson.stan_dir"), paste0("mlumr_binary_", model, ".stan")))
}

lesson_benchmarks <- function(dat) {
  estimates <- suppressMessages(suppressWarnings(c(
    naive = naive(dat, link = "logit")$estimate,
    stc = stc(dat, link = "logit")$estimate
  )))
  as.character(jsonlite::toJSON(as.list(estimates), auto_unbox = TRUE, digits = NA))
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
