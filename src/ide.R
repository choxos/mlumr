# R side of mlumr Playground. Sourced once after library(mlumr); everything lives
# in an attached environment, "ide:tools", so it never clutters the user's
# global environment.
local({
  `%||%` <- function(a, b) if (is.null(a)) b else a
  ide <- new.env()
  send <- function(type, ...) {
    msg <- jsonlite::toJSON(list(type = type, ...), auto_unbox = TRUE, null = "null", digits = NA)
    webr::eval_js(sprintf("Module.webr.channel.write({ type: 'mlumr', data: %s })", msg))
    invisible()
  }

  # ---- Environment pane -------------------------------------------------
  describe <- function(x) {
    if (is.function(x)) {
      a <- args(x)
      return(if (is.null(a)) "function" else sub("^function ", "function", trimws(deparse(a)[1])))
    }
    if (is.data.frame(x)) return(sprintf("%d obs. of %d variable%s", nrow(x), ncol(x), if (ncol(x) == 1) "" else "s"))
    if (is.matrix(x)) return(sprintf("%s [%d x %d]", typeof(x), nrow(x), ncol(x)))
    if (inherits(x, "mlumr_fit")) {
      return(sprintf("ML-UMR %s fit, %s%s, %d draws", x$model, x$family,
                     if (!is.null(x$distribution)) paste0(" (", x$distribution, ")") else "",
                     nrow(x$draws)))
    }
    if (is.atomic(x) && length(x) == 1 && is.null(attributes(x))) {
      return(if (is.character(x)) encodeString(x, quote = "\"") else format(x, digits = 6))
    }
    out <- tryCatch(utils::capture.output(utils::str(x, max.level = 0, give.attr = FALSE,
                                                     vec.len = 3, nchar.max = 60)),
                    error = function(e) class(x)[1])
    trimws(out[1])
  }
  group_of <- function(x) {
    if (is.function(x)) "Functions"
    else if (is.data.frame(x) || is.matrix(x)) "Data"
    else "Values"
  }
  ide$env <- function(envir = globalenv()) {
    nms <- sort(ls(envir))
    rows <- lapply(nms, function(n) {
      x <- get(n, envir = envir)
      list(name = n, group = group_of(x), class = class(x)[1], type = typeof(x),
           length = length(x),
           size = format(utils::object.size(x), units = "auto", standard = "SI"),
           desc = describe(x),
           expandable = is.list(x) && !is.function(x),
           viewable = is.data.frame(x) || is.matrix(x))
    })
    jsonlite::toJSON(rows, auto_unbox = TRUE, digits = NA)
  }
  ide$children <- function(name, path = character()) {
    x <- get(name, envir = globalenv())
    for (p in path) x <- x[[if (grepl("^[0-9]+$", p)) as.integer(p) else p]]
    n <- length(x)
    nm <- names(x) %||% rep("", n)
    rows <- lapply(seq_len(min(n, 200)), function(i) {
      el <- x[[i]]
      list(key = if (nzchar(nm[i])) nm[i] else as.character(i),
           label = if (nzchar(nm[i])) paste0("$ ", nm[i]) else paste0("[[", i, "]]"),
           class = class(el)[1], desc = describe(el),
           expandable = is.list(el) && length(el) > 0 && !is.function(el),
           viewable = is.data.frame(el) || is.matrix(el))
    })
    jsonlite::toJSON(list(rows = rows, more = max(0, n - 200)), auto_unbox = TRUE, digits = NA)
  }
  ide$view <- function(name, path = character(), max_rows = 1000L, max_cols = 100L) {
    x <- if (is.character(name) && exists(name, envir = globalenv())) get(name, envir = globalenv()) else name
    for (p in path) x <- x[[if (grepl("^[0-9]+$", p)) as.integer(p) else p]]
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    shown <- utils::head(df[, seq_len(min(ncol(df), max_cols)), drop = FALSE], max_rows)
    cols <- lapply(shown, function(col) {
      if (is.numeric(col)) as.character(signif(col, 6)) else as.character(col)
    })
    # Names, classes and every column stay arrays even for one column or one
    # row (docs.openData() maps over them); only the counts are scalars.
    jsonlite::toJSON(list(names = names(shown), classes = unname(vapply(shown, function(c) class(c)[1], "")),
                          nrow = jsonlite::unbox(nrow(df)), ncol = jsonlite::unbox(ncol(df)),
                          cols = unname(cols)),
                     auto_unbox = FALSE, digits = NA)
  }
  # Packages a script names (library(), pkg::, package = "pkg") that are not
  # installed yet but are in this site's repository.
  ide$missing <- function(pkgs) {
    pkgs <- unique(pkgs[!vapply(pkgs, function(p) nzchar(system.file(package = p)), TRUE)])
    if (!length(pkgs)) return("[]")
    local_repo <- getOption("webr_pkg_repos")[1]
    have <- tryCatch(rownames(utils::available.packages(
      contriburl = sprintf("%s/bin/emscripten/contrib/%s", sub("/$", "", local_repo),
                           sub("[.][^.]*$", "", as.character(getRversion()))))),
      error = function(e) character())
    jsonlite::toJSON(intersect(pkgs, have))
  }
  ide$memory <- function() {
    as.numeric(webr::eval_js("Module.HEAPU8.buffer.byteLength"))
  }

  # ---- Plots: keep a recorded copy of every page for SVG export and redraws
  ide$pages <- list()
  ide$page <- 0L
  record <- function() {
    if (ide$page > 0 && grDevices::dev.cur() > 1) {
      p <- tryCatch(grDevices::recordPlot(), error = function(e) NULL)
      if (!is.null(p)) ide$pages[[ide$page]] <- p
    }
  }
  on_new_page <- function(...) {
    record()
    ide$page <- length(ide$pages) + 1L
  }
  setHook("before.plot.new", on_new_page)
  setHook("before.grid.newpage", on_new_page)
  ide$after_command <- function() record()
  # The plot device: webR's canvas, sized to the Plots pane, with the display
  # list on so every page can be recorded, redrawn and exported as SVG.
  ide$device <- function(width, height) {
    options(device = function(...) {
      webr::canvas(width = width, height = height, bg = "white")
      grDevices::dev.control("enable")
    })
    for (d in rev(which(names(grDevices::dev.list()) == "canvas"))) grDevices::dev.off(grDevices::dev.list()[d])
    invisible()
  }
  ide$export_svg <- function(k, file, width, height) {
    p <- ide$pages[[k]]
    if (is.null(p)) stop("That plot is no longer available to redraw.")
    grDevices::svg(file, width = width, height = height)
    on.exit(grDevices::dev.off())
    grDevices::replayPlot(p)
    invisible(file)
  }
  ide$redraw <- function(k) {
    p <- ide$pages[[k]]
    if (!is.null(p)) {
      grDevices::replayPlot(p)
      ide$page <- k
    }
    invisible()
  }
  ide$forget_plots <- function(k = NULL) {
    if (is.null(k)) {
      ide$pages <- list()
      ide$page <- 0L
    } else if (k <= length(ide$pages)) {
      ide$pages[[k]] <- NULL
      if (ide$page == k) ide$page <- 0L else if (ide$page > k) ide$page <- ide$page - 1L
    }
    invisible()
  }

  # ---- Help and vignettes ---------------------------------------------------
  ide$help_html <- function(topic, package = NULL) {
    h <- if (is.null(package)) utils::help(topic, help_type = "text") else utils::help(topic, package = (package), help_type = "text")
    paths <- as.character(h)
    if (!length(paths)) return("")
    rd <- utils:::.getHelpFile(paths[1])
    out <- utils::capture.output(tools::Rd2HTML(rd, out = "", dynamic = TRUE,
                                                package = basename(dirname(dirname(paths[1])))))
    paste(out, collapse = "\n")
  }
  ide$help_index <- function(package = "mlumr") {
    rds <- readRDS(system.file("Meta", "Rd.rds", package = package))
    vig <- tryCatch(tools::getVignetteInfo(package), error = function(e) NULL)
    vigs <- if (!is.null(vig) && nrow(vig)) {
      lapply(seq_len(nrow(vig)), function(i) list(title = vig[i, "Title"],
        file = file.path(vig[i, "Dir"], "doc", vig[i, "PDF"])))
    } else list()
    keep <- !vapply(rds$Keywords, function(k) "internal" %in% k, TRUE)
    jsonlite::toJSON(list(package = package,
                          topics = lapply(which(keep), function(i) list(name = rds$Name[i], title = rds$Title[i])),
                          vignettes = vigs), auto_unbox = TRUE)
  }
  print_help <- function(x, ...) {
    paths <- as.character(x)
    topic <- attr(x, "topic")
    if (!length(paths)) {
      message(sprintf("No documentation for '%s' in the loaded packages.", topic))
    } else {
      send("help", topic = topic, package = basename(dirname(dirname(paths[1]))))
    }
    invisible(x)
  }

  # ---- Packages ---------------------------------------------------------
  ide$packages <- function() {
    ip <- utils::installed.packages(fields = "Title")
    jsonlite::toJSON(lapply(seq_len(nrow(ip)), function(i) list(
      name = ip[i, "Package"], version = ip[i, "Version"], title = ip[i, "Title"] %||% "",
      attached = paste0("package:", ip[i, "Package"]) %in% search(),
      loaded = ip[i, "Package"] %in% loadedNamespaces())), auto_unbox = TRUE)
  }

  # ---- Stan sampling in browser workers ---------------------------------
  # mlumr() prepares the Stan data as usual; this backend hands it to the
  # page, where TinyStan runs one chain per Web Worker, and blocks until the
  # draws come back through shared memory. It returns exactly what mlumr()'s
  # rstan and cmdstanr backends return, so everything after sampling is the
  # package's own code.
  bridge_js <- '
    (() => {
      const req = %s;
      const data = Module.FS.readFile("/tmp/.mlumr-stan/data.json", { encoding: "utf8" });
      const sab = new SharedArrayBuffer(64, { maxByteLength: 1024 * 1024 * 1024 });
      const ctl = new Int32Array(sab, 0, 4);
      Module.webr.channel.write({ type: "mlumr", data: { type: "stan", req, data, sab } });
      while (Atomics.load(ctl, 0) === 0) Atomics.wait(ctl, 0, 0, 1000);
      const state = Atomics.load(ctl, 0);
      const headerBytes = ctl[1], nDoubles = ctl[2];
      const header = new Uint8Array(sab, 16, headerBytes).slice();
      Module.FS.writeFile("/tmp/.mlumr-stan/header.txt", header);
      if (state === 1) {
        const offset = 16 + Math.ceil(headerBytes / 8) * 8;
        Module.FS.writeFile("/tmp/.mlumr-stan/draws.bin", new Uint8Array(sab, offset, nDoubles * 8).slice());
      }
      return state === 1 ? "ok" : state === 2 ? "error" : "cancelled";
    })()'
  dotted_to_brackets <- function(x) {
    sub("^([^.]+)\\.(.*)$", "\\1[\\2]", x) |> (\(s) ifelse(grepl("\\[", s), gsub("\\.", ",", s), s))()
  }
  # The Stan data as JSON, each variable with the number of dimensions its
  # model declares (`dims`, from stanc --info, in stan/manifest.json), so a
  # vector of length one stays an array and only true scalars are unboxed.
  # Logicals and factors become integers, as in cmdstanr::write_stan_json().
  # Initial values go through here too, with the parameters' dimensions.
  ide$stan_data_json <- function(stan_data, dims = list(), what = "Stan data") {
    sd <- lapply(stats::setNames(nm = names(stan_data)), function(nm) {
      x <- stan_data[[nm]]
      if (is.factor(x)) x <- as.integer(x)
      if (is.logical(x)) storage.mode(x) <- "integer"
      d <- dims[[nm]]
      if (!is.null(d) && d >= 1 && is.null(dim(x))) {
        if (d == 1 || length(x) == 0) {
          x <- array(x, dim = c(length(x), rep(0L, d - 1)))
        } else {
          stop(sprintf("%s `%s` must be an array with %d dimensions, but it is a vector of length %d.",
                       what, nm, d, length(x)), call. = FALSE)
        }
      }
      x
    })
    jsonlite::toJSON(sd, auto_unbox = TRUE, factor = "integer", digits = NA)
  }
  # The sampler settings TinyStan runs with, from mlumr()'s arguments and
  # `...` taken the way the rstan backend takes them: `control` merged with
  # adapt_delta and max_treedepth by mlumr's own rule, `init` and `init_r`
  # as rstan reads them, `thin` applied to the draws afterwards (TinyStan has
  # none). Anything else stops with its name instead of being dropped.
  ide$tinystan_request <- function(model_name, chains, iter, warmup, seed, adapt_delta, max_treedepth,
                                   dots = list(), param_dims = list()) {
    ns <- asNamespace("mlumr")
    merged <- if (exists(".merge_sampler_control", envir = ns, inherits = FALSE)) {
      get(".merge_sampler_control", envir = ns)(adapt_delta, max_treedepth, dots)
    } else {
      control <- utils::modifyList(list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
                                   dots$control %||% list())
      dots$control <- NULL
      list(control = control, dots = dots)
    }
    control <- merged$control
    dots <- merged$dots
    as_count <- function(x, what) {
      if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 1 || x != round(x)) {
        stop(sprintf("`%s` must be a single whole number of at least 1.", what), call. = FALSE)
      }
      as.integer(x)
    }
    names_map <- c(stepsize = "stepsize", stepsize_jitter = "stepsize_jitter", adapt_gamma = "gamma",
                   adapt_kappa = "kappa", adapt_t0 = "t0", adapt_init_buffer = "init_buffer",
                   adapt_term_buffer = "term_buffer", adapt_window = "window")
    unknown <- setdiff(names(control), c("adapt_delta", "max_treedepth", "metric", "adapt_engaged", names(names_map)))
    if (length(unknown)) {
      stop(sprintf("The TinyStan engine does not support control setting%s %s; mlumr_engine(\"rstan\") does.",
                   if (length(unknown) > 1) "s" else "", paste0("`", unknown, "`", collapse = ", ")), call. = FALSE)
    }
    metrics <- c(unit_e = 0L, dense_e = 1L, diag_e = 2L)
    metric <- control$metric %||% "diag_e"
    if (!is.character(metric) || length(metric) != 1 || !metric %in% names(metrics)) {
      stop("`control$metric` must be one of \"diag_e\", \"dense_e\" or \"unit_e\".", call. = FALSE)
    }
    # rstan's and cmdstanr's presentation and parallelism arguments change
    # nothing here: every chain already runs in its own worker.
    ignored <- c("cores", "parallel_chains", "open_progress", "show_messages", "show_exceptions")
    unknown <- setdiff(names(dots), c(ignored, "thin", "init", "init_r"))
    if (length(unknown)) {
      stop(sprintf("The TinyStan engine does not support %s; mlumr_engine(\"rstan\") does.",
                   paste0("`", unknown, "`", collapse = ", ")), call. = FALSE)
    }
    thin <- if (is.null(dots$thin)) 1L else as_count(dots$thin, "thin")
    init_radius <- if (is.null(dots$init_r)) 2 else as.numeric(dots$init_r)
    inits <- NULL
    init <- dots$init
    if (!is.null(init)) {
      if (is.numeric(init) && length(init) == 1) {
        init_radius <- as.numeric(init)          # 0 starts every chain at zero, as in rstan and cmdstanr
      } else if (identical(init, "0")) {
        init_radius <- 0
      } else if (identical(init, "random")) {
        # the default
      } else if (is.function(init)) {
        inits <- lapply(seq_len(chains), function(k) if ("chain_id" %in% names(formals(init))) init(chain_id = k) else init())
      } else if (is.list(init) && length(init) == chains && all(vapply(init, is.list, TRUE))) {
        inits <- init
      } else {
        stop("`init` must be \"random\", 0, a number, a function, or a list with one list per chain.", call. = FALSE)
      }
    }
    sampler <- list(delta = control$adapt_delta, max_depth = as.integer(control$max_treedepth),
                    metric = metrics[[metric]], init_radius = init_radius)
    for (nm in intersect(names(names_map), names(control))) sampler[[names_map[[nm]]]] <- control[[nm]]
    if (!is.null(control$adapt_engaged)) sampler$adapt <- isTRUE(control$adapt_engaged)
    list(req = list(model = model_name, chains = as.integer(chains), num_warmup = as.integer(warmup),
                    num_samples = as.integer(iter - warmup), seed = as.numeric(seed), sampler = sampler,
                    inits = if (is.null(inits)) NULL else
                      lapply(inits, function(x) as.character(ide$stan_data_json(x, param_dims, "Initial value")))),
         control = control, thin = thin, metric = metric)
  }
  fit_tinystan <- function(model_name, stan_data, chains, iter, warmup, seed,
                           adapt_delta, max_treedepth, refresh, verbose = TRUE, ...) {
    entry <- tryCatch(jsonlite::read_json("/tmp/.ide/manifest.json")$models[[model_name]], error = function(e) NULL)
    if (is.null(entry$data) || is.null(entry$params)) {
      stop("This site's stan/manifest.json gives no data or parameter dimensions for ", model_name, ".", call. = FALSE)
    }
    run <- ide$tinystan_request(model_name, chains, iter, warmup, seed, adapt_delta, max_treedepth, list(...),
                                param_dims = entry$params)
    dir.create("/tmp/.mlumr-stan", showWarnings = FALSE)
    unlink(c("/tmp/.mlumr-stan/header.txt", "/tmp/.mlumr-stan/draws.bin"))
    writeLines(ide$stan_data_json(stan_data, entry$data), "/tmp/.mlumr-stan/data.json")
    req <- run$req
    status <- webr::eval_js(sprintf(bridge_js, jsonlite::toJSON(req, auto_unbox = TRUE, null = "null", digits = NA)))
    header <- readLines("/tmp/.mlumr-stan/header.txt", warn = FALSE)
    if (status == "error") stop(paste(header, collapse = "\n"), call. = FALSE)
    if (status != "ok") stop("Sampling was stopped before it finished.", call. = FALSE)
    shape <- as.integer(strsplit(header[1], " ")[[1]])  # chains, draws per chain, params
    n_chain <- shape[1]; n_per <- shape[2]; n_par <- shape[3]
    names_raw <- header[1 + seq_len(n_par)]
    elapsed <- as.numeric(strsplit(header[2 + n_par], " ")[[1]])
    vals <- readBin("/tmp/.mlumr-stan/draws.bin", "double", n = n_chain * n_per * n_par)
    m <- matrix(vals, nrow = n_chain * n_per, ncol = n_par)
    colnames(m) <- dotted_to_brackets(names_raw)
    # thin, as rstan applies it: every thin-th post-warmup draw of each chain.
    if (run$thin > 1) {
      kept <- (seq_len(n_per) - 1) %% run$thin == 0
      m <- m[rep(kept, n_chain), , drop = FALSE]   # rows are chain by chain
      n_per <- sum(kept)
    }
    sampler <- c("accept_stat__", "stepsize__", "treedepth__", "n_leapfrog__", "divergent__", "energy__")
    draws <- as.data.frame(m[, !colnames(m) %in% sampler, drop = FALSE], check.names = FALSE)
    chain_ids <- rep(seq_len(n_chain), each = n_per)
    if (isTRUE(verbose)) {
      for (k in seq_along(elapsed)) message(sprintf("Chain %d finished in %.1f seconds.", k, elapsed[k]))
    }
    dd <- posterior::as_draws_df(cbind(draws, .chain = chain_ids,
                                       .iteration = rep(seq_len(n_per), n_chain)))
    summ <- posterior::summarise_draws(
      dd,
      mean = mean,
      se_mean = posterior::mcse_mean,
      sd = stats::sd,
      `2.5%` = function(.x) stats::quantile(.x, 0.025),
      `25%` = function(.x) stats::quantile(.x, 0.25),
      `50%` = function(.x) stats::quantile(.x, 0.50),
      `75%` = function(.x) stats::quantile(.x, 0.75),
      `97.5%` = function(.x) stats::quantile(.x, 0.975),
      n_eff = posterior::ess_bulk,
      ess_tail = posterior::ess_tail,
      Rhat = posterior::rhat
    )
    list(
      native_fit = list(sampler = "TinyStan (WebAssembly, browser workers)", model = model_name,
                        elapsed = elapsed, seed = seed, metric = run$metric, thin = run$thin),
      draws = draws,
      chain_ids = chain_ids,
      summary_df = as.data.frame(summ),
      n_divergent = sum(m[, "divergent__"]),
      n_max_td = sum(m[, "treedepth__"] >= run$control$max_treedepth),
      # What the sampler ran under: the merged control, as the rstan backend reports it.
      adapt_delta_used = run$control$adapt_delta,
      max_treedepth_used = run$control$max_treedepth,
      control_used = run$control,
      n_chains_requested = as.integer(chains),
      n_chains_returned = n_chain
    )
  }
  ide$enable_tinystan <- function() {
    ns <- asNamespace("mlumr")
    patch <- function(name, value) {
      environment(value) <- ns
      unlockBinding(name, ns)
      assign(name, value, envir = ns)
      lockBinding(name, ns)
    }
    original <- get(".mlumr_fit_backend", envir = ns)
    patch(".supported_engines", function() c("rstan", "cmdstanr", "tinystan"))
    backend <- function(engine, model_name, stan_data, chains, iter, warmup, seed,
                        adapt_delta, max_treedepth, refresh, verbose = TRUE, ...) {
      if (identical(engine, "tinystan")) {
        fit_tinystan(model_name, stan_data, chains, iter, warmup, seed,
                     adapt_delta, max_treedepth, refresh, verbose = verbose, ...)
      } else {
        original(engine, model_name, stan_data, chains, iter, warmup, seed,
                 adapt_delta, max_treedepth, refresh, verbose = verbose, ...)
      }
    }
    environment(backend) <- list2env(list(original = original, fit_tinystan = fit_tinystan), parent = ns)
    unlockBinding(".mlumr_fit_backend", ns)
    assign(".mlumr_fit_backend", backend, envir = ns)
    lockBinding(".mlumr_fit_backend", ns)
    options(mlumr.stan_engine = "tinystan")
    invisible(TRUE)
  }

  # webR runs R in one thread and parallel::detectCores() returns NA there.
  # The vignettes set options(mc.cores = parallel::detectCores()), and rstan
  # (which multinma uses) cannot compare NA with 1, so report the one core.
  local({
    ns <- asNamespace("parallel")
    unlockBinding("detectCores", ns)
    assign("detectCores", function(all.tests = FALSE, logical = TRUE) 1L, envir = ns)
    lockBinding("detectCores", ns)
  })

  tools_env <- new.env()
  tools_env$.ide <- ide
  tools_env$print.help_files_with_topic <- print_help
  attach(tools_env, name = "ide:tools", warn.conflicts = FALSE)
  registerS3method("print", "help_files_with_topic", print_help, envir = asNamespace("utils"))
})
