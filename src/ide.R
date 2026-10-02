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
    jsonlite::toJSON(list(names = names(shown), classes = vapply(shown, function(c) class(c)[1], ""),
                          nrow = nrow(df), ncol = ncol(df), cols = unname(cols)),
                     auto_unbox = TRUE, digits = NA)
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
  fit_tinystan <- function(model_name, stan_data, chains, iter, warmup, seed,
                           adapt_delta, max_treedepth, refresh, verbose = TRUE, ...) {
    dots <- list(...)
    dir.create("/tmp/.mlumr-stan", showWarnings = FALSE)
    unlink(c("/tmp/.mlumr-stan/header.txt", "/tmp/.mlumr-stan/draws.bin"))
    # As cmdstanr::write_stan_json(): logicals and factors as integers.
    sd <- lapply(stan_data, function(x) {
      if (is.logical(x)) storage.mode(x) <- "integer"
      if (is.factor(x)) x <- as.integer(x)
      x
    })
    jsonlite::write_json(sd, "/tmp/.mlumr-stan/data.json", auto_unbox = TRUE,
                         factor = "integer", digits = NA)
    req <- list(model = model_name, chains = as.integer(chains),
                num_warmup = as.integer(warmup), num_samples = as.integer(iter - warmup),
                seed = as.numeric(seed), delta = adapt_delta, max_depth = as.integer(max_treedepth),
                init_radius = as.numeric(dots$init_r %||% 2))
    status <- webr::eval_js(sprintf(bridge_js, jsonlite::toJSON(req, auto_unbox = TRUE, digits = NA)))
    header <- readLines("/tmp/.mlumr-stan/header.txt", warn = FALSE)
    if (status == "error") stop(paste(header, collapse = "\n"), call. = FALSE)
    if (status != "ok") stop("Sampling was stopped before it finished.", call. = FALSE)
    dims <- as.integer(strsplit(header[1], " ")[[1]])  # chains, draws per chain, params
    n_chain <- dims[1]; n_per <- dims[2]; n_par <- dims[3]
    names_raw <- header[1 + seq_len(n_par)]
    elapsed <- as.numeric(strsplit(header[2 + n_par], " ")[[1]])
    vals <- readBin("/tmp/.mlumr-stan/draws.bin", "double", n = n_chain * n_per * n_par)
    m <- matrix(vals, nrow = n_chain * n_per, ncol = n_par)
    colnames(m) <- dotted_to_brackets(names_raw)
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
                        elapsed = elapsed, seed = seed),
      draws = draws,
      chain_ids = chain_ids,
      summary_df = as.data.frame(summ),
      n_divergent = sum(m[, "divergent__"]),
      n_max_td = sum(m[, "treedepth__"] >= max_treedepth),
      adapt_delta_used = adapt_delta,
      max_treedepth_used = max_treedepth,
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
