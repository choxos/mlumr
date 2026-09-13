# Loads mlumr's R sources (without the Stan backends) into the browser's R
# session. S3 lookup does not search attached environments, so the methods the
# NAMESPACE declares are registered explicitly; otherwise print(naive(dat))
# would print a raw list.
local({
  dir <- "/home/web_user/mlumr"
  ml <- new.env()
  for (file in c("R/mlumr.R", "lesson-helpers.R")) {
    sys.source(file.path(dir, file), envir = ml, keep.source = FALSE)
  }
  namespace <- readLines(file.path(dir, "NAMESPACE"), warn = FALSE)
  for (m in regmatches(namespace, regexec("^S3method\\(([^,]+),([^)]+)\\)", namespace))) {
    if (length(m)) registerS3method(m[2], m[3], get(paste0(m[2], ".", m[3]), envir = ml), envir = ml)
  }
  attach(ml, name = "mlumr", warn.conflicts = FALSE)
  options(lesson.stan_dir = file.path(dir, "inst", "stan"))
})
