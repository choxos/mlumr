# The cache key decides whether an existing compiled executable is reused. It
# was `substr(paste0(md5s), 1, 32)`, and an MD5 digest is exactly 32 characters,
# so the key was the FIRST digest and every include hash was discarded. Editing
# an include left the key identical, and a user could keep running a binary
# built from the old likelihood.

test_that("the cache key changes when an include changes", {
  dir <- file.path(tempdir(), paste0("cachekey-", Sys.getpid()))
  inc <- file.path(dir, "include")
  dir.create(inc, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  main <- file.path(dir, "m.stan")
  writeLines("// main model", main)
  a <- file.path(inc, "a.stan")
  b <- file.path(inc, "b.stan")
  writeLines("// include a", a)
  writeLines("// include b", b)
  files <- c(main, a, b)

  before <- .cmdstanr_cache_key(files)
  expect_match(before, "^[0-9a-f]{32}$")

  # Change ONLY an include. The main model is untouched.
  writeLines("// include b, edited", b)
  after <- .cmdstanr_cache_key(files)
  expect_false(identical(before, after))

  # Restoring the content restores the key: it is content-addressed, not
  # time-addressed.
  writeLines("// include b", b)
  expect_identical(.cmdstanr_cache_key(files), before)
})

test_that("the cache key changes when the main model changes", {
  dir <- file.path(tempdir(), paste0("cachekey2-", Sys.getpid()))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  main <- file.path(dir, "m.stan")
  writeLines("// v1", main)
  k1 <- .cmdstanr_cache_key(main)
  writeLines("// v2", main)
  expect_false(identical(k1, .cmdstanr_cache_key(main)))
})

test_that("two different include sets do not share a key", {
  # The precise failure of the old expression: identical main file, different
  # includes, one key.
  dir <- file.path(tempdir(), paste0("cachekey3-", Sys.getpid()))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  main <- file.path(dir, "m.stan"); writeLines("// main", main)
  i1 <- file.path(dir, "i1.stan"); writeLines("// one", i1)
  i2 <- file.path(dir, "i2.stan"); writeLines("// two", i2)

  old_style <- function(fs) substr(paste(unname(tools::md5sum(fs)),
                                         collapse = ""), 1L, 32L)
  # The old expression really did collide.
  expect_identical(old_style(c(main, i1)), old_style(c(main, i2)))
  # The new one does not.
  expect_false(identical(.cmdstanr_cache_key(c(main, i1)),
                         .cmdstanr_cache_key(c(main, i2))))
})

test_that("a missing source does not collapse onto a shared key", {
  dir <- file.path(tempdir(), paste0("cachekey4-", Sys.getpid()))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  main <- file.path(dir, "m.stan"); writeLines("// main", main)
  gone1 <- file.path(dir, "absent1.stan")
  gone2 <- file.path(dir, "absent2.stan")
  # Different absent names are still different builds.
  expect_false(identical(.cmdstanr_cache_key(c(main, gone1)),
                         .cmdstanr_cache_key(c(main, gone2))))
})
