test_that(".hd_check_predictor_group() returns TRUE/FALSE for all/none", {
  expect_true(.hd_check_predictor_group(list(HDOM = 20, BA = 30, TPH = 1000)))
  expect_false(.hd_check_predictor_group(list(HDOM = NULL, BA = NULL, TPH = NULL)))
})

test_that(".hd_check_predictor_group() aborts on partial supply", {
  f <- function(HDOM = NULL, BA = NULL, TPH = NULL) {
    .hd_check_predictor_group(list(HDOM = HDOM, BA = BA, TPH = TPH), variant = "M5")
  }
  expect_error(f(HDOM = 20), "must be supplied together \\(M5\\)")
  expect_error(f(HDOM = 20), "Missing: `BA` and `TPH`")
  expect_error(f(HDOM = 20, BA = 1), "Supplied: `HDOM` and `BA`")
  # error is attributed to the caller
  err <- tryCatch(f(BA = 1), error = identity)
  expect_identical(deparse(conditionCall(err)), "f(BA = 1)")

  expect_error(
    .hd_check_predictor_group(list(HDOM = 1, BA = NULL)),
    "together, or not at all"
  )
})

test_that(".hd_check_predictor_group() validates args", {
  expect_error(.hd_check_predictor_group(list()), "named list")
  expect_error(.hd_check_predictor_group(list(1, 2)), "named list")
  expect_error(.hd_check_predictor_group(c(a = 1)), "named list")
})

test_that(".hd_warn_out_of_range() is silent when all rows are in range", {
  expect_no_warning(
    out <- .hd_warn_out_of_range(
      list(DBH = c(10, 20, NA)),
      lower = list(DBH = 5), upper = list(DBH = 50), fn = "hd_x"
    )
  )
  expect_identical(out, integer())
})

test_that(".hd_warn_out_of_range() emits exactly one warning listing rows", {
  x <- list(DBH = c(1, 20, 80, NA), BA = c(10, 90, 10, 10), HDOM = c(5, 5, 5, 5))
  expect_snapshot(
    out <- .hd_warn_out_of_range(
      x,
      lower = list(DBH = 2, BA = 5),
      upper = list(DBH = c(60, 60, 60, 60), BA = 85),
      fn = "hd_x"
    )
  )
  expect_identical(out, c(1L, 2L, 3L))

  w <- 0L
  withCallingHandlers(
    .hd_warn_out_of_range(x, lower = list(DBH = 2, BA = 5), upper = list(BA = 85), fn = "hd_x"),
    warning = function(cnd) {
      expect_s3_class(cnd, "CanadaForestAllometry_hd_out_of_range")
      w <<- w + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(w, 1L)
})

test_that(".hd_warn_out_of_range() handles per-row NA limits and truncates long lists", {
  # NA limit = no limit for that row
  expect_no_warning(
    .hd_warn_out_of_range(list(DBH = c(100, 100)), upper = list(DBH = c(NA, NA)), fn = "hd_x")
  )
  expect_warning(
    out <- .hd_warn_out_of_range(list(DBH = c(100, 100)), upper = list(DBH = c(NA, 50)), fn = "hd_x"),
    "1 row \\(2\\)"
  )
  expect_identical(out, 2L)

  expect_warning(
    .hd_warn_out_of_range(list(DBH = rep(100, 15)), upper = list(DBH = 50), fn = "hd_x", max_rows = 3),
    "15 rows \\(1, 2, 3 and 12 more\\)"
  )
})

test_that(".hd_warn_out_of_range() validates args", {
  expect_error(.hd_warn_out_of_range(list(), fn = "hd_x"), "named list")
  expect_error(.hd_warn_out_of_range(list(1), fn = "hd_x"), "named list")
  expect_error(
    .hd_warn_out_of_range(list(DBH = 1), lower = list(BA = 1), fn = "hd_x"),
    "unknown input"
  )
})
