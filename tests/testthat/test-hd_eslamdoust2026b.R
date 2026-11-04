test_that("TR-79 predictions match independent equation-derived values", {
  # Independently evaluated Equation 1 at Table 2 coefficients, not source benchmarks.
  expected <- c(
    6.80346081012098, 12.0332755483418, 15.7261046320672, 19.4957025574123
  )
  out <- hd_eslamdoust2026b(c(2.9, 10, 20, 50), HDOM = 18, BA = 30, TPH = 1500)
  expect_equal(out$height, expected, tolerance = 1e-12)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
})

test_that("TR-79 inputs recycle and work in mutate", {
  trees <- tibble::tibble(DBH = c(10, 20, 30), HDOM = c(12, 18, 24))
  out <- dplyr::mutate(trees, hd_eslamdoust2026b(DBH, HDOM = HDOM, BA = 30, TPH = 1500))
  expect_equal(
    out$height,
    hd_eslamdoust2026b(trees$DBH, "PINU.STR", trees$HDOM, 30, 1500)$height
  )
  expect_equal(
    hd_eslamdoust2026b(20, factor("PINUSTR"), 18, 30, 1500),
    hd_eslamdoust2026b(20, " PINU.STR ", 18, 30, 1500)
  )
  expect_error(hd_eslamdoust2026b(1:3, c("PINU.STR", NA), 18, 30, 1500), "Incompatible")
  expect_error(hd_eslamdoust2026b(1:3, HDOM = c(12, 18), BA = 30, TPH = 1500), "Incompatible")
  expect_error(hd_eslamdoust2026b(20, HDOM = 18, BA = numeric(), TPH = 1500), "Incompatible")
})

test_that("TR-79 validates predictors and species", {
  expect_error(hd_eslamdoust2026b(-1, HDOM = 18, BA = 30, TPH = 1500), ">= 0")
  expect_error(hd_eslamdoust2026b("20", HDOM = 18, BA = 30, TPH = 1500), "numeric")
  expect_error(hd_eslamdoust2026b(Inf, HDOM = 18, BA = 30, TPH = 1500), "finite")
  expect_error(hd_eslamdoust2026b(20, 1, 18, 30, 1500), "character")
  expect_error(hd_eslamdoust2026b(20, "PINU.RES", 18, 30, 1500), "not covered")
  expect_error(hd_eslamdoust2026b(20, "PINU.SPP", 18, 30, 1500), "not covered")
  expect_error(hd_eslamdoust2026b(20, HDOM = 0, BA = 30, TPH = 1500), "HDOM")
  expect_error(hd_eslamdoust2026b(20, HDOM = 18, BA = 0, TPH = 1500), "BA")
  expect_error(hd_eslamdoust2026b(20, HDOM = 18, BA = 30, TPH = -1), "TPH")
  expect_error(hd_eslamdoust2026b(20, BA = 30, TPH = 1500), "HDOM")
  expect_error(hd_eslamdoust2026b(20, HDOM = 18, TPH = 1500), "BA")
  expect_error(hd_eslamdoust2026b(20, HDOM = 18, BA = 30), "TPH")
  for (flag in list(NA, 1, logical(), c(TRUE, FALSE))) {
    expect_error(
      hd_eslamdoust2026b(20, HDOM = 18, BA = 30, TPH = 1500, keep_model_id = flag),
      "keep_model_id"
    )
  }
})

test_that("TR-79 propagates missing inputs and supports empty DBH", {
  out <- hd_eslamdoust2026b(
    c(NA, 20, 20, 20, 20, 20),
    c("PINU.STR", NA, rep("PINU.STR", 4)),
    c(18, 18, NA, 18, 18, 18),
    c(30, 30, 30, NA, 30, 30),
    c(1500, 1500, 1500, 1500, NA, 1500),
    keep_model_id = TRUE
  )
  expect_identical(is.na(out$height), c(rep(TRUE, 5), FALSE))
  expect_identical(out$model_id, c("eslamdoust2026b", NA, rep("eslamdoust2026b", 4)))
  expect_identical(
    hd_eslamdoust2026b(numeric(), HDOM = 18, BA = 30, TPH = 1500),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_eslamdoust2026b(numeric(), HDOM = 18, BA = 30, TPH = 1500, keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("TR-79 extrapolation warns and non-finite arithmetic aborts", {
  cases <- list(
    c(2, 18, 30, 1500), c(60, 18, 30, 1500),
    c(20, 4, 30, 1500), c(20, 34, 30, 1500),
    c(20, 18, 7, 1500), c(20, 18, 54, 1500),
    c(20, 18, 30, 400), c(20, 18, 30, 4800)
  )
  for (x in cases) {
    expect_warning(
      out <- hd_eslamdoust2026b(x[1], HDOM = x[2], BA = x[3], TPH = x[4]),
      class = "CanadaForestAllometry_hd_out_of_range"
    )
    expect_true(is.finite(out$height))
  }
  expect_no_warning(hd_eslamdoust2026b(
    c(2.9, 58.50), HDOM = c(4.57, 33.56), BA = c(7.55, 53.84), TPH = c(433, 4700)
  ))
  expect_error(
    suppressWarnings(hd_eslamdoust2026b(
      0, HDOM = 18, BA = .Machine$double.xmax, TPH = .Machine$double.xmin
    )),
    "non-finite"
  )
})

test_that("TR-79 plausibility checks follow the published signs, not a source benchmark", {
  d <- c(0, seq(2.9, 58.5, length.out = 30))
  h <- suppressWarnings(hd_eslamdoust2026b(d, HDOM = 18, BA = 30, TPH = 1500)$height)
  expect_equal(h[1], 1.3)
  expect_true(all(diff(h) > 0))
  expect_true(all(h < 1.3 + 1.8711 * 18^0.8053))
  expect_true(all(diff(hd_eslamdoust2026b(20, HDOM = c(8, 18, 30), BA = 30, TPH = 1500)$height) > 0))
  expect_true(all(diff(hd_eslamdoust2026b(20, HDOM = 18, BA = c(10, 30, 50), TPH = 1500)$height) > 0))
  expect_true(all(diff(hd_eslamdoust2026b(20, HDOM = 18, BA = 30, TPH = c(500, 1500, 4000))$height) < 0))
})

test_that("TR-79 is discoverable and selected only for plantation white pine", {
  info <- get_hd_model_info("eslamdoust2026b")
  expect_identical(info$engine, "hd_eslamdoust2026b")
  expect_identical(info$stand_origin, "plantation")
  expect_identical(info$species, "PINU.STR")
  expect_identical(info$predictors$arg, c("DBH", "HDOM", "BA", "TPH"))
  expect_true(all(info$predictors$required))
  expect_no_warning(
    out <- hd(20, "PINU.STR", "ON", stand_origin = "plantation", HDOM = 18, BA = 30, TPH = 1500)
  )
  expect_identical(out$model_id, "eslamdoust2026b")
  expect_equal(out$height, hd_eslamdoust2026b(20, HDOM = 18, BA = 30, TPH = 1500)$height)
  natural <- hd(20, "PINU.STR", "ON", HDOM = 18, BA = 30, TPH = 1500)
  expect_identical(natural$model_id, "eslamdoust2026_m5")
  expect_equal(
    natural$height,
    hd_eslamdoust2026(20, "PINU.STR", HDOM = 18, BA = 30, TPH = 1500)$height
  )
})
