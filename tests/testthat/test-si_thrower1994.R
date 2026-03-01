testthat::test_that("si_thrower1994 predicts height from si", {
  out <- CanadaForestAllometry::si_thrower1994(
    age = c(25, 40, 60, 80),
    si = c(14, 18, 20, 16),
    species = c("PINU.CON", "THUJ.PLI", "PINU.MON", "LARI.OCC")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_thrower1994 predicts si from height", {
  h_in <- CanadaForestAllometry::si_thrower1994(
    age = c(25, 40, 60, 80),
    si = c(14, 18, 20, 16),
    species = c("PINU.CON", "THUJ.PLI", "PINU.MON", "LARI.OCC")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_thrower1994(
    age = c(25, 40, 60, 80),
    height = h_in,
    species = c("PINU.CON", "THUJ.PLI", "PINU.MON", "LARI.OCC")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_thrower1994 matches manual equation for PINU.CON (logexp form)", {
  age <- 35
  si <- 16

  b1 <- 1 + exp(7.815 - 1.285 * log(50) - 1.007 * log(si - 1.3))
  b2 <- 1 + exp(7.815 - 1.285 * log(age) - 1.007 * log(si - 1.3))
  h_expected <- 1.3 + (si - 1.3) * (b1 / b2)

  out <- CanadaForestAllometry::si_thrower1994(
    age = age,
    si = si,
    species = "PINU.CON"
  )

  testthat::expect_equal(out$height[[1]], h_expected, tolerance = 1e-10)
})

testthat::test_that("si_thrower1994 uses explicit inverse for PSEU.MEN", {
  age <- 45
  h <- 22
  si_expected <- 0.39 + 0.31 * h + 33.38 * h / age

  out <- CanadaForestAllometry::si_thrower1994(
    age = age,
    height = h,
    species = "PSEU.MEN"
  )

  testthat::expect_equal(out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_thrower1994 supports scalar recycling", {
  out <- CanadaForestAllometry::si_thrower1994(
    age = c(20, 30, 40),
    si = 16,
    species = "PINU.CON"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
})

testthat::test_that("si_thrower1994 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 20,
      height = 10,
      si = 12,
      species = "PINU.CON"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 20,
      si = 12,
      species = "XXXX.YYY"
    ),
    "Unrecognized species codes|No Thrower1994 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = c(20, 30),
      si = c(12, 14, 16),
      species = c("PINU.CON", "PINU.CON")
    ),
    "length 1 or",
    ignore.case = TRUE
  )
})
