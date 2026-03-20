testthat::test_that("si_carmean1996 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 40, 60),
    si = c(10, 14, 18),
    species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_carmean1996 predicts si from height and returns single-column tibble", {
  h_in <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 40, 60),
    si = c(10, 14, 18),
    species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 40, 60),
    height = h_in,
    species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_carmean1996 matches manual black spruce equation evaluation", {
  age <- 40
  si <- 15

  h_expected <- 1.3 +
    (si - 1.3) *
    (1 + exp(9.2248 - 1.28875 * log(50) - 1.5612 * log(si - 1.3))) /
    (1 + exp(9.2248 - 1.28875 * log(age) - 1.5612 * log(si - 1.3)))

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "PICE.MAR"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  height <- 12
  si_expected <- 9.0023 +
    0.4396 * (height - 1.3) +
    1.4753 * log(height - 1.3) -
    0.7996 * log(age) -
    0.3976 * log(age)^2 +
    19.26275 * (height - 1.3) / age

  si_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = height,
    species = "PICE.MAR"
  )
  testthat::expect_equal(si_out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmean1996 round-trips white spruce numeric inverse", {
  age <- 35
  si <- 16

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "PICE.GLA"
  )

  si_back <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = h_out$height[[1]],
    species = "PICE.GLA"
  )

  testthat::expect_equal(si_back$si[[1]], si, tolerance = 1e-5)
})

testthat::test_that("si_carmean1996 supports scalar recycling", {
  out <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 30, 40),
    si = 14,
    species = "PICE.MAR"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
})

testthat::test_that("si_carmean1996 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = c(20, 30),
      si = c(15, 15, 15),
      species = "PICE.MAR"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = c(20, -1),
      height = c(10, 12),
      species = c("PICE.MAR", "PICE.MAR")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 10,
      species = "NOPE.SPP"
    ),
    "No Carmean1996 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 10,
      si = 12,
      species = "PICE.MAR"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1996 validates species-specific lower bounds", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      si = 1.3,
      species = "PICE.GLA"
    ),
    "si.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 1.3,
      species = "PICE.MAR"
    ),
    "height.*> 1.3",
    ignore.case = TRUE
  )
})
