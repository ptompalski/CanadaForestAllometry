testthat::test_that("si_sharma2022 predicts height from site index", {
  out <- CanadaForestAllometry::si_sharma2022(
    age = c(40, 60),
    si = c(15, 18),
    species = c("PICE.MAR", "POPU.TRE")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_sharma2022 predicts site index from height", {
  h_in <- CanadaForestAllometry::si_sharma2022(
    age = c(40, 60),
    si = c(15, 18),
    species = c("PICE.MAR", "POPU.TRE")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_sharma2022(
    age = c(40, 60),
    height = h_in,
    species = c("PICE.MAR", "POPU.TRE")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$si, c(15, 18), tolerance = 1e-10)
})

testthat::test_that("si_sharma2022 matches manual equation evaluation", {
  age <- c(40, 60)
  si <- c(15, 18)
  species <- c("PICE.MAR", "POPU.TRE")

  a0 <- c(48.2867, 36.7879)
  a1 <- c(1.1390, 1.0700)

  h_expected <- a0 / (1 - (1 - a0 / si) * (50 / age)^a1)

  h_out <- CanadaForestAllometry::si_sharma2022(
    age = age,
    si = si,
    species = species
  )
  testthat::expect_equal(h_out$height, h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_sharma2022(
    age = age,
    height = h_expected,
    species = species
  )
  testthat::expect_equal(si_out$si, si, tolerance = 1e-10)
})

testthat::test_that("si_sharma2022 supports custom base age and scalar recycling", {
  out <- CanadaForestAllometry::si_sharma2022(
    age = c(20, 30, 40),
    si = 14,
    species = "PICE.MAR",
    base_age = 25
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_sharma2022 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = c(20, 30),
      si = c(12, 12, 12),
      species = "PICE.MAR"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = 0,
      si = 12,
      species = "PICE.MAR"
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = 20,
      height = 10,
      si = 12,
      species = "PICE.MAR"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = 20,
      species = "PICE.MAR"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = 20,
      si = 12,
      species = "ABIE.BAL"
    ),
    "No Sharma2022 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = 20,
      si = 12,
      species = "PICE.MAR",
      base_age = 0
    ),
    "base_age.*> 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharma2022 validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_sharma2022(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharma2022 catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2022(
        age = 20,
        si = 12,
        species = "PICE.MAR"
      ),
      .mcdill_amateis_height = function(age, si, base_age, a0, a1) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2022(
        age = 20,
        si = 12,
        species = "PICE.MAR"
      ),
      .mcdill_amateis_height = function(age, si, base_age, a0, a1) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharma2022 catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2022(
        age = 20,
        height = 10,
        species = "PICE.MAR"
      ),
      .mcdill_amateis_si = function(age, height, base_age, a0, a1) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2022(
        age = 20,
        height = 10,
        species = "PICE.MAR"
      ),
      .mcdill_amateis_si = function(age, height, base_age, a0, a1) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
