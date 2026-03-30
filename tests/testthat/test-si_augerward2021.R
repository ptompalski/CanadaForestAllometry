testthat::test_that("si_augerward2021 predicts height from si for both species", {
  out <- CanadaForestAllometry::si_augerward2021(
    age = c(20, 30),
    si = c(9, 12),
    species = c("PICE.MAR", "PINU.BAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_augerward2021 predicts si from height for both species", {
  h_in <- CanadaForestAllometry::si_augerward2021(
    age = c(20, 30),
    si = c(9, 12),
    species = c("PICE.MAR", "PINU.BAN")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_augerward2021(
    age = c(20, 30),
    height = h_in,
    species = c("PICE.MAR", "PINU.BAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_augerward2021 matches the published black spruce equation", {
  age <- 40
  si <- 9
  base_age <- 25
  beta0 <- 24.6300
  beta2 <- 1.5481

  h_expected <- beta0 /
    (1 - (1 - beta0 / si) * (base_age / age)^beta2)

  h_out <- CanadaForestAllometry::si_augerward2021(
    age = age,
    si = si,
    species = "PICE.MAR",
    base_age = base_age
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_augerward2021(
    age = age,
    height = h_expected,
    species = "PICE.MAR",
    base_age = base_age
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-10)
})

testthat::test_that("si_augerward2021 matches the published jack pine equation", {
  age <- 40
  si <- 12
  base_age <- 25
  beta0 <- 26.5930
  beta2 <- 1.2178

  h_expected <- beta0 - beta0 * (1 - si / beta0)^((age / base_age)^beta2)

  h_out <- CanadaForestAllometry::si_augerward2021(
    age = age,
    si = si,
    species = "PINU.BAN",
    base_age = base_age
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_augerward2021(
    age = age,
    height = h_expected,
    species = "PINU.BAN",
    base_age = base_age
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-10)
})

testthat::test_that("si_augerward2021 returns site index unchanged at base age", {
  out <- CanadaForestAllometry::si_augerward2021(
    age = c(25, 25),
    si = c(9, 12),
    species = c("PICE.MAR", "PINU.BAN")
  )

  testthat::expect_equal(out$height, c(9, 12), tolerance = 1e-12)
})

testthat::test_that("si_augerward2021 supports scalar recycling and custom base age", {
  out <- CanadaForestAllometry::si_augerward2021(
    age = c(20, 30, 40),
    si = 12,
    species = "PINU.BAN",
    base_age = 50
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_augerward2021 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = c(20, 30),
      si = c(12, 12, 12),
      species = c("PINU.BAN", "PINU.BAN")
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 0,
      si = 12,
      species = "PINU.BAN"
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 20,
      height = 10,
      si = 12,
      species = "PINU.BAN"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 20,
      species = "PINU.BAN"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 20,
      si = 12,
      species = "PINU.BAN",
      base_age = 0
    ),
    "base_age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 20,
      si = 12,
      species = "ABIE.BAL"
    ),
    "No AugerWard2021 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_warning(
    out <- CanadaForestAllometry::si_augerward2021(
      age = 101,
      si = 12,
      species = "PINU.BAN"
    ),
    "age.*100",
    ignore.case = TRUE
  )

  testthat::expect_named(out, "height")
  testthat::expect_true(is.finite(out$height[[1]]))
})

testthat::test_that("si_augerward2021 validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_augerward2021 validates species-specific asymptote bounds", {
  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 20,
      si = 24.63,
      species = "PICE.MAR"
    ),
    "asymptote.*PICE.MAR",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_augerward2021(
      age = 20,
      height = 26.593,
      species = "PINU.BAN"
    ),
    "asymptote.*PINU.BAN",
    ignore.case = TRUE
  )
})

testthat::test_that("si_augerward2021 catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_augerward2021(
        age = 20,
        si = 12,
        species = "PINU.BAN"
      ),
      .augerward2021_height = function(age, si, base_age, Species) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_augerward2021(
        age = 20,
        si = 12,
        species = "PINU.BAN"
      ),
      .augerward2021_height = function(age, si, base_age, Species) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_augerward2021 catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_augerward2021(
        age = 20,
        height = 10,
        species = "PICE.MAR"
      ),
      .augerward2021_si = function(age, height, base_age, Species) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_augerward2021(
        age = 20,
        height = 10,
        species = "PICE.MAR"
      ),
      .augerward2021_si = function(age, height, base_age, Species) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
