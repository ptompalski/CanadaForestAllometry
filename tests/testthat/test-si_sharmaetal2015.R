testthat::test_that("si_sharma2015 predicts total height from site index by default", {
  out <- CanadaForestAllometry::si_sharma2015(
    age = c(20, 30),
    si = c(9, 10),
    species = c("PINU.BAN", "PICE.MAR")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 1.3))
})

testthat::test_that("si_sharma2015 predicts site index from total height by default", {
  h_in <- CanadaForestAllometry::si_sharma2015(
    age = c(20, 30),
    si = c(9, 10),
    species = c("PINU.BAN", "PICE.MAR")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_sharma2015(
    age = c(20, 30),
    height = h_in,
    species = c("PINU.BAN", "PICE.MAR")
  )

  testthat::expect_equal(out$si, c(9, 10), tolerance = 1e-10)
})

testthat::test_that("si_sharma2015 matches manual equation evaluation", {
  age <- c(20, 30)
  si <- c(9, 10)
  species <- c("PINU.BAN", "PICE.MAR")

  a0 <- c(32.2567, 36.8046)
  a1 <- c(1.2156, 1.1638)

  h_above_bh <- a0 / (1 - (1 - a0 / si) * (50 / age)^a1)

  h_out <- CanadaForestAllometry::si_sharma2015(
    age = age,
    si = si,
    species = species
  )
  testthat::expect_equal(h_out$height, h_above_bh + 1.3, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_sharma2015(
    age = age,
    height = h_above_bh + 1.3,
    species = species
  )
  testthat::expect_equal(si_out$si, si, tolerance = 1e-10)
})

testthat::test_that("si_sharma2015 supports source-scale height with total_height = FALSE", {
  h_out <- CanadaForestAllometry::si_sharma2015(
    age = c(20, 30),
    si = c(9, 10),
    species = c("PINU.BAN", "PICE.MAR"),
    total_height = FALSE
  )

  si_out <- CanadaForestAllometry::si_sharma2015(
    age = c(20, 30),
    height = h_out$height,
    species = c("PINU.BAN", "PICE.MAR"),
    total_height = FALSE
  )

  testthat::expect_equal(si_out$si, c(9, 10), tolerance = 1e-10)
})

testthat::test_that("si_sharma2015 supports scalar recycling and custom base age", {
  out <- CanadaForestAllometry::si_sharma2015(
    age = c(20, 30, 40),
    si = 10,
    species = "PINU.BAN",
    base_age = 25
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_sharma2015 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = c(20, 30),
      si = c(10, 10, 10),
      species = "PINU.BAN"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = 0,
      si = 10,
      species = "PINU.BAN"
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = 20,
      height = 10,
      si = 10,
      species = "PINU.BAN"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = 20,
      si = 10,
      species = "ABIE.BAL"
    ),
    "No SharmaEtAl2015 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = 20,
      si = 10,
      species = "PINU.BAN",
      base_age = 0
    ),
    "base_age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = 20,
      si = 10,
      species = "PINU.BAN",
      total_height = NA
    ),
    "total_height.*TRUE/FALSE",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = 20,
      height = 1.3,
      species = "PINU.BAN"
    ),
    "height.*> 1.3",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharma2015 validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_sharma2015(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharma2015 catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2015(
        age = 20,
        si = 10,
        species = "PINU.BAN"
      ),
      .mcdill_amateis_height = function(age, si, base_age, a0, a1) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2015(
        age = 20,
        si = 10,
        species = "PINU.BAN"
      ),
      .mcdill_amateis_height = function(age, si, base_age, a0, a1) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharma2015 catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2015(
        age = 20,
        height = 10,
        species = "PINU.BAN"
      ),
      .mcdill_amateis_si = function(age, height, base_age, a0, a1) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharma2015(
        age = 20,
        height = 10,
        species = "PINU.BAN"
      ),
      .mcdill_amateis_si = function(age, height, base_age, a0, a1) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
