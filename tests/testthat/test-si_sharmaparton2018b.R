testthat::test_that("si_sharmaparton2018b predicts total height from si by default", {
  out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = c(20, 30, 40),
    si = c(9, 11, 13)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 1.3))
})

testthat::test_that("si_sharmaparton2018b predicts si from total height by default", {
  h_in <- CanadaForestAllometry::si_sharmaparton2018b(
    age = c(20, 30, 40),
    si = c(9, 11, 13)
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = c(20, 30, 40),
    height = h_in
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("SharmaParton2018b site-index function matches manual equation evaluation", {
  age <- 40
  si <- 12
  base_age <- 25

  h_bh_expected <- 43.7309 / (1 - (1 - 43.7309 / si) * (base_age / age)^1.2000)
  h_total_expected <- h_bh_expected + 1.3

  h_out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = age,
    si = si,
    base_age = base_age
  )
  testthat::expect_equal(h_out$height[[1]], h_total_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = age,
    height = h_total_expected,
    base_age = base_age
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-10)
})

testthat::test_that("si_sharmaparton2018b supports source-scale height with total_height = FALSE", {
  age <- 40
  si <- 12
  h_bh_expected <- 43.7309 / (1 - (1 - 43.7309 / si) * (25 / age)^1.2000)

  h_out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = age,
    si = si,
    base_age = 25,
    total_height = FALSE
  )
  testthat::expect_equal(h_out$height[[1]], h_bh_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = age,
    height = h_bh_expected,
    base_age = 25,
    total_height = FALSE
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-10)
})

testthat::test_that("si_sharmaparton2018b supports scalar recycling and custom base age", {
  out <- CanadaForestAllometry::si_sharmaparton2018b(
    age = c(20, 30, 40),
    si = 12,
    base_age = 50
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 1.3))
})

testthat::test_that("si_sharmaparton2018b input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = c(20, 30),
      si = c(12, 12, 12)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = 0,
      si = 12
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = 20,
      height = 10,
      si = 12
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = 20
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = 20,
      si = 12,
      base_age = 0
    ),
    "base_age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = 20,
      height = 1.3
    ),
    "height.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = 20,
      si = 12,
      total_height = NA
    ),
    "total_height.*TRUE/FALSE",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharmaparton2018b validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_sharmaparton2018b(
      age = numeric(0),
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharmaparton2018b catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharmaparton2018b(
        age = 20,
        si = 12
      ),
      .mcdill_amateis_height = function(age, si, base_age, a0, a1) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharmaparton2018b(
        age = 20,
        si = 12
      ),
      .mcdill_amateis_height = function(age, si, base_age, a0, a1) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_sharmaparton2018b catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharmaparton2018b(
        age = 20,
        height = 10
      ),
      .mcdill_amateis_si = function(age, height, base_age, a0, a1) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_sharmaparton2018b(
        age = 20,
        height = 10
      ),
      .mcdill_amateis_si = function(age, height, base_age, a0, a1) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
