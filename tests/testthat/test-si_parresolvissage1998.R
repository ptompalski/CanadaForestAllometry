testthat::test_that("si_parresolvissage1998 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_parresolvissage1998(
    age = c(25, 50, 70),
    si = c(12, 18, 24)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_parresolvissage1998 predicts si from height and returns single-column tibble", {
  out <- CanadaForestAllometry::si_parresolvissage1998(
    age = c(25, 50, 70),
    height = c(10, 18, 24)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_parresolvissage1998 matches source worked examples", {
  h_25 <- CanadaForestAllometry::si_parresolvissage1998(
    age = 35,
    si = 30 / 3.28084,
    base_age = 25
  )
  testthat::expect_equal(h_25$height[[1]] * 3.28084, 47.0, tolerance = 0.1)

  h_50 <- CanadaForestAllometry::si_parresolvissage1998(
    age = 70,
    si = 80 / 3.28084,
    base_age = 50
  )
  testthat::expect_equal(h_50$height[[1]] * 3.28084, 102.2, tolerance = 0.1)
})

testthat::test_that("si_parresolvissage1998 matches manual equation evaluation", {
  age <- 70
  base_age <- 50
  si_ft <- 80

  h_ft_expected <- exp(
    exp(8.6188 * (1 / age - 1 / base_age)) *
      (log(si_ft) + 74.7099 / base_age - 2.0862) -
      74.7099 / age + 2.0862
  )

  h_out <- CanadaForestAllometry::si_parresolvissage1998(
    age = age,
    si = si_ft / 3.28084,
    base_age = base_age
  )
  testthat::expect_equal(h_out$height[[1]] * 3.28084, h_ft_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_parresolvissage1998(
    age = age,
    height = h_ft_expected / 3.28084,
    base_age = base_age
  )
  testthat::expect_equal(si_out$si[[1]] * 3.28084, si_ft, tolerance = 1e-10)
})

testthat::test_that("si_parresolvissage1998 supports scalar recycling", {
  out <- CanadaForestAllometry::si_parresolvissage1998(
    age = c(20, 30, 40),
    si = 14
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_parresolvissage1998 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = c(20, 30),
      si = c(15, 15, 15)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = c(20, 9),
      height = c(10, 12)
    ),
    "age.*>= 10",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      height = 10,
      si = 12
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      si = 12,
      base_age = NA_real_
    ),
    "base_age.*finite numeric value >= 10",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      si = 12,
      base_age = 9
    ),
    "base_age.*finite numeric value >= 10",
    ignore.case = TRUE
  )
})

testthat::test_that("si_parresolvissage1998 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = numeric(0),
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = "20",
      si = 10
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_parresolvissage1998 validates positive finite predictors in both modes", {
  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 9,
      si = 12
    ),
    "age.*>= 10",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      si = 0
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      height = 0
    ),
    "height.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_parresolvissage1998(
      age = 20,
      height = NA_real_
    ),
    "height.*cannot contain NA",
    ignore.case = TRUE
  )
})

testthat::test_that("si_parresolvissage1998 catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_parresolvissage1998(
        age = 20,
        si = 12
      ),
      .parresolvissage1998_height = function(age, si_ft, base_age) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_parresolvissage1998(
        age = 20,
        si = 12
      ),
      .parresolvissage1998_height = function(age, si_ft, base_age) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_parresolvissage1998 catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_parresolvissage1998(
        age = 20,
        height = 10
      ),
      .parresolvissage1998_si = function(age, height_ft, base_age) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_parresolvissage1998(
        age = 20,
        height = 10
      ),
      .parresolvissage1998_si = function(age, height_ft, base_age) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
