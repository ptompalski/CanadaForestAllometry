testthat::test_that("si_buckman2006 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_buckman2006(
    age = c(15, 25, 50),
    si = c(14, 16, 18)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_buckman2006 predicts si from height and returns single-column tibble", {
  out <- CanadaForestAllometry::si_buckman2006(
    age = c(15, 25, 50),
    height = c(4, 10, 18)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_buckman2006 matches manual equation evaluation for both branches", {
  age_young <- 15
  age_old <- 35
  si_ft <- 60

  k <- 1.41876e-3
  m <- 1.05304e-6
  A <- 1.8604
  B <- 0.020928
  C <- 1.4349

  h_young_ft <- si_ft * (k * age_young^2 - m * age_young^4)
  h_old_ft <- si_ft * A * (1 - exp(-B * age_old))^C

  out_young <- CanadaForestAllometry::si_buckman2006(
    age = age_young,
    si = si_ft / 3.28084
  )
  out_old <- CanadaForestAllometry::si_buckman2006(
    age = age_old,
    si = si_ft / 3.28084
  )

  testthat::expect_equal(out_young$height[[1]] * 3.28084, h_young_ft, tolerance = 1e-10)
  testthat::expect_equal(out_old$height[[1]] * 3.28084, h_old_ft, tolerance = 1e-10)

  si_young <- CanadaForestAllometry::si_buckman2006(
    age = age_young,
    height = h_young_ft / 3.28084
  )
  si_old <- CanadaForestAllometry::si_buckman2006(
    age = age_old,
    height = h_old_ft / 3.28084
  )

  testthat::expect_equal(si_young$si[[1]] * 3.28084, si_ft, tolerance = 1e-10)
  testthat::expect_equal(si_old$si[[1]] * 3.28084, si_ft, tolerance = 1e-10)
})

testthat::test_that("si_buckman2006 returns site index at age 50 unchanged", {
  out <- CanadaForestAllometry::si_buckman2006(
    age = 50,
    si = 22
  )

  testthat::expect_equal(out$height[[1]], 22, tolerance = 1e-4)
})

testthat::test_that("si_buckman2006 uses the old-growth branch at age 20", {
  age <- 20
  si_ft <- 55
  A <- 1.8604
  B <- 0.020928
  C <- 1.4349
  h_ft <- si_ft * A * (1 - exp(-B * age))^C

  out <- CanadaForestAllometry::si_buckman2006(
    age = age,
    si = si_ft / 3.28084
  )

  testthat::expect_equal(out$height[[1]] * 3.28084, h_ft, tolerance = 1e-10)
})

testthat::test_that("si_buckman2006 supports scalar recycling", {
  out <- CanadaForestAllometry::si_buckman2006(
    age = c(10, 20, 30),
    si = 18
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_buckman2006 breakout age formula matches quadratic derivation", {
  si_ft <- 60
  k <- 1.41876e-3
  m <- 1.05304e-6

  expected <- sqrt((k - sqrt(k^2 - 18 * m / si_ft)) / (2 * m))
  got <- CanadaForestAllometry:::.buckman2006_bh_age(si_ft)

  testthat::expect_equal(got, expected, tolerance = 1e-12)
})

testthat::test_that("si_buckman2006 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = c(10, 20),
      si = c(15, 15, 15)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20,
      height = 10,
      si = 12
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_buckman2006 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = numeric(0),
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = "20",
      si = 10
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20,
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_buckman2006 validates positive finite predictors in both modes", {
  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 0,
      si = 12
    ),
    "age.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20,
      si = 0
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20,
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20,
      height = 0
    ),
    "height.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_buckman2006(
      age = 20,
      height = NA_real_
    ),
    "height.*cannot contain NA",
    ignore.case = TRUE
  )
})

testthat::test_that("si_buckman2006 catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_buckman2006(
        age = 20,
        si = 12
      ),
      .buckman2006_height = function(age, si_ft) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_buckman2006(
        age = 20,
        si = 12
      ),
      .buckman2006_height = function(age, si_ft) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_buckman2006 catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_buckman2006(
        age = 20,
        height = 10
      ),
      .buckman2006_si = function(age, height_ft) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_buckman2006(
        age = 20,
        height = 10
      ),
      .buckman2006_si = function(age, height_ft) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_buckman2006 catches invalid relative height multipliers", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry:::.buckman2006_si(
        age = 20,
        height_ft = 30
      ),
      .buckman2006_relative_height = function(age) c(0),
      .package = "CanadaForestAllometry"
    ),
    "Invalid relative height multiplier",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry:::.buckman2006_si(
        age = 20,
        height_ft = 30
      ),
      .buckman2006_relative_height = function(age) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Invalid relative height multiplier",
    ignore.case = TRUE
  )
})
