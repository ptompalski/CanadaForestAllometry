testthat::test_that("si_pregent2016 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_pregent2016(
    age = c(20, 30, 40),
    si = c(8, 10, 12)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_pregent2016 predicts si from height and returns single-column tibble", {
  h_in <- CanadaForestAllometry::si_pregent2016(
    age = c(20, 30, 40),
    si = c(8, 10, 12)
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_pregent2016(
    age = c(20, 30, 40),
    height = h_in
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_pregent2016 matches the published Norway spruce equation", {
  age <- 40
  si <- 11
  base_age <- 25
  beta0 <- 148.2581
  beta2 <- -0.4405

  h_expected <- beta0 * (si / beta0)^((age / base_age)^beta2)

  h_out <- CanadaForestAllometry::si_pregent2016(
    age = age,
    si = si,
    base_age = base_age
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_pregent2016(
    age = age,
    height = h_expected,
    base_age = base_age
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-10)
})

testthat::test_that("si_pregent2016 returns site index unchanged at base age", {
  out <- CanadaForestAllometry::si_pregent2016(
    age = 25,
    si = 10
  )

  testthat::expect_equal(out$height[[1]], 10, tolerance = 1e-12)
})

testthat::test_that("si_pregent2016 supports scalar recycling and custom base age", {
  out <- CanadaForestAllometry::si_pregent2016(
    age = c(20, 30, 40),
    si = 12,
    base_age = 30
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_pregent2016 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = c(20, 30),
      si = c(12, 12, 12)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = 0,
      si = 12
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = 20,
      height = 10,
      si = 12
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = 20
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = 20,
      si = 12,
      base_age = 0
    ),
    "base_age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = 20,
      si = 200
    ),
    "must contain values <",
    ignore.case = TRUE
  )

  testthat::expect_warning(
    out <- CanadaForestAllometry::si_pregent2016(
      age = 71,
      si = 12
    ),
    "age.*70",
    ignore.case = TRUE
  )

  testthat::expect_named(out, "height")
  testthat::expect_true(is.finite(out$height[[1]]))
})

testthat::test_that("si_pregent2016 validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_pregent2016(
      age = numeric(0),
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_pregent2016 catches non-finite and negative height predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_pregent2016(
        age = 20,
        si = 12
      ),
      .pregent2016_height = function(age, si, base_age) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_pregent2016(
        age = 20,
        si = 12
      ),
      .pregent2016_height = function(age, si, base_age) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_pregent2016 catches non-finite and negative site-index predictions", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_pregent2016(
        age = 20,
        height = 10
      ),
      .pregent2016_si = function(age, height, base_age) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::si_pregent2016(
        age = 20,
        height = 10
      ),
      .pregent2016_si = function(age, height, base_age) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
