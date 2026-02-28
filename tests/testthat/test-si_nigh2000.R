testthat::test_that("si_nigh2000 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_nigh2000(
    age = c(25, 50, 90),
    si = c(12, 16, 22)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_nigh2000 predicts si from height and returns single-column tibble", {
  h_in <- CanadaForestAllometry::si_nigh2000(
    age = c(25, 50, 90),
    si = c(12, 16, 22)
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_nigh2000(
    age = c(25, 50, 90),
    height = h_in
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("Nigh2000 site-index function matches manual equation evaluation", {
  age <- 45
  si <- 14

  h_expected <- 1.3 + (si - 1.3) *
    (1 + exp(9.474 - 1.340 * log(49.5) - 1.244 * log(si - 1.3))) /
    (1 + exp(9.474 - 1.340 * log(age - 0.5) - 1.244 * log(si - 1.3)))

  h_out <- CanadaForestAllometry::si_nigh2000(
    age = age,
    si = si
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_nigh2000(
    age = age,
    height = h_expected
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-5)
})

testthat::test_that("si_nigh2000 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = c(20, 30),
      si = c(15, 15, 15)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 0.5,
      si = 12
    ),
    "age.*> 0.5",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20,
      height = 10,
      si = 12
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh2000 supports scalar recycling", {
  out <- CanadaForestAllometry::si_nigh2000(
    age = c(20, 30, 40),
    si = 14
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_nigh2000 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = numeric(0),
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = "20",
      si = 10
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20,
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh2000 validates positive finite predictors in both modes", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20,
      si = 1.3
    ),
    "si.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20,
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20,
      height = 0
    ),
    "height.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000(
      age = 20,
      height = NA_real_
    ),
    "height.*cannot contain NA",
    ignore.case = TRUE
  )
})
