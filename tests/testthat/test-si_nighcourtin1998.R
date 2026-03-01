testthat::test_that("si_nighcourtin1998 predicts height from si", {
  out <- CanadaForestAllometry::si_nighcourtin1998(
    age = c(10, 25, 40),
    si = c(14, 18, 22)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_nighcourtin1998 predicts si from height", {
  h_in <- CanadaForestAllometry::si_nighcourtin1998(
    age = c(10, 25, 40),
    si = c(14, 18, 22)
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_nighcourtin1998(
    age = c(10, 25, 40),
    height = h_in
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_nighcourtin1998 supports SI50 conversion in both directions", {
  age <- c(10, 25, 40)
  si25 <- c(14, 18, 22)
  si50 <- -0.4063 + 1.313 * si25

  h_from_si25 <- CanadaForestAllometry::si_nighcourtin1998(
    age = age,
    si = si25
  ) |>
    dplyr::pull(height)

  h_from_si50 <- CanadaForestAllometry::si_nighcourtin1998(
    age = age,
    si = si50,
    si50 = TRUE
  ) |>
    dplyr::pull(height)

  testthat::expect_equal(h_from_si50, h_from_si25, tolerance = 1e-10)

  si50_out <- CanadaForestAllometry::si_nighcourtin1998(
    age = age,
    height = h_from_si25,
    si50 = TRUE
  )
  testthat::expect_equal(si50_out$si, si50, tolerance = 1e-2)
})

testthat::test_that("NighCourtin1998 model matches manual equations", {
  age <- 25
  si <- 20

  h_expected <- 1.3 + 1.693 * (si - 1.3) /
    (1 + exp(3.600 - 1.240 * log(age - 0.5)))

  h_out <- CanadaForestAllometry::si_nighcourtin1998(
    age = age,
    si = si
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_nighcourtin1998(
    age = age,
    height = h_expected
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-2)
})

testthat::test_that("si_nighcourtin1998 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = c(20, 30),
      si = c(15, 15, 15)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 0.5,
      si = 12
    ),
    "age.*> 0.5",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      height = 10,
      si = 12
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      si = 12,
      si50 = "yes"
    ),
    "si50.*TRUE or FALSE",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nighcourtin1998 supports scalar recycling", {
  out <- CanadaForestAllometry::si_nighcourtin1998(
    age = c(10, 20, 30),
    si = 16
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_nighcourtin1998 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = numeric(0),
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = "20",
      si = 10
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nighcourtin1998 validates positive finite predictors", {
  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      si = 1.3
    ),
    "si.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      height = 0
    ),
    "height.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      height = NA_real_
    ),
    "height.*cannot contain NA",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nighcourtin1998 catches non-finite and negative outputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 1e300,
      si = 1.79e308
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 0.5000000000000001,
      height = 1e308
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      height = 0.1
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nighcourtin1998 validates SI50 input domain after conversion", {
  testthat::expect_error(
    CanadaForestAllometry::si_nighcourtin1998(
      age = 20,
      si = 1,
      si50 = TRUE
    ),
    "correspond to SI25 > 1.3",
    ignore.case = TRUE
  )
})
