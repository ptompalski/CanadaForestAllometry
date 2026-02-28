testthat::test_that("si_nigh2000_gi predicts site index and returns single-column tibble", {
  out <- CanadaForestAllometry::si_nigh2000_gi(
    age = c(5, 15, 30, 45),
    gi = c(12, 8, 6, 5)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("Nigh2000 GI function matches manual equation evaluation", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get("parameters_Nigh2000_gi", envir = ns, inherits = FALSE) |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$age == 15L) |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(pars) == 1)

  gi <- 8
  si_expected <- with(pars, 1.3 + b0 * (gi^b1))

  out <- CanadaForestAllometry::si_nigh2000_gi(
    age = 15,
    gi = gi
  )

  testthat::expect_equal(out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_nigh2000_gi input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = c(5, 10),
      gi = c(8, 9, 10)
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = 20.5,
      gi = 8
    ),
    "integer breast-height ages",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = 51,
      gi = 8
    ),
    "age.*\\[1, 50\\]",
    ignore.case = TRUE
  )

})

testthat::test_that("si_nigh2000_gi supports scalar recycling", {
  out <- CanadaForestAllometry::si_nigh2000_gi(
    age = c(10, 20, 30),
    gi = 8
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "si")
  testthat::expect_true(all(is.finite(out$si)))
})

testthat::test_that("si_nigh2000_gi validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = numeric(0),
      gi = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = "20",
      gi = 10
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = 20,
      gi = "10"
    ),
    "gi.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh2000_gi validates positive finite predictors", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = 20,
      gi = 0
    ),
    "gi.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_nigh2000_gi(
      age = 20,
      gi = NA_real_
    ),
    "gi.*cannot contain NA",
    ignore.case = TRUE
  )
})
