testthat::test_that("si_kerbowling1991 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_kerbowling1991(
    age = c(25, 40, 60),
    si = c(11, 13, 16),
    species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_kerbowling1991 predicts si from height and returns single-column tibble", {
  out <- CanadaForestAllometry::si_kerbowling1991(
    age = c(25, 40, 60),
    height = c(8, 14, 20),
    species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("KerBowling1991 function matches manual equation evaluation", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get("parameters_KerBowling1991", envir = ns, inherits = FALSE) |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$Species == "ABIE.BAL") |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(pars) == 1)

  age <- 45
  si <- 14

  h_expected <- with(
    pars,
    1.3 + (b0 + b1 * si) * (1 - exp(-b2 * age))^(b3 + b4 * si)
  )

  h_out <- CanadaForestAllometry::si_kerbowling1991(
    age = age,
    si = si,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_kerbowling1991(
    age = age,
    height = h_expected,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-8)
})

testthat::test_that("si_kerbowling1991 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = c(20, 30),
      si = c(15, 15, 15),
      species = "ABIE.BAL"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = c(20, -1),
      height = c(10, 12),
      species = c("ABIE.BAL", "ABIE.BAL")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      height = 10,
      species = "NOPE.SPP"
    ),
    "No KerBowling1991 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      height = 10,
      si = 12,
      species = "ABIE.BAL"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      species = "ABIE.BAL"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_kerbowling1991 supports scalar recycling", {
  out <- CanadaForestAllometry::si_kerbowling1991(
    age = c(20, 30, 40),
    si = 14,
    species = "ABIE.BAL"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_kerbowling1991 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = "20",
      si = 10,
      species = "ABIE.BAL"
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      si = "10",
      species = "ABIE.BAL"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_kerbowling1991 validates positive finite predictors in both modes", {
  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      si = 0,
      species = "ABIE.BAL"
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      si = NA_real_,
      species = "ABIE.BAL"
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      height = 0,
      species = "ABIE.BAL"
    ),
    "height.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_kerbowling1991(
      age = 20,
      height = NA_real_,
      species = "ABIE.BAL"
    ),
    "height.*cannot contain NA",
    ignore.case = TRUE
  )
})
