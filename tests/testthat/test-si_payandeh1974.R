testthat::test_that("si_payandeh1974 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_payandeh1974(
    age = c(20, 40, 60),
    si = c(12, 18, 24),
    species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_payandeh1974 predicts si from height and returns single-column tibble", {
  out <- CanadaForestAllometry::si_payandeh1974(
    age = c(20, 40, 60),
    height = c(8, 16, 28),
    species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("Payandeh1974 function matches manual equation evaluation", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get("parameters_Payandeh1974", envir = ns, inherits = FALSE) |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$Species == "ABIE.BAL") |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(pars) == 1)

  age <- 45
  si_m <- 18
  si_ft <- si_m * 3.28084

  h_ft_expected <- with(
    pars,
    height_b1 *
      (si_ft^height_b2) *
      (1 - exp(height_b3 * age))^(height_b4 * (si_ft^height_b5))
  )
  h_m_expected <- h_ft_expected / 3.28084

  h_out <- CanadaForestAllometry::si_payandeh1974(
    age = age,
    si = si_m,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(h_out$height[[1]], h_m_expected, tolerance = 1e-10)

  h_m <- 15
  h_ft <- h_m * 3.28084
  si_ft_expected <- with(
    pars,
    si_b1 *
      (h_ft^si_b2) *
      (1 - exp(si_b3 * age))^(si_b4 * (h_ft^si_b5))
  )
  si_m_expected <- si_ft_expected / 3.28084

  si_out <- CanadaForestAllometry::si_payandeh1974(
    age = age,
    height = h_m,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(si_out$si[[1]], si_m_expected, tolerance = 1e-10)
})

testthat::test_that("si_payandeh1974 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = c(20, 30),
      si = c(15, 15, 15),
      species = "ABIE.BAL"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = c(20, -1),
      height = c(10, 12),
      species = c("ABIE.BAL", "ABIE.BAL")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      height = 10,
      species = "NOPE.SPP"
    ),
    "No Payandeh1974 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      height = 10,
      si = 12,
      species = "ABIE.BAL"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      species = "ABIE.BAL"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_payandeh1974 supports scalar recycling", {
  out <- CanadaForestAllometry::si_payandeh1974(
    age = c(20, 30, 40),
    si = 18,
    species = "ABIE.BAL"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_payandeh1974 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = "20",
      si = 10,
      species = "ABIE.BAL"
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      si = "10",
      species = "ABIE.BAL"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_payandeh1974 validates positive finite predictors in both modes", {
  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      si = 0,
      species = "ABIE.BAL"
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      si = NA_real_,
      species = "ABIE.BAL"
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      height = 0,
      species = "ABIE.BAL"
    ),
    "height.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_payandeh1974(
      age = 20,
      height = NA_real_,
      species = "ABIE.BAL"
    ),
    "height.*cannot contain NA",
    ignore.case = TRUE
  )
})
