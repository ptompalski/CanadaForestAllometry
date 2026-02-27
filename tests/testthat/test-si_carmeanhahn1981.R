testthat::test_that("si_carmeanhahn1981 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_carmeanhahn1981(
    age = c(30, 50, 70),
    si = c(12, 16, 20),
    species = c("ABIE.BAL", "ABIE.BAL", "PICE.GLA")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_carmeanhahn1981 predicts si from height and returns single-column tibble", {
  out <- CanadaForestAllometry::si_carmeanhahn1981(
    age = c(30, 50, 70),
    height = c(8, 15, 22),
    species = c("ABIE.BAL", "ABIE.BAL", "PICE.GLA")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("CarmeanHahn1981 function matches manual equation evaluation", {
  age <- 60
  si_m <- 18
  si_ft <- si_m * 3.28084

  # ABIE.BAL coefficients from Carmean & Hahn (1981), H equation
  h_ft_expected <- 2.0901 *
    (si_ft^0.9296) *
    (1 - exp(-0.0280 * age))^(2.8280 * (si_ft^-0.1403))
  h_m_expected <- h_ft_expected / 3.28084

  h_out <- CanadaForestAllometry::si_carmeanhahn1981(
    age = age,
    si = si_m,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(h_out$height[[1]], h_m_expected, tolerance = 1e-10)

  height_m <- 18
  height_ft <- height_m * 3.28084

  # PICE.GLA coefficients from Carmean & Hahn (1981), S equation
  si_ft_expected <- 0.0833 *
    (height_ft^1.3965) *
    (1 - exp(-0.0196 * age))^(-8.0895 * (height_ft^-0.3659))
  si_m_expected <- si_ft_expected / 3.28084

  si_out <- CanadaForestAllometry::si_carmeanhahn1981(
    age = age,
    height = height_m,
    species = "PICE.GLA"
  )
  testthat::expect_equal(si_out$si[[1]], si_m_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmeanhahn1981 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmeanhahn1981(
      age = c(30, 40),
      si = c(15, 15, 15),
      species = "ABIE.BAL"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmeanhahn1981(
      age = c(30, -1),
      height = c(10, 12),
      species = c("ABIE.BAL", "ABIE.BAL")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmeanhahn1981(
      age = 30,
      height = 10,
      species = "PINU.BAN"
    ),
    "No CarmeanHahn1981 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmeanhahn1981(
      age = 30,
      height = 10,
      si = 12,
      species = "ABIE.BAL"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmeanhahn1981(
      age = 30,
      species = "ABIE.BAL"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})
