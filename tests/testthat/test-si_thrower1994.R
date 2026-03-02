testthat::test_that("si_thrower1994 predicts height from si", {
  out <- CanadaForestAllometry::si_thrower1994(
    age = c(25, 40, 60, 80),
    si = c(14, 18, 20, 16),
    species = c("PINU.CON", "THUJ.PLI", "PINU.MON", "LARI.OCC")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_thrower1994 predicts si from height", {
  h_in <- CanadaForestAllometry::si_thrower1994(
    age = c(25, 40, 60, 80),
    si = c(14, 18, 20, 16),
    species = c("PINU.CON", "THUJ.PLI", "PINU.MON", "LARI.OCC")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_thrower1994(
    age = c(25, 40, 60, 80),
    height = h_in,
    species = c("PINU.CON", "THUJ.PLI", "PINU.MON", "LARI.OCC")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_thrower1994 matches manual equation for PINU.CON (logexp form)", {
  age <- 35
  si <- 16

  b1 <- 1 + exp(7.815 - 1.285 * log(50) - 1.007 * log(si - 1.3))
  b2 <- 1 + exp(7.815 - 1.285 * log(age) - 1.007 * log(si - 1.3))
  h_expected <- 1.3 + (si - 1.3) * (b1 / b2)

  out <- CanadaForestAllometry::si_thrower1994(
    age = age,
    si = si,
    species = "PINU.CON"
  )

  testthat::expect_equal(out$height[[1]], h_expected, tolerance = 1e-10)
})

testthat::test_that("si_thrower1994 uses explicit inverse for PSEU.MEN", {
  age <- 45
  h <- 22
  si_expected <- 0.39 + 0.31 * h + 33.38 * h / age

  out <- CanadaForestAllometry::si_thrower1994(
    age = age,
    height = h,
    species = "PSEU.MEN"
  )

  testthat::expect_equal(out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_thrower1994 supports scalar recycling", {
  out <- CanadaForestAllometry::si_thrower1994(
    age = c(20, 30, 40),
    si = 16,
    species = "PINU.CON"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
})

testthat::test_that("si_thrower1994 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 20,
      height = 10,
      si = 12,
      species = "PINU.CON"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 20,
      si = 12,
      species = "XXXX.YYY"
    ),
    "Unrecognized species codes|No Thrower1994 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = c(20, 30),
      si = c(12, 14, 16),
      species = c("PINU.CON", "PINU.CON")
    ),
    "length 1 or",
    ignore.case = TRUE
  )
})

testthat::test_that("si_thrower1994 catches non-finite and negative outputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 30,
      si = 1,
      species = "PINU.CON"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 1,
      si = 100,
      species = "ABIE.LAS"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 20,
      height = 1e308,
      species = "PSEU.MEN"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_thrower1994(
      age = 200,
      height = 0.01,
      species = "LARI.OCC"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("Thrower1994 internal height form branches behave as expected", {
  pars <- CanadaForestAllometry:::.thrower1994_parameters()

  pars_logexp <- dplyr::filter(pars, .data$Species == "PINU.CON")
  testthat::expect_true(is.nan(
    CanadaForestAllometry:::.thrower1994_height_one(
      age = 30,
      si = 1,
      pars = pars_logexp
    )
  ))

  pars_rational <- dplyr::filter(pars, .data$Species == "ABIE.LAS")
  testthat::expect_true(is.nan(
    CanadaForestAllometry:::.thrower1994_height_one(
      age = 30,
      si = 1.3,
      pars = pars_rational
    )
  ))

  h_abies <- CanadaForestAllometry:::.thrower1994_height_one(
    age = 25,
    si = 20,
    pars = pars_rational
  )
  b1 <- pars_rational$b1_const[[1]] + pars_rational$b1_s_div_coef[[1]] / (20 - pars_rational$s_base[[1]])
  b2 <- pars_rational$b2_const[[1]] + pars_rational$b2_s_div_coef[[1]] / (20 - pars_rational$s_base[[1]])
  b3 <- pars_rational$b3_const[[1]] + pars_rational$b3_s_div_coef[[1]] / (20 - pars_rational$s_base[[1]])
  h_base <- pars_rational$h_scale[[1]] * (pars_rational$h_base[[1]] + 25^2 / (b1 + b2 * 25 + b3 * 25^2))
  testthat::expect_false(isTRUE(all.equal(h_abies, h_base)))

  pars_thuj <- dplyr::filter(pars, .data$Species == "THUJ.PLI")
  h_thuj <- CanadaForestAllometry:::.thrower1994_height_one(
    age = 70,
    si = 20,
    pars = pars_thuj
  )
  b1_t <- pars_thuj$b1_const[[1]] + pars_thuj$b1_s_div_coef[[1]] / (20 - pars_thuj$s_base[[1]])
  b2_t <- pars_thuj$b2_const[[1]] + pars_thuj$b2_s_div_coef[[1]] / (20 - pars_thuj$s_base[[1]])
  b3_t <- pars_thuj$b3_const[[1]] + pars_thuj$b3_s_div_coef[[1]] / (20 - pars_thuj$s_base[[1]])
  h_base_t <- pars_thuj$h_scale[[1]] * (pars_thuj$h_base[[1]] + 70^2 / (b1_t + b2_t * 70 + b3_t * 70^2))
  testthat::expect_lt(h_thuj, h_base_t)

  pars_tsug <- dplyr::filter(pars, .data$Species == "TSUG.HET")
  h_tsug <- CanadaForestAllometry:::.thrower1994_height_one(
    age = 35,
    si = 20,
    pars = pars_tsug
  )
  testthat::expect_true(is.finite(h_tsug))
  testthat::expect_gt(h_tsug, 0)

  pars_exp <- dplyr::filter(pars, .data$Species == "PINU.PON")
  testthat::expect_true(is.nan(
    CanadaForestAllometry:::.thrower1994_height_one(
      age = 30,
      si = 1.0,
      pars = pars_exp
    )
  ))
  h_exp <- CanadaForestAllometry:::.thrower1994_height_one(
    age = 30,
    si = 20,
    pars = pars_exp
  )
  testthat::expect_true(is.finite(h_exp))
  testthat::expect_gt(h_exp, 0)
})

testthat::test_that("Thrower1994 internal solver handles exact grid roots and aborts on no bracket", {
  pars <- CanadaForestAllometry:::.thrower1994_parameters()
  pars_logexp <- dplyr::filter(pars, .data$Species == "PINU.CON")

  si_exact <- 1.300001
  h_exact <- CanadaForestAllometry:::.thrower1994_height_one(
    age = 30,
    si = si_exact,
    pars = pars_logexp
  )
  root <- CanadaForestAllometry:::.thrower1994_solve_si_one(
    age = 30,
    height = h_exact,
    pars = pars_logexp
  )
  testthat::expect_equal(root, si_exact, tolerance = 1e-12)

  testthat::expect_error(
    CanadaForestAllometry:::.thrower1994_solve_si_one(
      age = 30,
      height = 0.01,
      pars = pars_logexp
    ),
    "Failed to bracket a site-index solution",
    ignore.case = TRUE
  )
})
