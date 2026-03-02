testthat::test_that("si_lundgrendolid1970 predicts height from si for both model forms", {
  out_exp <- CanadaForestAllometry::si_lundgrendolid1970(
    age = c(20, 40, 60),
    si = c(12, 18, 24),
    species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
    model = "exponential_monomolecular"
  )

  out_mono <- CanadaForestAllometry::si_lundgrendolid1970(
    age = c(20, 40, 60),
    si = c(12, 18, 24),
    species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
    model = "monomolecular"
  )

  testthat::expect_s3_class(out_exp, "tbl_df")
  testthat::expect_named(out_exp, "height")
  testthat::expect_equal(nrow(out_exp), 3L)
  testthat::expect_true(all(is.finite(out_exp$height)))
  testthat::expect_true(all(out_exp$height > 0))

  testthat::expect_s3_class(out_mono, "tbl_df")
  testthat::expect_named(out_mono, "height")
  testthat::expect_equal(nrow(out_mono), 3L)
  testthat::expect_true(all(is.finite(out_mono$height)))
  testthat::expect_true(all(out_mono$height > 0))
})

testthat::test_that("si_lundgrendolid1970 predicts si from height for both model forms", {
  out_exp <- CanadaForestAllometry::si_lundgrendolid1970(
    age = c(20, 40, 60),
    height = c(8, 16, 22),
    species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
    model = "exponential_monomolecular"
  )

  out_mono <- CanadaForestAllometry::si_lundgrendolid1970(
    age = c(20, 40, 60),
    height = c(8, 16, 22),
    species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
    model = "monomolecular"
  )

  testthat::expect_s3_class(out_exp, "tbl_df")
  testthat::expect_named(out_exp, "si")
  testthat::expect_equal(nrow(out_exp), 3L)
  testthat::expect_true(all(is.finite(out_exp$si)))
  testthat::expect_true(all(out_exp$si > 0))

  testthat::expect_s3_class(out_mono, "tbl_df")
  testthat::expect_named(out_mono, "si")
  testthat::expect_equal(nrow(out_mono), 3L)
  testthat::expect_true(all(is.finite(out_mono$si)))
  testthat::expect_true(all(out_mono$si > 0))
})

testthat::test_that("si_lundgrendolid1970 matches manual equation evaluation", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get("parameters_LungrenDolid1970", envir = ns, inherits = FALSE) |>
    dplyr::as_tibble()

  pars_exp <- pars |>
    dplyr::filter(.data$Species == "PICE.MAR", .data$model == "exponential_monomolecular") |>
    dplyr::slice(1)
  pars_mono <- pars |>
    dplyr::filter(.data$Species == "PICE.MAR", .data$model == "monomolecular") |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(pars_exp) == 1)
  testthat::skip_if_not(nrow(pars_mono) == 1)

  age <- 45
  si_m <- 18
  si_ft <- si_m * 3.28084

  h_ft_exp_expected <- with(
    pars_exp,
    b1 * si_ft * (1 - exp(b2 * age))^b3
  )
  h_m_exp_expected <- h_ft_exp_expected / 3.28084

  h_out_exp <- CanadaForestAllometry::si_lundgrendolid1970(
    age = age,
    si = si_m,
    species = "PICE.MAR",
    model = "exponential_monomolecular"
  )
  testthat::expect_equal(h_out_exp$height[[1]], h_m_exp_expected, tolerance = 1e-10)

  h_ft_mono_expected <- with(
    pars_mono,
    si_ft * (a + b1 * exp(b2 * age))
  )
  h_m_mono_expected <- h_ft_mono_expected / 3.28084

  h_out_mono <- CanadaForestAllometry::si_lundgrendolid1970(
    age = age,
    si = si_m,
    species = "PICE.MAR",
    model = "monomolecular"
  )
  testthat::expect_equal(h_out_mono$height[[1]], h_m_mono_expected, tolerance = 1e-10)
})

testthat::test_that("si_lundgrendolid1970 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = c(20, 30),
      si = c(15, 15, 15),
      species = "PICE.MAR"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = c(20, -1),
      height = c(10, 12),
      species = c("PICE.MAR", "PICE.MAR")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 20,
      height = 10,
      species = "NOPE.SPP"
    ),
    "No LundgrenDolid1970",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 20,
      height = 10,
      si = 12,
      species = "PICE.MAR"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lundgrendolid1970 validates model choice", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 20,
      si = 15,
      species = "PICE.MAR",
      model = "bad_model"
    ),
    "must be one of",
    ignore.case = TRUE
  )
})
testthat::test_that("si_lundgrendolid1970 validates missing parameters in monomolecular branch", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 25,
      si = 15,
      species = "NOPE.SPP",
      model = "monomolecular"
    ),
    "No LundgrenDolid1970",
    ignore.case = TRUE
  )
})
testthat::test_that("si_lundgrendolid1970 catches invalid denominator in monomolecular mode", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 1,
      height = 15,
      species = "PINU.BAN",
      model = "monomolecular"
    ),
    "Invalid denominator generated",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lundgrendolid1970 catches non-finite site-index predictions", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 20,
      height = 1e308,
      species = "PICE.MAR",
      model = "exponential_monomolecular"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lundgrendolid1970 catches negative height predictions", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 1,
      si = 10,
      species = "PINU.BAN",
      model = "monomolecular"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lundgrendolid1970 catches non-finite height predictions", {
  testthat::expect_error(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 20,
      si = 1e308,
      species = "PICE.MAR",
      model = "exponential_monomolecular"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lundgrendolid1970 emits white spruce caution message once per model", {
  msgs_exp_1 <- testthat::capture_messages(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 40,
      si = 15,
      species = "PICE.GLA",
      model = "exponential_monomolecular"
    )
  )
  msgs_exp_2 <- testthat::capture_messages(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 40,
      si = 15,
      species = "PICE.GLA",
      model = "exponential_monomolecular"
    )
  )

  testthat::expect_lte(length(msgs_exp_1), 1L)
  testthat::expect_length(msgs_exp_2, 0L)
  if (length(msgs_exp_1) == 1L) {
    testthat::expect_match(msgs_exp_1[[1]], "not recommended for white spruce", ignore.case = TRUE)
  }

  msgs_mono_1 <- testthat::capture_messages(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 40,
      si = 15,
      species = "PICE.GLA",
      model = "monomolecular"
    )
  )
  msgs_mono_2 <- testthat::capture_messages(
    CanadaForestAllometry::si_lundgrendolid1970(
      age = 40,
      si = 15,
      species = "PICE.GLA",
      model = "monomolecular"
    )
  )

  testthat::expect_lte(length(msgs_mono_1), 1L)
  testthat::expect_length(msgs_mono_2, 0L)
  if (length(msgs_mono_1) == 1L) {
    testthat::expect_match(msgs_mono_1[[1]], "not recommended for white spruce", ignore.case = TRUE)
  }
})

testthat::test_that(".lundgrendolid1970_prepare validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry:::.lundgrendolid1970_prepare(
      age = numeric(0),
      x = numeric(0),
      species = character(0),
      x_name = "si",
      model = "exponential_monomolecular"
    ),
    "age.*length > 0",
    ignore.case = TRUE
  )
})
