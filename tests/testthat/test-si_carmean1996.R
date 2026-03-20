testthat::test_that("si_carmean1996 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 40, 60),
    si = c(10, 14, 18),
    species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_carmean1996 predicts si from height and returns single-column tibble", {
  h_in <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 40, 60),
    si = c(10, 14, 18),
    species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 40, 60),
    height = h_in,
    species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_carmean1996 matches manual black spruce equation evaluation", {
  age <- 40
  si <- 15

  h_expected <- 1.3 +
    (si - 1.3) *
    (1 + exp(9.2248 - 1.28875 * log(50) - 1.5612 * log(si - 1.3))) /
    (1 + exp(9.2248 - 1.28875 * log(age) - 1.5612 * log(si - 1.3)))

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "PICE.MAR"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  height <- 12
  si_expected <- 9.0023 +
    0.4396 * (height - 1.3) +
    1.4753 * log(height - 1.3) -
    0.7996 * log(age) -
    0.3976 * log(age)^2 +
    19.26275 * (height - 1.3) / age

  si_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = height,
    species = "PICE.MAR"
  )
  testthat::expect_equal(si_out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmean1996 round-trips white spruce numeric inverse", {
  age <- 35
  si <- 16

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "PICE.GLA"
  )

  si_back <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = h_out$height[[1]],
    species = "PICE.GLA"
  )

  testthat::expect_equal(si_back$si[[1]], si, tolerance = 1e-5)
})

testthat::test_that("si_carmean1996 matches manual balsam fir equation evaluation", {
  age <- 40
  si <- 14

  h_expected <- (4.5 +
    8.12 * (3.28 * si)^0.6748 *
      (1 - exp(-0.0111 * age))^(6.4229 * (3.28 * si)^(-0.4586))) / 3.28

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  height <- 12
  si_expected <- (4.5 +
    0.0061 * (3.28 * height)^1.3539 *
      (1 - exp(-0.00019 * age))^(-1.0286 * (3.28 * height)^(-0.0723))) / 3.28

  si_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = height,
    species = "ABIE.BAL"
  )
  testthat::expect_equal(si_out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmean1996 matches manual white birch equation evaluation", {
  age <- 40
  si <- 15

  h_expected <- (4.5 +
    2.4321 * (3.28 * si - 4.5)^0.9207 *
      (1 - exp(-0.0168 * age))^(1.5247 * (3.28 * si - 4.5)^(-0.1042))) / 3.28

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "BETU.PAP"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  height <- 13
  si_expected <- (4.5 +
    0.5119 * (3.28 * height - 4.5)^1.0229 *
      (1 - exp(-0.0167 * age))^(-1.0284 * (3.28 * height - 4.5)^(-0.0049))) / 3.28

  si_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = height,
    species = "BETU.PAP"
  )
  testthat::expect_equal(si_out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmean1996 matches manual tamarack equation evaluation", {
  age <- 40
  si <- 14

  h_expected <- (4.5 +
    1.547 * (3.28 * si - 4.5) *
      (1 - exp(-0.0225 * age))^1.1129) / 3.28

  h_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    si = si,
    species = "LARI.LAR"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  height <- 11
  si_expected <- (4.5 +
    0.6464 * (3.28 * height - 4.5) *
      (1 - exp(-0.0225 * age))^(-1.1129)) / 3.28

  si_out <- CanadaForestAllometry::si_carmean1996(
    age = age,
    height = height,
    species = "LARI.LAR"
  )
  testthat::expect_equal(si_out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmean1996 supports scalar recycling", {
  out <- CanadaForestAllometry::si_carmean1996(
    age = c(20, 30, 40),
    si = 14,
    species = "PICE.MAR"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
})

testthat::test_that("si_carmean1996 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = c(20, 30),
      si = c(15, 15, 15),
      species = "PICE.MAR"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = c(20, -1),
      height = c(10, 12),
      species = c("PICE.MAR", "PICE.MAR")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 10,
      species = "NOPE.SPP"
    ),
    "No Carmean1996 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 10,
      si = 12,
      species = "PICE.MAR"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1996 validates species-specific lower bounds", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      si = 1.3,
      species = "PICE.GLA"
    ),
    "si.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 1.3,
      species = "PICE.MAR"
    ),
    "height.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      si = 1.3,
      species = "PICE.MAR"
    ),
    "si.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      si = 4.5 / 3.28,
      species = "BETU.PAP"
    ),
    "si.*>",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 20,
      height = 4.5 / 3.28,
      species = "LARI.LAR"
    ),
    "height.*>",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1996 validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_prepare(
      age = numeric(0),
      x = numeric(0),
      species = character(0),
      x_name = "si"
    ),
    "age.*length > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1996 catches non-finite and negative height predictions", {
  testthat::local_mocked_bindings(
    .carmean1996_height_one = function(...) NaN,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 40,
      si = 10,
      species = "PINU.BAN"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::local_mocked_bindings(
    .carmean1996_height_one = function(...) -1,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 40,
      si = 10,
      species = "PINU.BAN"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1996 catches non-finite and negative site-index predictions", {
  testthat::local_mocked_bindings(
    .carmean1996_si_one = function(...) NaN,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 40,
      height = 10,
      species = "PINU.BAN"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::local_mocked_bindings(
    .carmean1996_si_one = function(...) -1,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1996(
      age = 40,
      height = 10,
      species = "PINU.BAN"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("Carmean1996 dispatchers error on unsupported model family", {
  bad_pars <- tibble::tibble(
    Species = "TEST.SPP",
    model_family = "nope"
  )

  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_height_one(
      age = 40,
      si = 10,
      pars = bad_pars
    ),
    "Unsupported Carmean1996 model family",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_si_one(
      age = 40,
      height = 10,
      pars = bad_pars
    ),
    "Unsupported Carmean1996 model family",
    ignore.case = TRUE
  )
})

testthat::test_that("carmean1996 family-k solver handles exact grid root and bracketing failure", {
  pars <- tibble::tibble(
    Species = "PINU.BAN",
    source_si_offset = 1.3
  )

  testthat::local_mocked_bindings(
    .carmean1996_height_family_k = function(age, si, pars) si,
    .package = "CanadaForestAllometry"
  )
  out_exact <- CanadaForestAllometry:::.carmean1996_si_family_k(
    age = 40,
    height = 1.300001,
    pars = pars
  )
  testthat::expect_equal(out_exact, 1.300001, tolerance = 1e-12)

  testthat::local_mocked_bindings(
    .carmean1996_height_family_k = function(age, si, pars) 0,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_si_family_k(
      age = 40,
      height = 10,
      pars = pars
    ),
    "Failed to bracket a site-index solution",
    ignore.case = TRUE
  )
})

testthat::test_that("carmean1996 internal lower-bound guards are enforced", {
  pars_k <- tibble::tibble(
    Species = "PINU.BAN",
    source_si_offset = 1.3
  )
  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_si_family_k(
      age = 40,
      height = 1.3,
      pars = pars_k
    ),
    "height.*> 1.3",
    ignore.case = TRUE
  )

  pars_bw <- tibble::tibble(
    Species = "BETU.PAP",
    source_length_factor = 3.28,
    source_height_offset = 4.5
  )
  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_si_metric_ft_subtract45(
      age = 40,
      height = 4.5 / 3.28,
      pars = pars_bw
    ),
    "height.*>",
    ignore.case = TRUE
  )

  pars_tl <- tibble::tibble(
    Species = "LARI.LAR",
    source_length_factor = 3.28,
    source_height_offset = 4.5,
    source_si_offset = 4.5
  )
  testthat::expect_error(
    CanadaForestAllometry:::.carmean1996_height_metric_ft_subtract45_linear(
      age = 40,
      si = 4.5 / 3.28,
      pars = pars_tl
    ),
    "si.*>",
    ignore.case = TRUE
  )
})
