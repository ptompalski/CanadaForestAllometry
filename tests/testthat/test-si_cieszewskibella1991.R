testthat::test_that("si_cieszewskibella1991 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_cieszewskibella1991(
    age = c(25, 50, 80),
    si = c(12, 16, 20),
    species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_cieszewskibella1991 predicts si from height and returns single-column tibble", {
  h_in <- CanadaForestAllometry::si_cieszewskibella1991(
    age = c(25, 50, 80),
    si = c(12, 16, 20),
    species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_cieszewskibella1991(
    age = c(25, 50, 80),
    height = h_in,
    species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 1.3))
})

testthat::test_that("CieszewskiBella1991 function matches manual equation evaluation", {
  pars <- CanadaForestAllometry:::.cieszewskibella1991_parameters() |>
    dplyr::filter(.data$Species == "PINU.CON") |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(pars) == 1)

  age <- 45
  si <- 14
  s <- si - 1.3
  j <- -1 - pars$a[[1]]
  d <- 20 * pars$b[[1]] * (pars$base_age_bh[[1]]^j)
  root <- d + 2 * s

  h_expected <- (root + d) /
    (2 + (80 * pars$b[[1]] * (age^j)) / (root - d)) +
    1.3

  h_out <- CanadaForestAllometry::si_cieszewskibella1991(
    age = age,
    si = si,
    species = "PINU.CON"
  )
  testthat::expect_equal(h_out$height[[1]], h_expected, tolerance = 1e-10)

  si_out <- CanadaForestAllometry::si_cieszewskibella1991(
    age = age,
    height = h_expected,
    species = "PINU.CON"
  )
  testthat::expect_equal(si_out$si[[1]], si, tolerance = 1e-10)
})

testthat::test_that("si_cieszewskibella1991 returns site index at base age unchanged", {
  out <- CanadaForestAllometry::si_cieszewskibella1991(
    age = 50,
    si = c(10, 15, 20),
    species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
  )

  testthat::expect_equal(out$height, c(10, 15, 20), tolerance = 1e-10)
})

testthat::test_that("si_cieszewskibella1991 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = c(20, 30),
      si = c(15, 15, 15),
      species = "PINU.CON"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = c(20, -1),
      height = c(10, 12),
      species = c("PINU.CON", "PINU.CON")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 20,
      height = 10,
      species = "NOPE.SPP"
    ),
    "No CieszewskiBella1991 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 20,
      height = 10,
      si = 12,
      species = "PINU.CON"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 20,
      species = "PINU.CON"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_cieszewskibella1991 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = "20",
      si = 10,
      species = "PINU.CON"
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 20,
      si = "10",
      species = "PINU.CON"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_cieszewskibella1991 validates height and si > 1.3", {
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 20,
      si = 1.3,
      species = "PINU.CON"
    ),
    "si.*> 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 20,
      height = 1.3,
      species = "PINU.CON"
    ),
    "height.*> 1.3",
    ignore.case = TRUE
  )
})

testthat::test_that("CieszewskiBella1991 parameters are present in internal data", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get(
    "parameters_CieszewskiBella1991",
    envir = ns,
    inherits = FALSE
  ) |>
    dplyr::as_tibble()

  req <- c("Species", "a", "b", "base_age_bh")
  testthat::expect_true(all(req %in% names(pars)))
  testthat::expect_equal(nrow(pars), 4L)
  testthat::expect_setequal(
    pars$Species,
    c("PINU.CON", "PICE.GLA", "PICE.MAR", "POPU.TRE")
  )
})

testthat::test_that("si_model_registry wiring for CieszewskiBella1991 is correct", {
  reg <- CanadaForestAllometry::si_model_registry()
  row <- reg |>
    dplyr::filter(.data$model_id == "cieszewskibella1991")

  testthat::expect_equal(nrow(row), 1L)
  testthat::expect_identical(
    row$params_key[[1]],
    "parameters_CieszewskiBella1991"
  )
  testthat::expect_null(row$species_manual[[1]])

  reg_sp <- CanadaForestAllometry:::si_model_registry_species()
  row_sp <- reg_sp |>
    dplyr::filter(.data$model_id == "cieszewskibella1991")

  testthat::expect_equal(row_sp$n_species[[1]], 4L)
  testthat::expect_setequal(
    row_sp$species[[1]],
    c("PINU.CON", "PICE.GLA", "PICE.MAR", "POPU.TRE")
  )
})

testthat::test_that(".cieszewskibella1991_height_one handles invalid internal states", {
  out_si_floor <- CanadaForestAllometry:::.cieszewskibella1991_height_one(
    age = 40,
    si = 1.3,
    a = 0.2,
    b = 100,
    base_age_bh = 50
  )
  testthat::expect_true(is.nan(out_si_floor))

  out_nonfinite <- CanadaForestAllometry:::.cieszewskibella1991_height_one(
    age = 40,
    si = 10,
    a = 0.2,
    b = Inf,
    base_age_bh = 50
  )
  testthat::expect_true(is.nan(out_nonfinite))

})

testthat::test_that(".cieszewskibella1991_si_one handles invalid internal states", {
  out_height_floor <- CanadaForestAllometry:::.cieszewskibella1991_si_one(
    age = 40,
    height = 1.3,
    a = 0.2,
    b = 100,
    base_age_bh = 50
  )
  testthat::expect_true(is.nan(out_height_floor))
})

testthat::test_that(".cieszewskibella1991_parameters validates required columns", {
  testthat::local_mocked_bindings(
    .get_internal_data = function(...) {
      tibble::tibble(Species = "PINU.CON", a = 0.2, b = 100)
    },
    .package = "CanadaForestAllometry"
  )

  testthat::expect_error(
    CanadaForestAllometry:::.cieszewskibella1991_parameters(),
    "Missing required columns",
    ignore.case = TRUE
  )
})

testthat::test_that("si_cieszewskibella1991 catches non-finite and negative predictions", {
  testthat::local_mocked_bindings(
    .cieszewskibella1991_height_one = function(...) NaN,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 40,
      si = 10,
      species = "PINU.CON"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::local_mocked_bindings(
    .cieszewskibella1991_height_one = function(...) -1,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 40,
      si = 10,
      species = "PINU.CON"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )

  testthat::local_mocked_bindings(
    .cieszewskibella1991_si_one = function(...) NaN,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 40,
      height = 12,
      species = "PINU.CON"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )

  testthat::local_mocked_bindings(
    .cieszewskibella1991_si_one = function(...) -1,
    .package = "CanadaForestAllometry"
  )
  testthat::expect_error(
    CanadaForestAllometry::si_cieszewskibella1991(
      age = 40,
      height = 12,
      species = "PINU.CON"
    ),
    "Negative site index prediction",
    ignore.case = TRUE
  )
})
