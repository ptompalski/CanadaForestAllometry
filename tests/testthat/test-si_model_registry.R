testthat::test_that("si_model_registry has expected structure and key models", {
  reg <- CanadaForestAllometry::si_model_registry()

  testthat::expect_s3_class(reg, "tbl_df")
  testthat::expect_true(all(
    c(
      "model_id", "engine", "rank", "subregion_type",
      "subregion_scope", "subregion_required"
    ) %in% names(reg)
  ))
  testthat::expect_true(all(c(
    "nigh2000",
    "nighcourtin1998_si25",
    "lafleche2013_potential",
    "lafleche2013_observed",
    "parresolvissage1998",
    "sharma2015",
    "sharmareid2018",
    "sharma2022",
    "sharmaparton2018a",
    "sharmaparton2018b",
    "sharmaparton2019",
    "sharma2021",
    "thrower1994",
    "huang1994"
  ) %in% reg$model_id))
})

testthat::test_that("si_model_registry includes Sharma2022 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharma2022")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharma2022")
  testthat::expect_identical(s$reference[[1]], "@Sharma2022")
  testthat::expect_identical(s$species_manual[[1]], c("PICE.MAR", "POPU.TRE"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes Lafleche2013 potential metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "lafleche2013_potential")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_lafleche2013")
  testthat::expect_identical(s$reference[[1]], "@LaflecheEtAl2013")
  testthat::expect_identical(s$fixed_args[[1]], list(curve_set = "potential"))
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_identical(s$supports_predict_si[[1]], FALSE)
  testthat::expect_identical(s$supports_predict_height[[1]], TRUE)
  testthat::expect_identical(s$subregion_type[[1]], "qc_ecological_subregion")
  testthat::expect_identical(s$subregion_arg[[1]], "ecological_subregion")
  testthat::expect_identical(s$params_key[[1]], "parameters_QC_IQS2013")
})

testthat::test_that("si_model_registry includes Lafleche2013 observed metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "lafleche2013_observed")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_lafleche2013")
  testthat::expect_identical(s$reference[[1]], "@LaflecheEtAl2013")
  testthat::expect_identical(s$fixed_args[[1]], list(curve_set = "observed"))
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_identical(s$supports_predict_si[[1]], FALSE)
  testthat::expect_identical(s$supports_predict_height[[1]], TRUE)
  testthat::expect_identical(s$params_key[[1]], "parameters_QC_IQS2013")
})

testthat::test_that("si_model_registry includes Sharma2021 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharma2021")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharma2021")
  testthat::expect_identical(s$reference[[1]], "@Sharma2021SI")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaReid2018 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmareid2018")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmareid2018")
  testthat::expect_identical(s$reference[[1]], "@SharmaReid2018")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaEtAl2015 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharma2015")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharma2015")
  testthat::expect_identical(s$reference[[1]], "@SharmaEtAl2015")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaParton2019 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmaparton2019")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmaparton2019")
  testthat::expect_identical(s$reference[[1]], "@SharmaParton2019")
  testthat::expect_identical(s$species_manual[[1]], "PINU.STR")
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaParton2018a metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmaparton2018a")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmaparton2018a")
  testthat::expect_identical(s$reference[[1]], "@SharmaParton2018a")
  testthat::expect_identical(s$species_manual[[1]], "PICE.GLA")
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaParton2018b metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmaparton2018b")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmaparton2018b")
  testthat::expect_identical(s$reference[[1]], "@SharmaParton2018b")
  testthat::expect_identical(s$species_manual[[1]], "PINU.RES")
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes ParresolVissage1998 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  p <- reg |>
    dplyr::filter(.data$model_id == "parresolvissage1998")

  testthat::expect_equal(nrow(p), 1L)
  testthat::expect_identical(p$engine[[1]], "si_parresolvissage1998")
  testthat::expect_identical(p$reference[[1]], "@ParresolVissage1998")
  testthat::expect_identical(p$species_manual[[1]], "PINU.STR")
  testthat::expect_true(is.na(p$params_key[[1]]))
})

testthat::test_that("si_model_registry includes Huang1994 subregion metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  h <- reg |>
    dplyr::filter(.data$model_id == "huang1994")

  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_identical(h$subregion_type[[1]], "ab_natural_region_group")
  testthat::expect_identical(h$subregion_arg[[1]], "subregion")
  testthat::expect_identical(h$params_key[[1]], "parameters_Huang1994_si")
})

testthat::test_that("si_model_registry includes subregion metadata for BC-specific models", {
  reg <- CanadaForestAllometry::si_model_registry()

  bc_focus <- reg |>
    dplyr::filter(.data$model_id %in% c("nigh2000", "nighcourtin1998_si25", "thrower1994"))

  testthat::expect_true(all(bc_focus$subregion_type == "bec_region"))
  testthat::expect_true(all(lengths(bc_focus$subregion_scope) >= 1L))
  testthat::expect_true(all(!bc_focus$subregion_required))
})

testthat::test_that("si_model_registry_species returns species metadata", {
  out <- CanadaForestAllometry:::si_model_registry_species()
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("species", "n_species", "species_text") %in% names(out)))
  testthat::expect_true(all(out$n_species >= 0))
})

testthat::test_that("si_model_registry_species prefers manual species over params lookup", {
  reg_one <- tibble::tibble(
    model_id = "m1",
    params_key = "dummy_params",
    species_manual = list(c("ABIE.BAL", "PICE.GLA"))
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::si_model_registry_species()
    },
    si_model_registry = function() reg_one,
    get_params_tbl = function(params_key) stop("should not be called"),
    .package = "CanadaForestAllometry"
  )

  testthat::expect_identical(out$species[[1]], c("ABIE.BAL", "PICE.GLA"))
  testthat::expect_identical(out$n_species[[1]], 2L)
})

testthat::test_that("si_model_registry_species applies backward-compatible defaults", {
  minimal_reg <- tibble::tibble(
    model_id = "m1",
    params_key = "dummy_params"
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::si_model_registry_species()
    },
    si_model_registry = function() minimal_reg,
    get_params_tbl = function(params_key) tibble::tibble(Species = c("ABIE.BAL", "PICE.GLA")),
    .package = "CanadaForestAllometry"
  )

  testthat::expect_true("subregion_required" %in% names(out))
  testthat::expect_true("subregion_arg" %in% names(out))
  testthat::expect_true("subregion_type" %in% names(out))
  testthat::expect_true("species_manual" %in% names(out))
  testthat::expect_identical(out$subregion_required[[1]], FALSE)
  testthat::expect_true(is.na(out$subregion_arg[[1]]))
  testthat::expect_identical(out$subregion_type[[1]], "none")
})

testthat::test_that("si_model_registry_species wraps get_params_tbl errors with model context", {
  minimal_reg <- tibble::tibble(
    model_id = "model_x",
    params_key = "missing_x",
    species_manual = list(NULL),
    subregion_required = FALSE,
    subregion_arg = NA_character_,
    subregion_type = "none"
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      {
        CanadaForestAllometry:::si_model_registry_species()
      },
      si_model_registry = function() minimal_reg,
      get_params_tbl = function(params_key) rlang::abort("boom"),
      .package = "CanadaForestAllometry"
    ),
    "Failed to build species list for model_id=model_x",
    fixed = FALSE
  )
})

testthat::test_that("si_model_registry_species returns empty species when no source is provided", {
  reg_none <- tibble::tibble(
    model_id = "m_none",
    params_key = NA_character_,
    species_manual = list(NULL),
    subregion_required = FALSE,
    subregion_arg = NA_character_,
    subregion_type = "none"
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::si_model_registry_species()
    },
    si_model_registry = function() reg_none,
    .package = "CanadaForestAllometry"
  )

  testthat::expect_identical(out$species[[1]], character(0))
  testthat::expect_identical(out$n_species[[1]], 0L)
  testthat::expect_true(is.na(out$species_text[[1]]))
})
