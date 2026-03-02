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
    "thrower1994"
  ) %in% reg$model_id))
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
