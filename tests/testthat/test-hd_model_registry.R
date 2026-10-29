# Fake models used to exercise the registry helpers while no hd_ model exists.
fake_definitions <- function() {
  list(
    .hd_registry_row(
      model_id = "fake_on",
      engine = "hd_fake_on",
      reference = "@Fake2007",
      scope = "regional",
      province_scope = "ON",
      stand_origin = "natural",
      equation_family = "chapman_richards",
      description = "Fake ON model with stand predictors.",
      calibration_published = TRUE,
      species_manual = c("PINU.RES", "PICE.MAR")
    ),
    .hd_registry_row(
      model_id = "fake_ab",
      engine = "hd_fake_ab",
      reference = "@Fake2016",
      scope = "provincial",
      province_scope = "AB",
      stand_origin = "both",
      equation_family = "power",
      description = "Fake AB model with optional region.",
      region_arg = "natural_subregion",
      params_key = "parameters_Huang94"
    ),
    .hd_registry_row(
      model_id = "fake_all",
      engine = "hd_fake_all",
      reference = "@Fake2020",
      scope = "provincial",
      province_scope = "ALL",
      stand_origin = "plantation",
      equation_family = "korf",
      description = "Fake national DBH-only model.",
      fixed_args = list(variant = "dbh"),
      species_manual = "POPU.TRE"
    )
  )
}

fake_predictors <- function() {
  tibble::tribble(
    ~model_id, ~arg, ~level, ~units, ~required, ~definition, ~source_locator,
    "fake_on", "DBH", "tree", "cm", TRUE, "DBH at 1.3 m", "p. 1",
    "fake_on", "HDOM", "plot", "m", TRUE, "Top height", "p. 2",
    "fake_on", "BA", "plot", "m2/ha", TRUE, "All-species BA", "p. 2",
    "fake_on", "TPH", "plot", "stems/ha", TRUE, "Stems > 9 cm", "p. 2",
    "fake_ab", "DBH", "tree", "cm", TRUE, "DBH at 1.3 m", "Table 1",
    "fake_ab", "natural_subregion", "plot", "code", FALSE, "2006 codes", "Table 2",
    "fake_all", "DBH", "tree", "cm", TRUE, "DBH at 1.3 m", "eq. 1"
  )
}

fake_engines <- function() {
  env <- new.env()
  env$hd_fake_on <- function(DBH, species, HDOM, BA, TPH) NULL
  env$hd_fake_ab <- function(DBH, species, natural_subregion = NULL) NULL
  env$hd_fake_all <- function(DBH, species, variant = "dbh") NULL
  env
}

local_fake_registry <- function(env = parent.frame()) {
  local_mocked_bindings(
    .hd_model_definitions = fake_definitions,
    .hd_model_predictors = fake_predictors,
    .env = env
  )
}

test_that("hd_model_registry() has the documented columns and types", {
  reg <- hd_model_registry()
  expect_s3_class(reg, "tbl_df")
  expect_identical(
    names(reg),
    c(
      "model_id", "engine", "fixed_args", "reference", "scope",
      "province_scope", "stand_origin", "equation_family",
      "required_predictors", "optional_predictors", "region_arg",
      "prediction_types", "calibration_published", "params_key",
      "species_manual", "description"
    )
  )
  list_cols <- c(
    "fixed_args", "province_scope", "required_predictors",
    "optional_predictors", "prediction_types", "species_manual"
  )
  expect_true(all(vapply(reg[list_cols], is.list, logical(1))))
  expect_type(reg$calibration_published, "logical")
  expect_false(anyDuplicated(reg$model_id) > 0)
})

test_that("predictor table has the documented columns", {
  expect_identical(
    names(.hd_model_predictors()),
    c("model_id", "arg", "level", "units", "required", "definition", "source_locator")
  )
})

test_that("registry and predictor table are consistent with engine formals()", {
  expect_identical(.hd_registry_problems(), character())
})

test_that("registry derives required/optional predictors from the predictor table", {
  local_fake_registry()
  reg <- hd_model_registry()
  expect_equal(nrow(reg), 3L)
  on <- reg[reg$model_id == "fake_on", ]
  expect_identical(on$required_predictors[[1]], c("DBH", "HDOM", "BA", "TPH"))
  expect_identical(on$optional_predictors[[1]], character())
  ab <- reg[reg$model_id == "fake_ab", ]
  expect_identical(ab$optional_predictors[[1]], "natural_subregion")
  expect_identical(.hd_registry_problems(env = fake_engines()), character())
})

test_that("hd_model_registry_species() uses species_manual or the params table", {
  local_fake_registry()
  reg <- hd_model_registry_species()
  sp <- stats::setNames(reg$species, reg$model_id)
  expect_identical(sp$fake_on, c("PICE.MAR", "PINU.RES"))
  expect_true("PICE.GLA" %in% sp$fake_ab)
  expect_identical(reg$n_species[reg$model_id == "fake_all"], 1L)
  expect_identical(reg$species_text[reg$model_id == "fake_on"], "PICE.MAR, PINU.RES")

  none <- hd_model_registry()[1, ]
  none$species_manual <- list(NULL)
  out <- hd_model_registry_species(none)
  expect_identical(out$species[[1]], character())
  expect_true(is.na(out$species_text))

  bad <- hd_model_registry()[2, ]
  bad$params_key <- "parameters_does_not_exist"
  expect_error(hd_model_registry_species(bad), "model_id=fake_ab")
})

test_that("get_hd_model_info() returns metadata and predictors", {
  local_fake_registry()
  info <- get_hd_model_info("fake_on")
  expect_type(info, "list")
  expect_identical(info$engine, "hd_fake_on")
  expect_identical(info$province_scope, "ON")
  expect_identical(info$species, c("PICE.MAR", "PINU.RES"))
  expect_identical(info$fixed_args, list())
  expect_null(info$n_species)
  expect_s3_class(info$predictors, "tbl_df")
  expect_identical(info$predictors$arg, c("DBH", "HDOM", "BA", "TPH"))
  expect_false("model_id" %in% names(info$predictors))
  expect_identical(get_hd_model_info("fake_all")$fixed_args, list(variant = "dbh"))

  expect_error(get_hd_model_info("nope"), "fake_on, fake_ab, fake_all")
  expect_error(get_hd_model_info(c("a", "b")), "single character")
})

test_that("get_hd_model_info() errors helpfully on an empty registry", {
  local_mocked_bindings(.hd_model_definitions = function() list())
  expect_error(get_hd_model_info("anything"), "no models registered")
})

test_that("find_hd_models() filters by jurisdiction and species", {
  local_fake_registry()
  expect_setequal(find_hd_models()$model_id, c("fake_on", "fake_ab", "fake_all"))
  expect_setequal(find_hd_models(jurisdiction = "on")$model_id, c("fake_on", "fake_all"))
  expect_setequal(find_hd_models(jurisdiction = c("AB", "ON"))$model_id, c("fake_on", "fake_ab", "fake_all"))
  expect_identical(find_hd_models(species = "PINURES")$model_id, "fake_on")
  expect_identical(find_hd_models(jurisdiction = "QC", species = "PICE.MAR")$model_id, character())
})

test_that("find_hd_models() reports missing predictors and orders usable first", {
  local_fake_registry()
  out <- find_hd_models(jurisdiction = "ON", species = "PICE.MAR", available = c("DBH", "BA", "TPH"))
  expect_identical(out$model_id, "fake_on")
  expect_identical(out$missing_predictors[[1]], "HDOM")
  expect_false(out$usable)

  out <- find_hd_models()
  expect_identical(out$usable, c(TRUE, TRUE, FALSE))
  expect_identical(out$model_id[[3]], "fake_on")
  expect_identical(out$missing_predictors[[3]], c("HDOM", "BA", "TPH"))

  out <- find_hd_models(available = c("DBH", "HDOM", "BA", "TPH"))
  expect_true(all(out$usable))
})

test_that("find_hd_models() validates inputs", {
  expect_error(find_hd_models(jurisdiction = 1), "jurisdiction")
  expect_error(find_hd_models(jurisdiction = NA_character_), "jurisdiction")
  expect_error(find_hd_models(jurisdiction = "XX"), "Unknown province")
  expect_error(find_hd_models(species = character()), "species")
  expect_error(find_hd_models(species = "spruce"), class = "ctae_invalid_species_code")
  expect_error(find_hd_models(available = NA_character_), "available")
  expect_s3_class(find_hd_models(), "tbl_df")
})

test_that(".hd_registry_row() validates its inputs", {
  ok <- list(
    model_id = "m", engine = "e", reference = "@R", scope = "provincial",
    province_scope = "ON", stand_origin = "natural", equation_family = "f",
    description = "d"
  )
  row <- do.call(.hd_registry_row, ok)
  expect_equal(nrow(row), 1L)
  expect_identical(row$prediction_types[[1]], "population")

  expect_error(do.call(.hd_registry_row, utils::modifyList(ok, list(scope = "national"))), "scope")
  expect_error(do.call(.hd_registry_row, utils::modifyList(ok, list(stand_origin = "x"))), "stand_origin")
  expect_error(do.call(.hd_registry_row, utils::modifyList(ok, list(province_scope = character()))), "province_scope")
  expect_error(do.call(.hd_registry_row, utils::modifyList(ok, list(province_scope = 1))), "province_scope")
  expect_error(do.call(.hd_registry_row, c(ok, list(fixed_args = "x"))), "fixed_args")
  expect_error(do.call(.hd_registry_row, c(ok, list(calibration_published = NA))), "calibration_published")
  expect_error(do.call(.hd_registry_row, c(ok, list(prediction_types = "other"))), "prediction_types")
})

test_that(".hd_registry_problems() detects inconsistencies", {
  local_fake_registry()
  env <- fake_engines()
  reg <- hd_model_registry()
  preds <- fake_predictors()

  # duplicated model_id
  expect_match(.hd_registry_problems(reg[c(1, 1), ], preds, env), "Duplicated model_id", all = FALSE)

  # predictors for unregistered model, duplicated predictor, bad level, species
  bad_preds <- dplyr::bind_rows(
    preds,
    tibble::tibble(model_id = "ghost", arg = "DBH", level = "tree", required = TRUE),
    preds[1, ],
    tibble::tibble(model_id = "fake_all", arg = "species", level = "stand", required = FALSE)
  )
  probs <- .hd_registry_problems(reg, bad_preds, env)
  expect_match(probs, "unregistered model_id: ghost", all = FALSE)
  expect_match(probs, "Duplicated predictor: fake_on DBH", all = FALSE)
  expect_match(probs, "Invalid predictor level: stand", all = FALSE)
  expect_match(probs, "`species` must not", all = FALSE)

  # missing engine
  env2 <- fake_engines()
  rm("hd_fake_all", envir = env2)
  expect_match(.hd_registry_problems(reg, preds, env2), "engine `hd_fake_all` not found", all = FALSE)

  # predictor not in formals, missing DBH, undeclared no-default arg, bad region_arg
  env3 <- fake_engines()
  env3$hd_fake_on <- function(DBH, species, HDOM, BA, extra) NULL
  reg3 <- reg
  reg3$required_predictors[[3]] <- character()
  reg3$region_arg[[2]] <- "ecoregion"
  probs <- .hd_registry_problems(reg3, preds, env3)
  expect_match(probs, "fake_on: not arguments of the engine: TPH", all = FALSE)
  expect_match(probs, "fake_on: .*without default.*: extra", all = FALSE)
  expect_match(probs, "fake_all: `DBH` must be a required predictor", all = FALSE)
  expect_match(probs, "fake_ab: region_arg `ecoregion`", all = FALSE)
})
