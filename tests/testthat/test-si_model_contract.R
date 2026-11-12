test_that("every SI variant has a consistent, evidenced operation contract", {
  reg <- si_model_registry()
  expect_identical(.si_registry_problems(reg), character())
  inputs <- .si_model_predictors(reg)
  constraints <- .si_model_constraints(reg)
  expect_identical(dplyr::bind_rows(reg$operation_inputs), inputs)
  expect_identical(dplyr::bind_rows(reg$input_constraints), constraints)
  expanded <- si_model_registry_species()
  for (column in c("operation_inputs", "input_constraints", "native_defaults",
                   "variant_args", "plot_args")) {
    expect_identical(expanded[[column]], reg[[column]], info = column)
  }
})

test_that("height and SI are required only in their respective operations", {
  reg <- si_model_registry()
  inputs <- .si_model_predictors(reg)
  conventional <- inputs[inputs$model_id == "sharmaparton2018a", ]
  required <- conventional[conventional$role == "required", ]
  expect_setequal(
    required$arg[required$operation == "infer_si_from_height"], c("age", "height")
  )
  expect_setequal(
    required$arg[required$operation == "predict_height_from_si"], c("age", "si")
  )
  expect_identical(
    conventional$role[conventional$arg == "si" &
                        conventional$operation == "infer_si_from_height"],
    "forbidden"
  )
  row <- reg[reg$model_id == "sharmaparton2018a", ]
  expect_true(all(row$scientific_definitions[[1]]$si_datum == "above_breast_height"))
  expect_match(conventional$definition[conventional$arg == "height"][[1]],
               "total_height")
})

test_that("GI contracts describe the age-specific interval and discrete support", {
  reg <- si_model_registry()
  for (id in c("nigh2000_gi", "nigh1998_gi")) {
    row <- reg[reg$model_id == id, ]
    inputs <- row$operation_inputs[[1]]
    expect_identical(unique(inputs$operation), "infer_si_from_gi")
    expect_setequal(inputs$arg[inputs$role == "required"], c("age", "gi"))
    expect_match(inputs$definition[inputs$arg == "gi"], "A - A_p", fixed = TRUE)
    rules <- row$input_constraints[[1]]
    support <- rules[rules$kind == "discrete_support" & rules$arg == "age", ]
    expect_identical(support$values, row$age_constraints[[1]]$values)
    expect_identical(support$engine_action, "error")
  }
})

test_that("ecological contracts preserve exact keys and conditional subregions", {
  reg <- si_model_registry()
  row <- reg[reg$model_id == "lafleche2013_potential", ]
  inputs <- row$operation_inputs[[1]]
  expect_identical(unique(inputs$operation), "evaluate_ecological_curve")
  expect_true(all(inputs$role[inputs$arg %in% c("height", "si")] == "forbidden"))
  expect_setequal(
    inputs$arg[inputs$role == "required" & is.na(inputs$condition_id)],
    c("age", "species", "ecological_region", "ecological_type")
  )
  rules <- row$input_constraints[[1]]
  conditional <- rules[rules$kind == "conditional_key_requirement", ]
  key <- list(species = "PICE.GLA", ecological_region = "5e", ecological_type = "MS22")
  match_key <- vapply(conditional$selectors, identical, logical(1), y = key)
  expect_equal(sum(match_key), 1L)
  expect_setequal(conditional$values[[which(match_key)]], c("5eS", "5eT"))
  expect_true(conditional$condition_id[[which(match_key)]] %in% inputs$condition_id)
  unsupported <- rules[rules$kind == "unsupported_key_form", ]
  expect_identical(unsupported$selectors[[1]], list(
    species = "PICE.GLA", ecological_region = "5b",
    ecological_subregion = NA_character_, ecological_type = "MS22"
  ))
  expect_identical(unsupported$engine_action, "error")
  keys <- rules$values[[which(rules$kind == "discrete_support")]]
  expect_false(any(keys$species == "PICE.GLA" & keys$ecological_region == "5b" &
                     keys$ecological_type == "MS22"))
  expect_named(si_lafleche2013(
    age = 50, species = "PICE.GLA", ecological_region = "5e",
    ecological_subregion = "5eS", ecological_type = "MS22"
  ), c("height", "si"))
  expect_error(si_lafleche2013(
    age = 50, species = "PICE.GLA", ecological_region = "5e", ecological_type = "MS22"
  ), "No Lafleche2013")
  expect_error(si_lafleche2013(
    age = 50, species = "PICE.GLA", ecological_region = "5b", ecological_type = "MS22"
  ), "LOGIST3")
})

test_that("climate inputs and aggregation semantics do not leak into non-climate HI", {
  reg <- si_model_registry()
  climate_args <- c("MAP", "MWMT", "MAP_normal", "MWMT_normal")
  for (id in c("huang2026", "huang2026_climate")) {
    row <- reg[reg$model_id == id, ]
    inputs <- row$operation_inputs[[1]]
    climate <- inputs[inputs$arg %in% climate_args, ]
    expect_equal(nrow(climate), 8L)
    expect_true(all(climate$role == if (id == "huang2026") "forbidden" else "required"))
  }
  rules <- reg$input_constraints[[match("huang2026_climate", reg$model_id)]]
  windows <- rules[rules$kind == "semantic_prerequisite", ]
  pine <- windows[vapply(windows$selectors, function(x) {
    identical(x$species, "PINU.CON")
  }, logical(1)), ]
  expect_equal(nrow(pine), 4L)
  expect_true(all(vapply(pine$values, function(x) {
    identical(x$window, "PY") && identical(x$normal_period, c(1961L, 1990L)) &&
      isFALSE(x$include_current_year) && isFALSE(x$include_pre_initiation)
  }, logical(1))))
  expect_true(all(pine$engine_action == "not_enforced"))
  expect_true(any(rules$kind == "mathematical_domain" &
                    rules$arg == "MAP" & rules$engine_action == "error"))
  age_range <- rules[rules$arg == "age" & rules$kind == "calibration_range", ]
  expect_true(all(vapply(age_range$values, identical, logical(1), y = c(NA_real_, 180))))
  expect_true(all(age_range$engine_action == "warning"))
})

test_that("native, variant and plotting arguments remain distinct", {
  reg <- si_model_registry()
  for (id in c("pregent2010", "pregent2016")) {
    row <- reg[reg$model_id == id, ]
    expect_identical(row$native_defaults[[1]]$base_age, 25)
    expect_identical(row$variant_args[[1]], list())
    expect_identical(row$plot_args[[1]], list(base_age = 50))
    expect_true(all(row$operation_inputs[[1]]$role[
      row$operation_inputs[[1]]$arg == "base_age"
    ] == "optional"))
  }
  birch <- reg[reg$model_id == "nigh2009", ]
  expect_identical(birch$variant_args[[1]], list(variant = 1))
  expect_true(all(birch$operation_inputs[[1]]$role[
    birch$operation_inputs[[1]]$arg == "variant"
  ] == "fixed"))
  expect_false(any(grepl("^nigh2009_", reg$model_id)))
  for (id in c("nigh2002", "nigh2004")) {
    row <- reg[reg$model_id == id, ]
    expect_true(all(row$operation_inputs[[1]]$role[
      row$operation_inputs[[1]]$arg == "bec_zone"
    ] == "optional"))
    rules <- row$input_constraints[[1]]
    support <- rules[rules$kind == "discrete_support" & rules$arg == "bec_zone", ]
    expect_true(all(vapply(support$values, identical, logical(1),
                          y = row$subregion_scope[[1]])))
  }
})

test_that("SI checks detect duplicate, unknown and illegal input definitions", {
  reg <- si_model_registry()
  inputs <- .si_model_predictors(reg)
  expect_match(paste(.si_registry_problems(dplyr::bind_rows(reg, reg[1, ])),
                     collapse = "\n"), "Duplicated model_id")
  expect_match(paste(.si_registry_problems(
    reg, inputs = dplyr::bind_rows(inputs, inputs[1, ])
  ), collapse = "\n"), "Duplicated predictor")
  bad <- inputs
  bad$arg[[1]] <- "not_an_engine_argument"
  expect_match(paste(.si_registry_problems(reg, inputs = bad), collapse = "\n"),
               "not arguments of the engine")
  bad$model_id[[1]] <- "unknown_model"
  expect_match(paste(.si_registry_problems(reg, inputs = bad), collapse = "\n"),
               "unregistered model_id")
})

test_that("SI checks enforce operation-specific required inputs and exclusivity", {
  reg <- si_model_registry()
  inputs <- .si_model_predictors(reg)
  remove <- inputs$model_id == "huang1994" & inputs$arg == "species" &
    inputs$operation == "infer_si_from_height"
  expect_match(paste(.si_registry_problems(reg, inputs = inputs[!remove, ]),
                     collapse = "\n"), "without default")
  bad <- inputs
  bad$role[bad$model_id == "buckman2006" & bad$arg == "si" &
             bad$operation == "infer_si_from_height"] <- "required"
  expect_match(paste(.si_registry_problems(reg, inputs = bad), collapse = "\n"),
               "height/SI exclusivity")
  bad <- reg
  bad$supports_predict_height[bad$model_id == "nigh2000_gi"] <- TRUE
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"),
               "capabilities")
})

test_that("SI checks reject conflicting selectors and incomplete scientific metadata", {
  reg <- si_model_registry()
  bad <- reg
  i <- match("pregent2010", bad$model_id)
  bad$fixed_arg_roles[[i]] <- c(base_age = "variant")
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"), "fixed")
  bad <- reg
  bad$fixed_args[[i]] <- unname(bad$fixed_args[[i]])
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"), "fixed")
  bad <- reg
  i <- match("huang2026_climate", bad$model_id)
  bad$variant_args[[i]]$variant <- "non_climate"
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"), "fixed")
  bad <- reg
  bad$scientific_definitions[[1]] <- bad$scientific_definitions[[1]][0, ]
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"),
               "scientific definitions")
  bad <- reg
  i <- match("payandeh1974", bad$model_id)
  bad$scientific_definitions[[i]] <- bad$scientific_definitions[[i]][-1, ]
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"),
               "species.*coverage")
})

test_that("SI checks reject orphan conditions, empty support and absent evidence", {
  reg <- si_model_registry()
  inputs <- .si_model_predictors(reg)
  inputs$condition_id[[1]] <- "missing-condition"
  expect_match(paste(.si_registry_problems(reg, inputs = inputs), collapse = "\n"),
               "Unknown condition")
  rules <- .si_model_constraints(reg)
  i <- which(rules$model_id == "nigh1998_gi" & rules$kind == "discrete_support")
  rules$values[[i]] <- integer()
  expect_match(paste(.si_registry_problems(reg, constraints = rules), collapse = "\n"),
               "discrete support")
  inputs <- .si_model_predictors(reg)
  inputs$source_locator[[1]] <- ""
  expect_match(paste(.si_registry_problems(reg, inputs = inputs), collapse = "\n"),
               "evidence")
  rules <- .si_model_constraints(reg)
  i <- which(rules$kind == "conditional_key_requirement")[[1]]
  rules$selectors[[i]] <- list(not_a_selector = "5e")
  expect_match(paste(.si_registry_problems(reg, constraints = rules), collapse = "\n"),
               "selector")
})

test_that("SI checks detect missing ecological exclusions and species climate rules", {
  reg <- si_model_registry()
  rules <- .si_model_constraints(reg)
  missing <- rules$model_id == "lafleche2013_potential" &
    rules$kind == "unsupported_key_form"
  expect_match(paste(.si_registry_problems(reg, constraints = rules[!missing, ]),
                     collapse = "\n"), "unsupported ecological form")
  missing <- rules$model_id == "huang2026_climate" &
    rules$kind == "semantic_prerequisite" & rules$arg == "MAP" &
    vapply(rules$selectors, function(x) identical(x$species, "PINU.CON"), logical(1))
  expect_match(paste(.si_registry_problems(reg, constraints = rules[!missing, ]),
                     collapse = "\n"), "species climate aggregation")
  bad <- reg
  i <- match("huang2026_climate", reg$model_id)
  bad$scientific_definitions[[i]]$productivity_quantity <- "site_index"
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"),
               "scientific quantity")
  bad <- reg
  bad$scientific_definitions[[1]]$reference_age_basis <- "ALL"
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"),
               "unknown is not ALL")
  bad$age_basis[[1]] <- NA_character_
  bad$scientific_definitions[[1]]$productivity_quantity <- NA_character_
  expect_match(paste(.si_registry_problems(bad), collapse = "\n"),
               "scientific")
})

test_that("completing scientific metadata cannot invent a new model's scale", {
  reg <- si_model_registry()
  for (id in "lafleche2013_observed") {
    defs <- reg$scientific_definitions[[match(id, reg$model_id)]]
    expect_true(all(defs$status == "unresolved"))
  }
  missing <- reg[1, ]
  missing$scientific_definitions[[1]] <- missing$scientific_definitions[[1]][0, ]
  expect_error(.si_complete_scientific_metadata(missing), "No scientific definition")
})
