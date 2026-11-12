test_that("SI discovery covers all families without selecting or ranking models", {
  found <- find_si_models()
  reg <- si_model_registry()
  expect_identical(unique(found$model_id), reg$model_id)
  expect_setequal(unique(found$operation), c(
    "infer_si_from_height", "predict_height_from_si",
    "infer_si_from_gi", "evaluate_ecological_curve"
  ))
  expect_setequal(unique(found$productivity_quantity), c(
    "site_index", "height_index", "inherent_height_index", "ecological_iqs"
  ))
  expect_true(all(found$eligibility == "not_evaluated"))
  expect_true(all(found$scientific_compatibility == "not_requested"))
  expect_true(all(lengths(found$missing_predictors) > 0L))
  expect_false("usable" %in% names(found))
})

test_that("scope discovery normalizes codes and keeps scientific near-misses", {
  found <- find_si_models(
    jurisdiction = "on", species = "picemar",
    operation = "infer_si_from_height", available = c("age", "height"),
    stand_origin = "plantation", si_datum = "total_height"
  )
  expect_true(all(found$query_species == "PICE.MAR"))
  expect_true(all(vapply(found$species, function(x) "PICE.MAR" %in% x, logical(1))))
  sharma <- found[found$model_id == "sharma2021", ]
  expect_equal(nrow(sharma), 1L)
  expect_identical(sharma$scientific_compatibility, "incompatible")
  expect_match(paste(sharma$incompatibility_reasons[[1]], collapse = " "),
               "stand_origin")
  expect_match(paste(sharma$incompatibility_reasons[[1]], collapse = " "),
               "si_datum")
  carmean <- found[found$model_id == "carmean1996", ]
  expect_identical(carmean$scientific_compatibility, "incompatible")
  expect_match(paste(carmean$incompatibility_reasons[[1]], collapse = " "), "stand_origin")
  expect_identical(carmean$missing_predictors[[1]], character())
})

test_that("plantation discovery uses total age and retains years-since-planting definitions", {
  found <- find_si_models(
    jurisdiction = "QC", operation = "infer_si_from_height",
    available = c("age", "height", "species"),
    input_age_basis = "total_age", reference_age = 25,
    reference_age_basis = "total_age", stand_origin = "plantation"
  )
  for (id in c("augerward2021", "pregent2010", "pregent2016")) {
    row <- found[found$model_id == id, ]
    expect_identical(row$scientific_compatibility, "compatible", info = id)
    expect_identical(row$discovery_status, "names_complete", info = id)
    expect_match(row$native_scientific_definitions[[1]]$age_definition, "Years since planting")
    age_input <- row$operation_inputs[[1]]
    expect_match(age_input$definition[age_input$arg == "age"], "Years since planting")
  }
})

test_that("operations separate inputs without treating available columns as supplied", {
  gi <- find_si_models(
    species = "THUJ.PLI", jurisdiction = "BC", operation = "infer_si_from_gi",
    available = c("age", "gi")
  )
  cedar <- gi[gi$model_id == "nigh2000_gi", ]
  expect_true(cedar$inputs_available)
  expect_identical(cedar$discovery_status, "names_complete")
  conventional <- gi[gi$model_id == "nigh2000", ]
  expect_false(conventional$operation_supported)
  expect_identical(conventional$discovery_status, "incompatible")
  expect_match(conventional$incompatibility_reasons[[1]], "operation")

  both <- find_si_models(operation = "infer_si_from_height",
                         available = c("age", "height", "si", "species"))
  row <- both[both$model_id == "carmean1996", ]
  expect_identical(row$forbidden_predictors[[1]], "si")
  expect_true(row$inputs_available)
  expect_identical(row$discovery_status, "names_complete")
  reverse <- find_si_models(operation = "predict_height_from_si", available = "age")
  expect_setequal(reverse$missing_predictors[[match("carmean1996", reverse$model_id)]],
                  c("si", "species"))
})

test_that("climate presence is not scientific or row-level eligibility", {
  found <- find_si_models(
    species = "PICE.MAR", jurisdiction = "AB",
    operation = "infer_si_from_height", available = c("age", "height"),
    productivity_quantity = "site_index"
  )
  climate <- found[found$model_id == "huang2026_climate", ]
  expect_setequal(climate$missing_predictors[[1]],
                  c("MAP", "MWMT", "MAP_normal", "MWMT_normal"))
  expect_identical(climate$scientific_compatibility, "incompatible")
  complete <- find_si_models(
    species = "PICE.MAR", jurisdiction = "AB", operation = "infer_si_from_height",
    available = c("age", "height", "MAP", "MWMT", "MAP_normal", "MWMT_normal"),
    productivity_quantity = "inherent_height_index"
  )
  climate <- complete[complete$model_id == "huang2026_climate", ]
  expect_true(climate$inputs_available)
  expect_identical(climate$eligibility, "not_evaluated")
  expect_true(any(climate$input_constraints[[1]]$kind == "semantic_prerequisite"))
  plain <- complete[complete$model_id == "huang2026", ]
  expect_setequal(plain$forbidden_predictors[[1]],
                  c("MAP", "MWMT", "MAP_normal", "MWMT_normal"))
})

test_that("native scales and conditional height datums are used, not plot overrides", {
  found <- find_si_models(
    species = "PICE.GLA", jurisdiction = "QC", operation = "infer_si_from_height",
    available = c("age", "height"), reference_age = 50,
    input_age_basis = "breast_height_age"
  )
  pregent <- found[found$model_id == "pregent2010", ]
  expect_identical(pregent$native_scientific_definitions[[1]]$reference_age, 25)
  expect_identical(pregent$scientific_compatibility, "incompatible")
  expect_identical(pregent$native_defaults[[1]]$base_age, 25)
  expect_identical(pregent$plot_args[[1]]$base_age, 50)

  found <- find_si_models(
    species = "PICE.GLA", jurisdiction = "ON", operation = "infer_si_from_height",
    height_datum = "above_breast_height", si_datum = "above_breast_height"
  )
  parton <- found[found$model_id == "sharmaparton2018a", ]
  expect_identical(parton$scientific_compatibility, "incompatible")
  expect_identical(parton$native_scientific_definitions[[1]]$height_datum, "total_height")
  expect_identical(parton$native_scientific_definitions[[1]]$si_datum,
                   "above_breast_height")
})

test_that("species definitions do not leak across multi-species discovery", {
  found <- find_si_models(
    species = c("PINU.CON", "PSEU.MEN"), operation = "infer_si_from_height",
    reference_age = 80
  )
  payandeh <- found[found$model_id == "payandeh1974", ]
  expect_identical(payandeh$query_species, c("PINU.CON", "PSEU.MEN"))
  expect_identical(payandeh$scientific_compatibility, c("compatible", "incompatible"))
  expect_identical(payandeh$native_scientific_definitions[[1]]$reference_age, 80)
  expect_identical(payandeh$native_scientific_definitions[[2]]$reference_age, 100)
  unspecified <- find_si_models(operation = "infer_si_from_height", reference_age = 80)
  expect_identical(
    unspecified$scientific_compatibility[unspecified$model_id == "payandeh1974"],
    "unknown"
  )
})

test_that("reference clocks stay independent and combined origins cover either origin", {
  found <- find_si_models(
    species = "PICE.MAR", jurisdiction = "AB", operation = "infer_si_from_height",
    input_age_basis = "total_age", reference_age_basis = "breast_height_age"
  )
  expect_identical(
    found$scientific_compatibility[found$model_id == "huang2009_bh_age"], "compatible"
  )
  total <- found[found$model_id == "huang2009_total_age", ]
  expect_identical(total$scientific_compatibility, "incompatible")
  expect_match(total$incompatibility_reasons[[1]], "reference_age_basis")
  for (origin in c("natural", "plantation")) {
    found <- find_si_models(
      species = "PINU.CON", jurisdiction = "BC", operation = "infer_si_from_height",
      stand_origin = origin
    )
    expect_identical(
      found$scientific_compatibility[found$model_id == "batho2014"], "compatible"
    )
  }
  info <- get_si_model_info("nighcourtin1998_si50")
  expect_false(info$native_defaults$si50)
  expect_true(info$effective_args$si50)
  expect_identical(info$native_scientific_definitions$reference_age, 50)
})

test_that("the remaining ecological clock dispute does not establish compatibility", {
  for (clock in c("breast_height_age", "total_age")) {
    found <- find_si_models(operation = "infer_si_from_height", input_age_basis = clock)
    disputed <- found[found$model_id == "lafleche2013_observed", ]
    expect_true(all(disputed$scientific_compatibility == "unknown"))
    expect_true(all(lengths(disputed$missing_information) > 0L))
  }
})

test_that("assumed natural origin is discoverable but never reported as publication verified", {
  found <- find_si_models(
    species = "PICE.MAR", jurisdiction = "ON", operation = "infer_si_from_height",
    stand_origin = "natural"
  )
  expect_identical(found$scientific_compatibility[found$model_id == "carmean1996"], "compatible")
  info <- get_si_model_info("carmean1996")
  expect_identical(info$applicability$stand_origin, "natural")
  evidence <- info$unresolved_evidence
  assumption <- evidence[evidence$subject == "stand_origin_assumption", ]
  expect_identical(assumption$evidence_kind, "assumption")
  expect_identical(assumption$status, "unresolved")
  expect_match(assumption$note, "assumed natural")
  expect_identical(get_si_model_info("sharma2015")$applicability$stand_origin, "plantation")
  expect_identical(get_si_model_info("batho2014")$applicability$stand_origin, "natural_and_plantation")
})

test_that("Lundgren-Dolid discovery retains species clocks and nonrecommended near-misses", {
  found <- find_si_models(
    species = c("ABIE.BAL", "PICE.GLA", "PICE.MAR"), operation = "infer_si_from_height",
    input_age_basis = "total_age", reference_age_basis = "total_age"
  )
  rows <- found[grepl("^lundgrendolid", found$model_id), ]
  expect_identical(rows$scientific_compatibility, rep(c("incompatible", "incompatible", "compatible"), 2))
  white <- rows[rows$query_species == "PICE.GLA", ]
  expect_true(all(white$discovery_status == "incompatible"))
  expect_true(all(vapply(white$incompatibility_reasons, function(x) any(grepl("not recommend", x)), logical(1))))
  for (id in unique(rows$model_id)) {
    expect_equal(nrow(get_si_model_info(id)$native_scientific_definitions), 11L)
  }
})

test_that("Sharma-Reid discovery resolves above-BH compatibility and retains total-height near-misses", {
  for (operation in c("infer_si_from_height", "predict_height_from_si")) {
    args <- list(
      species = c("PINU.BAN", "PICE.MAR"), jurisdiction = "ON",
      operation = operation, stand_origin = "natural",
      input_age_basis = "breast_height_age", reference_age = 50,
      reference_age_basis = "breast_height_age",
      height_datum = "above_breast_height", si_datum = "above_breast_height"
    )
    found <- do.call(find_si_models, args)
    reid <- found[found$model_id == "sharmareid2018", ]
    expect_equal(nrow(reid), 2L)
    expect_true(all(reid$scientific_compatibility == "compatible"))
    for (field in c("height_datum", "si_datum")) {
      mismatch <- args
      mismatch[[field]] <- "total_height"
      found <- do.call(find_si_models, mismatch)
      reid <- found[found$model_id == "sharmareid2018", ]
      expect_true(all(reid$scientific_compatibility == "incompatible"))
      expect_true(all(vapply(reid$incompatibility_reasons, function(x) {
        any(grepl(field, x, fixed = TRUE))
      }, logical(1))))
    }
  }
  expect_equal(nrow(get_si_model_info("sharmareid2018")$unresolved_evidence), 0L)
})

test_that("Payandeh's accepted total-age contract retains assumptions without blocking discovery", {
  species <- c("ABIE.BAL", "PICE.GLA", "PICE.RUB", "PINU.MON", "PINU.CON",
               "PINU.PON", "PSEU.MEN", "PICE.SIT", "TSUG.HET")
  for (clock in c("total_age", "breast_height_age")) {
    found <- find_si_models(
      species = species, operation = "infer_si_from_height",
      available = c("age", "height"),
      input_age_basis = clock, reference_age_basis = clock
    )
    payandeh <- found[found$model_id == "payandeh1974", ]
    expect_equal(nrow(payandeh), length(species))
    expected <- if (clock == "total_age") "compatible" else "incompatible"
    expect_true(all(payandeh$scientific_compatibility == expected))
    expect_true(all(vapply(payandeh$limitations, function(x) {
      any(grepl("assumed", x))
    }, logical(1))))
  }
  info <- get_si_model_info("payandeh1974")
  assumptions <- info$unresolved_evidence[
    info$unresolved_evidence$subject == "age_basis_assumption",
  ]
  expect_equal(nrow(assumptions), 1L)
  expect_match(assumptions$note, "ABIE.BAL")
  expect_match(assumptions$note, "PICE.GLA")
  expect_match(assumptions$note, "not publication-verified")
})

test_that("conditional ecological requirements stay explicitly unresolved by names", {
  found <- find_si_models(
    species = "PICE.GLA", jurisdiction = "QC",
    operation = "evaluate_ecological_curve",
    available = c("age", "ecological_region", "ecological_type")
  )
  eco <- found[found$model_id == "lafleche2013_potential", ]
  expect_identical(eco$missing_predictors[[1]], character())
  expect_identical(eco$conditional_missing_predictors[[1]], "ecological_subregion")
  expect_true(is.na(eco$inputs_available))
  expect_identical(eco$discovery_status, "needs_review")
  expect_true(all(vapply(eco$conditional_requirements[[1]]$selectors, function(x) {
    identical(x$species, "PICE.GLA")
  }, logical(1))))
  expect_match(paste(eco$missing_information[[1]], collapse = " "), "conditional")
  expect_true(any(eco$input_constraints[[1]]$kind == "unsupported_key_form"))
})

test_that("SI model information exposes complete evidence and argument views", {
  info <- get_si_model_info("pregent2010")
  expect_identical(info$species, "PICE.GLA")
  expect_identical(info$native_defaults$base_age, 25)
  expect_identical(info$effective_args$base_age, 25)
  expect_identical(info$plot_args$base_age, 50)
  expect_identical(info$native_scientific_definitions$reference_age, 25)
  expect_true(nrow(info$applicability) > 0L)
  expect_true(nrow(info$metadata_evidence) > 0L)
  expect_true(nrow(info$operation_inputs) > 0L)
  expect_true(length(info$limitations) > 0L)
  expect_identical(get_si_model_info("nigh2009")$effective_args$variant, 1)
  gi <- get_si_model_info("nigh1998_gi")
  expect_identical(gi$operations, "infer_si_from_gi")
  expect_identical(gi$age_constraints$values[[1]], c(5L, 10L, 20L, 30L, 40L, 50L))
  expect_true(nrow(get_si_model_info("payandeh1974")$unresolved_evidence) > 0L)
})

test_that("SI discovery validates queries and preserves empty output types", {
  expect_error(get_si_model_info("absent"), "Unknown site-index model")
  expect_error(get_si_model_info(c("pregent2010", "pregent2016")), "single character")
  expect_error(find_si_models(species = character()), "species")
  expect_error(find_si_models(jurisdiction = NA_character_), "jurisdiction")
  expect_error(find_si_models(operation = "height"), "operation")
  expect_error(find_si_models(available = NA_character_), "available")
  expect_error(find_si_models(available = ""), "available")
  expect_error(find_si_models(reference_age = -1), "reference_age")
  expect_error(find_si_models(reference_age = Inf), "reference_age")
  expect_error(find_si_models(reference_age = c(25, 50)), "reference_age")
  expect_error(find_si_models(input_age_basis = "unresolved"), "input_age_basis")
  expect_error(find_si_models(stand_origin = "mixed"), "stand_origin")
  expect_error(find_si_models(si_datum = NA_character_), "si_datum")
  empty <- find_si_models(species = "ZZZZ.ZZZ")
  full <- find_si_models()
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(full))
  expect_identical(vapply(empty, typeof, character(1)),
                   vapply(full, typeof, character(1)))
})
