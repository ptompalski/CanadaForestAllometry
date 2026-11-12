test_that("native SI dispatch matches engines without applying plot overrides", {
  result <- si(
    age = c(20, 30), height = c(10, 15), species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation"
  )
  expect_equal(result$si, si_pregent2010(age = c(20, 30), height = c(10, 15))$si)
  expect_identical(result$model_id, rep("pregent2010", 2))
  expect_identical(result$reference_age, c(25, 25))
  expect_identical(result$reference_age_basis, rep("total_age", 2))
  expect_identical(result$si_datum, rep("total_height", 2))
  expect_true(all(result$status == "success"))
  expect_true(all(lengths(result$diagnostics) > 0L))
})

test_that("plantation models use total-age labels without changing predictions", {
  models <- c(
    augerward2021 = "PICE.MAR",
    pregent2010 = "PICE.GLA",
    pregent2016 = "PICE.ABI"
  )
  for (id in names(models)) {
    species <- models[[id]]
    engine <- get(paste0("si_", id))
    for (input in c("height", "si")) {
      args <- list(
        age = c(20, 30), species = species, jurisdiction = "QC",
        input_age_basis = "total_age", stand_origin = "plantation"
      )
      args[[input]] <- c(12, 16)
      if (input == "si") args$model_id <- id
      result <- do.call(si, args)
      direct <- list(age = args$age)
      direct[[input]] <- args[[input]]
      if ("species" %in% names(formals(engine))) direct$species <- species
      output <- if (input == "height") "si" else "height"
      expect_equal(result[[output]], do.call(engine, direct)[[output]], info = id)
      expect_identical(result$model_id, rep(id, 2))
      expect_identical(result$input_age_basis, rep("total_age", 2))
      expect_identical(result$reference_age_basis, rep("total_age", 2))
      expect_true(all(result$status == "success"), info = id)
    }
  }
})

test_that("supplied SI is defined explicitly and stays on its originating model", {
  result <- si(
    age = c(20, 40), si = 18, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", model_id = "pregent2010",
    stand_origin = "plantation"
  )
  expect_equal(result$height, si_pregent2010(age = c(20, 40), si = 18)$height)
  expect_identical(result$model_id, rep("pregent2010", 2))
  expect_error(si(
    age = 30, si = 18, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation"
  ), "definition")
  expect_warning(defined <- si(
    age = 30, si = 18, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation",
    reference_age = 25, reference_age_basis = "total_age",
    si_datum = "total_height", productivity_quantity = "site_index"
  ), class = "cfa_si_unpinned_si")
  expect_equal(defined$height, si_pregent2010(age = 30, si = 18)$height)
})

test_that("scientific mismatches and missing rows never trigger clock conversions", {
  expect_warning(result <- si(
    age = c(25, 25, NA), height = 12, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = c("breast_height_age", "total_age", "total_age"),
    stand_origin = "plantation"
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(result$si[[1]]))
  expect_match(paste(result$diagnostics[[1]]$reason, collapse = " "), "input_age_basis")
  expect_equal(result$si[[2]], si_pregent2010(age = 25, height = 12)$si)
  expect_identical(result$status[[3]], "missing_input")
  expect_warning(mismatch <- si(
    age = 25, height = 12, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation", reference_age = 50
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(mismatch$si))
})

test_that("explicit models preserve independent age clocks and SI and height datums", {
  expect_warning(huang <- si(
    age = 60, height = 20, species = "PICE.MAR", jurisdiction = "AB",
    input_age_basis = "total_age", model_id = "huang2009_bh_age"
  ), class = "cfa_si_applicability")
  expect_equal(huang$si, si_huang2009(age = 60, height = 20, species = "PICE.MAR")$si)
  expect_identical(huang$input_age_basis, "total_age")
  expect_identical(huang$reference_age_basis, "breast_height_age")
  sharma <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural", model_id = "sharma2021"
  )
  expect_equal(sharma$si, si_sharma2021(age = 40, height = 18, species = "PICE.MAR")$si)
  expect_identical(sharma$si_datum, "above_breast_height")
  expect_identical(sharma$height_datum, "total_height")
})

test_that("Sharma-Reid dispatch preserves native above-BH predictions in both directions", {
  species <- c("PINU.BAN", "PICE.MAR")
  for (input in c("height", "si")) {
    args <- list(
      age = c(40, 60), species = species, jurisdiction = "ON",
      input_age_basis = "breast_height_age", stand_origin = "natural",
      height_datum = "above_breast_height", model_id = "sharmareid2018"
    )
    args[[input]] <- c(16, 14)
    result <- do.call(si, args)
    direct <- list(age = args$age, species = species)
    direct[[input]] <- args[[input]]
    output <- if (input == "height") "si" else "height"
    expect_equal(result[[output]], do.call(si_sharmareid2018, direct)[[output]])
    expect_identical(result$model_id, rep("sharmareid2018", 2))
    expect_identical(result$reference_age, c(50, 50))
    expect_true(all(result$input_age_basis == "breast_height_age"))
    expect_true(all(result$reference_age_basis == "breast_height_age"))
    expect_true(all(result$si_datum == "above_breast_height"))
    expect_true(all(result$height_datum == "above_breast_height"))
    expect_true(all(result$status == "success"))
    mismatches <- list(
      height_datum = "total_height", si_datum = "total_height",
      input_age_basis = "total_age", reference_age_basis = "total_age",
      stand_origin = "plantation"
    )
    for (field in names(mismatches)) {
      mismatch <- args
      mismatch[[field]] <- mismatches[[field]]
      expect_warning(rejected <- do.call(si, mismatch), class = "cfa_si_no_prediction")
      expect_true(all(is.na(rejected[[output]])))
      expect_match(rejected$diagnostics[[1]]$reason, field)
    }
  }
})

test_that("Sharma-Reid is automatically eligible only on its native height datum", {
  args <- list(
    age = 40, height = 16, species = c("PINU.BAN", "PICE.MAR"), jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural",
    height_datum = "above_breast_height"
  )
  comparison <- do.call(si, c(args, list(pick_best = FALSE)))
  for (i in seq_along(comparison)) {
    reid <- comparison[[i]][comparison[[i]]$model_id == "sharmareid2018", ]
    expect_identical(reid$status, "success")
    expect_equal(reid$si, si_sharmareid2018(
      age = 40, height = 16, species = args$species[[i]]
    )$si)
  }
  args$species <- "PINU.BAN"
  best <- do.call(si, args)
  expect_identical(best$model_id, "sharmareid2018")
  expect_identical(best$height_datum, "above_breast_height")
  args$height_datum <- "total_height"
  comparison <- do.call(si, c(args, list(pick_best = FALSE)))[[1]]
  reid <- comparison[comparison$model_id == "sharmareid2018", ]
  expect_identical(reid$status, "excluded")
  expect_match(reid$reason, "height_datum")
})

test_that("Payandeh total-age dispatch preserves direct predictions and species SI scales", {
  species <- c("ABIE.BAL", "PICE.GLA", "PICE.RUB", "PINU.MON", "PINU.CON",
               "PINU.PON", "PSEU.MEN", "PICE.SIT", "TSUG.HET")
  for (input in c("height", "si")) {
    args <- list(
      age = 60, species = species, jurisdiction = "ON",
      input_age_basis = "total_age", stand_origin = "natural", model_id = "payandeh1974"
    )
    args[[input]] <- 18
    result <- do.call(si, args)
    direct <- list(age = 60, species = species)
    direct[[input]] <- 18
    output <- if (input == "height") "si" else "height"
    expect_equal(result[[output]], do.call(si_payandeh1974, direct)[[output]])
    expect_identical(result$model_id, rep("payandeh1974", length(species)))
    expect_identical(result$reference_age, c(50, 50, 50, 50, 80, 100, 100, 100, 100))
    expect_true(all(result$input_age_basis == "total_age"))
    expect_true(all(result$reference_age_basis == "total_age"))
    expect_match(
      paste(result$diagnostics[[1]]$limitations[[1]], collapse = " "), "assumed"
    )
    args$input_age_basis <- "breast_height_age"
    expect_warning(rejected <- do.call(si, args), class = "cfa_si_no_prediction")
    expect_true(all(is.na(rejected[[output]])))
    expect_match(rejected$diagnostics[[1]]$reason, "input_age_basis")
  }
  automatic <- si(
    age = 60, height = 18, species = "ABIE.BAL", jurisdiction = "ON",
    input_age_basis = "total_age", stand_origin = "natural", pick_best = FALSE
  )
  payandeh <- automatic[[1]][automatic[[1]]$model_id == "payandeh1974", ]
  expect_identical(payandeh$status, "success")
  expect_match(paste(payandeh$limitations[[1]], collapse = " "), "assumed natural")
})

test_that("missing observation origin and known scientific mismatches remain excluded", {
  expect_warning(result <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age"
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(result$si))
  candidates <- result$diagnostics[[1]]
  expect_true(all(candidates$status == "excluded"))
  expect_match(candidates$reason[candidates$model_id == "sharma2021"], "stand_origin")
  expect_warning(disputed <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "total_age", stand_origin = "natural",
    reference_age_basis = "breast_height_age", model_id = "scottvoorhis1986_bh_age"
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(disputed$si))
  expect_match(disputed$diagnostics[[1]]$reason, "reference_age_basis")
})

test_that("source-reconciled variants match direct engines on independent native clocks", {
  cases <- list(
    list(id = "scottvoorhis1986_bh_age", engine = si_scottvoorhis1986,
         species = "PICE.MAR", clock = "total_age", reference = "total_age",
         options = list(convert_to_total_age = FALSE)),
    list(id = "scottvoorhis1986_total_age", engine = si_scottvoorhis1986,
         species = "PICE.MAR", clock = "breast_height_age", reference = "total_age",
         options = list(convert_to_total_age = TRUE)),
    list(id = "parresolvissage1998", engine = si_parresolvissage1998,
         species = "PINU.STR", clock = "total_age", reference = "total_age"),
    list(id = "nigh2004", engine = si_nigh2004, age = 10,
         species = c("PINU.CON", "PICE.GLA"), jurisdiction = "BC",
         clock = "total_age", reference = "breast_height_age"),
    list(id = "nighcourtin1998_si50", engine = si_nighcourtin1998,
         species = "ALNU.RUB", jurisdiction = "BC", bec_zone = "CWH",
         clock = "breast_height_age", reference = "breast_height_age",
         options = list(si50 = TRUE))
  )
  species <- c("ABIE.BAL", "PINU.RES", "PINU.BAN", "PINU.STR", "PICE.MAR",
               "LARI.LAR", "THUJ.OCC", "POPU.TRE", "QUER.RUB", "BETU.PAP")
  for (variant in c("exponential_monomolecular", "monomolecular")) {
    cases[[length(cases) + 1L]] <- list(
      id = paste0("lundgrendolid1970_", variant), engine = si_lundgrendolid1970,
      species = species,
      clock = ifelse(species == "ABIE.BAL", "breast_height_age", "total_age"),
      reference = ifelse(species == "ABIE.BAL", "breast_height_age", "total_age"),
      options = list(variant = variant)
    )
  }
  for (case in cases) {
    direct <- c(list(age = case$age %||% 40), case$options)
    if ("species" %in% names(formals(case$engine))) direct$species <- case$species
    heights <- do.call(case$engine, c(direct, list(si = 18)))$height
    for (input in c("height", "si")) {
      args <- list(
        age = direct$age, species = case$species,
        jurisdiction = case$jurisdiction %||% "ON", stand_origin = "natural",
        model_id = case$id, input_age_basis = case$clock,
        reference_age = 50, reference_age_basis = case$reference
      )
      if (!is.null(case$bec_zone)) args$bec_zone <- case$bec_zone
      args[[input]] <- if (input == "si") 18 else heights
      engine_args <- direct
      engine_args[[input]] <- args[[input]]
      result <- do.call(si, args)
      output <- if (input == "si") "height" else "si"
      expect_equal(result[[output]], do.call(case$engine, engine_args)[[output]], info = case$id)
      expect_true(all(result$status == "success"), info = case$id)
      expect_identical(result$input_age_basis, rep_len(case$clock, nrow(result)))
      expect_identical(result$reference_age_basis, rep_len(case$reference, nrow(result)))
      args$input_age_basis <- ifelse(case$clock == "total_age", "breast_height_age", "total_age")
      expect_warning(mismatch <- do.call(si, args), class = "cfa_si_no_prediction")
      expect_true(all(is.na(mismatch[[output]])), info = case$id)
    }
  }
})

test_that("Lundgren-Dolid recommendations restrict automatic use and remain explicit", {
  for (variant in c("exponential_monomolecular", "monomolecular")) {
    id <- paste0("lundgrendolid1970_", variant)
    comparison <- si(
      age = 40, height = 14, species = "PICE.GLA", jurisdiction = "ON",
      input_age_basis = "breast_height_age", stand_origin = "natural", pick_best = FALSE
    )[[1]]
    row <- comparison[comparison$model_id == id, ]
    expect_identical(row$status, "excluded")
    expect_match(row$reason, "not recommend")
    expect_warning(explicit <- si(
      age = 40, height = 14, species = "PICE.GLA", jurisdiction = "ON",
      input_age_basis = "breast_height_age", stand_origin = "natural", model_id = id
    ), class = "cfa_si_applicability")
    expect_equal(explicit$si, si_lundgrendolid1970(
      age = 40, height = 14, species = "PICE.GLA", variant = variant
    )$si)
  }
  for (age in c(19, 20)) {
    comparison <- si(
      age = age, height = 8, species = "ABIE.BAL", jurisdiction = "ON",
      input_age_basis = "breast_height_age", stand_origin = "natural", pick_best = FALSE
    )[[1]]
    row <- comparison[comparison$model_id == "lundgrendolid1970_exponential_monomolecular", ]
    expect_identical(row$status, if (age < 20) "excluded" else "success")
    if (age < 20) expect_match(row$reason, "20")
  }
})

test_that("specialized models cannot enter dispatch even by explicit ID", {
  for (id in c("nigh2000_gi", "lafleche2013_potential", "huang2026_climate")) {
    expect_error(si(
      age = 30, height = 15, species = "PICE.GLA", jurisdiction = "QC",
      input_age_basis = "total_age", model_id = id
    ), "specialized")
  }
  result <- si(
    age = 30, height = 12, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation",
    pick_best = FALSE
  )[[1]]
  expect_identical(result$status[result$model_id == "lafleche2013_potential"], "excluded")
})

test_that("comparison mode and mutate retain provenance and row identities", {
  data <- tibble::tibble(age = c(20, 30), height = c(10, 15))
  result <- data |>
    dplyr::mutate(si(
      age, height, species = "PICE.GLA", jurisdiction = "QC",
      input_age_basis = "total_age", stand_origin = "plantation"
    ))
  expect_equal(nrow(result), 2)
  comparison <- si(
    age = data$age, height = data$height, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation", pick_best = FALSE
  )
  expect_length(comparison, 2)
  for (i in seq_along(comparison)) {
    success <- comparison[[i]][comparison[[i]]$status == "success", ]
    expect_identical(success$model_id, "pregent2010")
    expect_equal(success$si, result$si[[i]])
    expect_true(nzchar(success$definition_id))
  }
  no_id <- si(
    age = 20, height = 10, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation", keep_model_id = FALSE
  )
  expect_false("model_id" %in% names(no_id))
  expect_true("model_id" %in% names(no_id$diagnostics[[1]]))
})

.si_test_candidates <- function() {
  reg <- si_model_registry_species()
  reg <- reg[reg$model_id %in% c("sharma2015", "sharma2021"), ]
  reg$rank <- c(100, 100)
  reg$applicability <- lapply(reg$applicability, function(x) {
    x$stand_origin <- "natural"
    x
  })
  reg
}

test_that("rank ties are stable and comparison candidates are not silently discarded", {
  registry <- .si_test_candidates()
  local_mocked_bindings(si_model_registry_species = function() registry)
  result <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  )
  expect_identical(result$model_id, "sharma2015")
  expect_identical(result$diagnostics[[1]]$model_id, c("sharma2015", "sharma2021"))
  expect_identical(result$diagnostics[[1]]$status, c("success", "not_tried"))
  i <- match("sharma2021", registry$model_id)
  registry$rank[[i]] <- 101
  ranked <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  )
  expect_identical(ranked$model_id, "sharma2021")
  registry$scientific_definitions[[i]]$input_age_basis <- "total_age"
  compatible <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  )
  expect_identical(compatible$model_id, "sharma2015")
})

test_that("failed predictions fall back only within the same scientific definition", {
  registry <- .si_test_candidates()
  local_mocked_bindings(
    si_model_registry_species = function() registry,
    si_sharma2015 = function(...) tibble::tibble(si = NaN)
  )
  expect_warning(result <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  ), class = "cfa_si_fallback")
  expect_identical(result$model_id, "sharma2021")
  expect_identical(result$diagnostics[[1]]$status, c("failed", "success"))
  expect_match(result$diagnostics[[1]]$reason[[1]], "non-finite")
  i <- match("sharma2021", registry$model_id)
  registry$scientific_definitions[[i]]$reference_age <- 25
  expect_warning(no_fallback <- si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(no_fallback$si))
  expect_match(no_fallback$diagnostics[[1]]$reason[[2]], "fallback")
})

test_that("engine errors propagate rather than hiding behind fallback", {
  registry <- .si_test_candidates()
  local_mocked_bindings(
    si_model_registry_species = function() registry,
    si_sharma2015 = function(...) stop("unexpected engine bug")
  )
  expect_error(si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  ), "unexpected engine bug", class = "cfa_si_engine_error")
})

test_that("supplied SI cannot fall back even to an identically labelled scale", {
  registry <- .si_test_candidates()
  local_mocked_bindings(
    si_model_registry_species = function() registry,
    si_sharma2015 = function(...) tibble::tibble(height = NaN)
  )
  expect_warning(expect_warning(result <- si(
    age = 40, si = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural",
    reference_age = 50, reference_age_basis = "breast_height_age",
    si_datum = "above_breast_height", productivity_quantity = "site_index"
  ), class = "cfa_si_unpinned_si"), class = "cfa_si_no_prediction")
  expect_true(is.na(result$height))
  expect_identical(result$diagnostics[[1]]$status, c("failed", "excluded"))
})

test_that("malformed engine outputs abort rather than looking like candidate failure", {
  registry <- .si_test_candidates()
  local_mocked_bindings(
    si_model_registry_species = function() registry,
    si_sharma2015 = function(...) tibble::tibble(height = 18)
  )
  expect_error(si(
    age = 40, height = 18, species = "PICE.MAR", jurisdiction = "ON",
    input_age_basis = "breast_height_age", stand_origin = "natural"
  ), "output shape", class = "cfa_si_engine_contract")
})

test_that("non-climate HI remains labelled separately and engine values are unchanged", {
  expect_warning(result <- si(
    age = c(30, 60), height = 20, species = "PICE.MAR", jurisdiction = "AB",
    input_age_basis = "total_age", model_id = "huang2026"
  ), class = "cfa_si_applicability")
  expect_equal(
    result$si,
    si_huang2026(age = c(30, 60), height = 20, species = "PICE.MAR")$si
  )
  expect_identical(result$productivity_quantity, rep("height_index", 2))
  expect_true(all(vapply(result$diagnostics, function(x) {
    identical(x$engine_args[[1]]$variant, "non_climate")
  }, logical(1))))
  expect_warning(mismatch <- si(
    age = 30, height = 20, species = "PICE.MAR", jurisdiction = "AB",
    input_age_basis = "total_age", model_id = "huang2026",
    productivity_quantity = "site_index"
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(mismatch$si))
})

test_that("engine warnings and extrapolation remain visible with the result", {
  expect_warning(result <- si(
    age = 65, height = 25, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation", model_id = "pregent2010"
  ))
  expect_equal(result$si, suppressWarnings(si_pregent2010(age = 65, height = 25))$si)
  expect_identical(result$status, "success_with_warnings")
  expect_true(length(result$diagnostics[[1]]$warnings[[1]]) > 0)
})

test_that("BC source geography is required automatically and known mismatches excluded", {
  unknown <- si(
    age = 40, height = 18, species = "PINU.CON", jurisdiction = "BC",
    input_age_basis = "breast_height_age", stand_origin = "natural", pick_best = FALSE
  )[[1]]
  batho <- unknown[unknown$model_id == "batho2014", ]
  expect_identical(batho$status, "excluded")
  expect_match(batho$reason, "SBS")
  valid <- si(
    age = 40, height = 18, species = "PINU.CON", jurisdiction = "BC",
    input_age_basis = "breast_height_age", stand_origin = "natural", bec_zone = "SBS",
    model_id = "batho2014"
  )
  expect_identical(valid$model_id, "batho2014")
  expect_equal(valid$si, si_batho2014(age = 40, height = 18, species = "PINU.CON")$si)
  expect_warning(wrong <- si(
    age = 40, height = 18, species = "PINU.CON", jurisdiction = "BC",
    input_age_basis = "breast_height_age", stand_origin = "natural", bec_zone = "CWH",
    model_id = "batho2014"
  ), class = "cfa_si_no_prediction")
  expect_true(is.na(wrong$si))
})

test_that("wrapper validates lengths, choices, flags, selectors and positive inputs", {
  args <- list(
    age = 30, height = 12, species = "PICE.GLA", jurisdiction = "QC",
    input_age_basis = "total_age", stand_origin = "plantation"
  )
  expect_error(do.call(si, c(args, list(si = 20))), "exactly one")
  expect_error(do.call(si, utils::modifyList(args, list(age = numeric()))), "length")
  expect_error(do.call(si, utils::modifyList(args, list(age = c(20, 30), height = 1:3))),
               "length")
  expect_error(do.call(si, utils::modifyList(args, list(pick_best = NA))), "pick_best")
  expect_error(do.call(si, utils::modifyList(args, list(age = -1))), "age")
  expect_error(do.call(si, utils::modifyList(args, list(input_age_basis = "unknown"))),
               "input_age_basis")
  expect_error(do.call(si, c(args, list(base_age = 50))), "base_age")
  expect_error(do.call(si, c(args, list(model_id = "absent"))), "model_id")
})
