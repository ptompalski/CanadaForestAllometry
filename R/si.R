#' Estimate site index or height with automatic model selection
#'
#' Estimates site index from age and height, or height from age and site index.
#' Selects a suitable non-climate age-height model for each observation using
#' species, jurisdiction, stand type and age type. Does not convert between
#' age types, height definitions or site indices from different models.
#'
#' @param age Numeric vector of positive ages in years. Its type must be
#'   declared with `input_age_basis`. For Auger-Ward and Prégent models, use
#'   years since planting with `input_age_basis = "total_age"`; no nursery-age
#'   adjustment is applied.
#' @param height,si Supply exactly one positive numeric vector, in metres.
#'   Supply `height` to estimate site index, or `si` to predict height.
#'   When supplying `si`, the simplest option is to provide the `model_id`
#'   of the model used to estimate it. Alternatively, supply `reference_age`,
#'   `reference_age_basis`, `si_datum` and `productivity_quantity` to describe
#'   the value. Without a model ID, a warning is issued because models sharing
#'   these definitions can still predict different heights at other ages.
#' @param species Character vector of NFI species codes. Compact codes are
#'   accepted. Species must match a model's supported codes; a genus-level
#'   model is not substituted automatically.
#' @param jurisdiction Character vector of province/territory codes.
#' @param ... Named additional model inputs, currently `natural_subregion`.
#'   Optional inputs use the selected model's defaults when missing.
#'   To change settings such as `base_age`, `total_height` or `variant`,
#'   choose a registered model version or call the individual model function.
#' @param input_age_basis Character vector: `"total_age"`,
#'   `"breast_height_age"` or `"corrected_age_above_1m"`.
#'   Required: describes how the supplied age was measured. Breast-height age
#'   is years since reaching 1.3 m. Ages are used as supplied, not converted.
#'   `"total_age"` also covers years since planting for models defined that way.
#' @param reference_age Optional positive numeric vector of requested SI
#'   reference ages, in years. For example, `50` requests site index at age 50.
#'   `NULL` uses the selected model's reference age when estimating SI.
#'   This restricts model selection; it does not change a model's reference age.
#' @param reference_age_basis Optional character vector describing how the
#'   SI reference age is measured, with the same choices as `input_age_basis`.
#'   It can differ from the type of age supplied in `age`.
#' @param productivity_quantity Optional character vector: `"site_index"` or
#'   `"height_index"`, depending on the quantity used by the model.
#'   `NULL` allows either; the result identifies which was used.
#' @param si_datum Optional character vector: `"total_height"` or
#'   `"above_breast_height"`. Describes whether the height defining the
#'   requested or supplied SI is measured from the ground or above 1.3 m.
#' @param height_datum Character vector describing how measured or predicted
#'   height is expressed: `"total_height"` (from the ground, the default) or
#'   `"above_breast_height"` (above 1.3 m). This can differ from the height
#'   definition used for SI.
#' @param stand_origin Character vector: `"natural"`, `"plantation"` or `NA`.
#'   Specify whether the stand is natural or planted for automatic selection.
#'   Missing stand type is not inferred.
#' @param bec_zone Optional character vector of British Columbia
#'   biogeoclimatic zone codes. Helps select models for the appropriate region
#'   and is passed to model functions that accept it.
#' @param model_id Optional character vector of model IDs from
#'   [si_model_registry()]. Selects a specific model for each observation;
#'   `NA` uses automatic selection. The model must still match your inputs.
#'   Some models need an explicit ID and issue a warning when suitability
#'   cannot be fully checked. Keep the same model ID and regional settings
#'   when predicting heights at different ages.
#' @param pick_best Logical scalar. `TRUE` returns one row per observation;
#'   `FALSE` returns a list of candidate tibbles, one per observation, including
#'   exclusions and failures rather than only successful predictions.
#' @param keep_model_id Logical scalar. Include the top-level `model_id`
#'   column (default `TRUE`). Model IDs remain available in `diagnostics`
#'   and the `definition_id` column still identifies the model and its settings
#'   when this is `FALSE`.
#'
#' @details
#' Automatic selection first checks which models match the supplied species,
#' jurisdiction, stand type, age type and requested SI and height definitions.
#' It then chooses the matching model with the highest registry `rank`.
#' Ties are resolved alphabetically by model ID. Rank is a selection preference,
#' not a guarantee of better predictions. Use [find_si_models()] to explore
#' models and [get_si_model_info()] to check an individual model's requirements.
#'
#' When estimating SI without a requested reference age, the result uses the
#' selected model's own definition. If a model returns an unusable SI prediction,
#' only models with the same SI quantity, reference age, age type and height
#' definitions can be tried instead. Supplied SI is never passed to another
#' model after an unsuccessful prediction. Site-index values from different
#' models should not be treated as interchangeable.
#'
#' Warnings from individual models are shown and saved in `diagnostics`.
#' Missing inputs or no usable prediction produce missing values and a warning;
#' inspect `status`, `reason` and `diagnostics` for an explanation.
#' Invalid inputs and errors reported by individual model functions stop the
#' calculation rather than silently selecting a different model.
#'
#' Growth-intercept models (using annual height growth), ecological-site curves
#' and climate-dependent models are not selected by `si()`. Call their individual
#' functions instead.
#'
#' @return With `pick_best = TRUE`, a tibble with `si` or `height`, optional
#'   `model_id`, and information describing the model and the result:
#'   `definition_id`, `input_age_basis`,
#'   `reference_age`, `reference_age_basis`, `productivity_quantity`, `si_datum`,
#'   `height_datum`, and `reference_age_role`. `status` and `reason` report
#'   success, success with warnings, missing inputs, failed predictions or no
#'   suitable model. For each observation, `diagnostics` lists the models
#'   considered, why any were excluded, predictions attempted, model inputs
#'   (`engine_args`), warnings and limitations.
#'
#'   With `pick_best = FALSE`, returns a list of these model-comparison tibbles,
#'   one per observation, ordered by rank then model ID. An observation with no
#'   models covering its species and jurisdiction has an empty tibble.
#'   Single input values are repeated to match longer vectors; incompatible
#'   vector lengths cause an error.
#'
#' @seealso [find_si_models()], [get_si_model_info()], [si_model_registry()]
#' @examples
#' si(age = c(20, 30), height = c(10, 15),
#'    species = "PICE.GLA", jurisdiction = "QC",
#'    input_age_basis = "total_age", stand_origin = "plantation")
#'
#' # Use the model that defines SI as height at 25 years since planting.
#' si(age = c(20, 40), si = 18, species = "PICE.GLA", jurisdiction = "QC",
#'    input_age_basis = "total_age", stand_origin = "plantation",
#'    model_id = "pregent2010")
#' @export
si <- function(
  age, height = NULL, si = NULL, species, jurisdiction, ...,
  input_age_basis, reference_age = NULL, reference_age_basis = NULL,
  productivity_quantity = NULL, si_datum = NULL, height_datum = "total_height",
  stand_origin = NA_character_, bec_zone = NA_character_, model_id = NULL,
  pick_best = TRUE, keep_model_id = TRUE
) {
  if (!xor(is.null(height), is.null(si))) {
    cli::cli_abort("Supply exactly one of {.arg height} or {.arg si}.")
  }
  for (flag in c("pick_best", "keep_model_id")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort("{.arg {flag}} must be TRUE or FALSE.")
    }
  }
  x_name <- if (is.null(height)) "si" else "height"
  output <- if (x_name == "si") "height" else "si"
  assert_numeric_vec(age, "age", gt = 0, allow_na = TRUE)
  x <- if (x_name == "si") si else height
  assert_numeric_vec(x, x_name, gt = 0, allow_na = TRUE)
  if (!is.null(reference_age)) {
    assert_numeric_vec(reference_age, "reference_age", gt = 0, allow_na = TRUE)
  }
  clocks <- c("total_age", "breast_height_age", "corrected_age_above_1m")
  datums <- c("total_height", "above_breast_height")
  assert_choice(input_age_basis, "input_age_basis", clocks, multiple = TRUE, allow_na = TRUE)
  choices <- list(
    reference_age_basis = clocks, si_datum = datums, height_datum = datums,
    productivity_quantity = c("site_index", "height_index"),
    stand_origin = c("natural", "plantation")
  )
  for (arg in names(choices)) {
    value <- get(arg)
    if (!is.null(value)) {
      assert_choice(value, arg, choices[[arg]], multiple = TRUE, allow_na = TRUE)
    }
  }
  for (arg in c("species", "jurisdiction", "bec_zone", "model_id")) {
    value <- get(arg)
    if (!is.null(value) && !is.character(value) && !is.factor(value)) {
      cli::cli_abort("{.arg {arg}} must be a character vector.")
    }
  }
  dots <- list(...)
  .hd_check_dots(dots, valid = "natural_subregion")
  if (length(dots)) {
    if (!is.character(dots$natural_subregion)) {
      cli::cli_abort("{.arg natural_subregion} must be a character vector.")
    }
  }
  inputs <- c(list(
    age = age, x = x, species = as.character(species),
    jurisdiction = as.character(jurisdiction), input_age_basis = input_age_basis,
    reference_age = reference_age %||% NA_real_,
    reference_age_basis = reference_age_basis %||% NA_character_,
    productivity_quantity = productivity_quantity %||% NA_character_,
    si_datum = si_datum %||% NA_character_, height_datum = height_datum,
    stand_origin = stand_origin, bec_zone = as.character(bec_zone),
    model_id = as.character(model_id %||% NA_character_)
  ), dots)
  if (any(lengths(inputs) == 0L)) cli::cli_abort("Inputs must have length > 0.")
  v <- do.call(assert_len_compat, inputs)
  v$species <- standardize_species_code(v$species, keep_all = FALSE)
  known <- !is.na(v$jurisdiction)
  v$jurisdiction[known] <- standardize_jurisdiction_code(v$jurisdiction[known])
  v$bec_zone <- toupper(trimws(v$bec_zone))
  bc_region <- .hd_tree_bc_region(v$bec_zone, NA_character_, v$jurisdiction)
  reg <- si_model_registry_species()
  ids <- unique(v$model_id[!is.na(v$model_id)])
  unknown <- setdiff(ids, reg$model_id)
  if (length(unknown)) cli::cli_abort("Unknown {.arg model_id}: {.val {unknown}}.")
  for (id in ids) {
    if (!.si_conventional(reg[reg$model_id == id, ])) {
      cli::cli_abort("Model {.val {id}} belongs to a specialized family outside {.fn si}.")
    }
  }
  if (x_name == "si") {
    unpinned <- is.na(v$model_id)
    undefined <- unpinned & (is.na(v$reference_age) | is.na(v$reference_age_basis) |
                              is.na(v$si_datum) | is.na(v$productivity_quantity))
    if (any(undefined)) {
      cli::cli_abort(c(
        "Supplied {.arg si} requires its definition.",
        "i" = "Supply an originating {.arg model_id}, or reference_age, reference_age_basis, si_datum and productivity_quantity."
      ))
    }
    if (any(unpinned)) {
      cli::cli_warn(
        "Supplied SI has no originating model ID for some observations; matching definitions do not prove interchangeability. No model fallback will be used.",
        class = "cfa_si_unpinned_si"
      )
    }
  }
  results <- lapply(seq_along(v$age), function(i) {
    observation <- lapply(v, `[[`, i)
    .si_dispatch_one(reg, observation, x_name, output, bc_region[[i]], pick_best, i)
  })
  review <- unlist(lapply(results, `[[`, "applicability"), use.names = FALSE)
  if (length(review)) {
    cli::cli_warn(c(
      "Explicit SI models have unverified applicability.",
      "i" = paste(review, collapse = "\n")
    ), class = "cfa_si_applicability")
  }
  missing <- which(vapply(results, function(x) !x$success, logical(1)))
  if (length(missing)) {
    cli::cli_warn(c(
      "{.fn si}: {length(missing)} observation{?s} without a prediction.",
      "i" = "Rows: {.val {missing}}.",
      "i" = "Inspect status, reason and candidate diagnostics."
    ), class = "cfa_si_no_prediction")
  }
  if (!pick_best) {
    return(lapply(results, function(x) {
      out <- x$candidates
      if (!keep_model_id) out$model_id <- NULL
      out
    }))
  }
  out <- dplyr::bind_rows(lapply(results, `[[`, "best"))
  if (!keep_model_id) out$model_id <- NULL
  out
}

.si_conventional <- function(row) {
  operations <- unique(row$operation_inputs[[1]]$operation)
  quantity <- unique(row$scientific_definitions[[1]]$productivity_quantity)
  all(c("infer_si_from_height", "predict_height_from_si") %in% operations) &&
    length(quantity) > 0L && all(quantity %in% c("site_index", "height_index"))
}

.si_dispatch_prototype <- function(output) {
  out <- tibble::tibble(
    model_id = character(), rank = numeric(), status = character(), reason = character(),
    definition_id = character(), input_age_basis = character(), reference_age = numeric(),
    reference_age_basis = character(), productivity_quantity = character(),
    si_datum = character(), height_datum = character(), reference_age_role = character(),
    engine_args = list(), warnings = list(), limitations = list()
  )
  out[[output]] <- numeric()
  out
}

.si_geographic_screen <- function(row, obs, bc_region) {
  known_mismatch <- character()
  unknown <- character()
  if (obs$jurisdiction != "BC") return(list(mismatch = known_mismatch, unknown = unknown))
  geography <- row$applicability[[1]]$geographic_definition
  scope <- row$subregion_scope[[1]]
  required_region <- if ("BC_COAST" %in% scope || "Coastal British Columbia" %in% geography) {
    "coastal"
  } else if ("BC_INTERIOR" %in% scope || "Interior British Columbia" %in% geography) {
    "interior"
  } else NA_character_
  required_zone <- if ("British Columbia SBS zone" %in% geography) {
    "SBS"
  } else if ("British Columbia ESSF zone" %in% geography) "ESSF" else NA_character_
  if (!is.na(required_region)) {
    if (is.na(bc_region)) unknown <- "BC coast/interior applicability requires bec_zone."
    else if (bc_region != required_region) known_mismatch <- "BC coast/interior applicability mismatch."
  }
  if (!is.na(required_zone)) {
    if (is.na(obs$bec_zone)) unknown <- paste0("Geographic applicability requires BEC zone ", required_zone, ".")
    else if (obs$bec_zone != required_zone) {
      known_mismatch <- paste0("Geographic applicability requires BEC zone ", required_zone, ".")
    }
  }
  list(mismatch = known_mismatch, unknown = unknown)
}

.si_dispatch_candidate <- function(row, obs, x_name, output, bc_region) {
  defs <- .si_native_definitions(row, obs$species)
  candidate <- tibble::add_row(
    .si_dispatch_prototype(output),
    model_id = row$model_id, rank = as.numeric(row$rank), status = "not_tried",
    engine_args = list(list()), warnings = list(character()),
    limitations = list(.si_discovery_limitations(row))
  )
  provenance <- c(
    "definition_id", "input_age_basis", "reference_age", "reference_age_basis",
    "productivity_quantity", "si_datum", "height_datum", "reference_age_role"
  )
  if (nrow(defs) == 1L) candidate[provenance] <- defs[provenance]
  reasons <- character()
  review <- character()
  if (!.si_conventional(row)) reasons <- c(reasons, "Specialized family outside conventional dispatch.")
  if (!obs$species %in% row$species[[1]]) reasons <- c(reasons, "Species is outside registered coverage.")
  if (!obs$jurisdiction %in% normalize_province_scope(row$province_scope[[1]])) {
    reasons <- c(reasons, "Jurisdiction is outside registered coverage.")
  }
  if (nrow(defs) != 1L) reasons <- c(reasons, "No unambiguous species-specific native definition.")
  if (nrow(defs) == 1L) {
    fields <- c("reference_age", "reference_age_basis", "productivity_quantity", "si_datum")
    requested <- lapply(fields, function(field) {
      if (is.na(obs[[field]])) defs[[field]][[1]] else obs[[field]]
    })
    names(requested) <- fields
    requested$input_age_basis <- obs$input_age_basis
    requested$height_datum <- obs$height_datum
    comparison <- .si_compare_science(row, defs, requested)
    reasons <- c(reasons, comparison$reason[comparison$status != "compatible"])
  }
  explicit <- !is.na(obs$model_id)
  if (is.na(obs$stand_origin)) {
    origin_reason <- "stand_origin: observation origin is not supplied."
    if (explicit) review <- c(review, origin_reason) else reasons <- c(reasons, origin_reason)
  } else {
    origin <- .si_compare_science(row, defs, list(stand_origin = obs$stand_origin))
    if (origin$status == "unknown" && explicit) review <- c(review, origin$reason)
    else reasons <- c(reasons, origin$reason[origin$status != "compatible"])
  }
  geo <- .si_geographic_screen(row, obs, bc_region)
  reasons <- c(reasons, geo$mismatch)
  if (explicit) review <- c(review, geo$unknown) else reasons <- c(reasons, geo$unknown)
  args <- row$variant_args[[1]]
  inputs <- row$operation_inputs[[1]]
  operation <- if (x_name == "height") "infer_si_from_height" else "predict_height_from_si"
  recommendations <- .si_recommendation_issues(row, operation, obs$species, obs$age)
  if (explicit) review <- c(review, recommendations) else reasons <- c(reasons, recommendations)
  inputs <- inputs[inputs$operation == operation, ]
  args$age <- obs$age
  args[[x_name]] <- obs$x
  if (row$requires_species) args$species <- obs$species
  for (arg in intersect(c("bec_zone", "natural_subregion"), inputs$arg[inputs$role == "optional"])) {
    if (!is.null(obs[[arg]]) && !is.na(obs[[arg]])) args[[arg]] <- obs[[arg]]
  }
  rules <- row$input_constraints[[1]]
  rules <- rules[rules$operation == operation & rules$kind == "discrete_support", ]
  for (j in seq_len(nrow(rules))) {
    arg <- rules$arg[[j]]
    if (!is.null(args[[arg]]) && !is.data.frame(rules$values[[j]]) &&
        !args[[arg]] %in% rules$values[[j]]) {
      reasons <- c(reasons, paste0(arg, ": unsupported discrete input."))
    }
  }
  candidate$engine_args <- list(args)
  if (length(reasons)) {
    candidate$status <- "excluded"
    candidate$reason <- paste(unique(reasons), collapse = " ")
  }
  list(candidate = candidate, review = review)
}

.si_call_engine <- function(row, args, output, observation) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(
      do.call(get(row$engine[[1]], envir = asNamespace("CanadaForestAllometry")), args),
      error = function(e) {
        cli::cli_abort(
          "SI engine {.val {row$model_id}} failed for observation {observation}: {conditionMessage(e)}",
          parent = e, class = "cfa_si_engine_error"
        )
      }
    ),
    warning = function(w) warnings <<- c(warnings, conditionMessage(w))
  )
  if (!is.data.frame(value) || nrow(value) != 1L || !is.numeric(value[[output]]) ||
      length(value[[output]]) != 1L) {
    cli::cli_abort(
      "SI engine {.val {row$model_id}} returned an invalid output shape for observation {observation}.",
      class = "cfa_si_engine_contract"
    )
  }
  list(value = value[[output]], warnings = unique(warnings))
}

.si_dispatch_one <- function(reg, obs, x_name, output, bc_region, pick_best, observation) {
  explicit <- !is.na(obs$model_id)
  scope <- if (explicit) {
    reg[reg$model_id == obs$model_id, ]
  } else {
    reg[vapply(reg$species, function(x) obs$species %in% x, logical(1)) &
          vapply(reg$province_scope, function(x) {
            obs$jurisdiction %in% normalize_province_scope(x)
          }, logical(1)), ]
  }
  scope <- scope[order(-scope$rank, scope$model_id, method = "radix"), ]
  missing <- anyNA(unlist(obs[c("age", "x", "species", "jurisdiction", "input_age_basis", "height_datum")]))
  screened <- if (missing) list() else lapply(seq_len(nrow(scope)), function(i) {
    .si_dispatch_candidate(scope[i, ], obs, x_name, output, bc_region)
  })
  candidates <- dplyr::bind_rows(
    .si_dispatch_prototype(output),
    dplyr::bind_rows(lapply(screened, `[[`, "candidate"))
  )
  eligible <- which(candidates$status == "not_tried")
  chosen <- NA_integer_
  applicability <- character()
  anchor <- if (length(eligible)) eligible[[1]] else NA_integer_
  definition <- c(
    "input_age_basis", "reference_age", "reference_age_basis", "productivity_quantity",
    "si_datum", "height_datum", "reference_age_role"
  )
  for (i in eligible) {
    if (pick_best && i != anchor &&
        (x_name == "si" || !identical(candidates[i, definition], candidates[anchor, definition]))) {
      candidates$status[[i]] <- "excluded"
      candidates$reason[[i]] <- "Unsafe fallback: a different native definition or supplied SI cannot be reinterpreted."
      next
    }
    if (length(screened[[i]]$review)) {
      text <- paste(screened[[i]]$review, collapse = " ")
      applicability <- c(
        applicability,
        paste0("Observation ", observation, ", ", scope$model_id[[i]], ": ", text)
      )
      candidates$warnings[[i]] <- text
    }
    result <- .si_call_engine(scope[i, ], candidates$engine_args[[i]], output, observation)
    candidates$warnings[[i]] <- c(candidates$warnings[[i]], result$warnings)
    if (!is.finite(result$value) || result$value <= 0) {
      candidates$status[[i]] <- "failed"
      candidates$reason[[i]] <- "Engine returned a non-finite or non-positive prediction."
      next
    }
    candidates[[output]][[i]] <- result$value
    candidates$status[[i]] <- if (length(candidates$warnings[[i]])) "success_with_warnings" else "success"
    if (is.na(chosen)) chosen <- i
    if (pick_best) {
      if (i != anchor) {
        cli::cli_warn(
          "SI model fallback used {.val {scope$model_id[[i]]}} for observation {observation}; inspect diagnostics.",
          class = "cfa_si_fallback"
        )
      }
      break
    }
  }
  success <- !is.na(chosen)
  if (success) {
    best <- candidates[chosen, ]
  } else {
    best <- tibble::tibble(
      model_id = NA_character_, status = if (missing) "missing_input" else if (
        any(candidates$status == "failed")
      ) "failed" else "no_model",
      reason = if (missing) "Missing required observation input." else "No candidate was eligible and successful.",
      definition_id = NA_character_, input_age_basis = obs$input_age_basis,
      reference_age = NA_real_, reference_age_basis = NA_character_,
      productivity_quantity = NA_character_, si_datum = NA_character_,
      height_datum = obs$height_datum, reference_age_role = NA_character_
    )
    best[[output]] <- NA_real_
  }
  best <- best[c(
    output, "model_id", "definition_id", "input_age_basis", "reference_age",
    "reference_age_basis", "productivity_quantity", "si_datum", "height_datum",
    "reference_age_role", "status", "reason"
  )]
  best$diagnostics <- list(candidates)
  list(best = best, candidates = candidates, success = success, applicability = applicability)
}
