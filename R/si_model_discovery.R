#' Look up a site-index model and its required inputs
#'
#' Returns information about one model version, including its species,
#' age and height definitions, required inputs and usage limits.
#' Does not calculate predictions.
#'
#' @param model_id Single identifier from [si_model_registry()].
#'
#' @return A named list containing the model's registry information, including:
#'   * `species`: supported NFI species codes.
#'   * `operations`: calculations the model supports.
#'   * `effective_args`: default argument values with settings for the selected
#'     model version applied, excluding settings used only for plots.
#'   * `native_scientific_definitions`: age, height and SI definitions under
#'     those settings, with separate rows where species differ.
#'     `scientific_definitions` also includes other documented settings.
#'   * `unresolved_evidence`: information that has not been fully confirmed.
#'   * `limitations`: usage limits and points needing further review.
#'
#'   `operation_inputs` lists the inputs, units and definitions for each
#'   calculation. `input_constraints` records supported ages, ecological-code
#'   combinations, additional requirements and climate-input requirements.
#'   Missing information about where a model can be used does not mean it is
#'   suitable everywhere. SI values from different models should not be treated
#'   as interchangeable.
#'
#' @seealso [si_model_registry()], [find_si_models()]
#' @examples
#' info <- get_si_model_info("pregent2010")
#' info$native_scientific_definitions
#' info$native_defaults$base_age # Default reference age: 25, not the 50 used in plots
#' get_si_model_info("huang2026_climate")$input_constraints
#' @export
get_si_model_info <- function(model_id) {
  assert_chr_scalar(model_id, "model_id")
  reg <- si_model_registry_species()
  if (!model_id %in% reg$model_id) {
    cli::cli_abort(c(
      "Unknown site-index model {.val {model_id}}.",
      "i" = "Available model_id values: {paste(reg$model_id, collapse = ', ')}."
    ))
  }
  row <- reg[reg$model_id == model_id, ]
  info <- lapply(as.list(row), function(col) if (is.list(col)) col[[1]] else col)
  info$n_species <- NULL
  info$species_text <- NULL
  info$operations <- unique(info$operation_inputs$operation)
  info$effective_args <- .si_effective_args(row)
  info$native_scientific_definitions <- .si_native_definitions(row)
  info$unresolved_evidence <- .si_unresolved_evidence(row)
  info$limitations <- .si_discovery_limitations(row)
  info
}

#' Find site-index models and check their input requirements
#'
#' Lists models covering the requested species and jurisdictions without
#' calculating predictions or selecting a model. Models that do not match the
#' requested age, height or stand definitions remain in the results with an
#' explanation.
#'
#' @param jurisdiction Optional character vector of province/territory codes.
#'   Coverage of any requested jurisdiction is sufficient. `NULL` does not
#'   restrict coverage. Listed coverage alone does not establish that a model
#'   is suitable for a particular stand.
#' @param species Optional character vector of NFI codes, including compact
#'   codes such as `"PICEMAR"`. Codes are standardized, then matched exactly;
#'   a genus-level model is not substituted automatically.
#'   Results have separate rows for each covered requested species.
#' @param operation Optional single operation: `"infer_si_from_height"`,
#'   `"predict_height_from_si"`, `"infer_si_from_gi"` or
#'   `"evaluate_ecological_curve"`. `NULL` lists each supported operation.
#'   With an explicit operation, unsupported models remain as exclusions.
#' @param available Character vector of case-sensitive model argument names
#'   you could supply, defaulting to none. A requested `species` also supplies
#'   that input name. Extra column names are allowed and do not mean those
#'   arguments will actually be passed to a model.
#' @param input_age_basis,reference_age_basis Optional requested age types:
#'   `"total_age"`, `"breast_height_age"` or `"corrected_age_above_1m"`.
#'   `"total_age"` includes years since planting for models defined that way.
#'   Input and SI-reference age types are independent.
#' @param reference_age Optional single positive finite SI reference age in
#'   years, compared with the selected model version's reference age.
#'   This does not change `base_age` or use the reference age chosen for plots.
#' @param productivity_quantity Optional requested quantity: `"site_index"`,
#'   `"height_index"`, `"inherent_height_index"` or `"ecological_iqs"`.
#' @param si_datum,height_datum Optional height definitions for SI and
#'   measured or predicted height: `"total_height"` (from the ground) or
#'   `"above_breast_height"` (above 1.3 m). Heights are not converted.
#' @param stand_origin Optional stand type: `"natural"` or `"plantation"`.
#'   Models not identified as plantation or mixed-origin models are recorded as
#'   natural-stand models by assumption. This does not determine the stand
#'   type of a user's observations.
#'
#' @details
#' Comparisons use the default arguments for the selected model version.
#' The function does not change options such as `total_height` to make a model
#' match. Prégent models are checked using their default reference age of 25,
#' not the age of 50 used in article plots.
#'
#' Only the requested definitions are compared. Missing or uncertain model
#' information can leave a comparison unknown. A match does not mean SI values
#' can be exchanged between models or that the model is suitable for every stand.
#' Published advice against using a model for a species is reported even when
#' its age and height definitions match. Age-related limits are listed but not
#' checked against observations.
#'
#' `available` checks input names only, not their values, missing observations,
#' supported ages, ecological-code combinations or climate histories.
#' Requirements that depend on other settings remain listed for review.
#' Inputs not accepted for a calculation are reported as arguments not to pass;
#' having those columns in your data does not by itself rule out the model.
#'
#' @return A tibble containing registry and species information, in registry
#'   order (not ranked), with one row per model, calculation and covered
#'   requested species (`query_species` is `NA` if species is unspecified).
#'   Additional columns are:
#'   * `operation`, `operation_supported`, `productivity_quantity`.
#'   * `native_scientific_definitions`: age, height and SI definitions for the
#'     selected species and model version.
#'   * `missing_predictors`: required input names absent from `available`.
#'   * `conditional_requirements`: additional rules that may apply, depending
#'     on other settings and the requested species.
#'   * `conditional_missing_predictors`: input names missing under those rules.
#'   * `forbidden_predictors`: available names that must not be passed in this
#'     operation. Their presence does not make `inputs_available` false.
#'   * `inputs_available`: `FALSE` for unsupported operations or missing
#'     required names; `NA` when additional inputs may be needed under a rule;
#'     otherwise `TRUE`. Even `TRUE` does not mean observations have been checked.
#'   * `compatibility`: per-field comparison table with requested and actual
#'     values, status and reason.
#'   * `scientific_compatibility`: `"incompatible"` if any requested comparison
#'     mismatches; otherwise `"unknown"` if any is unresolved; otherwise
#'     `"compatible"`, or `"not_requested"` when no scientific fields are set.
#'   * `incompatibility_reasons`, `missing_information`: explanatory lists.
#'   * `discovery_status`: `"incompatible"`, `"missing_inputs"`, `"needs_review"`
#'     or `"names_complete"`, in that order of priority. None guarantees that
#'     the model can be used for your observations.
#'   * `eligibility`: always `"not_evaluated"`.
#'   * `limitations`: usage limits and information that still needs review.
#'
#' @seealso [si_model_registry()], [get_si_model_info()]
#' @examples
#' candidates <- find_si_models(
#'   jurisdiction = "ON", species = "PICE.MAR",
#'   operation = "infer_si_from_height", available = c("age", "height"),
#'   input_age_basis = "breast_height_age", stand_origin = "natural",
#'   si_datum = "total_height"
#' )
#' candidates[c("model_id", "discovery_status", "scientific_compatibility",
#'              "missing_predictors", "incompatibility_reasons")]
#' find_si_models(species = "THUJ.PLI", jurisdiction = "BC",
#'                operation = "infer_si_from_gi", available = c("age", "gi"))
#' @export
find_si_models <- function(
  jurisdiction = NULL, species = NULL, operation = NULL, available = character(),
  input_age_basis = NULL, reference_age = NULL, reference_age_basis = NULL,
  productivity_quantity = NULL, si_datum = NULL, height_datum = NULL,
  stand_origin = NULL
) {
  if (!is.null(jurisdiction)) {
    .si_discovery_names(jurisdiction, "jurisdiction", allow_empty = FALSE)
    jurisdiction <- standardize_jurisdiction_code(jurisdiction)
  }
  if (!is.null(species)) {
    .si_discovery_names(species, "species", allow_empty = FALSE)
    species <- unique(standardize_species_code(species, keep_all = FALSE))
  }
  .si_discovery_names(available, "available")
  clocks <- c("total_age", "breast_height_age", "corrected_age_above_1m")
  datums <- c("total_height", "above_breast_height")
  .si_discovery_choice(operation, "operation", c(
    "infer_si_from_height", "predict_height_from_si",
    "infer_si_from_gi", "evaluate_ecological_curve"
  ))
  .si_discovery_choice(input_age_basis, "input_age_basis", clocks)
  .si_discovery_choice(reference_age_basis, "reference_age_basis", clocks)
  .si_discovery_choice(si_datum, "si_datum", datums)
  .si_discovery_choice(height_datum, "height_datum", datums)
  .si_discovery_choice(productivity_quantity, "productivity_quantity", c(
    "site_index", "height_index", "inherent_height_index", "ecological_iqs"
  ))
  .si_discovery_choice(stand_origin, "stand_origin", c("natural", "plantation"))
  if (!is.null(reference_age) &&
      (!is.numeric(reference_age) || length(reference_age) != 1L ||
       is.na(reference_age) || !is.finite(reference_age) || reference_age <= 0)) {
    cli::cli_abort("{.arg reference_age} must be a single positive finite number.")
  }
  requested <- list(
    input_age_basis = input_age_basis, reference_age = reference_age,
    reference_age_basis = reference_age_basis,
    productivity_quantity = productivity_quantity, si_datum = si_datum,
    height_datum = height_datum, stand_origin = stand_origin
  )
  requested <- requested[!vapply(requested, is.null, logical(1))]
  reg <- si_model_registry_species()
  if (!is.null(jurisdiction)) {
    reg <- reg[vapply(reg$province_scope, function(x) {
      any(jurisdiction %in% normalize_province_scope(x))
    }, logical(1)), ]
  }
  index <- lapply(seq_len(nrow(reg)), function(i) {
    matches <- if (is.null(species)) NA_character_ else species[species %in% reg$species[[i]]]
    operations <- if (is.null(operation)) unique(reg$operation_inputs[[i]]$operation) else operation
    tibble::tibble(
      row = rep(i, length(matches) * length(operations)),
      query_species = rep(matches, each = length(operations)),
      operation = rep(operations, times = length(matches))
    )
  })
  index <- dplyr::bind_rows(
    tibble::tibble(row = integer(), query_species = character(), operation = character()),
    dplyr::bind_rows(index)
  )
  out <- reg[index$row, ]
  out$query_species <- index$query_species
  out$operation <- index$operation
  summaries <- lapply(seq_len(nrow(out)), function(i) {
    .si_discovery_summary(out[i, ], available, requested)
  })
  template <- .si_discovery_summary(reg[0, ], available, requested, empty = TRUE)
  dplyr::bind_cols(out, dplyr::bind_rows(template, dplyr::bind_rows(summaries)))
}

.si_discovery_names <- function(x, arg, allow_empty = TRUE) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) ||
      (!allow_empty && length(x) == 0L)) {
    cli::cli_abort("{.arg {arg}} must be a character vector of non-empty names without NA.")
  }
}

.si_discovery_choice <- function(x, arg, choices) {
  if (!is.null(x)) assert_choice(x, arg, choices)
}

.si_effective_args <- function(row) {
  args <- row$native_defaults[[1]]
  fixed <- row$variant_args[[1]]
  args[names(fixed)] <- fixed
  args
}

.si_native_definitions <- function(row, species = NA_character_) {
  defs <- row$scientific_definitions[[1]]
  args <- .si_effective_args(row)
  conditions <- c("native", paste0("total_height=", args$total_height))
  defs <- defs[defs$condition %in% conditions, ]
  if (!is.na(species)) {
    defs <- defs[is.na(defs$species) | defs$species == species, ]
  }
  defs
}

.si_unresolved_evidence <- function(row) {
  evidence <- row$metadata_evidence[[1]]
  evidence[evidence$status != "verified", ]
}

.si_discovery_limitations <- function(row) {
  unique(c(
    "Names alone do not validate observations, source populations, sampling or scientific eligibility.",
    "Native productivity scales are not automatically interchangeable across models.",
    if (nrow(row$applicability[[1]]) == 0L) "Stand applicability has not been encoded.",
    row$scientific_definitions[[1]]$limitations,
    row$input_constraints[[1]]$definition[
      row$input_constraints[[1]]$kind == "published_recommendation"
    ],
    .si_unresolved_evidence(row)$note
  ))
}

.si_recommendation_issues <- function(row, operation, species, age = NULL) {
  rules <- row$input_constraints[[1]]
  rules <- rules[rules$operation == operation & rules$kind == "published_recommendation", ]
  issues <- vapply(seq_len(nrow(rules)), function(i) {
    selector <- rules$selectors[[i]]$species
    if (is.na(species) || (!is.null(selector) && !identical(selector, species))) return(FALSE)
    values <- rules$values[[i]]
    identical(values$allowed, FALSE) ||
      (!is.null(age) && !is.null(values$lower) && age < values$lower)
  }, logical(1))
  rules$definition[issues]
}

.si_disputed_fields <- function(evidence) {
  fields <- list(
    input_age_basis = "input_age_basis",
    input_and_reference_age = c("input_age_basis", "reference_age", "reference_age_basis"),
    input_and_reference_age_basis = c("input_age_basis", "reference_age_basis"),
    species_age_basis_and_recommendation = c("input_age_basis", "reference_age_basis"),
    reference_age = c("reference_age", "reference_age_basis"),
    si_and_height_datum = c("si_datum", "height_datum"),
    conversion_provenance = c("reference_age", "reference_age_basis", "si_datum")
  )
  unique(unlist(fields[evidence$subject], use.names = FALSE))
}

.si_compare_science <- function(row, defs, requested) {
  disputed <- .si_disputed_fields(.si_unresolved_evidence(row))
  comparisons <- lapply(names(requested), function(field) {
    actual <- if (field == "stand_origin") row$applicability[[1]]$stand_origin else defs[[field]]
    actual <- unique(actual)
    unknown <- length(actual) == 0L || anyNA(actual) ||
      any(actual %in% c("unresolved", "species_dependent")) || field %in% disputed
    matches <- actual == requested[[field]]
    if (field == "stand_origin") {
      matches <- matches | actual == "natural_and_plantation"
    }
    status <- if (unknown) "unknown" else if (all(matches)) "compatible" else if (!any(matches)) {
      "incompatible"
    } else "unknown"
    reason <- switch(status,
      compatible = paste0(field, ": matches the native registered definition."),
      incompatible = paste0(
        field, ": requested ", requested[[field]], "; native definition is ",
        paste(actual, collapse = ", "), "."
      ),
      unknown = paste0(
        field, ": missing, unresolved or conditional/species-dependent information; ",
        "compatibility is not established."
      )
    )
    tibble::tibble(
      field = field, requested = as.character(requested[[field]]),
      actual = list(actual), status = status, reason = reason
    )
  })
  dplyr::bind_rows(
    tibble::tibble(field = character(), requested = character(), actual = list(),
                   status = character(), reason = character()),
    dplyr::bind_rows(comparisons)
  )
}

.si_discovery_summary <- function(row, available, requested, empty = FALSE) {
  if (empty) {
    return(tibble::tibble(
      operation_supported = logical(), productivity_quantity = character(),
      native_scientific_definitions = list(), missing_predictors = list(),
      conditional_requirements = list(), conditional_missing_predictors = list(),
      forbidden_predictors = list(), inputs_available = logical(), compatibility = list(),
      scientific_compatibility = character(), incompatibility_reasons = list(),
      missing_information = list(), discovery_status = character(),
      eligibility = character(), limitations = list()
    ))
  }
  operation <- row$operation[[1]]
  species <- row$query_species[[1]]
  inputs <- row$operation_inputs[[1]]
  supported <- operation %in% inputs$operation
  inputs <- inputs[inputs$operation == operation, ]
  rules <- row$input_constraints[[1]]
  conditional <- rules[
    rules$operation == operation & rules$kind == "conditional_key_requirement",
  ]
  if (!is.na(species)) {
    available <- union(available, "species")
    conditional <- conditional[vapply(conditional$selectors, function(x) {
      is.null(x$species) || identical(x$species, species)
    }, logical(1)), ]
  }
  required <- inputs$arg[inputs$role == "required" & is.na(inputs$condition_id)]
  missing <- setdiff(required, available)
  conditional_missing <- setdiff(unique(conditional$arg), available)
  forbidden <- intersect(inputs$arg[inputs$role == "forbidden"], available)
  inputs_available <- if (!supported || length(missing)) FALSE else if (length(conditional_missing)) {
    NA
  } else TRUE
  defs <- .si_native_definitions(row, species)
  comparison <- .si_compare_science(row, defs, requested)
  science <- if (any(comparison$status == "incompatible")) "incompatible" else if (
    any(comparison$status == "unknown")
  ) "unknown" else if (nrow(comparison)) "compatible" else "not_requested"
  incompatible <- c(
    if (!supported) paste0("operation: ", operation, " is not supported by this variant."),
    comparison$reason[comparison$status == "incompatible"],
    .si_recommendation_issues(row, operation, species)
  )
  missing_information <- c(
    comparison$reason[comparison$status == "unknown"],
    if (length(conditional_missing)) paste0(
      "Input values are needed to resolve conditional requirements for: ",
      paste(conditional_missing, collapse = ", "), "."
    )
  )
  status <- if (length(incompatible)) "incompatible" else if (length(missing)) {
    "missing_inputs"
  } else if (length(missing_information)) "needs_review" else "names_complete"
  tibble::tibble(
    operation_supported = supported,
    productivity_quantity = paste(unique(defs$productivity_quantity), collapse = ", "),
    native_scientific_definitions = list(defs), missing_predictors = list(missing),
    conditional_requirements = list(conditional),
    conditional_missing_predictors = list(conditional_missing),
    forbidden_predictors = list(forbidden), inputs_available = inputs_available,
    compatibility = list(comparison), scientific_compatibility = science,
    incompatibility_reasons = list(as.character(incompatible)),
    missing_information = list(as.character(missing_information)),
    discovery_status = status, eligibility = "not_evaluated",
    limitations = list(.si_discovery_limitations(row))
  )
}
