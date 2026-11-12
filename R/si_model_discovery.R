#' Get scientific and input information about a site-index model
#'
#' Returns the registry fields for one registered variant, including species
#' coverage, operation-aware predictors, constraints, source evidence and
#' limitations. No model is executed.
#'
#' @param model_id Single identifier from [si_model_registry()].
#'
#' @return A named list with registry list-columns unwrapped, plus:
#'   * `species`: exact registered NFI coverage.
#'   * `operations`: supported operation names.
#'   * `effective_args`: native defaults with variant-defining arguments applied,
#'     excluding plot-only overrides.
#'   * `native_scientific_definitions`: definitions at those effective arguments,
#'     retaining species-specific rows. `scientific_definitions` also retains
#'     alternative documented conditions.
#'   * `unresolved_evidence`: claims with non-verified evidence status.
#'   * `limitations`: scientific caveats and the limits of discovery.
#'
#'   `operation_inputs` describes predictor roles, units, definitions and
#'   evidence separately for each operation. `input_constraints` retains exact
#'   ecological keys, conditional requirements, discrete GI support and climate
#'   aggregation semantics. Empty applicability metadata means unknown, not
#'   unrestricted. Native SI scales are not automatically interchangeable.
#'
#' @seealso [si_model_registry()], [find_si_models()]
#' @examples
#' info <- get_si_model_info("pregent2010")
#' info$native_scientific_definitions
#' info$native_defaults$base_age # 25, unlike the plot-only override of 50
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

#' Discover site-index models and scientific near-misses
#'
#' Browses all SI model families without executing, ranking or selecting a model.
#' Species and jurisdiction restrict registered coverage. Scientific requests
#' annotate candidates rather than hiding incompatible or unresolved near-misses.
#'
#' @param jurisdiction Optional character vector of province/territory codes.
#'   Coverage of any requested jurisdiction is sufficient. `NULL` does not
#'   restrict coverage. Registry coverage is not proof of source calibration
#'   or geographic transfer suitability.
#' @param species Optional character vector of NFI codes, including compact
#'   codes such as `"PICEMAR"`. Matching is exact after normalization, without
#'   genus fallback. Each covered requested species has a separate result row,
#'   so species-specific definitions cannot be combined accidentally.
#' @param operation Optional single operation: `"infer_si_from_height"`,
#'   `"predict_height_from_si"`, `"infer_si_from_gi"` or
#'   `"evaluate_ecological_curve"`. `NULL` lists each supported operation.
#'   With an explicit operation, unsupported models remain as exclusions.
#' @param available Character vector of case-sensitive engine argument names
#'   you could supply, defaulting to none. A requested `species` also supplies
#'   that input name. Extra column names are allowed and do not mean those
#'   arguments will actually be passed to an engine.
#' @param input_age_basis,reference_age_basis Optional requested age clocks:
#'   `"total_age"`, `"breast_height_age"`, `"plantation_age"` or
#'   `"corrected_age_above_1m"`. Input and SI-reference clocks are independent.
#' @param reference_age Optional single positive finite SI reference age in
#'   years, compared with the native variant scale, not a plotting override.
#'   This does not set an engine's `base_age`.
#' @param productivity_quantity Optional requested quantity: `"site_index"`,
#'   `"height_index"`, `"inherent_height_index"` or `"ecological_iqs"`.
#' @param si_datum,height_datum Optional requested vertical datum:
#'   `"total_height"` or `"above_breast_height"`. These do not describe the
#'   height sampling population or authorize a height offset.
#' @param stand_origin Optional requested origin: `"natural"` or `"plantation"`.
#'   Models without an explicit plantation or mixed-origin classification use
#'   the approved assumed-natural default. Evidence distinguishes assumptions
#'   from source-verified classifications; observation origin is not inferred.
#'
#' @details
#' All comparisons use native defaults plus variant-defining selectors.
#' Alternative `total_height` settings remain visible in
#' `scientific_definitions`, but discovery never changes a selector to achieve a
#' match. Likewise Pregent native SI25 is not replaced by the SI50 plot setting.
#'
#' Scientific compatibility refers only to the explicitly requested fields.
#' Unresolved or contradictory source claims cannot establish either a match
#' or a known mismatch for the affected field. Multiple possible definitions
#' without a species selection can also leave a comparison unknown.
#' A compatible comparison does not establish equivalent SI across models,
#' population/composition/sampling suitability, or publication verification.
#' Free-text applicability and evidence must still be reviewed.
#' A known species-level published non-recommendation is retained as a
#' discovery exclusion even when requested clock/datum labels match.
#' Age-dependent recommendations remain visible in constraints/limitations;
#' name-only discovery cannot evaluate their numeric age conditions.
#'
#' Input availability is a name-level screen, not validation of observations.
#' It does not check numeric values, missing rows, discrete ages, joint
#' ecological keys, selector values, calibration ranges, or climate histories.
#' Conditional requirements whose selector values are not known remain visible;
#' no ecological subregion or coefficient mode is selected automatically.
#' Available columns forbidden in an operation are reported as arguments not to
#' pass, not as evidence that the data itself is incompatible.
#'
#' @return A tibble retaining registry/species metadata, in registry order
#'   (legacy `rank` is not used), with one row per model, operation and covered
#'   requested species (`query_species` is `NA` if species is unspecified).
#'   Additional columns are:
#'   * `operation`, `operation_supported`, `productivity_quantity`.
#'   * `native_scientific_definitions`: species/condition-resolved native
#'     definitions; original definitions and evidence remain available.
#'   * `missing_predictors`: unconditional required names not available.
#'   * `conditional_requirements`: potentially applicable conditional rules,
#'     restricted by the requested species when known.
#'   * `conditional_missing_predictors`: names missing only under those rules.
#'   * `forbidden_predictors`: available names that must not be passed in this
#'     operation. Their presence does not make `inputs_available` false.
#'   * `inputs_available`: `FALSE` for unsupported operations or missing
#'     unconditional names; `NA` for missing potentially required conditional
#'     names; otherwise `TRUE`. Even `TRUE` is not row-level eligibility.
#'   * `compatibility`: per-field comparison table with requested and actual
#'     values, status and reason.
#'   * `scientific_compatibility`: `"incompatible"` if any requested comparison
#'     mismatches; otherwise `"unknown"` if any is unresolved; otherwise
#'     `"compatible"`, or `"not_requested"` when no scientific fields are set.
#'   * `incompatibility_reasons`, `missing_information`: explanatory lists.
#'   * `discovery_status`: `"incompatible"`, `"missing_inputs"`, `"needs_review"`
#'     or `"names_complete"`, in that precedence order. None means usable.
#'   * `eligibility`: always `"not_evaluated"`.
#'   * `limitations`: scientific caveats, including unencoded applicability.
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
  clocks <- c("total_age", "breast_height_age", "plantation_age", "corrected_age_above_1m")
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
