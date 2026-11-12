#' Dispatch conventional site-index and height-age models
#'
#' Estimates site index from age and height, or height from age and a defined
#' site index, using the conventional non-climate models in
#' [si_model_registry()]. Selection is observation-wise. No age-clock, datum
#' or cross-model SI conversion is performed.
#'
#' @param age Numeric vector of positive ages in years. Its clock must be
#'   declared with `input_age_basis`.
#' @param height,si Supply exactly one positive numeric vector, in metres.
#'   `height` estimates native-scale SI; `si` predicts height. For supplied SI,
#'   either `model_id` declares that model's native SI, or all four SI-definition
#'   fields (`reference_age`, `reference_age_basis`, `si_datum` and
#'   `productivity_quantity`) must be supplied. Matching labels alone do not
#'   establish cross-model SI interchangeability; supplying SI without an
#'   originating model ID emits a warning and disables model fallback.
#' @param species Character vector of NFI species codes. Compact codes are
#'   normalized; no genus fallback is used.
#' @param jurisdiction Character vector of province/territory codes.
#' @param ... Named additional engine predictors, currently `natural_subregion`.
#'   Missing optional values use native defaults. Definition-changing selectors
#'   such as `base_age`, `total_height` and `variant` are not accepted: use a
#'   registered model variant or call the engine directly.
#' @param input_age_basis Character vector: `"total_age"`,
#'   `"breast_height_age"`, `"plantation_age"` or `"corrected_age_above_1m"`.
#'   Required; never inferred from an SI reference clock or converted.
#' @param reference_age Optional positive numeric vector of requested SI
#'   reference ages. `NULL` preserves native reference ages when estimating SI.
#'   This filters models; it never changes `base_age` or uses plot overrides.
#' @param reference_age_basis Optional character vector of SI reference clocks,
#'   with the same choices as `input_age_basis`, independent of the input clock.
#' @param productivity_quantity Optional character vector: `"site_index"` or
#'   `"height_index"` (non-climate realized HI). `NULL` allows either, always
#'   labelled in the output.
#' @param si_datum Optional character vector: `"total_height"` or
#'   `"above_breast_height"`, describing the requested/supplied SI.
#' @param height_datum Character vector describing measured or predicted height:
#'   `"total_height"` (default) or `"above_breast_height"`. This is independent
#'   of SI datum and does not specify the sampling population.
#' @param stand_origin Character vector: `"natural"`, `"plantation"` or `NA`.
#'   Automatic selection requires a supplied matching observation origin.
#'   Models not explicitly classified as plantation or mixed-origin default
#'   to assumed natural origin, disclosed in evidence and limitations.
#'   Missing observation origin is not inferred.
#' @param bec_zone Optional character vector of BC BEC zone codes. Used for
#'   known coastal/interior and SBS/ESSF applicability restrictions, and passed
#'   to engines accepting it (explicitly selecting their supplied-zone mode).
#'   Missing values do not automatically select a zonal fit.
#' @param model_id Optional character vector of registered conventional model
#'   IDs, fixing each observation's model and native SI definition. `NA` uses
#'   automatic selection. Unknown applicability may be accepted for an explicit
#'   model with a warning and recorded limitations, but known mismatches and
#'   unresolved SI definitions are never overridden. Use the same ID and
#'   regional settings throughout a trajectory to avoid switching models.
#' @param pick_best Logical scalar. `TRUE` returns one row per observation;
#'   `FALSE` returns a list of candidate tibbles, one per observation, including
#'   exclusions and failures rather than only successful predictions.
#' @param keep_model_id Logical scalar. Include the top-level `model_id`
#'   column (default `TRUE`). IDs remain in best-mode diagnostics and native
#'   `definition_id` provenance even if this is `FALSE`.
#'
#' @details
#' Automatic selection checks registered species/jurisdiction coverage, input
#' clock, native SI definition, requested scientific fields, stand origin and
#' encoded geographic restrictions before ranking. Unresolved definitions or
#' source disputes are excluded. Remaining models use decreasing registry
#' `rank`, with lexical `model_id` tie-breaking. Rank is a pragmatic preference,
#' not evidence of statistical superiority. Sparse applicability metadata means
#' many models require explicit, source-reviewed use rather than automatic use.
#' Free-text composition, stand structure, local geography and height sampling
#' still require review using [get_si_model_info()].
#' Published Lundgren-Dolid recommendation violations exclude automatic use;
#' explicit-ID use warns and records the limitation, preserving direct values.
#'
#' SI estimation without a requested scale uses the preferred eligible model's
#' native definition. If it returns an invalid prediction, fallback is restricted
#' to that same definition (including quantity, reference age/clock and datums);
#' measured height is independently evaluated, never a failed model's SI.
#' Comparison mode can show different native scales, labelled separately.
#' Supplied SI is never moved to another model after a failed prediction.
#'
#' Native engine warnings, including extrapolation warnings, pass through and
#' are recorded; values are not clipped and new empirical bounds are not
#' invented. Engine errors abort with model/observation context and the original
#' error as parent: arbitrary errors are not swallowed to trigger fallback.
#' A malformed engine return also aborts. Non-finite/non-positive returned
#' predictions are explicit candidate failures. Missing observations return
#' missing predictions and a warning; invalid types, incompatible lengths and
#' non-positive/non-finite supplied numeric values are errors.
#'
#' @return With `pick_best = TRUE`, a tibble with `si` or `height`, optional
#'   `model_id`, and scientific provenance: `definition_id`, `input_age_basis`,
#'   `reference_age`, `reference_age_basis`, `productivity_quantity`, `si_datum`,
#'   `height_datum`, and `reference_age_role`. `status` and `reason` report
#'   success, success with warnings, missing inputs, failed predictions or no
#'   eligible model. `diagnostics` contains per-observation candidate tibbles
#'   with exclusions, untried candidates, failures, actual `engine_args`,
#'   warnings and limitations.
#'
#'   With `pick_best = FALSE`, returns those per-observation candidate tibbles
#'   directly. Candidates are ordered by rank then ID; empty scope matches
#'   produce typed empty tibbles. Inputs recycle to their common length.
#'   GI, ecological IQS and climate inherent HI are excluded even for explicit
#'   IDs; they remain available through their engines and discovery helpers.
#'
#' @seealso [find_si_models()], [get_si_model_info()], [si_model_registry()]
#' @examples
#' si(age = c(20, 30), height = c(10, 15),
#'    species = "PICE.GLA", jurisdiction = "QC",
#'    input_age_basis = "plantation_age", stand_origin = "plantation")
#'
#' # Explicit ID declares supplied SI on that model's native SI25 scale.
#' si(age = c(20, 40), si = 18, species = "PICE.GLA", jurisdiction = "QC",
#'    input_age_basis = "plantation_age", stand_origin = "plantation",
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
  clocks <- c("total_age", "breast_height_age", "plantation_age", "corrected_age_above_1m")
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
