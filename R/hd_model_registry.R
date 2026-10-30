# DBH–height (hd_) model registry and discovery helpers.
#
# Single source of truth for predictors: `.hd_model_predictors()` (one row per
# model_id x predictor argument). The registry's `required_predictors` /
# `optional_predictors` list-columns are derived from it.
#
# To add a model: append a `.hd_registry_row()` call to `.hd_model_definitions()`
# and its predictor rows to `.hd_model_predictors()`. `species` is not a
# predictor (coverage is derived from the params table / `species_manual`).

# internal
.hd_predictor_levels <- c("tree", "plot")

# internal
.hd_model_predictors <- function() {
  tibble::tribble(
    ~model_id, ~arg, ~level, ~units, ~required, ~definition, ~source_locator,
    "huang2016", "DBH", "tree", "cm", TRUE,
    "Tree diameter (cm) measured at a breast height of 1.3 m above the ground.",
    "T/609 p. 6 (eqs. [1]-[2])",
    "huang2016", "natural_subregion", "plot", "code (1-21 or e.g. 'CM')", FALSE,
    "Alberta 2006 natural subregion (Natural Regions Committee 2006); selects the natural-subregion-group model. NULL = provincial model.",
    "T/609 Table 2 (p. 2); Table 4 (p. 7)"
  )
}

# internal
.hd_registry_row <- function(
  model_id,
  engine,
  reference,
  scope,
  province_scope,
  stand_origin,
  equation_family,
  description,
  fixed_args = list(),
  region_arg = NA_character_,
  prediction_types = "population",
  calibration_published = FALSE,
  params_key = NA_character_,
  species_manual = NULL
) {
  assert_chr_scalar(model_id, "model_id")
  assert_chr_scalar(engine, "engine")
  assert_chr_scalar(reference, "reference")
  assert_choice(scope, "scope", c("provincial", "regional"))
  assert_choice(stand_origin, "stand_origin", c("natural", "plantation", "both"))
  assert_chr_scalar(equation_family, "equation_family")
  assert_chr_scalar(description, "description")
  assert_chr_scalar(region_arg, "region_arg", allow_na = TRUE)
  assert_chr_scalar(params_key, "params_key", allow_na = TRUE)
  assert_choice(
    prediction_types,
    "prediction_types",
    c("population", "calibrated"),
    multiple = TRUE
  )
  if (!is.character(province_scope) || length(province_scope) == 0L) {
    cli::cli_abort("{.arg province_scope} must be a non-empty character vector.")
  }
  if (!is.list(fixed_args)) {
    cli::cli_abort("{.arg fixed_args} must be a (named) list.")
  }
  if (!is.logical(calibration_published) || length(calibration_published) != 1L ||
    is.na(calibration_published)) {
    cli::cli_abort("{.arg calibration_published} must be TRUE or FALSE.")
  }

  tibble::tibble(
    model_id = model_id,
    engine = engine,
    fixed_args = list(fixed_args),
    reference = reference,
    scope = scope,
    province_scope = list(province_scope),
    stand_origin = stand_origin,
    equation_family = equation_family,
    region_arg = region_arg,
    prediction_types = list(prediction_types),
    calibration_published = calibration_published,
    params_key = params_key,
    species_manual = list(species_manual),
    description = description
  )
}

# internal
# One `.hd_registry_row()` per model variant.
.hd_model_definitions <- function() {
  list(
    .hd_registry_row(
      model_id = "huang2016",
      engine = "hd_huang2016",
      reference = "@Huang2016hd",
      scope = "provincial",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards; log_logistic",
      description = "Alberta T/609 population DBH-height models (base fits, Table 4); provincial or natural-subregion-group variants.",
      region_arg = "natural_subregion",
      calibration_published = TRUE,
      params_key = "parameters_hd_Huang2016"
    )
  )
}

#' DBH–height model registry
#'
#' Returns a registry (metadata table) of the DBH–height (`hd_*()`) models
#' available in CanadaForestAllometry, one row per model variant. Use it with
#' [get_hd_model_info()] and [find_hd_models()] to discover which model suits
#' your data, then call the corresponding `hd_*()` function directly. There is
#' no automatic model selection.
#'
#' All models predict total tree height (m) from DBH (cm) at the population
#' level (random effects set to zero for mixed-effects sources).
#'
#' @return A tibble with one row per model variant and columns:
#' \describe{
#'   \item{`model_id`}{Stable model identifier (with variant suffix).}
#'   \item{`engine`}{Name of the `hd_*()` function implementing the model.}
#'   \item{`fixed_args`}{List-column of arguments selecting the variant.}
#'   \item{`reference`}{BibTeX key of the source publication.}
#'   \item{`scope`}{`"provincial"` or `"regional"`.}
#'   \item{`province_scope`}{List-column of jurisdiction codes.}
#'   \item{`stand_origin`}{`"natural"`, `"plantation"` or `"both"`.}
#'   \item{`equation_family`}{Model form (e.g. `"chapman_richards"`).}
#'   \item{`required_predictors`, `optional_predictors`}{List-columns of
#'     argument names (excluding `species`).}
#'   \item{`region_arg`}{Name of the model-specific region argument, or `NA`.}
#'   \item{`prediction_types`}{List-column; currently `"population"`.}
#'   \item{`calibration_published`}{Whether the source publishes the variance
#'     components needed for local (plot) calibration.}
#'   \item{`params_key`}{Internal parameter table name.}
#'   \item{`species_manual`}{List-column of species, used only when
#'     coefficients are not stored in a parameter table.}
#'   \item{`description`}{One-line description.}
#' }
#'
#' @seealso [get_hd_model_info()], [find_hd_models()]
#' @examples
#' hd_model_registry()
#' @export
hd_model_registry <- function() {
  rows <- .hd_model_definitions()
  reg <- if (length(rows) == 0L) {
    # zero-row prototype with the correct column types
    .hd_registry_row(
      model_id = "x", engine = "x", reference = "x", scope = "provincial",
      province_scope = "ALL", stand_origin = "both", equation_family = "x",
      description = "x"
    )[0, ]
  } else {
    dplyr::bind_rows(rows)
  }

  preds <- .hd_model_predictors()
  pick <- function(id, req) {
    preds$arg[preds$model_id == id & preds$required == req]
  }
  reg$required_predictors <- lapply(reg$model_id, pick, req = TRUE)
  reg$optional_predictors <- lapply(reg$model_id, pick, req = FALSE)

  dplyr::relocate(
    reg,
    "required_predictors", "optional_predictors",
    .after = "equation_family"
  )
}

#' DBH–height model registry with species coverage
#'
#' @return A tibble like `hd_model_registry()` plus `species` (list-column),
#'   `n_species` (integer) and `species_text` (character).
#' @keywords internal
#' @noRd
hd_model_registry_species <- function(reg = hd_model_registry()) {
  species_of <- function(model_id, params_key, species_manual) {
    if (length(species_manual) > 0L) {
      return(sort(unique(as.character(species_manual))))
    }
    if (is.na(params_key) || !nzchar(params_key)) {
      return(character(0))
    }
    tryCatch(
      extract_species_from_params(get_params_tbl(params_key)),
      error = function(e) {
        cli::cli_abort(
          "Failed to build species list for model_id={model_id} (params_key={params_key}): {conditionMessage(e)}"
        )
      }
    )
  }

  reg$species <- purrr::pmap(
    list(reg$model_id, reg$params_key, reg$species_manual),
    species_of
  )
  reg$n_species <- lengths(reg$species)
  reg$species_text <- vapply(
    reg$species,
    function(s) if (length(s) == 0L) NA_character_ else paste(s, collapse = ", "),
    character(1)
  )
  reg
}

#' Get information about a DBH–height model
#'
#' Returns the registry metadata for one `hd_*()` model variant together with
#' its predictor table (argument, level, units, whether required, and the
#' source's definition of the predictor).
#'
#' @param model_id Single model identifier, as in `hd_model_registry()$model_id`.
#'
#' @return A named list with the registry fields of the model (list-columns
#'   unwrapped), its `species` coverage, and `predictors`, a tibble with columns
#'   `arg`, `level`, `units`, `required`, `definition` and `source_locator`.
#'
#' @seealso [hd_model_registry()], [find_hd_models()]
#' @examples
#' ids <- hd_model_registry()$model_id
#' if (length(ids) > 0) get_hd_model_info(ids[[1]])
#' @export
get_hd_model_info <- function(model_id) {
  assert_chr_scalar(model_id, "model_id")

  reg <- hd_model_registry()
  if (!model_id %in% reg$model_id) {
    avail <- if (nrow(reg) == 0L) "none (no models registered yet)" else
      paste(reg$model_id, collapse = ", ")
    cli::cli_abort(c(
      "Unknown DBH\u2013height model {.val {model_id}}.",
      "i" = "Available model_id values: {avail}."
    ))
  }

  row <- hd_model_registry_species(reg[reg$model_id == model_id, ])
  info <- lapply(as.list(row), function(col) if (is.list(col)) col[[1]] else col)
  info$n_species <- NULL
  info$species_text <- NULL

  preds <- .hd_model_predictors()
  info$predictors <- preds[preds$model_id == model_id, setdiff(names(preds), "model_id")]
  info
}

#' Find DBH–height models usable with your data
#'
#' Filters [hd_model_registry()] by jurisdiction and species, and reports which
#' required predictors each candidate model needs that you do not have. Models
#' with missing predictors are kept (as near-misses) so you can see what extra
#' data would make them usable. No model is selected automatically.
#'
#' @param jurisdiction Optional character vector of province/territory codes
#'   (e.g. `"ON"`). Models covering any of them are kept. `NULL` (default)
#'   applies no filter.
#' @param species Optional character vector of NFI species codes
#'   (e.g. `"PICE.MAR"`). Models covering any of them are kept. `NULL`
#'   (default) applies no filter.
#' @param available Character vector of predictor argument names you can
#'   supply (e.g. `c("DBH", "BA", "TPH")`). Names follow the `hd_*()` argument
#'   names, which are case-sensitive.
#'
#' @return The matching rows of `hd_model_registry()` with species coverage
#'   (`species`, `n_species`, `species_text`), plus `missing_predictors`
#'   (list-column of required predictors not in `available`) and `usable`
#'   (logical). Usable models are listed first.
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' find_hd_models(jurisdiction = "ON", species = "PICE.MAR",
#'                available = c("DBH", "BA", "TPH"))
#' @export
find_hd_models <- function(jurisdiction = NULL, species = NULL, available = "DBH") {
  if (!is.null(jurisdiction)) {
    if (!is.character(jurisdiction) || length(jurisdiction) == 0L || anyNA(jurisdiction)) {
      cli::cli_abort("{.arg jurisdiction} must be a character vector without NA.")
    }
    jurisdiction <- standardize_jurisdiction_code(jurisdiction)
  }
  if (!is.null(species)) {
    if (!is.character(species) || length(species) == 0L || anyNA(species)) {
      cli::cli_abort("{.arg species} must be a character vector without NA.")
    }
    species <- standardize_species_code(species, keep_all = FALSE)
  }
  if (!is.character(available) || anyNA(available)) {
    cli::cli_abort("{.arg available} must be a character vector without NA.")
  }

  reg <- hd_model_registry_species()

  if (!is.null(jurisdiction)) {
    keep <- vapply(
      reg$province_scope,
      function(p) any(jurisdiction %in% normalize_province_scope(p)),
      logical(1)
    )
    reg <- reg[keep, ]
  }
  if (!is.null(species)) {
    keep <- vapply(reg$species, function(s) any(species %in% s), logical(1))
    reg <- reg[keep, ]
  }

  reg$missing_predictors <- lapply(reg$required_predictors, setdiff, available)
  reg$usable <- lengths(reg$missing_predictors) == 0L
  reg[order(!reg$usable, lengths(reg$missing_predictors)), ]
}

# internal
# Returns a character vector describing registry / predictor-table
# inconsistencies (empty when consistent). Checked by the test suite.
.hd_registry_problems <- function(
  reg = hd_model_registry(),
  preds = .hd_model_predictors(),
  env = asNamespace("CanadaForestAllometry")
) {
  out <- character()

  dup <- unique(reg$model_id[duplicated(reg$model_id)])
  if (length(dup)) out <- c(out, paste0("Duplicated model_id: ", dup))

  unknown <- setdiff(preds$model_id, reg$model_id)
  if (length(unknown)) out <- c(out, paste0("Predictors for unregistered model_id: ", unknown))

  key <- paste(preds$model_id, preds$arg)
  dup_p <- unique(key[duplicated(key)])
  if (length(dup_p)) out <- c(out, paste0("Duplicated predictor: ", dup_p))

  bad_lvl <- unique(preds$level[!preds$level %in% .hd_predictor_levels])
  if (length(bad_lvl)) out <- c(out, paste0("Invalid predictor level: ", bad_lvl))

  if ("species" %in% preds$arg) {
    out <- c(out, "`species` must not be listed as a predictor.")
  }

  for (i in seq_len(nrow(reg))) {
    id <- reg$model_id[[i]]
    fn <- get0(reg$engine[[i]], envir = env, mode = "function")
    if (is.null(fn)) {
      out <- c(out, paste0(id, ": engine `", reg$engine[[i]], "` not found."))
      next
    }
    fmls <- formals(fn)
    args <- c(reg$required_predictors[[i]], reg$optional_predictors[[i]])

    if (!"DBH" %in% reg$required_predictors[[i]]) {
      out <- c(out, paste0(id, ": `DBH` must be a required predictor."))
    }
    not_formal <- setdiff(c(args, names(reg$fixed_args[[i]])), names(fmls))
    if (length(not_formal)) {
      out <- c(out, paste0(id, ": not arguments of the engine: ", paste(not_formal, collapse = ", ")))
    }
    no_default <- names(fmls)[vapply(fmls, function(x) identical(x, quote(expr = )), logical(1))]
    undeclared <- setdiff(no_default, c(reg$required_predictors[[i]], "species"))
    if (length(undeclared)) {
      out <- c(out, paste0(
        id, ": engine arguments without default not declared as required predictors: ",
        paste(undeclared, collapse = ", ")
      ))
    }
    if (!is.na(reg$region_arg[[i]]) && !reg$region_arg[[i]] %in% args) {
      out <- c(out, paste0(id, ": region_arg `", reg$region_arg[[i]], "` is not a predictor."))
    }
  }

  out
}
