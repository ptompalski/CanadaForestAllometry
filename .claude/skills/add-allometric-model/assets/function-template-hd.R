#' <Author> (<Year>) DBH–height model for <region/species>
#'
#' Vectorized implementation of the DBH–height model in <full citation>.
#' Predicts total tree height (m) from DBH (cm) <and stand predictors>.
#'
#' \strong{Model scope (species coverage):} <list NFI codes>. <Source species
#' mappings, e.g. "PINU.STR is predicted with the PINU.CON equation (Table 1)">.
#'
#' \strong{Height convention:} total height; breast height = <1.3 | 1.37> m.
#'
#' \strong{Population-level prediction:} <for mixed-effects sources> random
#' effects are set to zero (fixed effects from Table <X>), giving the
#' typical-plot curve. Local calibration is not implemented.
#'
#' \strong{Variants:} <M4 uses DBH only; M5 is used when HDOM, BA and TPH are all
#' supplied (all-or-none)>.
#'
#' Rows with `NA` in any predictor return `NA` height. Inputs outside the
#' published fitting ranges are computed and reported in a single warning.
#'
#' @param DBH Numeric vector. Diameter outside bark at 1.3 m (cm). Tree level.
#' @param species Character vector of NFI species codes (e.g. `"PICE.MAR"`).
#' @param HDOM Optional numeric. Plot level. <Source definition, e.g. "mean
#'   height of the 100 largest-DBH trees per ha"> (m). Length 1 or `length(DBH)`.
#' @param BA Optional numeric. Plot level. <all-species | species-specific>
#'   basal area of trees <threshold> (m2/ha).
#' @param TPH Optional numeric. Plot level. Stems per hectare <threshold>.
#' @param keep_model_id Logical. If `TRUE`, add a `model_id` column naming the
#'   variant used for each row. Only for functions with internal variants.
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' <full citation>
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' <fn_name>(DBH = c(10, 25), species = "PICE.MAR")
#'
#' @export
<fn_name> <- function(DBH, species, HDOM = NULL, BA = NULL, TPH = NULL,
                      keep_model_id = FALSE) {
  fn <- "<fn_name>"

  # --- variant selection: all-or-none optional group ---
  full <- .hd_check_predictor_group(
    list(HDOM = HDOM, BA = BA, TPH = TPH),
    variant = "<variant name>"
  )
  model_id <- if (full) "<model>_m5" else "<model>_m4"
  # inform_once() interpolates in its own frame: build the text here.
  inform_once(
    paste0(fn, "(): using variant '", model_id, "'."),
    .id = paste0(fn, "_", model_id)
  )

  # --- validate (NA allowed -> NA height) ---
  assert_numeric_vec(DBH, "DBH", gt = 0, allow_na = TRUE)
  if (full) {
    assert_numeric_vec(HDOM, "HDOM", gt = 0, allow_na = TRUE)
    assert_numeric_vec(BA, "BA", gte = 0, allow_na = TRUE)
    assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE)
  }
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }

  # --- recycle tree- and plot-level inputs to a common length ---
  v <- assert_len_compat(
    DBH = DBH, species = species,
    HDOM = HDOM %||% NA_real_, BA = BA %||% NA_real_, TPH = TPH %||% NA_real_
  )
  sp <- standardize_species_code(v$species)

  # --- parameters: join by Species (+ region / variant keys) ---
  # p <- .<model>_prepare(sp, variant = ...)  # see si_huang2009.R for the
  # pattern: .get_internal_data("parameters_hd_<Model>"), assert_required_cols(),
  # left_join, abort listing supported species on unmatched codes.

  # --- applicability ranges (one warning per call) ---
  # .hd_warn_out_of_range(
  #   list(DBH = v$DBH, BA = v$BA),
  #   lower = list(DBH = p$DBH_min, BA = p$BA_min),
  #   upper = list(DBH = p$DBH_max, BA = p$BA_max),
  #   fn = fn
  # )

  # --- evaluate the transcribed model form (fixed effects only, u = 0) ---
  # height <- 1.3 + <f(DBH, ...)>   # keep faithful to the source
  height <- rep(NA_real_, length(v$DBH)) # replace

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height <= 0))) {
    cli::cli_abort("{.fn {fn}} produced non-finite or non-positive heights.")
  }

  out <- tibble::tibble(height = height)
  if (keep_model_id) out$model_id <- model_id
  out
}
