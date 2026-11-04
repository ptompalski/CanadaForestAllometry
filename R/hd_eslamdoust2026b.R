#' Eslamdoust (2026; TR-79) DBH-height model for Ontario white pine plantations
#'
#' Predicts total tree height (m) for plantation-origin white pine
#' (`PINU.STR`) in Ontario from DBH and stand height, basal area and density.
#' This is the TR-79 model, not the natural-stand models in
#' [hd_eslamdoust2026()].
#'
#' The modified Chapman-Richards equation (Equation 1, p. 4) is
#' \deqn{H = 1.3 + \alpha_0 HDOM^{\alpha_1}
#'   [1 - \exp(-\alpha_2 (TPH / BA)^{\alpha_3} DBH)]^{\alpha_4}.}
#' Population predictions use the mixed-model fixed effects (Table 2, p. 7),
#' with the plot random effect on \eqn{\alpha_0} set to zero. Plot-specific
#' calibration is not implemented.
#'
#' Fitted to 2,265 trees in 41 Ontario plots. Fitting ranges (Table 1, p. 3)
#' are DBH 2.9-58.50 cm, `HDOM` 4.57-33.56 m, `BA` 7.55-53.84 m2/ha and
#' `TPH` 433-4,700 stems/ha. Inputs outside these ranges are computed with a
#' single extrapolation warning. The report notes increased bias above
#' DBH 50 cm and in the highest relative-height classes.
#'
#' Inputs recycle to a common length; incompatible lengths raise an error.
#' Rows with `NA` in any input return `NA` height.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m),
#'   in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species code; only
#'   `"PINU.STR"` (white pine, Pinus strobus) is supported. Compact
#'   `"PINUSTR"` is accepted. Defaults to `"PINU.STR"`.
#' @param HDOM Numeric vector. Plot level. Stand height (m), mean height of
#'   dominant and codominant trees (source SHT, p. 2). The source does not
#'   specify the species basis. Must be `> 0`.
#' @param BA Numeric vector. Plot level. Stand basal area (m2/ha; source BAH,
#'   p. 2). The source does not specify the minimum tree-size threshold or
#'   species basis. Must be `> 0`.
#' @param TPH Numeric vector. Plot level. Trees per hectare with
#'   DBH \eqn{\ge} 2.5 cm (source TPH, Equation 1, p. 4). The source does
#'   not specify the species basis. Must be `> 0`.
#' @param keep_model_id Logical scalar. If `TRUE`, add `model_id`.
#'
#' @return A tibble with `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id` (`"eslamdoust2026b"`).
#'
#' @references
#' Eslamdoust, J. 2026. Height-diameter model for white pine plantations in
#' Ontario. Ontario Ministry of Natural Resources, Science and Research Branch,
#' Peterborough, ON. Science and Research Technical Report TR-79. 12 p.
#'
#' @seealso [hd_eslamdoust2026()], [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' hd_eslamdoust2026b(DBH = c(10, 20, 30), HDOM = 18, BA = 30, TPH = 1500)
#' hd_eslamdoust2026b(
#'   DBH = 25, species = "PINU.STR", HDOM = 20, BA = 35, TPH = 1200,
#'   keep_model_id = TRUE
#' )
#' @export
hd_eslamdoust2026b <- function(DBH, species = "PINU.STR", HDOM, BA, TPH,
                              keep_model_id = FALSE) {
  fn <- "hd_eslamdoust2026b"
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(HDOM, "HDOM", gt = 0, allow_na = TRUE)
  assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE)
  assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- assert_len_compat(
    DBH = DBH, species = as.character(species), HDOM = HDOM, BA = BA, TPH = TPH
  )
  p <- .eslamdoust2026b_prepare(v$species)
  .hd_warn_out_of_range(
    list(DBH = v$DBH, HDOM = v$HDOM, BA = v$BA, TPH = v$TPH),
    lower = list(DBH = p$DBH_min, HDOM = p$HDOM_min, BA = p$BA_min, TPH = p$TPH_min),
    upper = list(DBH = p$DBH_max, HDOM = p$HDOM_max, BA = p$BA_max, TPH = p$TPH_max),
    fn = fn
  )
  height <- .hd_cr_stand(
    v$DBH, v$HDOM, v$BA, v$TPH, p$a0, p$a1, p$a2, p$a3, p$a4
  )
  complete <- !is.na(v$DBH) & !is.na(v$species) & !is.na(v$HDOM) &
    !is.na(v$BA) & !is.na(v$TPH)
  if (any(complete & (!is.finite(height) | height <= 0))) {
    cli::cli_abort("{.fn {fn}} produced non-finite or non-positive heights.")
  }
  out <- tibble::tibble(height = height)
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$Species), NA_character_, "eslamdoust2026b")
  }
  out
}

.eslamdoust2026b_prepare <- function(species, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Eslamdoust2026b")
  assert_required_cols(
    pars,
    c(
      "Species", "a0", "a1", "a2", "a3", "a4", "DBH_min", "DBH_max",
      "HDOM_min", "HDOM_max", "BA_min", "BA_max", "TPH_min", "TPH_max"
    ),
    object = "parameters_hd_Eslamdoust2026b"
  )
  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad)) {
    cli::cli_abort(c(
      "Species not covered by the Eslamdoust (2026; TR-79) DBH-height model: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }
  out <- as.data.frame(pars[match(sp, pars$Species), , drop = FALSE])
  rownames(out) <- NULL
  out
}
