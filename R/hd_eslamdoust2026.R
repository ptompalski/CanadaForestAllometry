#' Eslamdoust et al. (2026) DBH–height models for Ontario tree species
#'
#' Vectorized implementation of the population-level individual-tree
#' height–diameter models M4 and M5 of Eslamdoust et al. (2026) for eight tree
#' species of the boreal and mixed (Great Lakes–St. Lawrence) forests of
#' Ontario. Predicts total tree height (m) from DBH (cm) alone (M4) or from DBH
#' and three stand-level predictors: dominant stand height, basal area and
#' stand density (M5).
#'
#' \strong{Model scope (species coverage):} eastern white pine (`PINU.STR`),
#' eastern white cedar (`THUJ.OCC`), sugar maple (`ACER.SAH`), yellow birch
#' (`BETU.ALL`), American beech (`FAGU.GRA`), tamarack (`LARI.LAR`), basswood
#' (`TILI.AME`) and eastern hemlock (`TSUG.CAN`). Fitted to Ontario
#' growth-and-yield and NFI plots in natural stands (Table 1, p. 219), with
#' stand basal area \eqn{\ge} 5 m²/ha.
#'
#' \strong{Model forms} (eqs. 4 and 5, p. 220):
#' \deqn{M4: H = 1.3 + \alpha_0 [1 - \exp(-\alpha_1 DBH)]^{\alpha_2};}
#' \deqn{M5: H = 1.3 + \alpha_0 \, HDOM^{\alpha_1}
#'   [1 - \exp(-\alpha_2 (TPH / BA)^{\alpha_3} DBH)]^{\alpha_4}.}
#'
#' \strong{Height convention:} total tree height; DBH measured at 1.3 m, so
#' predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Population-level prediction:} coefficients are the fixed effects of
#' the mixed-effects fits (Tables 4 and 5, p. 225), evaluated with the
#' plot-level random effect on \eqn{\alpha_0} set to zero (\eqn{u = 0}), as in
#' the "fixed response" curves of Fig. 3. Plot-specific calibration with
#' measured heights is not implemented.
#'
#' \strong{Variants:} if `HDOM`, `BA` and `TPH` are all `NULL` (default), M4
#' (DBH only) is used. If all three are supplied, M5 is used for every species.
#' Supplying only some of them is an error. A one-time message reports the
#' variant used. The authors note that M4 performed as well as or better than
#' M5 for yellow birch and eastern hemlock (and basswood) and that M4 "may be
#' considered a more parsimonious and operationally practical model in cases
#' where stand-level variables are unavailable or unreliable" (p. 222, p. 225).
#'
#' \strong{Tamarack and basswood (M5):} the stand-density exponent
#' \eqn{\alpha_3} is reported as 0 (not significant; Table 5), so `BA` and
#' `TPH` do not change their predicted heights; they are still required for
#' M5.
#'
#' \strong{Corrected coefficient:} the tamarack M5 \eqn{\alpha_2} is printed
#' as 0.9793 in Table 5, which gives a near-flat curve (about 16 m at DBH 2.5
#' cm) inconsistent with Fig. 3b and the fitting data (Table 1). It is treated
#' as a misprint and 0.09793 is used, which reproduces the tamarack curve in
#' Fig. 3b. This value is inferred, not published.
#'
#' The authors report increased bias for DBH > 50 cm, particularly for white
#' pine, sugar maple, beech and hemlock (p. 225).
#'
#' Rows with `NA` in any input return `NA` height. Inputs outside the
#' per-species range of the fitting data (Table 1: DBH and, for M5, `HDOM`,
#' `BA` and `TPH`) are computed and reported in a single warning.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"ACER.SAH"`; compact `"ACERSAH"` is accepted).
#' @param HDOM Optional numeric vector. Plot level. Stand height (m): "mean
#'   height of dominant and co-dominant trees" in the plot (source "SHT",
#'   p. 219). The source does not state whether all species or only the
#'   subject species are included. Must be `> 0`. Supply with `BA` and `TPH`
#'   to use M5.
#' @param BA Optional numeric vector. Plot level. Stand basal area (m²/ha)
#'   (source "BAH", p. 219 and p. 220). The source does not state a minimum
#'   DBH or species basis for this total. Must be `> 0`.
#' @param TPH Optional numeric vector. Plot level. Number of trees per hectare
#'   "with DBH\eqn{\ge} 2.5cm" (source "TPH", p. 220). Must be `> 0`.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"eslamdoust2026_m4"` or `"eslamdoust2026_m5"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Eslamdoust, J., Vasiliauskas, S., Little, T., and Ashiq, M. W. (2026).
#' Height–diameter modeling of tree species in boreal and mixed forests using
#' a mixed-effects approach and stand-level variables. Forest Science and
#' Technology, 22(1), 217–230. \doi{10.1080/21580103.2025.2571222}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # M4: DBH only
#' hd_eslamdoust2026(DBH = c(10, 25, 40), species = c("ACER.SAH", "PINU.STR", "TSUG.CAN"))
#'
#' # M5: with dominant height, basal area and stems/ha
#' hd_eslamdoust2026(
#'   DBH = c(10, 25, 40),
#'   species = c("ACER.SAH", "PINU.STR", "TSUG.CAN"),
#'   HDOM = 22, BA = 30, TPH = 1200,
#'   keep_model_id = TRUE
#' )
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(12, 25, 18),
#'   species = c("FAGU.GRA", "BETU.ALL", "THUJ.OCC"),
#'   HDOM = c(20, 20, 15), BA = c(28, 28, 40), TPH = c(1500, 1500, 3000)
#' )
#' dplyr::mutate(trees, hd_eslamdoust2026(DBH, species, HDOM, BA, TPH))
#'
#' @export
hd_eslamdoust2026 <- function(DBH, species, HDOM = NULL, BA = NULL, TPH = NULL,
                              keep_model_id = FALSE) {
  fn <- "hd_eslamdoust2026"

  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  use_m5 <- .hd_check_predictor_group(
    list(HDOM = HDOM, BA = BA, TPH = TPH),
    variant = "model M5"
  )
  if (use_m5) {
    assert_numeric_vec(HDOM, "HDOM", gt = 0, allow_na = TRUE)
    assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE)
    assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE)
  }
  variant <- if (use_m5) "m5" else "m4"
  model_id <- paste0("eslamdoust2026_", variant)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- if (use_m5) {
    assert_len_compat(
      DBH = DBH, species = as.character(species), HDOM = HDOM, BA = BA, TPH = TPH
    )
  } else {
    assert_len_compat(DBH = DBH, species = as.character(species))
  }

  p <- .eslamdoust2026_prepare(v$species, variant)

  if (use_m5) {
    inform_once(
      paste0(fn, "(): `HDOM`, `BA` and `TPH` supplied; using model M5 (DBH and stand variables)."),
      .id = paste0(fn, "_m5")
    )
    .hd_warn_out_of_range(
      list(DBH = v$DBH, HDOM = v$HDOM, BA = v$BA, TPH = v$TPH),
      lower = list(DBH = p$DBH_min, HDOM = p$HDOM_min, BA = p$BA_min, TPH = p$TPH_min),
      upper = list(DBH = p$DBH_max, HDOM = p$HDOM_max, BA = p$BA_max, TPH = p$TPH_max),
      fn = fn
    )
    height <- .hd_cr_stand(
      v$DBH, v$HDOM, v$BA, v$TPH,
      p$a0, p$a1, p$a2, p$a3, p$a4
    )
    # (TPH / BA)^0 is 1 even for NA (tamarack, basswood); keep NA in -> NA out.
    height[is.na(v$BA) | is.na(v$TPH)] <- NA_real_
  } else {
    inform_once(
      paste0(fn, "(): `HDOM`, `BA` and `TPH` not supplied; using model M4 (DBH only)."),
      .id = paste0(fn, "_m4")
    )
    .hd_warn_out_of_range(
      list(DBH = v$DBH),
      lower = list(DBH = p$DBH_min),
      upper = list(DBH = p$DBH_max),
      fn = fn
    )
    height <- 1.3 + p$a0 * (1 - exp(-p$a1 * v$DBH))^p$a2
  }

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height <= 0))) {
    # nocov start
    # Defensive: published coefficients give finite heights >= 1.3 m.
    cli::cli_abort("{.fn {fn}} produced non-finite or non-positive heights.")
    # nocov end
  }

  out <- tibble::tibble(height = height)
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$Species), NA_character_, model_id)
  }
  out
}


# internal
# Returns one parameter row of `variant` ("m4" or "m5") per input row; NA
# species give an all-NA row.
.eslamdoust2026_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Eslamdoust2026")
  assert_required_cols(
    pars,
    c(
      "Species", "variant", "a0", "a1", "a2", "a3", "a4",
      "DBH_min", "DBH_max", "HDOM_min", "HDOM_max",
      "BA_min", "BA_max", "TPH_min", "TPH_max"
    ),
    object = "parameters_hd_Eslamdoust2026"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Eslamdoust et al. (2026) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
