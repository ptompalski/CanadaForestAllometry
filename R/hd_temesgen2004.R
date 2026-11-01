#' Temesgen and von Gadow (2004) DBH–height models for interior British
#' Columbia
#'
#' Vectorized implementation of the generalized height–diameter models of
#' Temesgen and von Gadow (2004) for eight tree species of complex
#' (mixed-species, uneven-aged) stands in the Interior Douglas-fir (IDF)
#' biogeoclimatic zone of southern interior British Columbia. Predicts total
#' tree height (m) from DBH (cm) alone (Model 2) or from DBH, the tree's basal
#' area in larger trees and two stand-density predictors (Model 7).
#'
#' \strong{Model scope (species coverage):} trembling aspen (`POPU.TRE`),
#' western redcedar (`THUJ.PLI`), paper birch (`BETU.PAP`), Douglas-fir
#' (`PSEU.MEN`), western larch (`LARI.OCC`), lodgepole pine (`PINU.CON`),
#' ponderosa pine (`PINU.PON`) and the interior spruce complex. The source
#' "spruce" combines Engelmann and white spruce (Table 1, p. 50); its
#' coefficients are used for `PICE.ENG`, `PICE.GLA` and the hybrid
#' `PICE.ENG.GLA`. Fitted to BC Ministry of Forests permanent sample plots
#' (49°–52°30′N) from which thinned, spaced and selectively logged plots were
#' excluded; the authors caution against using the models in such stands.
#'
#' \strong{Model forms} (Models 2 and 7, pp. 46–47):
#' \deqn{H = 1.3 + a [1 - \exp(-b \, DBH^{c})],}
#' with \eqn{a}, \eqn{b}, \eqn{c} species constants for Model 2 (Table 5,
#' p. 51), and for Model 7 (Table 4, p. 50)
#' \deqn{a = a_1 + a_2 BAL + a_3 TPH + a_4 BA, \quad c = a_5 + a_6 BAL.}
#' Terms dropped by the authors as non-significant (blank in Table 4) are
#' zero; e.g. Model 7 for ponderosa pine uses no stand predictor and is
#' practically identical to Model 2.
#'
#' \strong{Corrected exponent sign:} both equations are printed as
#' \eqn{1 - e^{b D^{c}}}. With the published (positive) \eqn{b} this gives
#' negative heights, so the standard Weibull form of Yang et al. (1978),
#' \eqn{1 - e^{-b D^{c}}}, is used. It reproduces the observed mean heights of
#' the fitting data (Table 1).
#'
#' \strong{Height convention:} total tree height; DBH measured outside bark at
#' 1.3 m, so predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Variants:} if `BAL`, `TPH` and `BA` are all `NULL` (default),
#' Model 2 (DBH only) is used. If all three are supplied, Model 7 is used for
#' every species, even for species where some or all of them have no effect.
#' Supplying only some of them is an error. A one-time message reports the
#' variant used. Coefficients are ordinary nonlinear least-squares fits (SAS
#' PROC NLIN); no calibration information is published.
#'
#' Rows with `NA` in any input return `NA` height. DBH outside the per-species
#' range of the fitting data (Table 1) is computed and reported in a single
#' warning. The source gives no ranges for `BAL`, `TPH` or `BA`; Model 7 inputs
#' extreme enough to make \eqn{a} or \eqn{c} non-positive raise an error.
#'
#' @param DBH Numeric vector. Tree level. Diameter outside bark at breast
#'   height (1.3 m above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PSEU.MEN"`; compact `"PSEUMEN"` is accepted).
#' @param BAL Optional numeric vector. Tree level. Basal area in larger trees
#'   (m²/ha): basal area per hectare of the trees larger than the subject tree
#'   in its plot (source "BAL", p. 47). The source does not state the species
#'   basis or a minimum DBH (plots recorded trees with DBH \eqn{\ge} 2 cm).
#'   Must be `>= 0`. Supply with `TPH` and `BA` to use Model 7.
#' @param TPH Optional numeric vector. Plot level. Number of trees per hectare
#'   (source "SPH", p. 47); minimum DBH not stated (plots recorded trees with
#'   DBH \eqn{\ge} 2 cm). Must be `> 0`.
#' @param BA Optional numeric vector. Plot level. Basal area per hectare
#'   (m²/ha) (source "BA", p. 47); minimum DBH not stated. Must be `> 0`.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"temesgen2004_m2"` or `"temesgen2004_m7"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Temesgen, H., and von Gadow, K. (2004). Generalized height–diameter models
#' — an application for major tree species in complex stands of interior
#' British Columbia. European Journal of Forest Research, 123(1), 45–51.
#' \doi{10.1007/s10342-004-0020-z}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Model 2: DBH only
#' hd_temesgen2004(DBH = c(10, 25, 40), species = c("PSEU.MEN", "PINU.CON", "PICE.ENG"))
#'
#' # Model 7: with basal area in larger trees, stems/ha and basal area
#' hd_temesgen2004(
#'   DBH = c(10, 25, 40),
#'   species = c("PSEU.MEN", "PINU.CON", "PICE.ENG"),
#'   BAL = c(20, 8, 2), TPH = 800, BA = 25,
#'   keep_model_id = TRUE
#' )
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(12, 25, 18),
#'   species = c("PSEU.MEN", "LARI.OCC", "POPU.TRE"),
#'   BAL = c(18, 6, 12), TPH = c(900, 900, 600), BA = c(24, 24, 15)
#' )
#' dplyr::mutate(trees, hd_temesgen2004(DBH, species, BAL, TPH, BA))
#'
#' @export
hd_temesgen2004 <- function(DBH, species, BAL = NULL, TPH = NULL, BA = NULL,
                            keep_model_id = FALSE) {
  fn <- "hd_temesgen2004"

  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  use_m7 <- .hd_check_predictor_group(
    list(BAL = BAL, TPH = TPH, BA = BA),
    variant = "Model 7"
  )
  if (use_m7) {
    assert_numeric_vec(BAL, "BAL", gte = 0, allow_na = TRUE)
    assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE)
    assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE)
  }
  variant <- if (use_m7) "m7" else "m2"
  model_id <- paste0("temesgen2004_", variant)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- if (use_m7) {
    assert_len_compat(
      DBH = DBH, species = as.character(species), BAL = BAL, TPH = TPH, BA = BA
    )
  } else {
    assert_len_compat(DBH = DBH, species = as.character(species))
  }

  p <- .temesgen2004_prepare(v$species, variant)

  inform_once(
    if (use_m7) {
      paste0(fn, "(): `BAL`, `TPH` and `BA` supplied; using Model 7 (DBH and stand variables).")
    } else {
      paste0(fn, "(): `BAL`, `TPH` and `BA` not supplied; using Model 2 (DBH only).")
    },
    .id = paste0(fn, "_", variant)
  )
  .hd_warn_out_of_range(
    list(DBH = v$DBH),
    lower = list(DBH = p$DBH_min),
    upper = list(DBH = p$DBH_max),
    fn = fn
  )

  if (use_m7) {
    # Terms dropped in Table 4 are NA in the params table and contribute 0.
    z <- function(x) ifelse(is.na(x), 0, x)
    asym <- p$a1 + z(p$a2) * v$BAL + z(p$a3) * v$TPH + z(p$a4) * v$BA
    shape <- p$a5 + z(p$a6) * v$BAL
    bad <- which(!is.na(asym) & !is.na(shape) & (asym <= 0 | shape <= 0))
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{.fn {fn}}: Model 7 asymptote or shape parameter is non-positive for row{?s} {bad}.",
        "i" = "`BAL`, `TPH` or `BA` are far outside the fitting data; use Model 2 or check the inputs."
      ))
    }
  } else {
    asym <- p$a1
    shape <- p$a5
  }
  height <- 1.3 + asym * (1 - exp(-p$b * v$DBH^shape))

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height <= 0))) {
    # nocov start
    # Defensive: positive a, b and c give finite heights >= 1.3 m.
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
# Returns one parameter row of `variant` ("m2" or "m7") per input row; NA
# species give an all-NA row.
.temesgen2004_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Temesgen2004")
  assert_required_cols(
    pars,
    c("Species", "variant", "a1", "a2", "a3", "a4", "b", "a5", "a6", "DBH_min", "DBH_max"),
    object = "parameters_hd_Temesgen2004"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Temesgen and von Gadow (2004) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
