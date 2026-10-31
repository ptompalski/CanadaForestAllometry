#' Rijal, Weiskittel and Kershaw (2012) DBH–height models for the Acadian
#' Region
#'
#' Vectorized implementation of the regional height–diameter equations of
#' Rijal, Weiskittel and Kershaw (2012) for 15 tree species of the North
#' American Acadian Region (Maine, New Brunswick, Nova Scotia, Prince Edward
#' Island and southern Québec). Predicts total tree height (m) from DBH (cm),
#' a climate-derived site index, stand crown competition factor and the
#' tree's basal area in larger trees.
#'
#' \strong{Model scope (species coverage):} American beech (`FAGU.GRA`), grey
#' birch (`BETU.POP`), paper birch (`BETU.PAP`), trembling aspen
#' (`POPU.TRE`), red maple (`ACER.RUB`), northern red oak (`QUER.RUB`), sugar
#' maple (`ACER.SAH`), yellow birch (`BETU.ALL`), balsam fir (`ABIE.BAL`),
#' black spruce (`PICE.MAR`), eastern hemlock (`TSUG.CAN`), red spruce
#' (`PICE.RUB`), eastern white cedar (`THUJ.OCC`), eastern white pine
#' (`PINU.STR`) and white spruce (`PICE.GLA`). Fitted to permanent and
#' research plots, mostly in mixed-species, uneven-aged stands (pp. 380–381).
#'
#' \strong{Model form} (eq. 4, p. 381):
#' \deqn{HT = 1.37 + (c_0 + CSI^{c_3})
#'   [1 - \exp(-c_1 DBH)]^{c_2 + c_4 \ln(CCF + 1) + c_5 BAL}.}
#'
#' \strong{Variants} (chosen with `variant`): `"gnls"` (default) uses the
#' generalized nonlinear least-squares coefficients (Table 5, p. 387), which
#' the authors recommend "when no observations are available for local
#' calibration" (p. 386). `"nlme"` uses the fixed effects of the
#' nonlinear mixed-effects fit (Table 6, p. 387) with the source, stand and
#' year random effects on \eqn{c_0} set to zero; without local calibration
#' these were more biased than GNLS (p. 386). For grey birch and northern red
#' oak the NLME BAL coefficient was not significant (printed "–") and is
#' zero.
#'
#' \strong{Height convention:} total tree height. The 1.37 m intercept (US
#' breast height) is used for all data, although Canadian DBH was measured at
#' 1.3 m; the authors judged the difference negligible (p. 381). Predicted
#' height is 1.37 m at DBH 0.
#'
#' Rows with `NA` in any input return `NA` height. DBH outside the
#' per-species range of the fitting data (Table 1, p. 381) is computed and
#' reported in a single warning. The source gives no ranges for `CSI`, `CCF`
#' or `BAL`; inputs extreme enough to make the exponent non-positive raise an
#' error.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height, in cm.
#'   Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.RUB"`; compact `"PICERUB"` is accepted).
#' @param CSI Numeric vector. Plot level. Climate-derived site index (m):
#'   "a climate-derived estimate of site index (CSI) ... based on a
#'   non-parametric regression model that relates observed site index to
#'   local climate variables (Weiskittel et al., 2011b)" (source "CSI",
#'   p. 381). Units and base age are not stated; metres are assumed (they
#'   reproduce the observed mean heights). Must be `> 0`.
#' @param CCF Numeric vector. Plot level. Stand crown competition factor
#'   (Krajicek et al. 1961), computed in the source with the maximum crown
#'   width equations of Russell and Weiskittel (2011) (source "CCF",
#'   pp. 380–381). Minimum tree size not stated. Must be `>= 0`.
#' @param BAL Numeric vector. Tree level. Basal area of trees larger than the
#'   subject tree (source "BAL", p. 381). Units are not stated; m²/ha is
#'   assumed. Species basis and minimum DBH are not stated. Must be `>= 0`.
#' @param variant Coefficient set: `"gnls"` (default; Table 5) or `"nlme"`
#'   (Table 6 fixed effects, random effects = 0).
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"rijal2012_gnls"` or `"rijal2012_nlme"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Rijal, B., Weiskittel, A.R., and Kershaw, J.A., Jr. (2012). Development of
#' regional height to diameter equations for 15 tree species in the North
#' American Acadian Region. Forestry, 85(3), 379–389.
#' \doi{10.1093/forestry/cps036}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' hd_rijal2012(
#'   DBH = c(10, 25, 40),
#'   species = c("PICE.RUB", "ABIE.BAL", "ACER.RUB"),
#'   CSI = 15, CCF = 150, BAL = c(20, 8, 2)
#' )
#'
#' # Mixed-model fixed effects
#' hd_rijal2012(25, "PICE.RUB", CSI = 15, CCF = 150, BAL = 10,
#'              variant = "nlme", keep_model_id = TRUE)
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(12, 25, 18),
#'   species = c("PICE.RUB", "TSUG.CAN", "BETU.ALL"),
#'   CSI = c(14, 14, 17), CCF = c(180, 180, 120), BAL = c(18, 6, 12)
#' )
#' dplyr::mutate(trees, hd_rijal2012(DBH, species, CSI, CCF, BAL))
#'
#' @export
hd_rijal2012 <- function(DBH, species, CSI, CCF, BAL,
                         variant = c("gnls", "nlme"),
                         keep_model_id = FALSE) {
  fn <- "hd_rijal2012"

  variant <- rlang::arg_match(variant)
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(CSI, "CSI", gt = 0, allow_na = TRUE)
  assert_numeric_vec(CCF, "CCF", gte = 0, allow_na = TRUE)
  assert_numeric_vec(BAL, "BAL", gte = 0, allow_na = TRUE)
  model_id <- paste0("rijal2012_", variant)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- assert_len_compat(
    DBH = DBH, species = as.character(species), CSI = CSI, CCF = CCF, BAL = BAL
  )

  p <- .rijal2012_prepare(v$species, variant)

  .hd_warn_out_of_range(
    list(DBH = v$DBH),
    lower = list(DBH = p$DBH_min),
    upper = list(DBH = p$DBH_max),
    fn = fn
  )

  # NLME c5 not significant for GB and RO (printed "-"): NA in the table, 0 here.
  c5 <- ifelse(is.na(p$c5), 0, p$c5)
  expo <- p$c2 + p$c4 * log(v$CCF + 1) + c5 * v$BAL
  bad <- which(!is.na(expo) & expo <= 0)
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.fn {fn}}: exponent c2 + c4*ln(CCF + 1) + c5*BAL is non-positive for row{?s} {bad}.",
      "i" = "`CCF` or `BAL` are far outside the fitting data; check the inputs."
    ))
  }
  height <- 1.37 + (p$c0 + v$CSI^p$c3) * (1 - exp(-p$c1 * v$DBH))^expo

  out <- tibble::tibble(height = height)
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$Species), NA_character_, model_id)
  }
  out
}


# internal
# Returns one parameter row of `variant` ("gnls" or "nlme") per input row; NA
# species give an all-NA row.
.rijal2012_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Rijal2012")
  assert_required_cols(
    pars,
    c("Species", "variant", "c0", "c1", "c2", "c3", "c4", "c5", "DBH_min", "DBH_max"),
    object = "parameters_hd_Rijal2012"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Rijal et al. (2012) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
