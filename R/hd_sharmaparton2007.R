#' Sharma and Parton (2007) DBH–height models for Ontario boreal tree species
#'
#' Vectorized implementation of the population-level individual-tree
#' height–diameter models of Sharma and Parton (2007) for boreal tree species
#' in northern Ontario. Predicts total tree height (m) from DBH (cm) and three
#' stand-level predictors: dominant stand height, basal area and stand density.
#'
#' \strong{Model scope (species coverage):} balsam fir (`ABIE.BAL`), balsam
#' poplar (`POPU.BAL`), black spruce (`PICE.MAR`), jack pine (`PINU.BAN`), red
#' pine (`PINU.RES`), trembling aspen (`POPU.TRE`), white birch (`BETU.PAP`)
#' and white spruce (`PICE.GLA`). Fitted to permanent growth study plots in
#' northern Ontario; tree origin (natural or planted) was recorded (p. 188),
#' but the source does not state whether planted trees were excluded.
#'
#' \strong{Model form} (eqs. 6 and 8, p. 190 and p. 192):
#' \deqn{H = 1.3 + \theta \, HDOM^{\delta}
#'   [1 - \exp(-\beta (TPH / BA)^{\phi} DBH)]^{\gamma}.}
#'
#' \strong{Height convention:} total tree height; DBH measured at 1.3 m, so
#' predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Population-level prediction:} coefficients are the fixed effects of
#' the mixed-effects fit (Method 2, Table 6, p. 195), evaluated with the
#' plot-level random effect on the asymptote set to zero (\eqn{u = 0}). This
#' gives the curve of a "typical" plot with the supplied stand
#' characteristics, as shown in Fig. 2 of the source. Plot-specific calibration
#' with measured heights (eq. 7) is not implemented.
#'
#' Rows with `NA` in any input return `NA` height. Inputs outside the range of
#' the fitting data (per-species DBH, Table 1; pooled plot BA, TPH and HDOM
#' ranges, p. 188) are computed and reported in a single warning.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.MAR"`; compact `"PICEMAR"` is accepted).
#' @param HDOM Numeric vector. Plot level. Dominant stand height (m): the
#'   "mean height of sampled dominant and codominant trees in a plot,
#'   irrespective of species" (source "SHT", p. 190). Must be `> 0`.
#' @param BA Numeric vector. Plot level. Total stand basal area (m²/ha) of all
#'   trees in the plot (source "BA/ha", p. 188 and p. 190). The source does not
#'   state a minimum DBH for this total. Must be `> 0`.
#' @param TPH Numeric vector. Plot level. Stand density (stems/ha),
#'   "calculated from the trees with DBH greater than or equal to 2.5 cm"
#'   (source "N/ha", p. 188). Must be `> 0`.
#'
#' @return A tibble with column `height` (m).
#'
#' @references
#' Sharma, M., and Parton, J. (2007). Height–diameter equations for boreal
#' tree species in Ontario using a mixed-effects modeling approach. Forest
#' Ecology and Management, 249(3), 187–198.
#' \doi{10.1016/j.foreco.2007.05.006}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' hd_sharmaparton2007(
#'   DBH = c(10, 20, 30),
#'   species = c("PICE.MAR", "PINU.BAN", "POPU.TRE"),
#'   HDOM = 20, BA = 25, TPH = 2500
#' )
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(12, 25, 18),
#'   species = c("ABIE.BAL", "PICE.GLA", "BETU.PAP"),
#'   HDOM = c(16, 16, 22), BA = c(30, 30, 18), TPH = c(3000, 3000, 1200)
#' )
#' dplyr::mutate(trees, hd_sharmaparton2007(DBH, species, HDOM, BA, TPH))
#'
#' @export
hd_sharmaparton2007 <- function(DBH, species, HDOM, BA, TPH) {
  fn <- "hd_sharmaparton2007"

  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(HDOM, "HDOM", gt = 0, allow_na = TRUE)
  assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE)
  assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE)

  if (length(DBH) == 0L) {
    return(tibble::tibble(height = numeric()))
  }
  v <- assert_len_compat(
    DBH = DBH,
    species = as.character(species),
    HDOM = HDOM,
    BA = BA,
    TPH = TPH
  )

  p <- .sharmaparton2007_prepare(v$species)

  .hd_warn_out_of_range(
    list(DBH = v$DBH, BA = v$BA, TPH = v$TPH, HDOM = v$HDOM),
    lower = list(DBH = p$DBH_min, BA = p$BA_min, TPH = p$TPH_min, HDOM = p$HDOM_min),
    upper = list(DBH = p$DBH_max, BA = p$BA_max, TPH = p$TPH_max, HDOM = p$HDOM_max),
    fn = fn
  )

  height <- .hd_cr_stand(
    v$DBH, v$HDOM, v$BA, v$TPH,
    p$theta, p$delta, p$beta, p$phi, p$gamma
  )

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height <= 0))) {
    # nocov start
    # Defensive: positive published coefficients give finite heights >= 1.3 m.
    cli::cli_abort("{.fn {fn}} produced non-finite or non-positive heights.")
    # nocov end
  }

  tibble::tibble(height = height)
}


# internal
# Returns one Method 2 (Table 6) parameter row per input row; NA species give
# an all-NA row.
.sharmaparton2007_prepare <- function(species, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_SharmaParton2007")
  assert_required_cols(
    pars,
    c(
      "Species", "method", "theta", "delta", "beta", "phi", "gamma",
      "DBH_min", "DBH_max", "BA_min", "BA_max", "TPH_min", "TPH_max",
      "HDOM_min", "HDOM_max"
    ),
    object = "parameters_hd_SharmaParton2007"
  )
  pars <- as.data.frame(pars[pars$method == 2L, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Sharma and Parton (2007) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
