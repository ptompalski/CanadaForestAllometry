#' Auger, DeBlois & Lambert (2025) DBH–height models for Québec plantations
#'
#' Vectorized implementation of the DBH–height relationships of Auger, DeBlois
#' and Lambert (2025; Avis technique SSS-10) for black spruce and jack pine
#' **plantations** in Québec. Predicts total tree height (m) from DBH (cm)
#' alone, or from DBH and a plot-level predictor: the plot mean DBH, or the
#' dominant height and dominant DBH.
#'
#' \strong{Model scope:} black spruce (`PICE.MAR`, source "EPN") and jack pine
#' (`PINU.BAN`, source "PIG") in monospecific plantations of the Québec
#' plantation monitoring network (p. 2). The models were not fitted to natural
#' stands; stand origin cannot be checked by the function.
#'
#' \strong{Variants} (chosen with `variant`):
#' \describe{
#'   \item{`"dbh"` (default)}{Korf form, eq. 10 (Tableau 7, p. 11), both
#'     species:
#'     \deqn{H = 1.3 + \alpha_1 \exp(-\alpha_2 DBH^{-\alpha_3}).}
#'     Estimates the mean height of the population for a given DBH; the
#'     authors note that its shape is unlikely to suit any particular plot
#'     (pp. 4--5).}
#'   \item{`"plot_mean_dbh"`}{Requires `DMEAN`. Jack pine, eq. 11
#'     (Chapman–Richards) and black spruce, eq. 12 (Hossfeld II) (Tableau 8,
#'     p. 14):
#'     \deqn{PIG: H = 1.3 + (\alpha_{10} + \alpha_{11} MD)
#'       [1 - \exp(-(\alpha_{20} + \alpha_{21} MD) DBH)]^{\alpha_3};}
#'     \deqn{EPN: H = 1.3 + (\alpha_{10} + \alpha_{11} MD)
#'       \frac{DBH^{\alpha_3}}{(\alpha_{20} + \alpha_{21} MD) + DBH^{\alpha_3}},}
#'     where \eqn{MD} is `DMEAN`.}
#'   \item{`"dominant"`}{Requires `HDOM` and `DDOM`. Jack pine, eq. 13
#'     and black spruce, eq. 14 (Tableau 9, p. 17), the same forms with
#'     \eqn{\alpha_{10} + \alpha_{11} HDOM} as asymptote term and
#'     \eqn{\alpha_{20} + \alpha_{22} DD} as rate/scale term, where \eqn{DD}
#'     is `DDOM`.}
#' }
#' Predictors not used by the selected variant are ignored with a warning.
#'
#' \strong{Height convention:} total tree height; DBH is measured at 1.3 m and
#' every form tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Fit:} weighted fixed-effects nonlinear regressions; a plot random
#' effect did not improve the fit and none was included (pp. 5--7), so the
#' published coefficients are used directly. Variance-function parameters are
#' not used.
#'
#' \strong{Korf asymptote:} the published \eqn{\alpha_1} values are very large
#' (1,630,094 m for jack pine and 101,489 m for black spruce), so the `"dbh"`
#' curves keep increasing beyond the data. Extrapolation beyond the calibration
#' DBH range is not recommended.
#'
#' Rows with `NA` in any used input return `NA` height. Inputs outside the
#' per-species calibration ranges (Tableau 1, p. 4: DBH and, depending on the
#' variant, mean DBH, dominant height and dominant DBH) are computed and
#' reported in a single warning.
#'
#' No worked example or table of predicted heights is published; the
#' implementation is checked for plausibility only (see
#' `data-raw/Auger2025_model-spec.md`).
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `> 0`.
#' @param species Character vector. Tree level. NFI species codes,
#'   `"PICE.MAR"` or `"PINU.BAN"` (compact `"PICEMAR"` is accepted).
#' @param variant Model variant: `"dbh"` (default), `"plot_mean_dbh"` or
#'   `"dominant"`. See Details.
#' @param DMEAN Numeric vector. Plot level. Arithmetic mean DBH (cm) of the plot,
#'   used by `variant = "plot_mean_dbh"`: "le DHP moyen de tous les arbres de
#'   la placette" (source "DMOY", p. 6), i.e. the arithmetic (not quadratic)
#'   mean DBH of all trees in the plot. The source does not state whether
#'   natural ingress is included or a minimum DBH. Must be `>= 0`.
#' @param HDOM Numeric vector. Plot level. Dominant height (m), used by
#'   `variant = "dominant"`: mean height of the dominant trees of the plot
#'   (source "HDOM", p. 7), the dominant trees being "les 8 plus hauts arbres
#'   plantés faisant partie de l'étage des arbres dominants et bien répartis
#'   dans la placette (200 arbres/ha)" (400 m² plots, p. 2). Must be `> 0`.
#' @param DDOM Numeric vector. Plot level. Mean DBH (cm) of the same
#'   dominant trees (source "DDOM", p. 7), used by `variant = "dominant"`.
#'   Must be `>= 0`.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"auger2025_dbh"`, `"auger2025_plot_mean_dbh"` or
#'   `"auger2025_dominant"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Auger, I., DeBlois, J., and Lambert, M.-C. (2025). Relations
#' hauteur-diamètre, tarifs de cubage et équations de défilement pour
#' l'épinette noire et le pin gris en plantation au Québec. Gouvernement du
#' Québec, ministère des Ressources naturelles et des Forêts, Direction de la
#' recherche forestière. Avis technique SSS-10. 31 p.
#' \doi{10.82204/dcwg-5v65}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # DBH only (Korf)
#' hd_auger2025(DBH = c(5, 10, 20), species = c("PICE.MAR", "PINU.BAN", "PINU.BAN"))
#'
#' # With the plot mean DBH
#' hd_auger2025(
#'   DBH = c(5, 10, 20), species = "PINU.BAN",
#'   variant = "plot_mean_dbh", DMEAN = 12
#' )
#'
#' # With dominant height and dominant DBH, inside dplyr::mutate()
#' trees <- data.frame(
#'   DBH = c(6, 9, 12),
#'   species = "PICE.MAR",
#'   HDOM = 9, DDOM = 11
#' )
#' dplyr::mutate(
#'   trees,
#'   hd_auger2025(DBH, species, variant = "dominant",
#'                HDOM = HDOM, DDOM = DDOM,
#'                keep_model_id = TRUE)
#' )
#'
#' @export
hd_auger2025 <- function(DBH, species,
                         variant = c("dbh", "plot_mean_dbh", "dominant"),
                         DMEAN = NULL, HDOM = NULL, DDOM = NULL,
                         keep_model_id = FALSE) {
  fn <- "hd_auger2025"

  variant <- rlang::arg_match(variant)
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gt = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }

  preds <- list(DMEAN = DMEAN, HDOM = HDOM, DDOM = DDOM)
  needed <- switch(variant,
    dbh = character(),
    plot_mean_dbh = "DMEAN",
    dominant = c("HDOM", "DDOM")
  )
  supplied <- names(preds)[!vapply(preds, is.null, logical(1))]
  missing <- setdiff(needed, supplied)
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.code variant = \"{variant}\"} requires {.arg {missing}}."
    )
  }
  unused <- setdiff(supplied, needed)
  if (length(unused) > 0L) {
    cli::cli_warn(
      c(
        "{.arg {unused}} {?is/are} not used by {.code variant = \"{variant}\"} and {?was/were} ignored."
      ),
      class = "CanadaForestAllometry_hd_unused_predictor"
    )
  }
  if ("DMEAN" %in% needed) assert_numeric_vec(DMEAN, "DMEAN", gte = 0, allow_na = TRUE)
  if ("HDOM" %in% needed) assert_numeric_vec(HDOM, "HDOM", gt = 0, allow_na = TRUE)
  if ("DDOM" %in% needed) {
    assert_numeric_vec(DDOM, "DDOM", gte = 0, allow_na = TRUE)
  }

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- do.call(
    assert_len_compat,
    c(list(DBH = DBH, species = as.character(species)), preds[needed])
  )

  p <- .auger2025_prepare(v$species, variant)

  lower <- list(DBH = p$DBH_min)
  upper <- list(DBH = p$DBH_max)
  # Range columns in the parameter table use the original names.
  range_col <- c(DMEAN = "mean_DBH", HDOM = "HDOM", DDOM = "dominant_DBH")
  for (nm in needed) {
    lower[[nm]] <- p[[paste0(range_col[[nm]], "_min")]]
    upper[[nm]] <- p[[paste0(range_col[[nm]], "_max")]]
  }
  .hd_warn_out_of_range(v[c("DBH", needed)], lower = lower, upper = upper, fn = fn)

  if (variant == "dbh") {
    height <- 1.3 + p$a1 * exp(-p$a2 * v$DBH^(-p$a3))
  } else {
    if (variant == "plot_mean_dbh") {
      asym <- p$a10 + p$a11 * v$DMEAN
      rate <- p$a20 + p$a21 * v$DMEAN
    } else {
      asym <- p$a10 + p$a11 * v$HDOM
      rate <- p$a20 + p$a22 * v$DDOM
    }
    height <- ifelse(
      p$equation == "chapman_richards",
      1.3 + asym * (1 - exp(-rate * v$DBH))^p$a3,
      1.3 + asym * v$DBH^p$a3 / (rate + v$DBH^p$a3)
    )
  }

  # Reachable far outside the data, e.g. jack pine "dominant" with
  # DDOM >= 62.7 cm, where the rate term a20 + a22 * DDOM <= 0.
  # Heights are otherwise > 1 m for all valid inputs.
  bad <- is.na(height) & !is.na(v$DBH) & !is.na(p$Species)
  if (variant != "dbh") bad <- bad & !is.na(asym) & !is.na(rate)
  if (any(bad)) {
    cli::cli_abort(c(
      "{.fn {fn}} produced undefined heights for {sum(bad)} row{?s}.",
      "i" = "Check that the predictors are within the calibration ranges of the source (Tableau 1)."
    ))
  }

  out <- tibble::tibble(height = as.numeric(height))
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$Species), NA_character_, paste0("auger2025_", variant))
  }
  out
}


# internal
# Returns one parameter row of `variant` per input row; NA species give an
# all-NA row.
.auger2025_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Auger2025")
  assert_required_cols(
    pars,
    c(
      "Species", "variant", "equation",
      "a1", "a2", "a3", "a10", "a11", "a20", "a21", "a22",
      "DBH_min", "DBH_max", "mean_DBH_min", "mean_DBH_max",
      "HDOM_min", "HDOM_max", "dominant_DBH_min", "dominant_DBH_max"
    ),
    object = "parameters_hd_Auger2025"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Auger et al. (2025) plantation DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
