#' Staudhammer and LeMay (2000) DBH–height models for coastal British
#' Columbia
#'
#' Vectorized implementation of the DBH–height models of Staudhammer and
#' LeMay (2000) for western redcedar (`THUJ.PLI`), red alder (`ALNU.RUB`),
#' Douglas-fir (`PSEU.MEN`) and western hemlock (`TSUG.HET`). Predicts total
#' tree height (m) from DBH (cm) alone, or from DBH and stand-density
#' measures.
#'
#' \strong{Model scope:} fitted to the 1995 inventory of the UBC Malcolm Knapp
#' Research Forest (about 5000 ha, Coastal Western Hemlock zone, coastal BC):
#' 327 plots in 82 clusters, heights measured on a subjectively chosen subset
#' of 763 trees (p. 304). The source alder model ("D") combines *Alnus rubra*
#' and *A. tenuifolia*; it is used here for red alder (`ALNU.RUB`) only. Alder
#' had only 28 trees on 16 plots.
#'
#' \strong{Model form} (eq. 2, Yang et al. 1978; p. 305):
#' \deqn{H = 1.3 + E_1 [1 - \exp(E_2 \, DBH^{E_3})].}
#' The intercept is constrained to 1.3 m at DBH = 0.
#'
#' \strong{Variants} (chosen with `variant`):
#' \itemize{
#'   \item `"dbh"` (default): eq. 2, with \eqn{E_1}, \eqn{E_2}, \eqn{E_3}
#'     species constants (Table 5, p. 308). All four species.
#'   \item `"density"`: parameter prediction (Table 5, p. 308). For red alder
#'     (eq. 2a), \eqn{E_k = c_{k0} + c_{k1} TPH + c_{k2} BA}. For western
#'     redcedar, Douglas-fir and western hemlock (eq. 2b),
#'     \eqn{E_k = c_{k0} + c_{k1} R + c_{k2} R^2}, where \eqn{R = DBH / DMAX}
#'     is the tree's relative DBH. `TPH` and `BA` are required only when the
#'     data include red alder, and `DMAX` only when they include the other
#'     species.
#' }
#' Within a plot (fixed `DMAX`), eq. 2b heights increase with DBH up to a
#' relative DBH of about 0.85–0.95 and then decrease slightly for the plot's
#' largest trees; this follows from the published quadratic in \eqn{R}.
#' Predictors not used by the selected variant or species are ignored with a
#' warning. Coefficients are ordinary nonlinear least-squares fits (SAS PROC
#' NLIN); no calibration information is published.
#'
#' \strong{Corrected coefficient:} for western redcedar (eq. 2b), the slope of
#' \eqn{E_2} on \eqn{R^2} is printed as 0.1551. With that value \eqn{E_2}
#' becomes positive for \eqn{R > 0.23}, giving negative heights (e.g. -47 m at
#' DBH = 30 cm, \eqn{R} = 0.5), which is inconsistent with the reported fit
#' (RMSE 4.308 m, Table 2). The value 0.01551 (a dropped zero) is used: it
#' gives plausible heights and matches the DBH-only \eqn{E_2} at \eqn{R = 1}.
#' This correction is an inference, not a published erratum.
#'
#' \strong{Validity:} far outside the fitting data the `"density"` parameters
#' can become invalid (\eqn{E_1 \le 0}, \eqn{E_2 \ge 0} or
#' \eqn{E_3 \le 0}): Douglas-fir with \eqn{R < 0.062}, redcedar with
#' \eqn{R < 0.019}, hemlock with \eqn{R < 0.008}, and red alder with
#' `TPH` above about 7900 stems/ha or large `BA` with low `TPH`. Such rows
#' raise an error; use `variant = "dbh"` for them. Inputs outside the
#' per-species ranges of the fitting data (Table 1, p. 304: DBH, and for
#' `"density"` also `TPH`, `BA` and \eqn{R}) are computed and reported in a
#' single warning. Rows with `NA` in any used input return `NA` height. The
#' source has no worked example, so the implementation is checked for
#' plausibility only.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m),
#'   in cm. Must be `>= 0`. Only trees above 2 cm DBH were recorded (p. 304).
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PSEU.MEN"`; compact `"PSEUMEN"` is accepted). Supported: `ALNU.RUB`,
#'   `PSEU.MEN`, `THUJ.PLI`, `TSUG.HET`.
#' @param variant Model variant: `"dbh"` (default; eq. 2) or `"density"`
#'   (eq. 2a for red alder, eq. 2b for the other species).
#' @param TPH Optional numeric vector. Plot level. Stems per hectare, all
#'   species combined (source "TSTEMSHA", p. 304; trees with DBH > 2 cm, live
#'   or dead status not stated). Must be `> 0`. Required by
#'   `variant = "density"` for red alder.
#' @param BA Optional numeric vector. Plot level. Basal area per hectare
#'   (m²/ha), all species combined (source "TBAHA", p. 304; same tree basis as
#'   `TPH`). Must be `> 0`. Required by `variant = "density"` for red alder.
#' @param DMAX Optional numeric vector. Plot level. Maximum DBH (cm) of all
#'   trees on the plot, all species; used to compute the relative DBH
#'   (source "REL_dbh" = "dbh divided by the maximum dbh for all species",
#'   p. 304). Must be `> 0` and `>= DBH`. Required by `variant = "density"`
#'   for western redcedar, Douglas-fir and western hemlock.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"staudhammerlemay2000_dbh"` or `"staudhammerlemay2000_density"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Staudhammer, C., and LeMay, V. (2000). Height prediction equations using
#' diameter and stand density measures. The Forestry Chronicle, 76(2),
#' 303–309. \doi{10.5558/tfc76303-2}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # DBH only (eq. 2)
#' hd_staudhammerlemay2000(
#'   DBH = c(10, 30, 60),
#'   species = c("PSEU.MEN", "TSUG.HET", "THUJ.PLI")
#' )
#'
#' # Stand density: relative DBH for conifers, stems/ha and basal area for alder
#' hd_staudhammerlemay2000(
#'   DBH = c(10, 30, 60, 15),
#'   species = c("PSEU.MEN", "TSUG.HET", "THUJ.PLI", "ALNU.RUB"),
#'   variant = "density",
#'   TPH = 1500, BA = 50, DMAX = 80,
#'   keep_model_id = TRUE
#' )
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(12, 45, 80),
#'   species = c("TSUG.HET", "PSEU.MEN", "PSEU.MEN"),
#'   DMAX = 80
#' )
#' dplyr::mutate(
#'   trees,
#'   hd_staudhammerlemay2000(DBH, species, variant = "density", DMAX = DMAX)
#' )
#'
#' @export
hd_staudhammerlemay2000 <- function(DBH, species,
                                    variant = c("dbh", "density"),
                                    TPH = NULL, BA = NULL, DMAX = NULL,
                                    keep_model_id = FALSE) {
  fn <- "hd_staudhammerlemay2000"

  variant <- rlang::arg_match(variant)
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE, allow_null = TRUE)
  assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE, allow_null = TRUE)
  assert_numeric_vec(DMAX, "DMAX", gt = 0, allow_na = TRUE, allow_null = TRUE)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }

  preds <- list(TPH = TPH, BA = BA, DMAX = DMAX)
  supplied <- names(preds)[!vapply(preds, is.null, logical(1))]
  v <- do.call(
    assert_len_compat,
    c(list(DBH = DBH, species = as.character(species)), preds[supplied])
  )
  n <- length(v$DBH)

  p <- .staudhammerlemay2000_prepare(v$species, variant)
  alder <- !is.na(p$Species) & p$Species == "ALNU.RUB"
  conif <- !is.na(p$Species) & !alder

  needed <- character()
  if (variant == "density") {
    needed <- c(if (any(alder)) c("TPH", "BA"), if (any(conif)) "DMAX")
    missing <- setdiff(needed, supplied)
    if (length(missing) > 0L) {
      cli::cli_abort(c(
        "{.code variant = \"density\"} requires {.arg {missing}} for the supplied species.",
        "i" = "Red alder (eq. 2a) uses {.arg TPH} and {.arg BA}; western redcedar, Douglas-fir and western hemlock (eq. 2b) use {.arg DMAX}."
      ))
    }
  }
  unused <- setdiff(supplied, needed)
  if (length(unused) > 0L) {
    cli::cli_warn(
      "{.arg {unused}} {?is/are} not used by {.code variant = \"{variant}\"} for the supplied species and {?was/were} ignored.",
      class = "CanadaForestAllometry_hd_unused_predictor"
    )
  }

  na <- rep(NA_real_, n)
  TPH <- if ("TPH" %in% needed) v$TPH else na
  BA <- if ("BA" %in% needed) v$BA else na
  rel <- na
  if ("DMAX" %in% needed) {
    over <- which(conif & !is.na(v$DBH) & !is.na(v$DMAX) & v$DBH > v$DMAX)
    if (length(over) > 0L) {
      cli::cli_abort(c(
        "{.arg DBH} exceeds {.arg DMAX} for row{?s} {over}.",
        "i" = "{.arg DMAX} is the maximum DBH of all trees on the plot."
      ))
    }
    rel <- ifelse(conif, v$DBH / v$DMAX, NA_real_)
  }

  x_rng <- list(DBH = v$DBH)
  lower <- list(DBH = p$DBH_min)
  upper <- list(DBH = p$DBH_max)
  if ("TPH" %in% needed) {
    x_rng$TPH <- ifelse(alder, TPH, NA_real_)
    x_rng$BA <- ifelse(alder, BA, NA_real_)
    lower[c("TPH", "BA")] <- list(p$TPH_min, p$BA_min)
    upper[c("TPH", "BA")] <- list(p$TPH_max, p$BA_max)
  }
  if ("DMAX" %in% needed) {
    x_rng[["DBH / DMAX"]] <- rel
    lower[["DBH / DMAX"]] <- p$rel_DBH_min
    upper[["DBH / DMAX"]] <- p$rel_DBH_max
  }
  .hd_warn_out_of_range(x_rng, lower = lower, upper = upper, fn = fn)

  # Terms absent from a species' equation are NA in the params table and
  # contribute 0 (whatever the, possibly unsupplied, predictor value).
  term <- function(coef, x) ifelse(is.na(coef), 0, coef * x)
  par <- function(k) {
    cf <- function(s) p[[paste0(k, "_", s)]]
    cf("0") + term(cf("TPH"), TPH) + term(cf("BA"), BA) +
      term(cf("rel"), rel) + term(cf("rel2"), rel^2)
  }
  E1 <- par("e1")
  E2 <- par("e2")
  E3 <- par("e3")

  bad <- which(
    !is.na(v$DBH) & v$DBH > 0 & !is.na(E1) & !is.na(E2) & !is.na(E3) &
      (E1 <= 0 | E2 >= 0 | E3 <= 0)
  )
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.fn {fn}}: the {.code \"density\"} parameters are invalid (E1 <= 0, E2 >= 0 or E3 <= 0) for row{?s} {bad}.",
      "i" = "The stand predictors are far outside the fitting data (Table 1); use {.code variant = \"dbh\"} or check the inputs."
    ))
  }
  height <- ifelse(v$DBH == 0, 1.3, 1.3 + E1 * (1 - exp(E2 * v$DBH^E3)))
  height[is.na(E1) | is.na(E2) | is.na(E3)] <- NA_real_

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height < 1.3))) {
    # nocov start
    # Defensive: E1 > 0, E2 < 0 and E3 > 0 give heights in [1.3, 1.3 + E1).
    cli::cli_abort("{.fn {fn}} produced non-finite heights or heights below 1.3 m.")
    # nocov end
  }

  out <- tibble::tibble(height = as.numeric(height))
  if (keep_model_id) {
    out$model_id <- ifelse(
      is.na(p$Species), NA_character_, paste0("staudhammerlemay2000_", variant)
    )
  }
  out
}


# internal
# Returns one parameter row of `variant` ("dbh" or "density") per input row;
# NA species give an all-NA row.
.staudhammerlemay2000_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_StaudhammerLeMay2000")
  assert_required_cols(
    pars,
    c(
      "Species", "variant",
      paste0(rep(c("e1", "e2", "e3"), each = 5), "_", c("0", "TPH", "BA", "rel", "rel2")),
      "DBH_min", "DBH_max", "TPH_min", "TPH_max", "BA_min", "BA_max",
      "rel_DBH_min", "rel_DBH_max"
    ),
    object = "parameters_hd_StaudhammerLeMay2000"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Staudhammer and LeMay (2000) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
