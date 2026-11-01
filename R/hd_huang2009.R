#' Huang, Meng and Yang (2009) DBH–height models for Alberta black spruce
#'
#' Vectorized implementation of the population-level Chapman–Richards
#' height–diameter models of Huang, Meng and Yang (2009) for black spruce
#' (`PICE.MAR`) in Alberta. Predicts total tree height (m) from DBH (cm)
#' alone (base model 9), or from DBH and top height (model 10), or from DBH,
#' top height and stand basal area (model 11).
#'
#' \strong{Model scope:} black spruce only. Fitted to 93 Alberta permanent
#' sample plots (917 trees) in the Lower Foothills natural region and
#' validated on 71 plots in the Boreal Mixedwood and Upper Foothills (p. 4).
#'
#' \strong{Model forms} (eqs. 9–11, pp. 4–5):
#' \deqn{(9): H = 1.3 + \beta_1 [1 - \exp(-\beta_2 DBH)]^{\beta_3};}
#' \deqn{(10): H = 1.3 + (\beta_1 + \beta_4 HTOP) [1 - \exp(-\beta_2 DBH)]^{\beta_3};}
#' \deqn{(11): H = 1.3 + (\beta_1 + \beta_4 HTOP + \beta_5 BA)
#'   [1 - \exp(-\beta_2 DBH)]^{\beta_3}.}
#'
#' \strong{Height convention:} total tree height; DBH measured at 1.3 m, so
#' predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Population-level prediction:} coefficients are the fixed effects of
#' the nonlinear mixed models (Table 2, p. 13), evaluated with the plot random
#' effects on \eqn{\beta_1} and \eqn{\beta_3} set to zero (the source's
#' "fixed only" predictions, Table 3). Plot-specific calibration with measured
#' heights is not implemented. The authors report that fixed-only predictions
#' are biased for individual plots and on average (Table 4: mean bias −7.5%,
#' −2.1% and −3.3% of mean height on the validation data for models 9, 10 and
#' 11), and that the base model (9) gave the most accurate calibrated
#' predictions despite ranking last by AIC/BIC. The ratio correction they
#' describe (eqs. 29–30) has no published value and is not applied.
#'
#' \strong{Variants:} if `HTOP` and `BA` are `NULL` (default), model 9 is
#' used; if only `HTOP` is supplied, model 10; if both are supplied, model 11.
#' Supplying `BA` without `HTOP` is an error. A one-time message reports the
#' variant used.
#'
#' Rows with `NA` in any input return `NA` height. Inputs outside the range of
#' the fitting data (Table 1: DBH and, where used, `HTOP` and `BA`) are
#' computed and reported in a single warning.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species code; only
#'   `"PICE.MAR"` is supported (compact `"PICEMAR"` is accepted).
#' @param HTOP Optional numeric vector. Plot level. Top height (m): "the
#'   average height of the 100 largest DBH trees per ha" (source "TopH",
#'   p. 5). This is not the mean height of dominant and codominant trees
#'   (`HDOM`). Must be `> 0`. Supply to use model 10, or with `BA` for
#'   model 11.
#' @param BA Optional numeric vector. Plot level. Stand basal area per ha
#'   (m²/ha) (source "BA", p. 5). The source does not state a minimum DBH or
#'   species basis for this total. Must be `> 0`. Requires `HTOP`.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"huang2009_m9"`, `"huang2009_m10"` or `"huang2009_m11"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Huang, S., Meng, S. X., and Yang, Y. (2009). Using nonlinear mixed model
#' technique to determine the optimal tree height prediction model for black
#' spruce. Modern Applied Science, 3(4), 3–18. \doi{10.5539/mas.v3n4p3}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()], [hd_huang2016()]
#' @examples
#' # Model 9: DBH only
#' hd_huang2009(DBH = c(7.9, 10.7, 20.3), species = "PICE.MAR")
#'
#' # Model 10: with top height
#' hd_huang2009(DBH = c(7.9, 10.7, 20.3), species = "PICE.MAR", HTOP = 14.5)
#'
#' # Model 11: with top height and basal area
#' hd_huang2009(
#'   DBH = c(7.9, 10.7, 20.3), species = "PICE.MAR",
#'   HTOP = 14.5, BA = 7.75, keep_model_id = TRUE
#' )
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(8, 15, 22),
#'   species = "PICE.MAR",
#'   HTOP = c(14, 14, 19), BA = c(10, 10, 25)
#' )
#' dplyr::mutate(trees, hd_huang2009(DBH, species, HTOP, BA))
#'
#' @export
hd_huang2009 <- function(DBH, species, HTOP = NULL, BA = NULL,
                         keep_model_id = FALSE) {
  fn <- "hd_huang2009"

  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  if (!is.null(BA) && is.null(HTOP)) {
    cli::cli_abort(c(
      "{.arg BA} requires {.arg HTOP} (model 11 uses both).",
      "i" = "Supply {.arg HTOP} alone for model 10, or neither for model 9."
    ))
  }
  if (!is.null(HTOP)) assert_numeric_vec(HTOP, "HTOP", gt = 0, allow_na = TRUE)
  if (!is.null(BA)) assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE)

  variant <- if (is.null(HTOP)) "m9" else if (is.null(BA)) "m10" else "m11"
  model_id <- paste0("huang2009_", variant)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- switch(variant,
    m9 = assert_len_compat(DBH = DBH, species = as.character(species)),
    m10 = assert_len_compat(DBH = DBH, species = as.character(species), HTOP = HTOP),
    m11 = assert_len_compat(
      DBH = DBH, species = as.character(species), HTOP = HTOP, BA = BA
    )
  )

  p <- .huang2009_hd_prepare(v$species, variant)

  inform_once(
    paste0(fn, "(): using model ", sub("m", "", variant, fixed = TRUE), switch(variant,
      m9 = " (DBH only).",
      m10 = " (DBH and top height `HTOP`).",
      m11 = " (DBH, top height `HTOP` and basal area `BA`)."
    )),
    .id = paste0(fn, "_", variant)
  )

  x <- list(DBH = v$DBH)
  lower <- list(DBH = p$DBH_min)
  upper <- list(DBH = p$DBH_max)
  asym <- p$beta1
  if (variant != "m9") {
    x$HTOP <- v$HTOP
    lower$HTOP <- p$HTOP_min
    upper$HTOP <- p$HTOP_max
    asym <- asym + p$beta4 * v$HTOP
  }
  if (variant == "m11") {
    x$BA <- v$BA
    lower$BA <- p$BA_min
    upper$BA <- p$BA_max
    asym <- asym + p$beta5 * v$BA
  }
  .hd_warn_out_of_range(x, lower = lower, upper = upper, fn = fn)

  height <- 1.3 + asym * (1 - exp(-p$beta2 * v$DBH))^p$beta3

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
# Returns the parameter row of `variant` ("m9", "m10" or "m11") per input
# row; NA species give an all-NA row.
.huang2009_hd_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Huang2009")
  assert_required_cols(
    pars,
    c(
      "Species", "variant", "beta1", "beta2", "beta3", "beta4", "beta5",
      "DBH_min", "DBH_max", "HTOP_min", "HTOP_max", "BA_min", "BA_max"
    ),
    object = "parameters_hd_Huang2009"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Huang et al. (2009) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
