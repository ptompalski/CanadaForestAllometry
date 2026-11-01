#' Sharma and Zhang (2004) DBH–height models for Ontario jack pine and black spruce
#'
#' Vectorized implementation of the individual-tree height–diameter models of
#' Sharma and Zhang (2004) for jack pine and black spruce in northern Ontario.
#' Predicts total tree height (m) from DBH (cm), optionally with stand basal
#' area and stand density.
#'
#' \strong{Model scope (species coverage):} jack pine (`PINU.BAN`) and black
#' spruce (`PICE.MAR`). Fitted to permanent growth study plots (400 m²) across
#' much of northern Ontario; trees of natural and planted origin were both
#' included (p. 443).
#'
#' \strong{Variants} (chosen with `variant`; Tables 3 and 4, p. 445):
#' \itemize{
#'   \item `"eq2"` (default): eq. (2), p. 444, the model the source recommends
#'     \deqn{H = 1.3 + \alpha \, BA^{\delta}
#'       [1 - \exp(-\beta \, TPH^{\phi} \, DBH)].}
#'   \item `"eq5"`: eq. (5), p. 444, the DBH-only Chapman–Richards model
#'     (form of Peng et al. 2001, refitted by the source)
#'     \deqn{H = 1.3 + \alpha [1 - \exp(-\beta \, DBH)]^{\gamma}.}
#' }
#' Eqs. (3) and (4) are not implemented: with the published coefficients they
#' predict a height that does not change with DBH.
#'
#' \strong{Height convention:} total tree height; the 1.3 m breast height is
#' added so that predicted height equals 1.3 m when DBH is 0 (p. 444).
#'
#' \strong{Fit:} ordinary (unweighted) nonlinear least squares; there are no
#' random effects, so the predictions are the published fixed curves.
#'
#' Rows with `NA` in any input used by the variant return `NA` height. Inputs
#' outside the per-species range of the data (DBH, Table 2; plot BA and
#' stems/ha, Table 1) are computed and reported in a single warning.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes
#'   (`"PINU.BAN"` or `"PICE.MAR"`; compact `"PINUBAN"` is accepted).
#' @param variant Model variant: `"eq2"` (default; DBH, `BA` and `TPH`) or
#'   `"eq5"` (DBH only).
#' @param BA Numeric vector. Plot level. Total stand basal area (m²/ha) of all
#'   trees in the plot, all species (source "BA", "total tree basal area",
#'   p. 443). Must be `> 0`. Required by `variant = "eq2"`.
#' @param TPH Numeric vector. Plot level. Stand density (stems/ha) "of the
#'   trees with DBH \eqn{\ge} 2.5 cm" (source "SD", p. 443). Must be `> 0`.
#'   Required by `variant = "eq2"`.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"sharmazhang2004_eq2"` or `"sharmazhang2004_eq5"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Sharma, M., and Zhang, S.Y. (2004). Height–diameter models using stand
#' characteristics for *Pinus banksiana* and *Picea mariana*. Scandinavian
#' Journal of Forest Research, 19(5), 442–451.
#' \doi{10.1080/02827580410030163}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' hd_sharmazhang2004(
#'   DBH = c(10, 20, 30),
#'   species = c("PINU.BAN", "PICE.MAR", "PICE.MAR"),
#'   BA = 25, TPH = 1500
#' )
#'
#' # DBH only
#' hd_sharmazhang2004(DBH = c(10, 20, 30), species = "PINU.BAN", variant = "eq5")
#'
#' # Inside dplyr::mutate() with tree and plot data
#' trees <- data.frame(
#'   DBH = c(12, 25, 18),
#'   species = c("PINU.BAN", "PICE.MAR", "PICE.MAR"),
#'   BA = c(30, 30, 18), TPH = c(3000, 3000, 1200)
#' )
#' dplyr::mutate(trees, hd_sharmazhang2004(DBH, species, BA = BA, TPH = TPH))
#'
#' @export
hd_sharmazhang2004 <- function(DBH, species,
                               variant = c("eq2", "eq5"),
                               BA = NULL, TPH = NULL,
                               keep_model_id = FALSE) {
  fn <- "hd_sharmazhang2004"

  variant <- rlang::arg_match(variant)
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(BA, "BA", gt = 0, allow_na = TRUE, allow_null = TRUE)
  assert_numeric_vec(TPH, "TPH", gt = 0, allow_na = TRUE, allow_null = TRUE)

  preds <- list(BA = BA, TPH = TPH)
  supplied <- names(preds)[!vapply(preds, is.null, logical(1))]
  needed <- if (variant == "eq2") c("BA", "TPH") else character()
  missing <- setdiff(needed, supplied)
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.code variant = \"eq2\"} requires {.arg {missing}}.",
      "i" = "Use {.code variant = \"eq5\"} to predict height from DBH only."
    ))
  }
  unused <- setdiff(supplied, needed)
  if (length(unused) > 0L) {
    cli::cli_warn(
      "{.arg {unused}} {?is/are} not used by {.code variant = \"{variant}\"} and {?was/were} ignored.",
      class = "CanadaForestAllometry_hd_unused_predictor"
    )
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

  p <- .sharmazhang2004_prepare(v$species, variant)

  x_rng <- list(DBH = v$DBH)
  lower <- list(DBH = p$DBH_min)
  upper <- list(DBH = p$DBH_max)
  if (variant == "eq2") {
    x_rng[c("BA", "TPH")] <- list(v$BA, v$TPH)
    lower[c("BA", "TPH")] <- list(p$BA_min, p$TPH_min)
    upper[c("BA", "TPH")] <- list(p$BA_max, p$TPH_max)
  }
  .hd_warn_out_of_range(x_rng, lower = lower, upper = upper, fn = fn)

  height <- if (variant == "eq2") {
    1.3 + p$alpha * v$BA^p$delta * (1 - exp(-p$beta * v$TPH^p$phi * v$DBH))
  } else {
    1.3 + p$alpha * (1 - exp(-p$beta * v$DBH))^p$gamma
  }

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height < 1.3))) {
    # nocov start
    # Defensive: alpha, beta (and gamma) > 0 give heights in [1.3, 1.3 + asymptote).
    cli::cli_abort("{.fn {fn}} produced non-finite heights or heights below 1.3 m.")
    # nocov end
  }

  out <- tibble::tibble(height = as.numeric(height))
  if (keep_model_id) {
    out$model_id <- ifelse(
      is.na(p$Species), NA_character_, paste0("sharmazhang2004_", variant)
    )
  }
  out
}


# internal
# Returns one parameter row of `variant` ("eq2" or "eq5") per input row; NA
# species give an all-NA row.
.sharmazhang2004_prepare <- function(species, variant, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_SharmaZhang2004")
  assert_required_cols(
    pars,
    c(
      "Species", "variant", "alpha", "beta", "gamma", "delta", "phi",
      "DBH_min", "DBH_max", "BA_min", "BA_max", "TPH_min", "TPH_max"
    ),
    object = "parameters_hd_SharmaZhang2004"
  )
  pars <- as.data.frame(pars[pars$variant == variant, , drop = FALSE])

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Sharma and Zhang (2004) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
