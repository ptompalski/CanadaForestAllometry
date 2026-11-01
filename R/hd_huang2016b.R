#' Huang and Yang (2016) expanded DBH–height models for Alberta tree species
#'
#' Vectorized implementation of the population-level expanded individual-tree
#' height–diameter models in Huang and Yang (2016; Alberta Tech. Rep. T/610).
#' Predicts total tree height (m) from DBH (cm), the basal area (m²/ha) and
#' the mean height of the dominant and co-dominant trees (m) of the tree's
#' species.
#'
#' \strong{Model scope (species coverage):} aspen (`POPU.TRE`), balsam poplar
#' (`POPU.BAL`), white birch (`BETU.PAP`), lodgepole pine (`PINU.CON`), white
#' spruce (`PICE.GLA`), black spruce (`PICE.MAR`), Douglas-fir (`PSEU.MEN`),
#' tamarack (`LARI.LAR`) and jack pine (`PINU.BAN`). Following the source
#' species grouping (Table 1), whitebark pine (`PINU.ALB`) and limber pine
#' (`PINU.FLE`) use the lodgepole pine equation, Engelmann spruce (`PICE.ENG`)
#' uses white spruce, and western (`LARI.OCC`) and alpine larch (`LARI.LYA`)
#' use tamarack; a one-time message reports the mapping. The source's combined
#' models are selected with `species = "DECID"` (all deciduous species),
#' `"CONIF"` (all conifers) or `"ALL"` (all species combined).
#'
#' \strong{Balsam fir is not available:} in the source, the balsam fir base
#' coefficients (Table A1, part 2) are an exact copy of the black spruce
#' column, while the fit statistics in Table 5 show a different balsam fir
#' model. Balsam fir (`ABIE.BAL`) and subalpine fir (`ABIE.LAS`, grouped with
#' balsam fir) therefore raise an error.
#'
#' \strong{Model forms:} deciduous species and `"DECID"` use eq. \[1\],
#' \deqn{H = 1.3 + (b_1 + b_2 BA + b_3 HDOM) [1 - \exp(-b_4 DBH)]^{b_5};}
#' conifers, `"CONIF"` and `"ALL"` use eq. \[2\],
#' \deqn{H = 1.3 + (b_1 + b_2 BA + b_3 HDOM) / (1 + \exp[b_4 + b_5 \ln(DBH + 0.01)]).}
#'
#' \strong{Height convention:} total tree height; DBH measured at 1.3 m, so
#' predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Population-level prediction:} coefficients are those of the base
#' (fixed-effects) models in Table A1, which the source recommends for trees
#' in plots without height measurements (Section 5). Plot-specific
#' calibration with the mixed models is not implemented.
#'
#' Rows with `NA` in any input return `NA` height. DBH, `BA` or `HDOM` values
#' outside the range of the fitting data of the species (Table 2) are
#' computed and reported in a single warning.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.GLA"`; compact `"PICEGLA"` is accepted), or one of `"DECID"`,
#'   `"CONIF"`, `"ALL"` for the source's combined models.
#' @param HDOM Numeric vector. Plot level. Mean height (m) of all the dominant
#'   and co-dominant trees with measured heights in the plot, computed for the
#'   tree's species (source group, Table 1), or for all deciduous, all conifer
#'   or all trees with `"DECID"`, `"CONIF"` or `"ALL"` (source symbol
#'   \eqn{H_{DC}}; Section 6.2). Must be `> 0`.
#' @param BA Numeric vector. Plot level. Basal area (m²/ha) of the trees
#'   taller than 1.3 m of the tree's species (source group, Table 1), or of
#'   all deciduous, all conifer or all trees with `"DECID"`, `"CONIF"` or
#'   `"ALL"` (Section 6.1). Must be `>= 0`.
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"huang2016b"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`.
#'
#' @references
#' Huang, S., and Yang, Y. (2016). Expanded individual tree height prediction
#' models from tree diameter and other variables. Report #5, Tech. Rep. Pub.
#' No. T/610. Forestry Division, Alberta Agriculture and Forestry, Edmonton,
#' Alberta.
#'
#' @seealso [hd_huang2016()] for the DBH-only models of the same report
#'   series; [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Aspen worked example (T/610 Table 6, plot 1)
#' hd_huang2016b(DBH = c(29.7, 1.6), species = "POPU.TRE",
#'               HDOM = 29.9333, BA = 36.8225)
#'
#' trees <- data.frame(
#'   DBH = c(12, 25, 40),
#'   species = c("PICE.MAR", "PICE.GLA", "PINU.CON"),
#'   HDOM = c(15, 24, 20),
#'   BA = c(20, 25, 34)
#' )
#' dplyr::mutate(trees, hd_huang2016b(DBH, species, HDOM, BA))
#'
#' # Combined conifer model (HDOM and BA of all conifers)
#' hd_huang2016b(DBH = 30, species = "CONIF", HDOM = 21, BA = 37)
#'
#' @export
hd_huang2016b <- function(DBH, species, HDOM, BA, keep_model_id = FALSE) {
  fn <- "hd_huang2016b"

  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(HDOM, "HDOM", gt = 0, allow_na = TRUE)
  assert_numeric_vec(BA, "BA", gte = 0, allow_na = TRUE)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) out$model_id <- character()
    return(out)
  }
  v <- assert_len_compat(
    DBH = DBH,
    species = as.character(species),
    HDOM = HDOM,
    BA = BA
  )

  p <- .huang2016b_prepare(v$species)
  .huang2016b_inform(p, fn)

  .hd_warn_out_of_range(
    list(DBH = v$DBH, BA = v$BA, HDOM = v$HDOM),
    lower = list(DBH = p$DBH_min, BA = p$BA_min, HDOM = p$HDOM_min),
    upper = list(DBH = p$DBH_max, BA = p$BA_max, HDOM = p$HDOM_max),
    fn = fn
  )

  height <- .huang2016b_height(v$DBH, v$HDOM, v$BA, p)

  out <- tibble::tibble(height = height)
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$species_key), NA_character_, "huang2016b")
  }
  out
}


# internal
# Source eqs. [1] (deciduous) and [2] (coniferous) with Table A1 base
# coefficients. NA in -> NA out.
.huang2016b_height <- function(DBH, HDOM, BA, p) {
  h <- rep(NA_real_, length(DBH))
  asym <- p$b1 + p$b2 * BA + p$b3 * HDOM
  eq1 <- !is.na(p$model_eq) & p$model_eq == 1L
  eq2 <- !is.na(p$model_eq) & p$model_eq == 2L
  h[eq1] <- 1.3 + asym[eq1] * (1 - exp(-p$b4[eq1] * DBH[eq1]))^p$b5[eq1]
  h[eq2] <- 1.3 + asym[eq2] /
    (1 + exp(p$b4[eq2] + p$b5[eq2] * log(DBH[eq2] + 0.01)))
  h
}


# internal
# Species groups whose Table A1 base coefficients are unusable (FB column
# duplicates SB; see data-raw/Huang2016b_model-spec.md).
.huang2016b_excluded <- c("ABIE.BAL", "ABIE.LAS")


# internal
# Returns one parameter row per input row. Rows with NA species get an
# all-NA row.
.huang2016b_prepare <- function(species, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Huang2016b")
  assert_required_cols(
    pars,
    c(
      "Species", "species_key", "source_species", "species_group", "model_eq",
      "b1", "b2", "b3", "b4", "b5", "DBH_min", "DBH_max", "BA_min", "BA_max",
      "HDOM_min", "HDOM_max"
    ),
    object = "parameters_hd_Huang2016b"
  )
  pars <- as.data.frame(pars)

  key <- .huang2016_species_key(species)

  fir <- unique(key[!is.na(key) & key %in% .huang2016b_excluded])
  if (length(fir) > 0L) {
    cli::cli_abort(c(
      "No usable Huang & Yang (2016) base model for {.val {fir}}.",
      "x" = "The balsam fir (FB) base coefficients in the source (Table A1) duplicate those of black spruce (SB).",
      "i" = "Use {.fn hd_huang2016} or {.code species = \"CONIF\"} instead."
    ), call = call)
  }

  bad <- unique(key[!is.na(key) & !key %in% pars$species_key])
  if (length(bad) > 0L) {
    supported <- c(sort(unique(stats::na.omit(pars$Species))), "DECID", "CONIF", "ALL")
    cli::cli_abort(c(
      "Species not covered by the Huang & Yang (2016) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(supported, collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(key, pars$species_key), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# internal
# One-time messages for source species mappings (Table 1).
# inform_once() interpolates in its own frame, so texts are built here.
.huang2016b_inform <- function(p, fn) {
  mapped <- !is.na(p$Species) & p$source_species != p$species_group
  if (!any(mapped)) {
    return(invisible(NULL))
  }
  pars <- .get_internal_data("parameters_hd_Huang2016b")
  primary <- pars[!is.na(pars$Species) & pars$source_species == pars$species_group, ]
  for (sp in sort(unique(p$Species[mapped]))) {
    grp <- p$species_group[match(sp, p$Species)]
    target <- primary$Species[match(grp, primary$species_group)]
    inform_once(
      paste0(
        fn, "(): ", sp, " is predicted with the ", grp, " (", target,
        ") equation (source species grouping, Table 1)."
      ),
      .id = paste0(fn, "_map_", sp)
    )
  }
  invisible(NULL)
}
