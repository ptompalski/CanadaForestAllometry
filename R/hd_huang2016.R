#' Huang, Yang and Aitkin (2016) DBH–height models for Alberta tree species
#'
#' Vectorized implementation of the population-level individual-tree
#' height–diameter models in Huang, Yang and Aitkin (2016; Alberta Tech. Rep.
#' T/609). Predicts total tree height (m) from DBH (cm), optionally by
#' natural-subregion group.
#'
#' \strong{Model scope (species coverage):} aspen (`POPU.TRE`), balsam poplar
#' (`POPU.BAL`), white birch (`BETU.PAP`), lodgepole pine (`PINU.CON`), white
#' spruce (`PICE.GLA`), black spruce (`PICE.MAR`), balsam fir (`ABIE.BAL`),
#' Douglas-fir (`PSEU.MEN`), tamarack (`LARI.LAR`) and jack pine (`PINU.BAN`).
#' Following the source species grouping (Table 1), whitebark pine
#' (`PINU.ALB`) and limber pine (`PINU.FLE`) use the lodgepole pine equations,
#' Engelmann spruce (`PICE.ENG`) uses white spruce, subalpine fir (`ABIE.LAS`)
#' uses balsam fir, and western (`LARI.OCC`) and alpine larch (`LARI.LYA`) use
#' tamarack; a one-time message reports the mapping. The source's combined
#' models are selected with `species = "DECID"` (all deciduous species),
#' `"CONIF"` (all conifers) or `"ALL"` (all species combined).
#'
#' \strong{Model forms:} deciduous species and `"DECID"` use eq. \[1\],
#' \deqn{H = 1.3 + b_1 [1 - \exp(-b_2 DBH)]^{b_3};}
#' conifers, `"CONIF"` and `"ALL"` use eq. \[2\],
#' \deqn{H = 1.3 + b_1 / (1 + \exp[b_2 + b_3 \ln(DBH + 0.01)]).}
#'
#' \strong{Height convention:} total tree height; DBH measured at 1.3 m, so
#' predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Population-level prediction:} coefficients are those of the base
#' (fixed-effects) models in Table 4, which the source recommends for trees
#' in plots without height measurements (Section 6). Plot-specific
#' calibration with the mixed models (Table 5) is not implemented.
#'
#' \strong{Variants (natural-subregion groups):} if `natural_subregion` is
#' `NULL` (default) the provincial models are used. Otherwise the model for the
#' species' natural-subregion group is used (e.g. white spruce: subregions
#' 7–9, 10, 11, and 1–6 and 12–21). Species fitted only at the provincial level
#' (`BETU.PAP`, `PSEU.MEN`, `LARI.LAR`, `PINU.BAN` and their mapped species, and
#' the combined models), and rows with `NA` `natural_subregion`, use the
#' provincial model. One-time messages report which variant is used.
#'
#' Rows with `NA` `DBH` or `species` return `NA` height. DBH values outside the
#' range of the fitting data of the selected model (Table 3) are computed and
#' reported in a single warning.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.GLA"`; compact `"PICEGLA"` is accepted), or one of `"DECID"`,
#'   `"CONIF"`, `"ALL"` for the source's combined models.
#' @param natural_subregion Optional. Plot level. Alberta natural subregion
#'   (Natural Regions Committee 2006), given either as the numeric code `1`–`21`
#'   or the character code (e.g. `"CM"` = Central Mixedwood, `"LF"` = Lower
#'   Foothills, `"UF"` = Upper Foothills), as listed in Table 2 of the source.
#'   Length 1 or `length(DBH)`. `NULL` (default) uses the provincial models.
#' @param keep_model_id Logical. If `TRUE`, add columns `model_id`
#'   (`"huang2016"`) and `subregion_group` (the natural-subregion group of the
#'   model used, e.g. `"7-9"`, or `"provincial"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id` and `subregion_group`.
#'
#' @references
#' Huang, S., Yang, Y., and Aitkin, D. (2016). Updated population and
#' plot-specific individual tree height-diameter models for major Alberta tree
#' species. Report #4, Tech. Rep. Pub. No. T/609. Forestry Division, Alberta
#' Agriculture and Forestry, Edmonton, Alberta.
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Provincial models
#' hd_huang2016(DBH = c(10, 25, 40), species = c("PICE.GLA", "POPU.TRE", "PINU.CON"))
#'
#' # Natural-subregion-group models (Lower Foothills), numeric or character code
#' hd_huang2016(DBH = 25, species = "PICE.MAR", natural_subregion = "LF")
#' hd_huang2016(DBH = 25, species = "PICE.MAR", natural_subregion = 11,
#'              keep_model_id = TRUE)
#'
#' # Combined conifer model
#' hd_huang2016(DBH = 30, species = "CONIF")
#'
#' @export
hd_huang2016 <- function(DBH, species, natural_subregion = NULL,
                         keep_model_id = FALSE) {
  fn <- "hd_huang2016"

  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }

  use_nsr <- !is.null(natural_subregion)
  nsr <- if (use_nsr) .huang2016_std_subregion(natural_subregion) else NA_integer_

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) {
      out$model_id <- character()
      out$subregion_group <- character()
    }
    return(out)
  }
  v <- assert_len_compat(
    DBH = DBH,
    species = as.character(species),
    natural_subregion = nsr
  )

  p <- .huang2016_prepare(v$species, v$natural_subregion)
  .huang2016_inform(p, v$natural_subregion, use_nsr, fn)

  .hd_warn_out_of_range(
    list(DBH = v$DBH),
    lower = list(DBH = p$DBH_min),
    upper = list(DBH = p$DBH_max),
    fn = fn
  )

  height <- .huang2016_height(v$DBH, p)

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height <= 0))) {
    # nocov start
    # Defensive: published coefficients give finite heights >= 1.3 m for DBH >= 0.
    cli::cli_abort("{.fn {fn}} produced non-finite or non-positive heights.")
    # nocov end
  }

  out <- tibble::tibble(height = height)
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$species_key), NA_character_, "huang2016")
    out$subregion_group <- p$subregion_group
  }
  out
}


# internal
# Source eqs. [1] (deciduous) and [2] (coniferous) with Table 4 coefficients.
.huang2016_height <- function(DBH, p) {
  h <- rep(NA_real_, length(DBH))
  eq1 <- !is.na(p$model_eq) & p$model_eq == 1L
  eq2 <- !is.na(p$model_eq) & p$model_eq == 2L
  h[eq1] <- 1.3 + p$b1[eq1] * (1 - exp(-p$b2[eq1] * DBH[eq1]))^p$b3[eq1]
  h[eq2] <- 1.3 + p$b1[eq2] /
    (1 + exp(p$b2[eq2] + p$b3[eq2] * log(DBH[eq2] + 0.01)))
  h
}


# internal
# Returns one parameter row per input row: the natural-subregion-group model
# when `nsr` is supplied and the species has one, otherwise the provincial
# model. Rows with NA species get an all-NA row.
.huang2016_prepare <- function(species, nsr, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Huang2016")
  assert_required_cols(
    pars,
    c(
      "Species", "species_key", "source_species", "species_group", "model_eq",
      "subregion_group", "natural_subregion", "b1", "b2", "b3",
      "DBH_min", "DBH_max"
    ),
    object = "parameters_hd_Huang2016"
  )
  pars <- as.data.frame(pars)

  key <- .huang2016_species_key(species)
  bad <- unique(key[!is.na(key) & !key %in% pars$species_key])
  if (length(bad) > 0L) {
    supported <- c(sort(unique(stats::na.omit(pars$Species))), "DECID", "CONIF", "ALL")
    cli::cli_abort(c(
      "Species not covered by the Huang et al. (2016) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: {paste(supported, collapse = ', ')}."
    ), call = call)
  }

  prov <- pars[is.na(pars$natural_subregion), , drop = FALSE]
  sub <- pars[!is.na(pars$natural_subregion), , drop = FALSE]

  out <- prov[match(key, prov$species_key), , drop = FALSE]
  i_sub <- match(
    paste(key, nsr),
    paste(sub$species_key, sub$natural_subregion)
  )
  has_sub <- !is.na(i_sub)
  out[has_sub, ] <- sub[i_sub[has_sub], , drop = FALSE]
  rownames(out) <- NULL
  out
}


# internal
# Upper-cases the combined-model keywords and standardizes NFI codes.
.huang2016_species_key <- function(species) {
  x <- toupper(trimws(species))
  combined <- !is.na(x) & x %in% c("DECID", "CONIF", "ALL")
  x[!combined] <- standardize_species_code(x[!combined], keep_all = FALSE)
  x
}


# internal
# Maps numeric (1-21) or character (e.g. "CM", "LF") natural-subregion codes
# (source Table 2) to integer codes. NA stays NA.
.huang2016_std_subregion <- function(x, call = rlang::caller_env()) {
  lookup <- .get_internal_data("hd_huang2016_natural_subregions")
  valid_txt <- paste0(
    lookup$code_numeric, " (", lookup$code_character, ")",
    collapse = ", "
  )
  invalid <- function(bad) {
    cli::cli_abort(c(
      "Invalid {.arg natural_subregion} value{?s}: {.val {bad}}.",
      "i" = "Use an Alberta 2006 natural subregion code: {valid_txt}."
    ), call = call)
  }

  if (is.factor(x)) x <- as.character(x)

  if (is.numeric(x)) {
    bad <- unique(x[!is.na(x) & !x %in% lookup$code_numeric])
    if (length(bad) > 0L) invalid(bad)
    return(as.integer(x))
  }

  if (is.character(x)) {
    y <- toupper(trimws(x))
    out <- lookup$code_numeric[match(y, toupper(lookup$code_character))]
    is_num <- is.na(out) & !is.na(y) & grepl("^[0-9]+$", y)
    out[is_num] <- lookup$code_numeric[match(as.integer(y[is_num]), lookup$code_numeric)]
    bad <- unique(x[!is.na(y) & is.na(out)])
    if (length(bad) > 0L) invalid(bad)
    return(as.integer(out))
  }

  if (all(is.na(x))) {
    return(rep(NA_integer_, length(x)))
  }
  cli::cli_abort(
    "{.arg natural_subregion} must be numeric or character.",
    call = call
  )
}


# internal
# One-time messages naming the variant used and any source species mapping.
# inform_once() interpolates in its own frame, so texts are built here.
.huang2016_inform <- function(p, nsr, use_nsr, fn) {
  known <- !is.na(p$species_key)

  if (!use_nsr) {
    inform_once(
      paste0(fn, "(): `natural_subregion` not supplied; using the provincial models."),
      .id = paste0(fn, "_provincial")
    )
  } else {
    inform_once(
      paste0(fn, "(): using natural-subregion-group models where available."),
      .id = paste0(fn, "_subregion")
    )
    fallback <- known & !is.na(nsr) & p$subregion_group == "provincial"
    if (any(fallback)) {
      sp <- sort(unique(p$species_key[fallback]))
      inform_once(
        paste0(
          fn, "(): no natural-subregion-group models for ",
          paste(sp, collapse = ", "), "; using the provincial model."
        ),
        .id = paste0(fn, "_fallback_", paste(sp, collapse = "_"))
      )
    }
    if (any(known & is.na(nsr))) {
      inform_once(
        paste0(fn, "(): rows with NA `natural_subregion` use the provincial models."),
        .id = paste0(fn, "_na_subregion")
      )
    }
  }

  mapped <- known & !is.na(p$Species) & p$source_species != p$species_group
  if (any(mapped)) {
    pars <- .get_internal_data("parameters_hd_Huang2016")
    primary <- pars[!is.na(pars$Species) & pars$source_species == pars$species_group, ]
  }
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
