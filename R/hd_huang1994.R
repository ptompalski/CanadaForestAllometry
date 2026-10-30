#' Huang, Titus, Lakusta and Held (1994) DBH–height models for Alberta tree species
#'
#' Vectorized implementation of the ecologically based individual-tree
#' height–diameter models in Huang, Titus, Lakusta and Held (1994; Alberta
#' Pub. T/291). Predicts total tree height (m) from DBH (cm), optionally by
#' natural-region group.
#'
#' \strong{Model scope (species coverage):} aspen (`POPU.TRE`), balsam poplar
#' (`POPU.BAL`), white birch (`BETU.PAP`), lodgepole pine (`PINU.CON`), jack
#' pine (`PINU.BAN`), white spruce (`PICE.GLA`), Engelmann spruce
#' (`PICE.ENG`), black spruce (`PICE.MAR`), balsam and subalpine fir
#' (`ABIE.BAL`, `ABIE.LAS`; fitted together in the source), Douglas-fir
#' (`PSEU.MEN`) and tamarack (`LARI.LAR`). The source's natural-region
#' "Softwood group" and "Hardwood group" models are selected with
#' `species = "CONIF"` and `"DECID"`; they exist only by natural-region group
#' and therefore require `natural_subregion`.
#'
#' \strong{Model form} (p. 2):
#' \deqn{H = 1.3 + a [1 - \exp(-b\, DBH)]^{c},}
#' fitted by weighted nonlinear least squares (weights \eqn{1/DBH}).
#'
#' \strong{Height convention:} total tree height; DBH outside bark at 1.3 m,
#' so predicted height tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Variants (natural-region groups):} if `natural_subregion` is `NULL`
#' (default) the provincial models (Table 2) are used. Otherwise the model for
#' the species' natural-region group (Table 1) is used; the source recommends
#' these where the natural region is known (p. 16). The 1994 natural regions
#' 1–20 share their numeric codes with the 2006 natural subregions; 2006
#' subregion 21 (Upper Boreal Highlands), split from 1994 region 6 (Boreal
#' Highlands), uses the region-6 models. The grassland regions 17–20 and rows
#' with `NA` `natural_subregion` have no regional models and use the
#' provincial model, as do species fitted only at the provincial level
#' (`BETU.PAP`, `PINU.BAN`, `PICE.ENG`, `PSEU.MEN`, `LARI.LAR`). For `"CONIF"`
#' and `"DECID"`, which have no provincial model, such rows return `NA` height
#' with a warning. One-time messages report which variant is used.
#'
#' Rows with `NA` `DBH` or `species` return `NA` height. DBH values outside the
#' provincial range of the fitting data for the species (Table 3) are computed
#' and reported in a single warning; no ranges are published for the
#' softwood/hardwood groups.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground), outside bark, in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.GLA"`; compact `"PICEGLA"` is accepted), or `"CONIF"` / `"DECID"`
#'   for the source's softwood / hardwood group models.
#' @param natural_subregion Optional. Plot level. Alberta natural region
#'   (source Appendix 2), given as the 2006 natural subregion numeric code
#'   `1`–`21` or character code (e.g. `"CM"` = Central Mixedwood, `"LF"` =
#'   Lower Foothills), as in [hd_huang2016()]. Length 1 or `length(DBH)`.
#'   `NULL` (default) uses the provincial models.
#' @param keep_model_id Logical. If `TRUE`, add columns `model_id`
#'   (`"huang1994"`) and `subregion_group` (the natural-region group of the
#'   model used, e.g. `"7,8,10"`, or `"provincial"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id` and `subregion_group`.
#'
#' @references
#' Huang, S., Titus, S.J., Lakusta, T.W., and Held, R.J. (1994). Ecologically
#' based individual tree height-diameter models for major Alberta tree
#' species. Report #2, Pub. No. T/291. Alberta Environmental Protection, Land
#' and Forest Services, Edmonton, Alberta.
#'
#' @seealso [hd_huang2016()], [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Provincial models
#' hd_huang1994(DBH = c(10, 25, 40), species = c("PICE.GLA", "POPU.TRE", "PINU.CON"))
#'
#' # Natural-region-group models (Lower Foothills), numeric or character code
#' hd_huang1994(DBH = 25, species = "PICE.MAR", natural_subregion = "LF")
#' hd_huang1994(DBH = 25, species = "PICE.MAR", natural_subregion = 11,
#'              keep_model_id = TRUE)
#'
#' # Softwood group model (natural-region groups only)
#' hd_huang1994(DBH = 30, species = "CONIF", natural_subregion = "CM")
#'
#' @export
hd_huang1994 <- function(DBH, species, natural_subregion = NULL,
                         keep_model_id = FALSE) {
  fn <- "hd_huang1994"

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

  key <- .huang1994hd_species_key(v$species)
  p <- .huang1994hd_prepare(key, v$natural_subregion)
  .huang1994hd_inform(p, key, v$natural_subregion, use_nsr, fn)

  .hd_warn_out_of_range(
    list(DBH = v$DBH),
    lower = list(DBH = p$DBH_min),
    upper = list(DBH = p$DBH_max),
    fn = fn
  )

  height <- 1.3 + p$a * (1 - exp(-p$b * v$DBH))^p$c

  ok <- !is.na(height)
  if (any(ok & (!is.finite(height) | height <= 0))) {
    # nocov start
    # Defensive: published coefficients give finite heights >= 1.3 m for DBH >= 0.
    cli::cli_abort("{.fn {fn}} produced non-finite or non-positive heights.")
    # nocov end
  }

  out <- tibble::tibble(height = height)
  if (keep_model_id) {
    out$model_id <- ifelse(is.na(p$species_key), NA_character_, "huang1994")
    out$subregion_group <- p$subregion_group
  }
  out
}


# internal
# Returns one parameter row per input row: the natural-region-group model when
# `nsr` is supplied and the species has one for that region, otherwise the
# provincial model. Rows with NA species, or a group key (CONIF/DECID) with no
# regional model, get an all-NA row.
.huang1994hd_prepare <- function(key, nsr, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Huang1994")
  assert_required_cols(
    pars,
    c(
      "Species", "species_key", "source_species", "species_group",
      "subregion_group", "natural_subregion", "a", "b", "c",
      "DBH_min", "DBH_max"
    ),
    object = "parameters_hd_Huang1994"
  )
  pars <- as.data.frame(pars)

  bad <- unique(key[!is.na(key) & !key %in% pars$species_key])
  if (length(bad) > 0L) {
    supported <- c(sort(unique(stats::na.omit(pars$Species))), "CONIF", "DECID")
    cli::cli_abort(c(
      "Species not covered by the Huang et al. (1994) DBH\u2013height models: {.val {bad}}.",
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
# Upper-cases the group keywords and standardizes NFI codes.
.huang1994hd_species_key <- function(species) {
  x <- toupper(trimws(species))
  grp <- !is.na(x) & x %in% c("CONIF", "DECID")
  x[!grp] <- standardize_species_code(x[!grp], keep_all = FALSE)
  x
}


# internal
# One-time messages naming the variant used; warning for group-model rows
# without a regional model. inform_once() interpolates in its own frame, so
# texts are built here.
.huang1994hd_inform <- function(p, key, nsr, use_nsr, fn) {
  known <- !is.na(p$species_key)
  grp_na <- !is.na(key) & key %in% c("CONIF", "DECID") & !known

  if (!use_nsr) {
    inform_once(
      paste0(fn, "(): `natural_subregion` not supplied; using the provincial models."),
      .id = paste0(fn, "_provincial")
    )
  } else {
    inform_once(
      paste0(fn, "(): using natural-region-group models where available."),
      .id = paste0(fn, "_subregion")
    )
    fallback <- known & !is.na(nsr) & p$subregion_group == "provincial"
    if (any(fallback)) {
      sp <- sort(unique(p$species_key[fallback]))
      inform_once(
        paste0(
          fn, "(): no natural-region-group model for ",
          paste(sp, collapse = ", "),
          " in the requested region(s); using the provincial model."
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

  if (any(grp_na)) {
    cli::cli_warn(c(
      "{.fn {fn}}: {sum(grp_na)} row{?s} with {.val CONIF}/{.val DECID} returned {.code NA}.",
      "i" = paste(
        "The softwood/hardwood group models exist only for natural regions",
        "1\u201316 (and 21); supply a non-grassland {.arg natural_subregion}."
      )
    ))
  }
  invisible(NULL)
}
