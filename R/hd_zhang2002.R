#' Zhang, Peng, Huang and Zhou (2002) ecoregion-based jack pine DBH–height models for Ontario
#'
#' Vectorized implementation of the Chapman–Richards height–diameter models of
#' Zhang et al. (2002) for jack pine (`PINU.BAN`) in Ontario. Predicts total
#' tree height (m) from DBH (cm), by ELC ecoregion, by forest region, or with
#' the province-wide (Overall) fit.
#'
#' \strong{Model form} (eq. \[1\]):
#' \deqn{H = 1.3 + a [1 - \exp(-b \cdot DBH)]^{c},}
#' with \eqn{a}, \eqn{b}, \eqn{c} from Table 2 of the source. Predicted height
#' tends to 1.3 m as DBH tends to 0.
#'
#' \strong{Fit:} ordinary nonlinear least squares (SAS PROC NLIN), fitted to
#' 10 727 jack pine trees from Ontario permanent sample plots (trees with forks
#' or damaged tops excluded). The source does not state stand origin.
#'
#' \strong{Parameter sets} (selected per row):
#' \itemize{
#'   \item `ecoregion` supplied: the ecoregion fit. Ecoregions 3E, 3S, 3W, 4S
#'     and 4W (Boreal forest region) and 4E and 5E (Central forest region) are
#'     covered (ELC Working Group 2000). Other ecoregion codes are rejected.
#'   \item `ecoregion` `NULL`/`NA` and `forest_region` supplied: the Boreal or
#'     Central regional fit.
#'   \item neither supplied: the Overall fit (all data combined).
#' }
#' If both are supplied, the ecoregion fit is used and `forest_region` must
#' match the forest region of the ecoregion. One-time messages report the
#' parameter sets in use. The authors found significant differences among
#' ecoregions and recommend the ecoregion models; the regional and Overall
#' fits produced significant prediction bias in individual ecoregions
#' (Tables 4–6 of the source).
#'
#' \strong{Ecoregion boundaries} follow the ELC Working Group (2000) map of
#' Ontario ecoregions used by the source; later revisions of the Ontario
#' ecoregion map (e.g. Crins et al. 2009) may differ.
#'
#' \strong{Ecoregion 3S caveat:} the authors note an "unusually large"
#' asymptote for ecoregion 3S (\eqn{a = 79.0082}), attributed to a lack of
#' large trees in the data; predictions beyond its DBH range (2.5–35.3 cm)
#' are unreliable.
#'
#' Rows with `NA` `DBH` or `species` return `NA` height. DBH values outside the
#' range of the fitting data of the selected parameter set (Table 1) are
#' computed and reported in a single warning.
#'
#' No worked example or prediction table is published, so this implementation
#' is validated against the equation and coefficients only (no source
#' benchmark).
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m
#'   above ground) outside bark, in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species code; only jack
#'   pine, `"PINU.BAN"` (compact `"PINUBAN"` is accepted), is covered.
#' @param ecoregion Optional character vector. Plot level. Ontario ecoregion
#'   code (ELC Working Group 2000): one of `"3E"`, `"3S"`, `"3W"`, `"4E"`,
#'   `"4S"`, `"4W"`, `"5E"` (case and spaces ignored, so `"3 e"` is accepted).
#'   Length 1 or `length(DBH)`. `NULL` (default) or `NA` uses the regional or
#'   Overall fit.
#' @param forest_region Optional character vector. Plot level. Ontario forest
#'   region: `"boreal"` or `"central"` (Great Lakes–St. Lawrence; `"glsl"` and
#'   `"great lakes-st. lawrence"` are also accepted; case-insensitive). Length
#'   1 or `length(DBH)`. Used when `ecoregion` is `NULL`/`NA`; otherwise it
#'   must agree with the ecoregion. `NULL` (default) or `NA` uses the Overall
#'   fit when `ecoregion` is not supplied.
#' @param keep_model_id Logical. If `TRUE`, add columns `model_id`
#'   (`"zhang2002_ecoregion"`, `"zhang2002_regional"` or
#'   `"zhang2002_overall"`), `variant` (`"ecoregion"`, `"regional"` or
#'   `"overall"`) and `parameter_set` (the ecoregion code, `"boreal"`,
#'   `"central"` or `"overall"`).
#'
#' @return A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
#'   `model_id`, `variant` and `parameter_set`.
#'
#' @references
#' Zhang, L., Peng, C., Huang, S., and Zhou, X. (2002). Development and
#' evaluation of ecoregion-based jack pine height-diameter models for Ontario.
#' The Forestry Chronicle, 78(4), 530–538. \doi{10.5558/tfc78530-4}
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Ecoregion models
#' hd_zhang2002(DBH = c(10, 20, 30), species = "PINU.BAN", ecoregion = c("3E", "4W", "5E"))
#'
#' # Forest-region model when the ecoregion is unknown
#' hd_zhang2002(DBH = 20, species = "PINU.BAN", forest_region = "central")
#'
#' # Overall (province-wide) model
#' hd_zhang2002(DBH = c(10, 20, 30), species = "PINU.BAN", keep_model_id = TRUE)
#'
#' # Inside dplyr::mutate()
#' trees <- data.frame(DBH = c(12, 25, 18), ecoregion = c("3W", "3W", "4S"))
#' dplyr::mutate(trees, hd_zhang2002(DBH, "PINU.BAN", ecoregion = ecoregion))
#'
#' @export
hd_zhang2002 <- function(DBH, species, ecoregion = NULL, forest_region = NULL,
                         keep_model_id = FALSE) {
  fn <- "hd_zhang2002"

  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  eco <- if (is.null(ecoregion)) NA_character_ else .zhang2002_std_ecoregion(ecoregion)
  fr <- if (is.null(forest_region)) NA_character_ else .zhang2002_std_forest_region(forest_region)

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (keep_model_id) {
      out$model_id <- character()
      out$variant <- character()
      out$parameter_set <- character()
    }
    return(out)
  }
  v <- assert_len_compat(
    DBH = DBH,
    species = as.character(species),
    ecoregion = eco,
    forest_region = fr
  )

  p <- .zhang2002_prepare(v$species, v$ecoregion, v$forest_region)
  .zhang2002_inform(p, fn)

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
    out$model_id <- ifelse(is.na(p$variant), NA_character_, paste0("zhang2002_", p$variant))
    out$variant <- p$variant
    out$parameter_set <- p$parameter_set
  }
  out
}


# internal
# Returns one parameter row per input row: the ecoregion fit when `eco` is
# non-NA, else the forest-region fit when `fr` is non-NA, else the Overall fit.
# Rows with NA species get an all-NA row. Aborts on unsupported species and on
# an ecoregion that conflicts with the supplied forest region.
.zhang2002_prepare <- function(species, eco, fr, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Zhang2002")
  assert_required_cols(
    pars,
    c(
      "Species", "variant", "ecoregion", "forest_region", "parameter_set",
      "a", "b", "c", "DBH_min", "DBH_max"
    ),
    object = "parameters_hd_Zhang2002"
  )
  pars <- as.data.frame(pars)

  sp <- trimws(species)
  has_sp <- !is.na(sp)
  sp[has_sp] <- standardize_species_code(sp[has_sp], keep_all = FALSE)
  bad <- unique(sp[has_sp & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Zhang et al. (2002) DBH\u2013height models: {.val {bad}}.",
      "i" = "Supported: PINU.BAN (jack pine)."
    ), call = call)
  }

  eco_rows <- pars[pars$variant == "ecoregion", , drop = FALSE]
  eco_region <- eco_rows$forest_region[match(eco, eco_rows$ecoregion)]
  conflict <- !is.na(eco) & !is.na(fr) & eco_region != fr
  if (any(conflict)) {
    pairs <- unique(paste0(eco[conflict], " + ", fr[conflict]))
    cli::cli_abort(c(
      "{.arg ecoregion} and {.arg forest_region} disagree: {.val {pairs}}.",
      "i" = "Ecoregions 3E, 3S, 3W, 4S and 4W are in the Boreal forest region; 4E and 5E are in the Central forest region."
    ), call = call)
  }

  set <- ifelse(!is.na(eco), eco, ifelse(!is.na(fr), fr, "overall"))
  set[!has_sp] <- NA_character_
  out <- pars[match(set, pars$parameter_set), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# internal
# Upper-cases and removes spaces ("3 e" -> "3E"); validates against the seven
# source ecoregions. NA stays NA.
.zhang2002_std_ecoregion <- function(x, call = rlang::caller_env()) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x) && !all(is.na(x))) {
    cli::cli_abort("{.arg ecoregion} must be a character vector.", call = call)
  }
  valid <- c("3E", "3S", "3W", "4E", "4S", "4W", "5E")
  y <- toupper(gsub("\\s+", "", as.character(x)))
  bad <- unique(x[!is.na(y) & !y %in% valid])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Invalid {.arg ecoregion} value{?s}: {.val {bad}}.",
      "i" = "Zhang et al. (2002) cover Ontario ecoregions {paste(valid, collapse = ', ')}."
    ), call = call)
  }
  y
}


# internal
# Maps forest-region labels to "boreal" / "central" (GLSL aliases -> central).
# NA stays NA.
.zhang2002_std_forest_region <- function(x, call = rlang::caller_env()) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x) && !all(is.na(x))) {
    cli::cli_abort("{.arg forest_region} must be a character vector.", call = call)
  }
  lookup <- c(
    "boreal" = "boreal",
    "central" = "central",
    "glsl" = "central",
    "great lakes-st. lawrence" = "central"
  )
  y <- tolower(gsub("\\s+", " ", trimws(as.character(x))))
  out <- unname(lookup[y])
  bad <- unique(x[!is.na(y) & is.na(out)])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Invalid {.arg forest_region} value{?s}: {.val {bad}}.",
      "i" = "Use {.val boreal} or {.val central} (also {.val glsl})."
    ), call = call)
  }
  out
}


# internal
# One-time messages naming the parameter sets used.
# inform_once() interpolates in its own frame, so texts are built here.
.zhang2002_inform <- function(p, fn) {
  used <- unique(stats::na.omit(p$variant))
  if ("ecoregion" %in% used) {
    inform_once(
      paste0(fn, "(): using the ecoregion models (Table 2)."),
      .id = paste0(fn, "_ecoregion")
    )
  }
  if ("regional" %in% used) {
    inform_once(
      paste0(fn, "(): `ecoregion` not supplied; using the forest-region (Boreal/Central) models."),
      .id = paste0(fn, "_regional")
    )
  }
  if ("overall" %in% used) {
    inform_once(
      paste0(fn, "(): `ecoregion` and `forest_region` not supplied; using the Overall (all Ontario) model."),
      .id = paste0(fn, "_overall")
    )
  }
  invisible(NULL)
}
