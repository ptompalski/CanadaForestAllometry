#' Fortin et al. (2009) DBH–height model for 20 Québec tree species
#'
#' Vectorized implementation of the population-level height–diameter
#' relationship of Fortin, Bernier, Saucier & Labbé (2009; Mémoire de
#' recherche forestière no 153) for 20 commercial tree species of Québec.
#' Predicts total tree height (m) from DBH (cm), plot basal area, the tree's
#' social status, mean annual temperature, drainage, ecological region and
#' recent light disturbance. All species use the same predictors. The newer
#' [hd_auger2016()] is the updated relation from the same Québec inventory
#' (27 species).
#'
#' \strong{Model form} (equations 6 and 7, p. 7):
#' \deqn{h = 1.3 + A \ln(DBH + 1) + B \ln(DBH + 1)^2,}
#' \deqn{A = \beta_{1,s} + \beta_{2,s} BA + \beta_3 MAT + \beta_{4,d} +
#'   \beta_{5,e} + \beta_{6,f} + \beta_7 ISS,}
#' \deqn{B = \beta_{8,s} + \beta_{9,s} BA + \beta_{10} ISS,}
#' \deqn{ISS = (DBH - QMD)^2,}
#' where \eqn{s} is the species, \eqn{d} the drainage class, \eqn{e} the
#' ecological subdivision and \eqn{f} the disturbance class. Source symbols map
#' to arguments as: ST = `BA`, dmq = `QMD`, T = `MAT`, ISS (« indice de
#' statut social ») is computed from `DBH` and `QMD`. Height is not
#' transformed, so no back-transformation applies.
#'
#' \strong{Population-level prediction:} the fixed effects with the plot random
#' effect and residual error set to zero, as in the source's validation (eq. 9,
#' p. 8). Plot calibration with measured heights (the source publishes the
#' variance components) is not implemented.
#'
#' \strong{Coefficients:} full-precision estimates from the CFSForestTools
#' implementation of this model (`fortin2009generalhdrelationship`,
#' `0_HDRelationBeta.csv`; LGPL-3). They are the same fit as Tableaux 4 and 5
#' (pp. 11–12) and agree with the printed values to the printed precision
#' (largest difference 5e-5). Predictions match CFSForestTools to within
#' 1e-8 m.
#'
#' \strong{Ecological subdivision:} the source groups Québec ecological regions
#' (Saucier et al. 1998) into four subdivisions, shown only as a map
#' (Figure 2, p. 8). As in CFSForestTools, `ecoregion` takes the ecological
#' region code and is mapped as follows:
#' \itemize{
#'   \item Ouest (West): 1a, 2a, 3a, 3b, 4a, 4b, 4c, 5a, 5b, 5c, 5d, 6a–6g.
#'   \item Centre: 2b, 2c, 3c, 3d, 4d, 4e, 4f, 5e, 5f.
#'   \item Nord-Est (Northeast): 6h–6l, 6n–6s, 7a, 7b, 7c.
#'   \item Sud-Est (Southeast): 4g, 4h, 5g, 5h, 5i, 5j, 6m.
#' }
#'
#' \strong{Figure 5:} the effect plots on p. 13 are reproduced within about
#' 0.05 m, except panel (e) (social status index). For a tree 10 cm larger or
#' smaller than `QMD`, the published coefficients give about 13.2 m while the
#' figure shows about 12.7 m. This function follows the coefficients (as
#' CFSForestTools does).
#'
#' \strong{Applicability:} fitted to merchantable trees (DBH \eqn{\ge} 9.1 cm)
#' of the Québec permanent sample plots, 1970–2006. Trees with DBH < 9.1 cm
#' are computed but reported as outside the range, and their predicted height
#' may be below 1.3 m. Inputs outside the ranges of the fitting data
#' (Tableau 2: per-species DBH; Tableau 1: `BA` 0.2–66.9 m²/ha, `MAT` −4.7 to
#' 6.8 °C) are computed and reported in a single warning. Rows with `NA` in any
#' input return `NA` height.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m),
#'   in cm (source "dhp"). Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.MAR"`; compact `"PICEMAR"` is accepted). Supported: `ABIE.BAL`,
#'   `ACER.RUB`, `ACER.SAH`, `BETU.ALL`, `BETU.PAP`, `FAGU.GRA`, `FRAX.NIG`,
#'   `LARI.LAR`, `OSTR.VIR`, `PICE.GLA`, `PICE.MAR`, `PICE.RUB`, `PINU.BAN`,
#'   `PINU.STR`, `POPU.GRA`, `POPU.TRE`, `QUER.RUB`, `THUJ.OCC`, `TILI.AME`,
#'   `TSUG.CAN`.
#' @param BA Numeric vector. Plot level. Basal area of all trees with DBH
#'   \eqn{\ge} 9.1 cm in the plot (m²/ha; source "ST"). Must be `>= 0`.
#' @param QMD Numeric vector. Plot level. Quadratic mean DBH of the plot's
#'   trees with DBH \eqn{\ge} 9.1 cm (cm; source "dmq"), used in the social
#'   status index `(DBH - QMD)^2`. Must be `> 0`.
#' @param ecoregion Character vector. Plot level. Québec ecological region
#'   code (« région écologique », Saucier et al. 1998), e.g. `"3a"` or `"6h"`;
#'   case-insensitive. Mapped to the four ecological subdivisions of the source
#'   (see Details).
#' @param drainage Character vector. Plot level. Drainage class (source "d"):
#'   `"xeric"`, `"mesic"`, `"subhydric"` or `"hydric"`; French `"xérique"`,
#'   `"mésique"`, `"subhydrique"`, `"hydrique"` (with or without accents) are
#'   accepted. CFSForestTools derives it from the physical environment
#'   (« milieu physique ») of the ecological type: 0 = xeric, 1–3 = mesic,
#'   4–6 = subhydric, 7–9 = hydric.
#' @param MAT Numeric vector. Plot level. Mean annual temperature, 1971–2000
#'   normal (°C; source "T", from BioSIM).
#' @param disturbance Character vector. Plot level. Light disturbance since the
#'   previous measurement (source "f"; partial cut or natural disturbance such
#'   as partial windthrow or light insect outbreak): `"anthropique"`
#'   (`"anthropogenic"`), `"naturelle"` (`"natural"`) or `"aucune"`
#'   (`"none"`).
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"fortin2009"`).
#'
#' @return A tibble with column `height` (m), plus `model_id` if
#'   `keep_model_id = TRUE`.
#'
#' @references
#' Fortin, M., Bernier, S., Saucier, J.-P., & Labbé, F. (2009). Une relation
#' hauteur-diamètre tenant compte de l'influence de la station et du climat
#' pour 20 espèces commerciales du Québec. Gouvernement du Québec, ministère
#' des Ressources naturelles et de la Faune, Direction de la recherche
#' forestière. Mémoire de recherche forestière no 153. 22 p.
#'
#' @seealso [hd_auger2016()], [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Reference tree of Figure 5 (balsam fir, Centre subdivision): about 13.4 m
#' hd_fortin2009(
#'   DBH = 20, species = "ABIE.BAL", BA = 20, QMD = 20, ecoregion = "4d",
#'   drainage = "mesic", MAT = 0, disturbance = "none"
#' )
#'
#' # Inside dplyr::mutate()
#' trees <- data.frame(
#'   DBH = c(14, 22, 30),
#'   species = c("PICE.MAR", "ABIE.BAL", "BETU.ALL"),
#'   BA = 22, QMD = 18, ecoregion = c("5a", "5a", "3b"),
#'   drainage = c("subhydrique", "mesique", "mesique"), MAT = c(0.5, 0.5, 3),
#'   disturbance = "aucune"
#' )
#' dplyr::mutate(
#'   trees,
#'   hd_fortin2009(DBH, species, BA, QMD, ecoregion, drainage, MAT, disturbance)
#' )
#'
#' @export
hd_fortin2009 <- function(DBH, species, BA, QMD, ecoregion, drainage, MAT,
                          disturbance, keep_model_id = FALSE) {
  fn <- "hd_fortin2009"

  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(BA, "BA", gte = 0, allow_na = TRUE)
  assert_numeric_vec(QMD, "QMD", gt = 0, allow_na = TRUE)
  assert_numeric_vec(MAT, "MAT", allow_na = TRUE)
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  subdivision <- .fortin2009hd_codes(ecoregion, "ecoregion")
  drainage <- .fortin2009hd_codes(drainage, "drainage")
  disturbance <- .fortin2009hd_codes(disturbance, "disturbance")

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (isTRUE(keep_model_id)) out$model_id <- character()
    return(out)
  }

  v <- assert_len_compat(
    DBH = DBH, species = as.character(species), BA = BA, QMD = QMD,
    subdivision = subdivision, drainage = drainage, MAT = MAT,
    disturbance = disturbance
  )

  p <- .fortin2009hd_prepare(v$species)
  has_sp <- !is.na(p$Species)

  eff <- .get_internal_data("parameters_hd_Fortin2009_effects")
  coef <- function(parameter, class = "") {
    eff$estimate[match(paste(parameter, class), paste(eff$parameter, eff$class))]
  }

  L <- log(v$DBH + 1)
  ISS <- (v$DBH - v$QMD)^2
  A <- p$b1 + p$b2 * v$BA + coef("b3") * v$MAT +
    coef("b4", v$drainage) + coef("b5", v$subdivision) +
    coef("b6", v$disturbance) + coef("b7") * ISS
  B <- p$b8 + p$b9 * v$BA + coef("b10") * ISS
  height <- 1.3 + A * L + B * L^2
  height[!has_sp] <- NA_real_

  .hd_warn_out_of_range(
    list(DBH = v$DBH, BA = v$BA, MAT = v$MAT),
    lower = list(DBH = p$DBH_min, BA = p$BA_min, MAT = p$MAT_min),
    upper = list(DBH = p$DBH_max, BA = p$BA_max, MAT = p$MAT_max),
    fn = fn
  )

  if (any(!is.na(height) & !is.finite(height))) {
    # nocov start
    # Defensive: inputs are finite, so eq. (6) is finite.
    cli::cli_abort("{.fn {fn}} produced non-finite heights.")
    # nocov end
  }

  out <- tibble::tibble(height = height)
  if (isTRUE(keep_model_id)) {
    out$model_id <- ifelse(has_sp, "fortin2009", NA_character_)
  }
  out
}


# internal
# Normalizes a class argument to the class keys of the effects table
# (ecoregion -> ecological subdivision; drainage and disturbance -> English
# class names). Accepts French or English, case- and accent-insensitive.
# Aborts on unknown values; NA and "" stay NA.
.fortin2009hd_codes <- function(x, arg, call = rlang::caller_env()) {
  if (is.logical(x) && all(is.na(x))) {
    x <- as.character(x)
  }
  if (!is.character(x) && !is.factor(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector.", call = call)
  }
  y <- tolower(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
  y[y == ""] <- NA_character_
  map <- switch(arg,
    ecoregion = {
      eco <- .get_internal_data("hd_fortin2009_ecoregions")
      stats::setNames(eco$ecological_subdivision, eco$ecoregion)
    },
    drainage = c(
      xeric = "xeric", xerique = "xeric",
      mesic = "mesic", mesique = "mesic",
      subhydric = "subhydric", subhydrique = "subhydric",
      hydric = "hydric", hydrique = "hydric"
    ),
    disturbance = c(
      anthropogenic = "anthropogenic", anthropique = "anthropogenic",
      natural = "natural", naturelle = "natural",
      none = "none", aucune = "none"
    )
  )
  bad <- unique(as.character(x)[!is.na(y) & !y %in% names(map)])
  if (length(bad)) {
    hint <- switch(arg,
      ecoregion = "Use a Qu\u00e9bec ecological region code such as \"3a\" or \"6h\" (1a to 7c).",
      drainage = "Use \"xeric\", \"mesic\", \"subhydric\" or \"hydric\" (or the French names).",
      disturbance = "Use \"anthropique\"/\"anthropogenic\", \"naturelle\"/\"natural\" or \"aucune\"/\"none\"."
    )
    cli::cli_abort(
      c("Invalid {.arg {arg}} value{?s}: {.val {bad}}.", "i" = hint),
      call = call
    )
  }
  unname(map[y])
}

# internal
# Returns one parameter row per input row; NA species give an all-NA row.
.fortin2009hd_prepare <- function(species, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Fortin2009")
  assert_required_cols(
    pars,
    c(
      "Species", "b1", "b2", "b8", "b9", "DBH_min", "DBH_max",
      "BA_min", "BA_max", "MAT_min", "MAT_max"
    ),
    object = "parameters_hd_Fortin2009"
  )
  pars <- as.data.frame(pars)

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Fortin et al. (2009) DBH\u2013height model: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}
