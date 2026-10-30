#' Auger (2016) DBH–height model for 27 Québec tree species
#'
#' Vectorized implementation of the population-level height–diameter
#' relationship of Auger (2016; Note de recherche forestière no 146) for 27
#' commercial tree species of Québec. Predicts total tree height (m) from DBH
#' (cm), plot merchantable basal area and, depending on the species, the
#' tree's relative DBH, climate normals, elevation and Québec ecological
#' classification codes.
#'
#' \strong{Model form} (equation 1, p. 4):
#' \deqn{h = 1.3 + A \ln(DBH + 1) + B \ln(DBH + 1)^2,}
#' \deqn{A = a + a_{ST} BA + a_{RDHP} \frac{DBH}{QMD} + a_{ALT} ALT +
#'   a_{PTOT} PTOT + a_{TMOY} TMOY + a_{SDOM} + a_{MP} + a_{VP} + a_{PERT},}
#' where \eqn{B} is a species constant and every covariate effect interacts
#' with \eqn{\ln(DBH + 1)}. Source symbols map to arguments as: ALT =
#' `elevation`, PTOT = `MAP`, TMOY = `MAT`, SDOM = `bioclim_subdomain`, MP =
#' `physical_env`, VP = `potential_veg`, PERT = `disturbance`. Height is not transformed, so no
#' back-transformation applies. Grey birch (`BETU.POP`) has no
#' \eqn{\ln(DBH + 1)^2} term in the source (Annexe 1) and is linear in
#' \eqn{\ln(DBH + 1)}.
#'
#' \strong{Species-specific predictors} (Tableau 5, pp. 10–11): `BA` is used
#' for all species; the other predictors only for the species whose selected
#' model contains them. A predictor is required only when a row's species uses
#' it; otherwise it may be `NULL` or `NA` and is ignored. Use
#' `get_hd_model_info("auger2016")` for definitions, and see the source for the
#' per-species predictor sets. For example, white spruce (`PICE.GLA`) uses
#' `BA`, `QMD`, `MAT`, `bioclim_subdomain` and `potential_veg`; silver maple, American elm and grey
#' birch use `BA` only.
#'
#' \strong{Population-level prediction:} the published fixed effects with the
#' plot random effect and residual set to zero, i.e. the marginal prediction
#' used by the source (p. 7). Plot calibration with measured heights is not
#' implemented.
#'
#' \strong{Class codes:} classes with few observations were grouped by the
#' author, and the group estimate is repeated for each member class
#' (Annexes 2–4). A class with no estimate for a species (absent from its
#' fitting data, e.g. physical environment 7 for red oak) gives an `NA` height
#' and one warning per call. Codes are matched case-insensitively.
#'
#' \strong{Coefficient precision:} coefficients are used as printed (4
#' decimals). The altitude and precipitation slopes therefore carry about one
#' significant digit; rounding can shift predictions by up to about 0.15 m.
#'
#' \strong{Applicability:} fitted to merchantable trees (DBH \eqn{\ge} 9.1 cm)
#' of the Québec permanent and temporary inventory plots. The author advises
#' against use for trees with DBH < 9 cm (p. 19); such trees are computed but
#' reported as outside the range, and their predicted height may be below
#' 1.3 m. Inputs outside the per-species ranges of the fitting data (Tableau 1:
#' DBH and relative DBH; Tableau 2: `BA`, `elevation`, `MAT`, `MAP`) are
#' computed and reported in a single warning. Rows with `NA` in a used input
#' return `NA` height.
#'
#' \strong{External implementation:} the `generalhdrelation2014` module of
#' CFSForestTools reproduces the published coefficients for 10 species
#' (`PRUN.SER`, `ACER.SAC`, `FRAX.AME`, `FRAX.NIG`, `LARI.LAR`, `ULMU.AME`,
#' `OSTR.VIR`, `PINU.STR`, `PINU.RES`, `TILI.AME`; within 0.02 m) but uses
#' different parameter estimates and predictor sets for the other 17 species
#' (differences up to about 2.6 m). This function follows the publication.
#'
#' @param DBH Numeric vector. Tree level. Diameter at breast height (1.3 m),
#'   in cm. Must be `>= 0`.
#' @param species Character vector. Tree level. NFI species codes (e.g.
#'   `"PICE.MAR"`; compact `"PICEMAR"` is accepted). Supported: `ABIE.BAL`,
#'   `ACER.RUB`, `ACER.SAC`, `ACER.SAH`, `BETU.ALL`, `BETU.PAP`, `BETU.POP`,
#'   `FAGU.GRA`, `FRAX.AME`, `FRAX.NIG`, `LARI.LAR`, `OSTR.VIR`, `PICE.GLA`,
#'   `PICE.MAR`, `PICE.RUB`, `PINU.BAN`, `PINU.RES`, `PINU.STR`, `POPU.BAL`,
#'   `POPU.GRA`, `POPU.TRE`, `PRUN.SER`, `QUER.RUB`, `THUJ.OCC`, `TILI.AME`,
#'   `TSUG.CAN`, `ULMU.AME`.
#' @param BA Numeric vector. Plot level. Merchantable basal area of the plot
#'   (m²/ha; source "ST", « surface terrière marchande dans la placette »,
#'   trees with DBH > 9 cm, all species). Must be `>= 0`. Required for all
#'   species.
#' @param QMD Optional numeric vector. Plot level. Quadratic mean DBH (cm) of
#'   the plot's merchantable trees. Used to compute the relative DBH
#'   (RDHP = `DBH / QMD`). Must be `> 0`. Required for 21 species.
#' @param bioclim_subdomain Optional character vector. Plot level.
#'   Bioclimatic subdomain (source "SDOM"; Saucier et al. 1998): `"1"`, `"2est"`, `"2ouest"`, `"3est"`, `"3ouest"`,
#'   `"4est"`, `"4ouest"`, `"5est"`, `"5ouest"`, `"6est"`, `"6ouest"`.
#'   Required for 11 species.
#' @param physical_env Optional character or integer vector. Plot level.
#'   Physical environment class (source "MP", « milieu physique », MRN 2013), `0` to `9`. Required
#'   for 10 species.
#' @param potential_veg Optional character vector. Plot level. Potential
#'   vegetation code (source "VP"; e.g. `"RS2"`, `"FE3"`; three characters). Required for 15 species.
#' @param MAT Optional numeric vector. Plot level. Mean annual temperature,
#'   1981–2010 normal (°C; source "TMOY"). Required for 7 species.
#' @param MAP Optional numeric vector. Plot level. Mean (total) annual
#'   precipitation, 1981–2010 normal (mm; source "PTOT"). Must be `>= 0`. Required for 10
#'   species.
#' @param elevation Optional numeric vector. Plot level. Elevation above sea
#'   level (m; source "ALT", altitude). Required
#'   for 4 species.
#' @param disturbance Optional character vector. Plot level. Occurrence of a
#'   light disturbance (« perturbation légère »): `"anthropique"`
#'   (`"anthropogenic"`), `"naturelle"` (`"natural"`) or `"aucune"`
#'   (`"none"`). Required for 5 species.
#'
#' @param keep_model_id Logical. If `TRUE`, add column `model_id`
#'   (`"auger2016"`).
#'
#' @return A tibble with column `height` (m), plus `model_id` if
#'   `keep_model_id = TRUE`.
#'
#' @references
#' Auger, I. (2016). Une nouvelle relation hauteur-diamètre tenant compte de
#' l'influence de la station et du climat pour 27 essences commerciales du
#' Québec. Gouvernement du Québec, ministère des Forêts, de la Faune et des
#' Parcs, Direction de la recherche forestière. Note de recherche forestière
#' no 146. 31 p.
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' # Worked example (Annexe 1): white spruce, 12.0 m
#' hd_auger2016(
#'   DBH = 20, species = "PICE.GLA", BA = 20, QMD = 15,
#'   bioclim_subdomain = "1", potential_veg = "FC1", MAT = 2.3
#' )
#'
#' # Species needing only basal area
#' hd_auger2016(DBH = c(15, 30), species = c("ACER.SAC", "ULMU.AME"), BA = 25)
#'
#' # Inside dplyr::mutate(): supply every predictor used by the species present
#' trees <- data.frame(
#'   DBH = c(14, 22, 18),
#'   species = c("PICE.MAR", "ABIE.BAL", "ACER.SAH"),
#'   BA = 22, QMD = c(15, 15, 20),
#'   bioclim_subdomain = c("5ouest", "5ouest", "3ouest"), physical_env = 2,
#'   potential_veg = c("RS2", "RS2", "FE3"), MAT = c(1, 1, 3.5),
#'   MAP = 1000, elevation = 300, disturbance = "none"
#' )
#' dplyr::mutate(
#'   trees,
#'   hd_auger2016(
#'     DBH, species, BA, QMD, bioclim_subdomain, physical_env, potential_veg,
#'     MAT, MAP, elevation, disturbance
#'   )
#' )
#'
#' @export
hd_auger2016 <- function(DBH, species, BA, QMD = NULL,
                         bioclim_subdomain = NULL, physical_env = NULL,
                         potential_veg = NULL, MAT = NULL, MAP = NULL,
                         elevation = NULL, disturbance = NULL,
                         keep_model_id = FALSE) {
  fn <- "hd_auger2016"

  assert_numeric_vec(DBH, "DBH", gte = 0, allow_na = TRUE)
  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  assert_numeric_vec(BA, "BA", gte = 0, allow_na = TRUE)
  assert_numeric_vec(QMD, "QMD", gt = 0, allow_na = TRUE, allow_null = TRUE)
  assert_numeric_vec(MAT, "MAT", allow_na = TRUE, allow_null = TRUE)
  assert_numeric_vec(MAP, "MAP", gte = 0, allow_na = TRUE, allow_null = TRUE)
  assert_numeric_vec(elevation, "elevation", allow_na = TRUE, allow_null = TRUE)
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }
  # Internal keys follow the source variable names (SDOM, MP, VP, PERT,
  # TMOY, PTOT, altitude), matching the parameter tables.
  codes <- list(
    SDOM = .auger2016_codes(bioclim_subdomain, "bioclim_subdomain"),
    MP = .auger2016_codes(physical_env, "physical_env"),
    VP = .auger2016_codes(potential_veg, "potential_veg"),
    PERT = .auger2016_codes(disturbance, "disturbance")
  )

  if (length(DBH) == 0L) {
    out <- tibble::tibble(height = numeric())
    if (isTRUE(keep_model_id)) out$model_id <- character()
    return(out)
  }

  inputs <- list(
    DBH = DBH, species = as.character(species), BA = BA, QMD = QMD,
    TMOY = MAT, PTOT = MAP, altitude = elevation,
    SDOM = codes$SDOM, MP = codes$MP, VP = codes$VP, PERT = codes$PERT
  )
  inputs <- inputs[!vapply(inputs, is.null, logical(1))]
  v <- do.call(assert_len_compat, inputs)
  n <- length(v$DBH)

  p <- .auger2016_prepare(v$species)
  has_sp <- !is.na(p$Species)

  # Continuous terms: (argument, coefficient column)
  cont <- list(
    QMD = "b_rdhp", altitude = "b_alt", PTOT = "b_ptot", TMOY = "b_tmoy"
  )
  cont_arg <- c(QMD = "QMD", altitude = "elevation", PTOT = "MAP", TMOY = "MAT")
  cls_arg <- c(
    SDOM = "bioclim_subdomain", MP = "physical_env", VP = "potential_veg",
    PERT = "disturbance"
  )
  uses <- c(
    lapply(cont, function(col) has_sp & !is.na(p[[col]])),
    lapply(stats::setNames(names(cls_arg), names(cls_arg)), function(var) {
      has_sp & p[[paste0("uses_", var)]] %in% TRUE
    })
  )
  arg_of <- c(cont_arg, cls_arg)
  for (nm in names(uses)) {
    arg <- arg_of[[nm]]
    if (any(uses[[nm]]) && is.null(v[[nm]])) {
      need <- unique(p$Species[uses[[nm]]])
      cli::cli_abort(
        c(
          "{.arg {arg}} is required for species {.val {need}}.",
          "i" = "Auger (2016) uses a species-specific set of predictors; see {.code get_hd_model_info(\"auger2016\")}."
        ),
        call = rlang::caller_env(0)
      )
    }
  }

  L <- log(v$DBH + 1)
  RDHP <- if (is.null(v$QMD)) rep(NA_real_, n) else v$DBH / v$QMD

  A <- p$b_ldhp + p$b_st * v$BA
  add_term <- function(A, used, coef, x) {
    A[used] <- A[used] + coef[used] * x[used]
    A
  }
  A <- add_term(A, uses$QMD, p$b_rdhp, RDHP)
  if (!is.null(v$altitude)) A <- add_term(A, uses$altitude, p$b_alt, v$altitude)
  if (!is.null(v$PTOT)) A <- add_term(A, uses$PTOT, p$b_ptot, v$PTOT)
  if (!is.null(v$TMOY)) A <- add_term(A, uses$TMOY, p$b_tmoy, v$TMOY)

  cls <- .get_internal_data("parameters_hd_Auger2016_classes")
  cls_key <- paste(cls$Species, cls$variable, cls$class, sep = "|")
  uncovered <- list()
  for (var in names(cls_arg)) {
    used <- uses[[var]] & !is.na(v[[var]] %||% NA_character_)
    if (!any(used)) next
    est <- cls$estimate[match(paste(p$Species, var, v[[var]], sep = "|"), cls_key)]
    miss <- used & is.na(est)
    if (any(miss)) uncovered[[cls_arg[[var]]]] <- which(miss)
    A[used] <- A[used] + est[used]
  }
  # NA in a used class code -> NA height (A stays NA via est = NA)
  for (var in names(cls_arg)) {
    if (!is.null(v[[var]])) A[uses[[var]] & is.na(v[[var]])] <- NA_real_
  }

  B <- ifelse(is.na(p$b_ldhp2), 0, p$b_ldhp2)
  height <- 1.3 + A * L + B * L^2
  height[!has_sp] <- NA_real_

  if (length(uncovered)) {
    .auger2016_warn_uncovered(uncovered, p$source_species, v, cls_arg, fn)
  }
  .hd_warn_out_of_range(
    list(
      DBH = v$DBH,
      BA = v$BA,
      RDHP = ifelse(uses$QMD, RDHP, NA_real_),
      elevation = if (is.null(v$altitude)) rep(NA_real_, n) else ifelse(uses$altitude, v$altitude, NA_real_),
      MAT = if (is.null(v$TMOY)) rep(NA_real_, n) else ifelse(uses$TMOY, v$TMOY, NA_real_),
      MAP = if (is.null(v$PTOT)) rep(NA_real_, n) else ifelse(uses$PTOT, v$PTOT, NA_real_)
    ),
    lower = list(
      DBH = p$DBH_min, BA = p$BA_min, RDHP = p$RDHP_min,
      elevation = p$altitude_min, MAT = p$TMOY_min, MAP = p$PTOT_min
    ),
    upper = list(
      DBH = p$DBH_max, BA = p$BA_max, RDHP = p$RDHP_max,
      elevation = p$altitude_max, MAT = p$TMOY_max, MAP = p$PTOT_max
    ),
    fn = fn
  )

  if (any(!is.na(height) & !is.finite(height))) {
    # nocov start
    # Defensive: inputs are finite, so eq. [1] is finite.
    cli::cli_abort("{.fn {fn}} produced non-finite heights.")
    # nocov end
  }

  out <- tibble::tibble(height = height)
  if (isTRUE(keep_model_id)) {
    out$model_id <- ifelse(has_sp, "auger2016", NA_character_)
  }
  out
}


# internal
# Normalizes a class-code argument to the codes used in the parameter table
# (SDOM lower case, VP upper case, MP digits, PERT French). Aborts on
# malformed codes; well-formed codes without an estimate for a species are
# handled later (NA + warning). NULL stays NULL.
.auger2016_codes <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.logical(x) && all(is.na(x))) {
    x <- as.character(x)
  }
  if (!is.character(x) && !is.factor(x) && !is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector.", call = call)
  }
  y <- trimws(as.character(x))
  y[y == ""] <- NA_character_
  given <- !is.na(y)
  ok <- switch(arg,
    bioclim_subdomain = {
      y <- tolower(y)
      grepl("^[0-9](est|ouest)?$", y)
    },
    physical_env = grepl("^[0-9]$", y),
    potential_veg = {
      y <- toupper(y)
      grepl("^[A-Z]{2}[0-9]$", y)
    },
    disturbance = {
      y <- tolower(y)
      map <- c(
        anthropique = "anthropique", anthropogenic = "anthropique",
        naturelle = "naturelle", natural = "naturelle",
        aucune = "aucune", none = "aucune"
      )
      ok <- y %in% names(map)
      y <- unname(map[y])
      ok
    }
  )
  bad <- unique(x[given & !ok])
  if (length(bad)) {
    hint <- switch(arg,
      bioclim_subdomain = "Use bioclimatic subdomain codes such as \"1\", \"2est\" or \"6ouest\".",
      physical_env = "Use physical environment classes 0 to 9.",
      potential_veg = "Use three-character potential vegetation codes such as \"RS2\" or \"FE3\".",
      disturbance = "Use \"anthropique\"/\"anthropogenic\", \"naturelle\"/\"natural\" or \"aucune\"/\"none\"."
    )
    cli::cli_abort(
      c("Invalid {.arg {arg}} value{?s}: {.val {as.character(bad)}}.", "i" = hint),
      call = call
    )
  }
  y
}

# internal
# Returns one parameter row per input row; NA species give an all-NA row.
.auger2016_prepare <- function(species, call = rlang::caller_env()) {
  pars <- .get_internal_data("parameters_hd_Auger2016")
  assert_required_cols(
    pars,
    c(
      "Species", "source_species", "b_ldhp", "b_ldhp2", "b_st", "b_rdhp",
      "b_alt", "b_ptot", "b_tmoy", "uses_SDOM", "uses_MP", "uses_VP",
      "uses_PERT", "DBH_min", "DBH_max", "RDHP_min", "RDHP_max", "BA_min",
      "BA_max", "altitude_min", "altitude_max", "TMOY_min", "TMOY_max",
      "PTOT_min", "PTOT_max"
    ),
    object = "parameters_hd_Auger2016"
  )
  pars <- as.data.frame(pars)

  sp <- standardize_species_code(trimws(species), keep_all = FALSE)
  bad <- unique(sp[!is.na(sp) & !sp %in% pars$Species])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Species not covered by the Auger (2016) DBH\u2013height model: {.val {bad}}.",
      "i" = "Supported: {paste(sort(pars$Species), collapse = ', ')}."
    ), call = call)
  }

  out <- pars[match(sp, pars$Species), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# internal
# One warning per call listing rows whose class code has no estimate for the
# row's species (class absent from that species' fitting data).
.auger2016_warn_uncovered <- function(uncovered, src_species, v, cls_arg, fn, max_rows = 10L) {
  var_of <- stats::setNames(names(cls_arg), cls_arg)
  bullets <- vapply(names(uncovered), function(arg) {
    rows <- uncovered[[arg]]
    combos <- unique(paste0(src_species[rows], " = ", v[[var_of[[arg]]]][rows]))
    shown <- rows[seq_len(min(length(rows), max_rows))]
    more <- if (length(rows) > max_rows) paste0(" and ", length(rows) - max_rows, " more") else ""
    paste0(
      "`", arg, "`: ", length(rows), " row", if (length(rows) > 1L) "s" else "",
      " (", paste(shown, collapse = ", "), more, "); species code = class: ",
      paste(combos, collapse = ", ")
    )
  }, character(1))
  n_rows <- length(unique(unlist(uncovered)))
  names(bullets) <- rep("*", length(bullets))
  cli::cli_warn(
    c(
      "{.fn {fn}}: {n_rows} row{?s} with a class not represented in the species' fitting data; height set to NA.",
      bullets
    ),
    class = "CanadaForestAllometry_hd_uncovered_level"
  )
  invisible(NULL)
}
