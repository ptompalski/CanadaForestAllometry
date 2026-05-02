#' Rijal et al. (2012) height-diameter model for Acadian Forest species
#'
#' Estimates total tree height from diameter, competition, and site covariates
#' using the GNLS fixed regional height-diameter equation from Rijal et al.
#' (2012) for 15 tree species in the North American Acadian Region.
#'
#' The implemented model is Equation (4) using Table 5 GNLS parameters:
#' \deqn{
#' HT = 1.37 + (c_0 + c_3 CSI)
#'   \left(1 - \exp(-c_1 DBH)\right)^{c_2 + c_4 \log(CCF + 1) + c_5 BAL}
#' }
#' where `HT` is total height (m), `DBH` is diameter at breast height (cm),
#' `CSI` is climate-derived site index, `CCF` is crown competition factor, and
#' `BAL` is basal area larger than the subject tree.
#'
#' The source paper also reports nonlinear mixed-effects parameters and random
#' effect variance components. This function implements the GNLS fixed model,
#' which the authors recommend when observations are not available for local
#' mixed-model calibration.
#'
#' @param DBH Numeric vector. Diameter at breast height (cm), with `DBH > 0`.
#' @param species Character vector. NFI species code.
#' @param CSI Numeric vector. Climate-derived site index (m), with `CSI > 0`.
#' @param CCF Numeric vector. Crown competition factor, with `CCF >= 0`.
#' @param BAL Numeric vector. Basal area larger than the subject tree, with
#'   `BAL >= 0`.
#'
#' @return A tibble with column `height`, predicted total tree height (m).
#'
#' @references
#' Rijal, B., Weiskittel, A. R., & Kershaw, J. A. (2012). Development of
#' regional height to diameter equations for 15 tree species in the North
#' American Acadian Region. \emph{Forestry}, 85(3), 379-390.
#'
#' @examples
#' hd_rijal2012(
#'   DBH = 20,
#'   species = "ABIE.BAL",
#'   CSI = 15,
#'   CCF = 120,
#'   BAL = 8
#' )
#'
#' @export
hd_rijal2012 <- function(DBH, species, CSI, CCF, BAL) {
  n <- max(length(DBH), length(species), length(CSI), length(CCF), length(BAL))
  if (n == 0L) {
    cli::cli_abort("{.arg DBH} must have length > 0.")
  }

  recycled <- assert_len_compat(
    DBH = DBH,
    species = species,
    CSI = CSI,
    CCF = CCF,
    BAL = BAL,
    .n = n,
    .recycle = TRUE
  )
  DBH <- recycled$DBH
  species <- recycled$species
  CSI <- recycled$CSI
  CCF <- recycled$CCF
  BAL <- recycled$BAL

  assert_numeric_vec(DBH, "DBH", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(CSI, "CSI", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(CCF, "CCF", finite = TRUE, gte = 0, allow_na = FALSE)
  assert_numeric_vec(BAL, "BAL", finite = TRUE, gte = 0, allow_na = FALSE)
  if (!is.character(species)) {
    cli::cli_abort("{.arg species} must be character.")
  }
  if (anyNA(species)) {
    cli::cli_abort("{.arg species} cannot contain NA values.")
  }

  species_std <- standardize_species_code(species, keep_all = FALSE)
  pars <- .rijal2012_hd_parameters() |>
    dplyr::select("Species", "c0", "c1", "c2", "c3", "c4", "c5")

  df <- dplyr::tibble(
    .row_id = seq_len(n),
    DBH = as.numeric(DBH),
    Species = species_std,
    CSI = as.numeric(CSI),
    CCF = as.numeric(CCF),
    BAL = as.numeric(BAL)
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(df$c0)) {
    bad <- unique(df$Species[is.na(df$c0)])
    cli::cli_abort(c(
      "No Rijal2012 height-diameter parameters found for species.",
      "x" = paste0("Unsupported species: ", paste(bad, collapse = ", "))
    ))
  }

  h <- .rijal2012_hd_height(
    DBH = df$DBH,
    CSI = df$CSI,
    CCF = df$CCF,
    BAL = df$BAL,
    c0 = df$c0,
    c1 = df$c1,
    c2 = df$c2,
    c3 = df$c3,
    c4 = df$c4,
    c5 = df$c5
  )

  if (any(!is.finite(h))) {
    cli::cli_abort(c(
      "Non-finite height prediction generated in {.fn hd_rijal2012}.",
      "i" = "Check DBH, species, CSI, CCF, and BAL inputs."
    ))
  }
  if (any(h < 1.37)) {
    cli::cli_abort(c(
      "Predicted height below breast height generated in {.fn hd_rijal2012}.",
      "i" = "Check DBH, species, CSI, CCF, and BAL inputs."
    ))
  }

  dplyr::tibble(height = as.numeric(h))
}


# internal
.rijal2012_hd_height <- function(DBH, CSI, CCF, BAL, c0, c1, c2, c3, c4, c5) {
  1.37 + (c0 + c3 * CSI) *
    (1 - exp(-c1 * DBH))^(c2 + c4 * log(CCF + 1) + c5 * BAL)
}


# internal
.rijal2012_hd_parameters <- function() {
  pars <- .get_internal_data("parameters_Rijal2012_hd") |>
    dplyr::as_tibble()

  req <- c(
    "Species", "c0", "c1", "c2", "c3", "c4", "c5",
    "var_power", "res_std", "R2"
  )
  assert_required_cols(pars, req, object = "parameters_Rijal2012_hd")

  pars
}
