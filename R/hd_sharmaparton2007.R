#' Sharma and Parton (2007) height-diameter model for Ontario boreal species
#'
#' Estimates total tree height from diameter and stand-level covariates using
#' the fixed-effect form of the height-diameter model from Sharma and Parton
#' (2007) for eight boreal tree species in Ontario.
#'
#' The implemented model is Equation (8) with the plot random effect omitted:
#' \deqn{
#' H = 1.3 + u SHT^d \left(1 - \exp(-b (TPH / BA)^w DBH)\right)^g
#' }
#' where `H` is total height (m), `DBH` is diameter at breast height (cm),
#' `SHT` is dominant stand height (m), `TPH` is trees per hectare, and `BA` is
#' stand basal area (m2/ha).
#'
#' By default, this function uses the fixed part of the mixed-effects fit
#' (`fit = "mixed_fixed"`; Table 6, Method 2). Set `fit = "nls_fixed"` to use
#' the coefficients from the nonlinear least-squares/fixed-only fit (Table 6,
#' Method 1).
#'
#' @param DBH Numeric vector. Diameter at breast height (cm), with `DBH > 0`.
#' @param species Character vector. NFI species code.
#' @param SHT Numeric vector. Dominant stand height (m), with `SHT > 0`.
#' @param TPH Numeric vector. Stand density (trees/ha), with `TPH > 0`.
#' @param BA Numeric vector. Stand basal area (m2/ha), with `BA > 0`.
#' @param fit Character scalar. Parameter set to use: `"mixed_fixed"` (default)
#'   or `"nls_fixed"`.
#'
#' @return A tibble with column `height`, predicted total tree height (m).
#'
#' @references
#' Sharma, M., & Parton, J. (2007). Height-diameter equations for boreal tree
#' species in Ontario using a mixed-effects modeling approach. \emph{Forest
#' Ecology and Management}, 249, 187-198.
#'
#' @examples
#' hd_sharmaparton2007(
#'   DBH = 20,
#'   species = "PICE.MAR",
#'   SHT = 20,
#'   TPH = 2500,
#'   BA = 25
#' )
#'
#' @export
hd_sharmaparton2007 <- function(
  DBH,
  species,
  SHT,
  TPH,
  BA,
  fit = c("mixed_fixed", "nls_fixed")
) {
  fit <- match.arg(fit)

  n <- max(length(DBH), length(species), length(SHT), length(TPH), length(BA))
  if (n == 0L) {
    cli::cli_abort("{.arg DBH} must have length > 0.")
  }

  recycled <- assert_len_compat(
    DBH = DBH,
    species = species,
    SHT = SHT,
    TPH = TPH,
    BA = BA,
    .n = n,
    .recycle = TRUE
  )
  DBH <- recycled$DBH
  species <- recycled$species
  SHT <- recycled$SHT
  TPH <- recycled$TPH
  BA <- recycled$BA

  assert_numeric_vec(DBH, "DBH", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(SHT, "SHT", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(TPH, "TPH", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(BA, "BA", finite = TRUE, gt = 0, allow_na = FALSE)
  if (!is.character(species)) {
    cli::cli_abort("{.arg species} must be character.")
  }
  if (anyNA(species)) {
    cli::cli_abort("{.arg species} cannot contain NA values.")
  }

  species_std <- standardize_species_code(species, keep_all = FALSE)
  pars <- .sharmaparton2007_hd_parameters() |>
    dplyr::filter(.data$fit == .env$fit) |>
    dplyr::select("Species", "u", "d", "b", "w", "g")

  df <- dplyr::tibble(
    .row_id = seq_len(n),
    DBH = as.numeric(DBH),
    Species = species_std,
    SHT = as.numeric(SHT),
    TPH = as.numeric(TPH),
    BA = as.numeric(BA)
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(df$u)) {
    bad <- unique(df$Species[is.na(df$u)])
    cli::cli_abort(c(
      "No SharmaParton2007 height-diameter parameters found for species.",
      "x" = paste0("Unsupported species: ", paste(bad, collapse = ", "))
    ))
  }

  h <- .sharmaparton2007_hd_height(
    DBH = df$DBH,
    SHT = df$SHT,
    TPH = df$TPH,
    BA = df$BA,
    u = df$u,
    d = df$d,
    b = df$b,
    w = df$w,
    g = df$g
  )

  if (any(!is.finite(h))) {
    cli::cli_abort(c(
      "Non-finite height prediction generated in {.fn hd_sharmaparton2007}.",
      "i" = "Check DBH, species, SHT, TPH, and BA inputs."
    ))
  }
  if (any(h < 1.3)) {
    cli::cli_abort(c(
      "Predicted height below breast height generated in {.fn hd_sharmaparton2007}.",
      "i" = "Check DBH, species, SHT, TPH, and BA inputs."
    ))
  }

  dplyr::tibble(height = as.numeric(h))
}


# internal
.sharmaparton2007_hd_height <- function(DBH, SHT, TPH, BA, u, d, b, w, g) {
  1.3 + u * (SHT^d) * (1 - exp(-b * ((TPH / BA)^w) * DBH))^g
}


# internal
.sharmaparton2007_hd_parameters <- function() {
  pars <- .get_internal_data("parameters_SharmaParton2007_hd") |>
    dplyr::as_tibble()

  req <- c("Species", "fit", "u", "d", "b", "w", "g", "sigma2_e", "sigma2_u", "AIC")
  assert_required_cols(pars, req, object = "parameters_SharmaParton2007_hd")

  pars
}
