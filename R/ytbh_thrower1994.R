#' Thrower et al. (1994) years-to-breast-height models
#'
#' Species-specific years-to-breast-height (YTBH) equations used with the
#' Thrower et al. (1994) interior BC site-index formulations.
#'
#' @param si Numeric vector. Site index (m, base age 50 years at breast height).
#' @param species Character vector of NFI species codes (e.g., `"PINU.CON"`).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{ytbh}{Predicted years to breast height (years).}
#' }
#'
#' @references
#' Thrower, J.S., Nussbaum, A.F., and Di Lucca, C.M. (1994). Site index curves
#' and tables for British Columbia: interior species (2nd ed.).
#' B.C. Ministry of Forests, Land Management Handbook, Field Guide Insert 6.
#'
#' @examples
#' ytbh_thrower1994(
#'   si = c(12, 16, 20),
#'   species = c("PINU.CON", "THUJ.PLI", "ABIE.LAS")
#' )
#'
#' @export
ytbh_thrower1994 <- function(si, species) {
  n <- max(length(si), length(species))
  if (n == 0L) {
    cli::cli_abort("{.arg si} must have length > 0.")
  }

  recycled <- assert_len_compat(
    si = si,
    species = species,
    .n = n,
    .recycle = TRUE
  )
  si <- recycled$si
  species <- recycled$species

  assert_numeric_vec(si, "si", finite = TRUE, gt = 0, allow_na = FALSE)
  species_std <- standardize_species_code(species)

  pars <- .thrower1994_parameters() |>
    dplyr::select(
      Species,
      ytb_const,
      ytb_s_div_coef,
      ytb_lnS_coef,
      ytb_s_linear_coef,
      ytb_s_linear_scale
    )

  out <- dplyr::tibble(
    si = as.numeric(si),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$ytb_const)) {
    bad <- unique(out$Species[is.na(out$ytb_const)])
    cli::cli_abort(
      "No Thrower1994 YTBH parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  ytbh <- out$ytb_const

  has_div <- !is.na(out$ytb_s_div_coef)
  ytbh[has_div] <- ytbh[has_div] + out$ytb_s_div_coef[has_div] / out$si[has_div]

  has_ln <- !is.na(out$ytb_lnS_coef)
  ytbh[has_ln] <- ytbh[has_ln] + out$ytb_lnS_coef[has_ln] * log(out$si[has_ln])

  has_lin <- !is.na(out$ytb_s_linear_coef)
  ytbh[has_lin] <- ytbh[has_lin] +
    out$ytb_s_linear_coef[has_lin] * (out$si[has_lin] / out$ytb_s_linear_scale[has_lin])

  if (any(!is.finite(ytbh))) {
    cli::cli_abort(c(
      "Non-finite years-to-breast-height prediction generated in {.fn ytbh_thrower1994}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(ytbh < 0)) {
    cli::cli_abort(c(
      "Negative years-to-breast-height prediction generated in {.fn ytbh_thrower1994}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(ytbh = ytbh)
}
