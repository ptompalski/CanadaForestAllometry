#' Carmean et al. (1989) years-to-breast-height model
#'
#' Returns years to breast height (YTBH) for the species retained from
#' Carmean et al. (1989).
#'
#' \strong{Model scope (species coverage):} this implementation includes only
#' selected Carmean et al. (1989) species that occur in Canada:
#' \code{ACER.SAC, BETU.ALL, FAGU.GRA, FRAX.AME, FRAX.NIG, PRUN.SER,
#' QUER.RUB, TILI.AME, ULMU.AME, CHAM.THY, TSUG.CAN}.
#'
#' For most species, the source provides a fixed years-to-breast-height value
#' in the figure caption. For Atlantic white-cedar (\code{CHAM.THY}), the
#' source provides a site-index table in feet, and this implementation uses
#' linear interpolation between the tabulated values.
#'
#' @param si Numeric vector. Site index (m, base age 50 years at total age).
#' @param species Character vector of species codes (e.g., `"ACER.SAC"`).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{ytbh}{Predicted years to breast height (years).}
#' }
#'
#' @references
#' Carmean, W. H., Hahn, J. T., & Jacobs, R. D. (1989). Site index curves for
#' forest tree species in the eastern United States. U.S. Department of
#' Agriculture, Forest Service, Northern Research Station.
#'
#' @examples
#' ytbh_carmean1989(
#'   si = c(18, 20, 22),
#'   species = c("ACER.SAC", "CHAM.THY", "TSUG.CAN")
#' )
#'
#' @export
ytbh_carmean1989 <- function(si, species) {
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

  pars <- .get_internal_data("parameters_Carmean1989") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE) |>
    dplyr::mutate(.has_species = TRUE) |>
    dplyr::select("Species", "years_to_bh", ".has_species")

  assert_required_cols(pars, c("Species", "years_to_bh"), object = "parameters_Carmean1989")

  out <- dplyr::tibble(
    si = as.numeric(si),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  missing_species <- is.na(out$.has_species)
  if (any(missing_species)) {
    bad <- unique(out$Species[missing_species])
    cli::cli_abort(
      paste0(
        "No Carmean1989 YTBH parameters found for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  missing_y2bh <- is.na(out$years_to_bh) & out$Species != "CHAM.THY"
  if (any(missing_y2bh)) {
    bad <- unique(out$Species[missing_y2bh])
    cli::cli_abort(
      paste0(
        "No Carmean1989 YTBH value available for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  ytbh <- out$years_to_bh
  is_awc <- out$Species == "CHAM.THY"
  if (any(is_awc)) {
    ytbh[is_awc] <- stats::approx(
      x = c(20, 30, 40, 50, 60, 70, 80),
      y = c(11, 10, 9, 8, 7, 6, 5),
      xout = out$si[is_awc] * 3.28084,
      method = "linear",
      rule = 2
    )$y
  }

  # Unreachable with current logic: non-cedar missing values are trapped above,
  # and CHAM.THY interpolation uses rule = 2, so finite positive SI yields a
  # finite YTBH value.
  # nocov start
  if (anyNA(ytbh)) {
    bad <- unique(out$Species[is.na(ytbh)])
    cli::cli_abort(
      paste0(
        "No Carmean1989 YTBH value available for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }
  # nocov end

  # nocov start
  if (any(!is.finite(ytbh))) {
    cli::cli_abort(c(
      "Non-finite years-to-breast-height prediction generated in {.fn ytbh_carmean1989}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # nocov end
  if (any(ytbh < 0)) {
    cli::cli_abort(c(
      "Negative years-to-breast-height prediction generated in {.fn ytbh_carmean1989}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(ytbh = ytbh)
}
