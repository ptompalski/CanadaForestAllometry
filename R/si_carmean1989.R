#' Carmean et al. (1989) site index model
#'
#' Implementation of selected Carmean et al. (1989)
#' site-index equations for eastern North American tree species
#'
#' \strong{Model scope (species coverage):} this implementation includes only
#' selected Carmean et al. (1989) species that occur in Canada:
#' \code{ACER.SAC, BETU.ALL, FAGU.GRA, FRAX.AME, FRAX.NIG, PRUN.SER,
#' QUER.RUB, TILI.AME, ULMU.AME, CHAM.THY, TSUG.CAN}.
#'
#' \strong{Geographic use:} use for eastern species only, and with caution
#' outside the source curve domains.
#'
#' \strong{Age definition note:} `age` is total age (years). For users working
#' with breast-height age, years-to-breast-height can be obtained separately
#' with \code{\link{ytbh_carmean1989}}.
#'
#' \strong{Base-age note:} site index in this model is total height at
#' 50 years total age.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs/outputs are metric; the original equations are in imperial units, so
#' the function converts internally.
#'
#' @param age Numeric vector. Total age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at total
#'   age). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"ACER.SAC"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Carmean, W. H., Hahn, J. T., & Jacobs, R. D. (1989). Site index curves for
#' forest tree species in the eastern United States. U.S. Department of
#' Agriculture, Forest Service, Northern Research Station.
#'
#' @examples
#' # Predict site index from age + height
#' si_carmean1989(
#'   age = c(30, 50, 60),
#'   height = c(12, 18, 20),
#'   species = c("ACER.SAC", "BETU.ALL", "QUER.RUB")
#' )
#'
#' # Predict height from age + site index
#' si_carmean1989(
#'   age = c(30, 50, 60),
#'   si = c(18, 20, 22),
#'   species = c("ACER.SAC", "BETU.ALL", "QUER.RUB")
#' )
#'
#' @export
si_carmean1989 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .carmean1989_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h_ft <- with(
      df,
      height_b1 *
        (si_ft^height_b2) *
        (1 - exp(height_b3 * age))^(height_b4 * (si_ft^height_b5))
    )

    if (any(!is.finite(h_ft))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_carmean1989}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # nocov start
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_carmean1989}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # nocov end

    return(dplyr::tibble(height = h_ft / 3.28084))
  }

  si_ft <- with(
    df,
    si_b1 *
      (height_ft^si_b2) *
      (1 - exp(si_b3 * age))^(si_b4 * (height_ft^si_b5))
  )

  if (any(!is.finite(si_ft))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_carmean1989}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # nocov start
  if (any(si_ft < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_carmean1989}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # nocov end

  dplyr::tibble(si = si_ft / 3.28084)
}


# internal
.carmean1989_prepare <- function(age, x, species, x_name) {
  n <- max(length(age), length(x), length(species))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    species = species,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x
  species <- recycled$species

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  species_std <- standardize_species_code(species)

  pars <- .get_internal_data("parameters_Carmean1989") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c(
    "Species",
    "height_b1",
    "height_b2",
    "height_b3",
    "height_b4",
    "height_b5",
    "si_b1",
    "si_b2",
    "si_b3",
    "si_b4",
    "si_b5"
  )
  assert_required_cols(pars, req, object = "parameters_Carmean1989")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$height_b1) || anyNA(out$si_b1)) {
    bad <- unique(out$Species[
      is.na(out$height_b1) | is.na(out$si_b1)
    ])
    cli::cli_abort(
      paste0(
        "No Carmean1989 parameters found for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}
