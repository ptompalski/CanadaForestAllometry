#' Payandeh (1974) site index model
#'
#' Unified, vectorized implementation of the Payandeh (1974) nonlinear site
#' index equations.
#'
#' \strong{Index-age note:} this model uses species-specific site-index base
#' ages (not a single common base age for all species). In this implementation:
#' 50 years for \code{ABIE.BAL, PICE.GLA, PICE.RUB, PINU.MON};
#' 80 years for \code{PINU.CON}; and
#' 100 years for \code{PICE.SIT, PINU.PON, PSEU.MEN, TSUG.HET}.
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
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast height). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"ABIE.BAL"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Payandeh, B. (1974). Nonlinear site index equations for several major
#' Canadian timber species. \emph{The Forestry Chronicle}, 50(5), 194-196.
#'
#' @examples
#' # Predict site index from age + height
#' si_payandeh1974(
#'   age = c(20, 40, 60),
#'   height = c(8, 16, 28),
#'   species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN")
#' )
#'
#' # Predict height from age + site index
#' si_payandeh1974(
#'   age = c(20, 40, 60),
#'   si = c(12, 18, 24),
#'   species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN")
#' )
#'
#' # Works with the pipe operator: predict site index from age + height
#' trees_h <- tibble::tibble(
#'   species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN"),
#'   age = c(20, 40, 60),
#'   height = c(8, 16, 28)
#' )
#'
#' trees_h |>
#'   dplyr::mutate(
#'     si_pred = si_payandeh1974(
#'       age = age,
#'       height = height,
#'       species = species
#'     )
#'   ) |>
#'   tidyr::unnest(si_pred)
#'
#' # With pipe: predict height from age + site index
#' trees_si <- tibble::tibble(
#'   species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN"),
#'   age = c(20, 40, 60),
#'   si = c(12, 18, 24)
#' )
#'
#' trees_si |>
#'   dplyr::mutate(
#'     height_pred = si_payandeh1974(
#'       age = age,
#'       si = si,
#'       species = species
#'     )
#'   ) |>
#'   tidyr::unnest(height_pred)
#'
#' @export
si_payandeh1974 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .payandeh1974_prepare(
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
        "Non-finite height prediction generated in {.fn si_payandeh1974}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_payandeh1974}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

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
      "Non-finite site index prediction generated in {.fn si_payandeh1974}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_ft < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_payandeh1974}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_ft / 3.28084)
}

# internal
.payandeh1974_prepare <- function(age, x, species, x_name) {
  n <- max(length(age), length(x), length(species))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycle_to <- function(v, n, arg) {
    if (length(v) == n) {
      return(v)
    }
    if (length(v) == 1L) {
      return(rep(v, n))
    }
    cli::cli_abort("{.arg {arg}} must have length 1 or {n}.")
  }

  age <- recycle_to(age, n, "age")
  x <- recycle_to(x, n, x_name)
  species <- recycle_to(species, n, "species")

  if (!is.numeric(age)) {
    cli::cli_abort("{.arg age} must be numeric.")
  }
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {x_name}} must be numeric.")
  }

  if (any(!is.finite(age) | age <= 0)) {
    cli::cli_abort("{.arg age} must contain only finite values > 0.")
  }
  if (any(!is.finite(x) | x <= 0)) {
    cli::cli_abort("{.arg {x_name}} must contain only finite values > 0.")
  }

  species_std <- standardize_species_code(species)

  pars <- .get_internal_data("parameters_Payandeh1974") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  inform_once(
    c(
      "Note that Payandeh1974 site index models use different index age by species.",
      "i" = "Index age 50: ABIE.BAL, PICE.GLA, PICE.RUB, PINU.MON",
      "i" = "Index age 80: PINU.CON",
      "i" = "Index age 100: PICE.SIT, PINU.PON, PSEU.MEN, TSUG.HET"
    ),
    .id = "si_payandeh1974_index_age_groups"
  )

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
  miss <- setdiff(req, names(pars))
  if (length(miss) > 0) {
    cli::cli_abort(c(
      "{.val parameters_Payandeh1974} is missing required columns.",
      "x" = "{paste(miss, collapse = ', ')}"
    ))
  }

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$height_b1) || anyNA(out$si_b1)) {
    bad <- unique(out$Species[is.na(out$height_b1) | is.na(out$si_b1)])
    cli::cli_abort(
      "No Payandeh1974 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}
