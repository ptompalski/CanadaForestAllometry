#' Sharma and Reid (2018) site index model for natural jack pine and black spruce
#'
#' Implementation of the fixed-effects McDill-Amateis dynamic stand-height
#' model published by Sharma and Reid (2018) for natural-origin jack pine
#' (\code{PINU.BAN}) and black spruce (\code{PICE.MAR}) stands in northern
#' Ontario.
#'
#' \strong{Species coverage:} \code{PINU.BAN}, \code{PICE.MAR}.
#'
#' \strong{Geographic use:} northern Ontario natural stands.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Height definition note:} the source model uses stand height
#' (top height) in metres measured from breast height.
#'
#' \strong{Base-age note:} the source paper recommends breast-height age 50
#' years as the operational base age for both species in Ontario. The
#' underlying dynamic equation is base-age invariant, so any positive
#' `base_age` can be supplied.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' This implementation uses Equation 1 from Sharma and Reid (2018), i.e. the
#' fixed-effects form without random effects or autocorrelation. The paper
#' recommends this form when stand-level repeated measurements are unavailable.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Stand height (m). If provided, `si`
#'   is predicted.
#' @param si Optional numeric vector. Site index (m) at `base_age` years
#'   breast-height age. If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"PINU.BAN"` or
#'   `"PICE.MAR"`).
#' @param base_age Positive numeric scalar. Site-index base age (years at
#'   breast height). Defaults to `50`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted stand height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Sharma, M., & Reid, D. E. B. (2018). Stand height/site index equations for
#' jack pine and black spruce trees grown in natural stands. Forest Science,
#' 64(1), 33-40. https://doi.org/10.5849/FS-2016-133
#'
#' @examples
#' # Predict site index from age + height
#' si_sharmareid2018(
#'   age = c(40, 60),
#'   height = c(12, 15),
#'   species = c("PINU.BAN", "PICE.MAR")
#' )
#'
#' # Predict height from age + site index
#' si_sharmareid2018(
#'   age = c(40, 60),
#'   si = c(16, 14),
#'   species = c("PINU.BAN", "PICE.MAR")
#' )
#'
#' @export
si_sharmareid2018 <- function(age, height = NULL, si = NULL, species, base_age = 50) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (!is.numeric(base_age) || length(base_age) != 1L || is.na(base_age) || !is.finite(base_age) || base_age <= 0) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value > 0.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .sharmareid2018_prepare(
    age = age,
    x = x,
    x_name = x_name,
    species = species,
    base_age = as.numeric(base_age)
  )

  if (mode == "predict_height") {
    h <- .mcdill_amateis_height(
      age = df$age,
      si = df$si,
      base_age = df$base_age,
      a0 = df$a0,
      a1 = df$a1
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_sharmareid2018}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_sharmareid2018}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- .mcdill_amateis_si(
    age = df$age,
    height = df$height,
    base_age = df$base_age,
    a0 = df$a0,
    a1 = df$a1
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_sharmareid2018}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_sharmareid2018}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.sharmareid2018_prepare <- function(age, x, x_name, species, base_age) {
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

  pars <- tibble::tibble(
    Species = c("PINU.BAN", "PICE.MAR"),
    a0 = c(30.7349, 31.6553),
    a1 = c(1.1205, 1.1580)
  )

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std,
    base_age = base_age
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$a0) || anyNA(out$a1)) {
    bad <- unique(out$Species[is.na(out$a0) | is.na(out$a1)])
    cli::cli_abort(
      paste0(
        "No SharmaReid2018 parameters found for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}
