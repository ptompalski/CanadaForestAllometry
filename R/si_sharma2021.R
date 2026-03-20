#' Sharma (2021) site index model for jack pine and black spruce in mixed natural stands
#'
#' Implementation of the fixed-effects non-climate McDill-Amateis dynamic
#' height equation reported by Sharma (2021) for jack pine (\code{PINU.BAN})
#' and black spruce (\code{PICE.MAR}) growing in natural-origin mixed stands
#' in Ontario.
#'
#' \strong{Species coverage:} \code{PINU.BAN}, \code{PICE.MAR}.
#'
#' \strong{Geographic use:} northern Ontario natural-origin mixed stands.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Height definition note:} the source model uses heights above breast
#' height (m), not total height. For consistency with other site-index
#' functions in this package, this implementation defaults to using total
#' height in the public API (`total_height = TRUE`) and converts internally by
#' subtracting or adding 1.3 m as needed. Set `total_height = FALSE` to work on
#' the source scale directly.
#'
#' \strong{Base-age note:} site index is defined at 50 years breast-height age.
#' The underlying dynamic equation is written in paired-age form, so this
#' implementation allows any positive `base_age`; the default remains `50` to
#' match the source definition.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' This implementation uses the fixed-effects no-climate form (Equation 8)
#' with stand-level random effects omitted.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Stand height (m). If
#'   `total_height = TRUE` (default), this is total height; otherwise it is
#'   height above breast height. If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m above breast height) at
#'   `base_age` years breast-height age. If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"PINU.BAN"` or
#'   `"PICE.MAR"`).
#' @param base_age Positive numeric scalar. Site-index base age (years at
#'   breast height). Defaults to `50`.
#' @param total_height Logical scalar. If `TRUE` (default), interpret input
#'   `height` as total height and return predicted `height` as total height. If
#'   `FALSE`, use the source-paper scale of height above breast height.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted stand height (m), returned when input `si` is provided. This is total height when `total_height = TRUE`, otherwise height above breast height.}
#'   \item{si}{Predicted site index (m above breast height), returned when input `height` is provided.}
#' }
#'
#' @references
#' Sharma, M. (2021). Climate effects on jack pine and black spruce
#' productivity in natural origin mixed stands and site index conversion
#' equations. Trees, Forests and People, 5, 100089.
#' https://doi.org/10.1016/j.tfp.2021.100089
#'
#' @examples
#' # Predict site index from age + height
#' si_sharma2021(
#'   age = c(60, 80),
#'   height = c(16, 18),
#'   species = c("PINU.BAN", "PICE.MAR")
#' )
#'
#' # Predict height from age + site index
#' si_sharma2021(
#'   age = c(60, 80),
#'   si = c(14, 13),
#'   species = c("PINU.BAN", "PICE.MAR")
#' )
#'
#' @export
si_sharma2021 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  base_age = 50,
  total_height = TRUE
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (
    !is.numeric(base_age) ||
      length(base_age) != 1L ||
      is.na(base_age) ||
      !is.finite(base_age) ||
      base_age <= 0
  ) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value > 0.")
  }
  if (!is.logical(total_height) || length(total_height) != 1L || is.na(total_height)) {
    cli::cli_abort("{.arg total_height} must be a single TRUE/FALSE value.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .sharma2021_prepare(
    age = age,
    x = x,
    x_name = x_name,
    species = species,
    base_age = as.numeric(base_age),
    total_height = total_height
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
        "Non-finite height prediction generated in {.fn si_sharma2021}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_sharma2021}.",
        "i" = "Check inputs and model domain."
      ))
    }

    if (isTRUE(total_height)) {
      h <- h + 1.3
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
      "Non-finite site index prediction generated in {.fn si_sharma2021}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_sharma2021}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.sharma2021_prepare <- function(age, x, x_name, species, base_age, total_height) {
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
    a0 = c(25.1986, 28.0245),
    a1 = c(1.2233, 1.2645)
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
        "No Sharma2021 parameters found for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  if (identical(x_name, "height")) {
    if (isTRUE(total_height)) {
      if (any(out$x <= 1.3)) {
        cli::cli_abort(
          "{.arg height} must contain values > 1.3 when {.arg total_height = TRUE}."
        )
      }
      out$height <- out$x - 1.3
    } else {
      out$height <- out$x
    }
  } else {
    out$si <- out$x
  }

  out
}
