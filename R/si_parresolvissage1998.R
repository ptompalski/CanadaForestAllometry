#' Parresol and Vissage (1998) site index model for eastern white pine
#'
#' Implementation of the base-age invariant polymorphic site-index equations
#' published by Parresol and Vissage (1998) for eastern white pine
#' (\code{PINU.STR}).
#'
#' \strong{Species coverage:} \code{PINU.STR}.
#'
#' \strong{Geographic use (Canada):} the source data were adopted for the
#' southern U.S. forest survey. In Canada, use is most defensible for eastern
#' white pine in eastern regions, and should be treated cautiously outside that
#' domain.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} the source paper derives a base-age invariant system.
#' This implementation accepts any positive `base_age`; the default is 50 years
#' at breast height.
#'
#' \strong{Domain note:} although the algebraic form can be evaluated below age
#' 10, the source data and paper discussion indicate reliable behavior for ages
#' 10 years and greater. This implementation therefore enforces `age >= 10` and
#' `base_age >= 10`.
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
#' Both directions are explicit closed forms from the source publication.
#'
#' @param age Numeric vector. Breast-height age (years), with `age >= 10`.
#' @param height Optional numeric vector. Dominant/codominant height (m). If
#'   provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m) at `base_age` years
#'   breast-height age. If provided, `height` is predicted.
#' @param base_age Positive numeric scalar. Site-index base age (years at breast
#'   height), with `base_age >= 10`. Defaults to `50`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted dominant/codominant height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Parresol, B. R., & Vissage, J. S. (1998). White pine site index for the
#' southern forest survey. U.S. Department of Agriculture, Forest Service,
#' Southern Research Station, Research Paper SRS-10.
#'
#' @examples
#' # Predict site index from age + height
#' si_parresolvissage1998(
#'   age = c(25, 50, 70),
#'   height = c(10, 18, 24)
#' )
#'
#' # Predict height from age + site index at a base age of 50 years
#' si_parresolvissage1998(
#'   age = c(25, 50, 70),
#'   si = c(12, 18, 24)
#' )
#'
#' # Predict height using an alternative base age
#' si_parresolvissage1998(
#'   age = 35,
#'   si = 30 / 3.28084,
#'   base_age = 25
#' )
#'
#' @export
si_parresolvissage1998 <- function(age, height = NULL, si = NULL, base_age = 50) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (!is.numeric(base_age) || length(base_age) != 1L || is.na(base_age) || !is.finite(base_age) || base_age < 10) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value >= 10.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .parresolvissage1998_prepare(
    age = age,
    x = x,
    x_name = x_name,
    base_age = as.numeric(base_age)
  )

  if (mode == "predict_height") {
    h_ft <- .parresolvissage1998_height(
      age = df$age,
      si_ft = df$si_ft,
      base_age = df$base_age
    )

    if (any(!is.finite(h_ft))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_parresolvissage1998}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_parresolvissage1998}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h_ft / 3.28084))
  }

  si_ft <- .parresolvissage1998_si(
    age = df$age,
    height_ft = df$height_ft,
    base_age = df$base_age
  )

  if (any(!is.finite(si_ft))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_parresolvissage1998}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_ft < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_parresolvissage1998}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_ft / 3.28084)
}


# internal
.parresolvissage1998_height <- function(age, si_ft, base_age) {
  a1 <- 8.6188
  a2 <- 74.7099
  a3 <- 2.0862

  log_h <- exp(a1 * (1 / age - 1 / base_age)) *
    (log(si_ft) + a2 / base_age - a3) -
    a2 / age + a3

  exp(log_h)
}


# internal
.parresolvissage1998_si <- function(age, height_ft, base_age) {
  a1 <- 8.6188
  a2 <- 74.7099
  a3 <- 2.0862

  log_si <- exp(a1 * (1 / base_age - 1 / age)) *
    (log(height_ft) + a2 / age - a3) -
    a2 / base_age + a3

  exp(log_si)
}


# internal
.parresolvissage1998_prepare <- function(age, x, x_name, base_age) {
  n <- max(length(age), length(x))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x

  assert_numeric_vec(age, "age", finite = TRUE, gte = 10, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    base_age = base_age
  )

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}
