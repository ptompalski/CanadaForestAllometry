#' Nigh and Courtin (1998) site index model for red alder (BC coast)
#'
#' Implementation of the Nigh and Courtin (1998) anamorphic
#' height-age/site-index equations for red alder (\code{ALNU.RUB}) in
#' coastal British Columbia.
#'
#' \strong{Species coverage:} \code{ALNU.RUB}.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} site index in this model is SI25 (m at breast-height
#' age 25 years), not SI50.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Both directions are explicit closed forms from the source publication.
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Dominant height (m). If provided, `si`
#'   is predicted.
#' @param si Optional numeric vector. Site index SI25 (m at breast-height age 25).
#'   If provided, `height` is predicted.
#' @param si50 Logical scalar. Default `FALSE`. If `TRUE`, interpret input `si`
#'   (when provided) as SI50 and return predicted `si` (when `height` is
#'   provided) as SI50 using:
#'   \deqn{SI50 = -0.4063 + 1.313 \times SI25}
#'   and its inverse.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted dominant height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index SI25 (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Nigh, G. D., & Courtin, P. J. (1998). Height models for Red Alder
#' (\emph{Alnus rubra} Bong.) in British Columbia.
#' \emph{New Forests}, 16, 59-70.
#'
#' @examples
#' # Predict site index SI25 from age + height
#' si_nighcourtin1998(
#'   age = c(10, 25, 40),
#'   height = c(8, 18, 24)
#' )
#'
#' # Predict height from age + site index SI25
#' si_nighcourtin1998(
#'   age = c(10, 25, 40),
#'   si = c(14, 18, 22)
#' )
#'
#' # Use SI50 instead of SI25
#' si_nighcourtin1998(
#'   age = c(10, 25, 40),
#'   si = c(18, 23, 28),
#'   si50 = TRUE
#' )
#'
#' @export
si_nighcourtin1998 <- function(age, height = NULL, si = NULL, si50 = FALSE) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (!is.logical(si50) || length(si50) != 1L || is.na(si50)) {
    cli::cli_abort("{.arg si50} must be TRUE or FALSE.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nighcourtin1998_prepare(
    age = age,
    x = x,
    x_name = x_name,
    si50 = si50
  )

  if (mode == "predict_height") {
    h <- with(
      df,
      .nighcourtin1998_height(age = age, si = si)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nighcourtin1998}.",
        "i" = "Check inputs and model domain."
      ))
    }
    # Unreachable with validated domain: age > 0.5 and si > 1.3 imply
    # h = 1.3 + positive_term, so predicted height cannot be negative.
    # nocov start
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_nighcourtin1998}.",
        "i" = "Check inputs and model domain."
      ))
    }
    # nocov end

    return(dplyr::tibble(height = h))
  }

  si_est <- with(
    df,
    .nighcourtin1998_si(age = age, height = height)
  )
  if (isTRUE(si50)) {
    si_est <- .nighcourtin1998_si25_to_si50(si_est)
  }

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nighcourtin1998}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_nighcourtin1998}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nighcourtin1998_height <- function(age, si) {
  1.3 + 1.693 * (si - 1.3) /
    (1 + exp(3.600 - 1.240 * log(age - 0.5)))
}


# internal
.nighcourtin1998_si <- function(age, height) {
  1.3 + (height - 1.3) *
    (0.5906 + 21.61 * exp(-1.240 * log(age - 0.5)))
}


# internal
.nighcourtin1998_si25_to_si50 <- function(si25) {
  -0.4063 + 1.313 * si25
}


# internal
.nighcourtin1998_si50_to_si25 <- function(si50) {
  (si50 + 0.4063) / 1.313
}


# internal
.nighcourtin1998_prepare <- function(age, x, x_name, si50) {
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

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0.5, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x)
  )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
    if (isTRUE(si50)) {
      out$si <- .nighcourtin1998_si50_to_si25(out$si)
    }
    if (any(out$si <= 1.3)) {
      cli::cli_abort(
        "{.arg si} must contain values that correspond to SI25 > 1.3 for {.fn si_nighcourtin1998}."
      )
    }
  }

  out
}
