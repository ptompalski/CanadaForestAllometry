#' Sharma and Parton (2018) site index model for red pine plantations
#'
#' Implementation of the non-climate-sensitive McDill-Amateis dynamic height
#' equation reported by Sharma and Parton (2018) for plantation-grown red pine
#' (\code{PINU.RES}) in Ontario.
#'
#' \strong{Species coverage:} \code{PINU.RES}.
#'
#' \strong{Geographic use:} Ontario red pine plantations.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Height definition note:} this implementation assumes the source
#' model uses heights above breast height (m), not total height. For
#' consistency with other site-index functions in this package, it defaults to
#' using total height in the public API (`total_height = TRUE`) and converts
#' internally by subtracting or adding 1.3 m as needed. Set
#' `total_height = FALSE` to work on the source scale directly.
#'
#' \strong{Base-age note:} the underlying dynamic equation is written in a
#' base-age invariant paired-age form. For consistency with other site-index
#' functions in this package, this implementation defaults to a base age of 50
#' years breast-height age, while still allowing users to supply any positive
#' `base_age`.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs and outputs are metric and match the source model scale.
#'
#' Both directions are explicit closed forms derived from the same dynamic
#' equation.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Stand height (m). If
#'   `total_height = TRUE` (default), this is total height; otherwise it is
#'   height above breast height. If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m above breast height) at
#'   `base_age` years breast-height age. If provided, `height` is predicted.
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
#' Sharma, M., & Parton, J. (2018). Climatic effects on site productivity of
#' red pine plantations. Forest Science, 64(5), 544-554.
#'
#' @examples
#' # Predict site index from age + height
#' si_sharmaparton2018b(
#'   age = c(20, 30, 40),
#'   height = c(7, 10, 13)
#' )
#'
#' # Predict height from age + site index
#' si_sharmaparton2018b(
#'   age = c(20, 30, 40),
#'   si = c(9, 11, 13)
#' )
#'
#' @export
si_sharmaparton2018b <- function(age, height = NULL, si = NULL, base_age = 50, total_height = TRUE) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (!is.numeric(base_age) || length(base_age) != 1L || is.na(base_age) || !is.finite(base_age) || base_age <= 0) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value > 0.")
  }
  if (!is.logical(total_height) || length(total_height) != 1L || is.na(total_height)) {
    cli::cli_abort("{.arg total_height} must be a single TRUE/FALSE value.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .sharmaparton2018b_prepare(
    age = age,
    x = x,
    x_name = x_name,
    base_age = as.numeric(base_age),
    total_height = total_height
  )

  if (mode == "predict_height") {
    h <- .mcdill_amateis_height(
      age = df$age,
      si = df$si,
      base_age = df$base_age,
      a0 = 43.7309,
      a1 = 1.2000
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_sharmaparton2018b}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_sharmaparton2018b}.",
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
    a0 = 43.7309,
    a1 = 1.2000
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_sharmaparton2018b}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_sharmaparton2018b}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.sharmaparton2018b_prepare <- function(age, x, x_name, base_age, total_height) {
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

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    base_age = base_age
  )

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
