#' Nigh (2000) site index model for interior western redcedar (BC)
#'
#' Implementation of the Nigh (2000) polymorphic
#' site-index equation for interior western redcedar (\code{THUJ.PLI}).
#'
#' \strong{Species coverage:} \code{THUJ.PLI}.
#'
#' \strong{Geographic use:} interior British Columbia.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} site index in this model is referenced to base age
#' 50 years at breast height.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs/outputs are metric (m), matching the source model.
#'
#' Predicting `height` from `si` is direct. Predicting `si` from `height` is
#' implicit and solved numerically with `stats::uniroot()`.
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Top height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at
#'   breast height). If provided, `height` is predicted.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Nigh, G. D. (2000). Western redcedar site index models for the interior of
#' British Columbia. British Columbia Ministry of Forests, Research Report 18.
#'
#' @examples
#' # Predict site index from age + height
#' si_nigh2000(
#'   age = c(25, 50, 90),
#'   height = c(9, 16, 27)
#' )
#'
#' # Predict height from age + site index
#' si_nigh2000(
#'   age = c(25, 50, 90),
#'   si = c(12, 16, 22)
#' )
#'
#' @export
si_nigh2000 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nigh2000_prepare(
    age = age,
    x = x,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- with(
      df,
      .nigh2000_height(age = age, si = si)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh2000}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_nigh2000}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .nigh2000_solve_si_one(
        age = df$age[[i]],
        height = df$height[[i]]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh2000}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_nigh2000}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nigh2000_height <- function(age, si) {
  num <- 1 + exp(9.474 - 1.340 * log(49.5) - 1.244 * log(si - 1.3))
  den <- 1 + exp(9.474 - 1.340 * log(age - 0.5) - 1.244 * log(si - 1.3))
  1.3 + (si - 1.3) * num / den
}


# internal
.nigh2000_prepare <- function(age, x, x_name) {
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
    if (any(out$si <= 1.3)) {
      cli::cli_abort(
        "{.arg si} must contain values > 1.3 for {.fn si_nigh2000}."
      )
    }
  }

  out
}


# internal
.nigh2000_solve_si_one <- function(age, height) {
  f <- function(si) {
    .nigh2000_height(age = age, si = si) - height
  }

  lower <- 1.300001
  upper <- max(60, height * 3)
  bracket <- NULL

  for (iter in seq_len(8)) {
    grid <- unique(c(lower, seq(1.31, upper, length.out = 300)))
    vals <- vapply(grid, f, numeric(1))

    keep <- is.finite(vals)
    grid <- grid[keep]
    vals <- vals[keep]

    if (length(vals) >= 2L) {
      exact <- which(vals == 0)
      if (length(exact) > 0L) {
        return(grid[exact[[1]]])
      }

      idx <- which(vals[-1] * vals[-length(vals)] < 0)
      if (length(idx) > 0L) {
        i <- idx[[1]]
        bracket <- c(grid[[i]], grid[[i + 1L]])
        break
      }
    }

    upper <- upper * 2
  }

  if (is.null(bracket)) {
    cli::cli_abort(c(
      "Failed to bracket a site-index solution in {.fn si_nigh2000}.",
      "i" = "Check age/height inputs are within model domain."
    ))
  }

  stats::uniroot(
    f,
    interval = bracket,
    tol = .Machine$double.eps^0.5
  )$root
}
