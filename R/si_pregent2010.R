#' Prégent et al. (2010) site index model for white spruce plantations
#'
#' Implementation of the Chapman-Richards difference equation reported by
#' Prégent et al. (2010) for white spruce plantations (\code{PICE.GLA}) in
#' Quebec.
#'
#' \strong{Species coverage:} \code{PICE.GLA}.
#'
#' \strong{Geographic use:} Quebec white spruce plantations.
#'
#' \strong{Age definition note:} `age` is plantation age (years since planting),
#' not breast-height age.
#'
#' \strong{Height definition note:} the model uses dominant height (m) based on
#' the mean height of the 100 tallest trees per hectare.
#'
#' \strong{Base-age note:} site index is dominant height at plantation age
#' 25 years by default, matching the source study. The underlying difference
#' equation is base-age invariant, so other positive `base_age` values can also
#' be used. The package article plots this model with `base_age = 50` for
#' visual comparison with other curves.
#'
#' \strong{Domain note:} the source tables and curves are presented for
#' plantation ages up to 60 years. This implementation warns when `age > 60`
#' and returns extrapolated values.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs and outputs are metric and follow the source model scale directly.
#'
#' @param age Numeric vector. Plantation age (years), with `age > 0`. A warning
#'   is emitted when `age > 60`, because that exceeds the published range.
#' @param height Optional numeric vector. Dominant height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m) at `base_age` years. If
#'   provided, `height` is predicted.
#' @param base_age Positive numeric scalar. Site-index base age (years since
#'   planting). Defaults to `25`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted dominant height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Prégent, G., Picher, G., & Auger, I. (2010). \emph{Tarif de cubage, tables
#' de rendement et modèles de croissance pour les plantations d'épinette
#' blanche au Québec}. Mémoire de recherche forestière no 160. Gouvernement du
#' Québec, ministère des Ressources naturelles et de la Faune.
#'
#' @examples
#' si_pregent2010(
#'   age = c(20, 30, 40),
#'   height = c(6, 10, 14)
#' )
#'
#' si_pregent2010(
#'   age = c(20, 30, 40),
#'   si = c(8, 10, 12)
#' )
#'
#' @export
si_pregent2010 <- function(age, height = NULL, si = NULL, base_age = 25) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (!is.numeric(base_age) || length(base_age) != 1L || is.na(base_age) || !is.finite(base_age) || base_age <= 0) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value > 0.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .pregent2010_prepare(
    age = age,
    x = x,
    x_name = x_name,
    base_age = as.numeric(base_age)
  )

  if (mode == "predict_height") {
    h <- .pregent2010_height(
      age = df$age,
      si = df$si,
      base_age = df$base_age
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_pregent2010}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_pregent2010}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- .pregent2010_si(
    age = df$age,
    height = df$height,
    base_age = df$base_age
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_pregent2010}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_pregent2010}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.pregent2010_prepare <- function(age, x, x_name, base_age) {
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
  if (any(age > 60)) {
    cli::cli_warn(c(
      "{.arg age} contains values > 60 for {.fn si_pregent2010}.",
      "i" = "Predictions beyond 60 years are extrapolations outside the published plantation-age range."
    ))
  }

  asymptote <- .pregent2010_params()$beta0
  if (any(x >= asymptote)) {
    cli::cli_abort(
      "{.arg {x_name}} must contain values < {format(asymptote, trim = TRUE)} for {.fn si_pregent2010}."
    )
  }

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    base_age = base_age
  )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
.pregent2010_params <- function() {
  list(
    beta0 = 34.6683,
    beta2 = 1.5077
  )
}


# internal
.pregent2010_height <- function(age, si, base_age) {
  pars <- .pregent2010_params()

  pars$beta0 *
    (
      1 -
        (
          1 - (si / pars$beta0)^(1 / pars$beta2)
        )^(age / base_age)
    )^pars$beta2
}


# internal
.pregent2010_si <- function(age, height, base_age) {
  pars <- .pregent2010_params()

  pars$beta0 *
    (
      1 -
        (
          1 - (height / pars$beta0)^(1 / pars$beta2)
        )^(base_age / age)
    )^pars$beta2
}
