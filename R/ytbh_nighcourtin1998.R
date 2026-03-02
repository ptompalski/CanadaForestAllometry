#' Nigh and Courtin (1998) years-to-breast-height model for red alder
#'
#' Implementation of the Nigh and Courtin (1998) years-to-breast-height
#' model for red alder (\code{ALNU.RUB}) in coastal British Columbia.
#'
#' The source relation is piecewise:
#' \deqn{
#' YTBH = 5.494 - 0.1789 \times SI25, \quad SI25 \le 25
#' }
#' \deqn{
#' YTBH = 1.0, \quad SI25 > 25
#' }
#'
#' @param si Numeric vector. Site index SI25 (m at breast-height age 25 years).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{ytbh}{Predicted years to breast height (years).}
#' }
#'
#' @references
#' Nigh, G. D., & Courtin, P. J. (1998). Height models for Red Alder
#' (\emph{Alnus rubra} Bong.) in British Columbia.
#' \emph{New Forests}, 16, 59-70.
#'
#' @examples
#' ytbh_nighcourtin1998(
#'   si = c(12, 20, 28)
#' )
#'
#' @export
ytbh_nighcourtin1998 <- function(si) {
  n <- length(si)
  if (n == 0L) {
    cli::cli_abort("{.arg si} must have length > 0.")
  }

  assert_numeric_vec(si, "si", finite = TRUE, gt = 0, allow_na = FALSE)

  si <- as.numeric(si)
  ytbh <- ifelse(si <= 25, 5.494 - 0.1789 * si, 1.0)

  # Unreachable with validated inputs: `si` is finite and `ytbh` is a piecewise
  # linear transform of `si`, so predictions remain finite.
  # nocov start
  if (any(!is.finite(ytbh))) {
    cli::cli_abort(c(
      "Non-finite years-to-breast-height prediction generated in {.fn ytbh_nighcourtin1998}.",
      "i" = "Check inputs and model domain."
    ))
  }
  # nocov end
  # Unreachable with this capped piecewise function: for SI25 > 25, YTBH is
  # set to 1.0, so predictions cannot be negative.
  # nocov start
  if (any(ytbh < 0)) {
    cli::cli_abort(c(
      "Negative years-to-breast-height prediction generated in {.fn ytbh_nighcourtin1998}.",
      "i" = "Check inputs and model domain."
    ))
  }
  # nocov end

  dplyr::tibble(ytbh = ytbh)
}
