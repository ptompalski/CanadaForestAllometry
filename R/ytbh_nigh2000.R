#' Nigh (2000) years-to-breast-height model for interior western redcedar
#'
#' Iimplementation of the Nigh (2000) years-to-breast-height model
#' for interior western redcedar (\code{THUJ.PLI}).
#'
#' The source equation is:
#' \deqn{YTBH = 18.18 - 0.5526 \times SI}
#'
#' @param si Numeric vector. Site index (m, base age 50 years at breast height).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{ytbh}{Predicted years to breast height (years).}
#' }
#'
#' @references
#' Nigh, G. D. (2000). Western redcedar site index models for the interior of
#' British Columbia. British Columbia Ministry of Forests, Research Report 18.
#'
#' @examples
#' ytbh_nigh2000(
#'   si = c(10, 15, 20)
#' )
#'
#' @export
ytbh_nigh2000 <- function(si) {
  n <- length(si)
  if (n == 0L) {
    cli::cli_abort("{.arg si} must have length > 0.")
  }

  assert_numeric_vec(si, "si", finite = TRUE, gt = 0, allow_na = FALSE)

  ytbh <- 18.18 - 0.5526 * as.numeric(si)

  if (any(!is.finite(ytbh))) {
    cli::cli_abort(c(
      "Non-finite years-to-breast-height prediction generated in {.fn ytbh_nigh2000}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(ytbh < 0)) {
    cli::cli_abort(c(
      "Negative years-to-breast-height prediction generated in {.fn ytbh_nigh2000}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(ytbh = ytbh)
}
