#' Ker and Bowling (1991) site index model
#'
#' Unified, vectorized implementation of the Ker and Bowling (1991)
#' polymorphic site-index equation (model 1) for New Brunswick softwoods.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' In model 1, predicting `height` from `si` is direct. Predicting `si` from
#' `height` is implicit (site index appears both in a linear term and in the
#' exponent), so there is no closed-form algebraic inverse. For that mode, this
#' implementation solves for `si` numerically with `stats::uniroot()`.
#'
#' Inputs/outputs are metric (m), matching the source model.
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at
#'   breast height). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"ABIE.BAL"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Ker, M. F., & Bowling, C. (1991). Polymorphic site index equations for four
#' New Brunswick softwood species. \emph{Canadian Journal of Forest Research},
#' 21(5), 728-732.
#'
#' @examples
#' # Predict site index from age + height
#' si_kerbowling1991(
#'   age = c(25, 40, 60),
#'   height = c(8, 14, 20),
#'   species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
#' )
#'
#' # Predict height from age + site index
#' si_kerbowling1991(
#'   age = c(25, 40, 60),
#'   si = c(11, 13, 16),
#'   species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
#' )
#'
#' # Works with the pipe operator: predict site index from age + height
#' trees_h <- tibble::tibble(
#'   species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN"),
#'   age = c(25, 40, 60),
#'   height = c(8, 14, 20)
#' )
#'
#' trees_h |>
#'   dplyr::mutate(
#'     si_pred = si_kerbowling1991(
#'       age = age,
#'       height = height,
#'       species = species
#'     )
#'   ) |>
#'   tidyr::unnest(si_pred)
#'
#' # With pipe: predict height from age + site index
#' trees_si <- tibble::tibble(
#'   species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN"),
#'   age = c(25, 40, 60),
#'   si = c(11, 13, 16)
#' )
#'
#' trees_si |>
#'   dplyr::mutate(
#'     height_pred = si_kerbowling1991(
#'       age = age,
#'       si = si,
#'       species = species
#'     )
#'   ) |>
#'   tidyr::unnest(height_pred)
#'
#' @export
si_kerbowling1991 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .kerbowling1991_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- with(
      df,
      1.3 + (b0 + b1 * si) * (1 - exp(-b2 * age))^(b3 + b4 * si)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_kerbowling1991}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_kerbowling1991}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .kerbowling1991_solve_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        b0 = df$b0[[i]],
        b1 = df$b1[[i]],
        b2 = df$b2[[i]],
        b3 = df$b3[[i]],
        b4 = df$b4[[i]]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_kerbowling1991}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_kerbowling1991}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.kerbowling1991_prepare <- function(age, x, species, x_name) {
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

  pars <- .get_internal_data("parameters_KerBowling1991") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c("Species", "b0", "b1", "b2", "b3", "b4")
  assert_required_cols(pars, req, object = "parameters_KerBowling1991")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$b0) || anyNA(out$b1) || anyNA(out$b2) ||
      anyNA(out$b3) || anyNA(out$b4)) {
    bad <- unique(out$Species[
      is.na(out$b0) | is.na(out$b1) | is.na(out$b2) | is.na(out$b3) | is.na(out$b4)
    ])
    cli::cli_abort(
      "No KerBowling1991 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
.kerbowling1991_solve_si_one <- function(age, height, b0, b1, b2, b3, b4) {
  f <- function(si) {
    1.3 + (b0 + b1 * si) * (1 - exp(-b2 * age))^(b3 + b4 * si) - height
  }

  lower <- 1e-6
  upper <- max(30, height * 2)

  f_lower <- f(lower)
  f_upper <- f(upper)

  iter <- 0L
  while (is.finite(f_lower) && is.finite(f_upper) &&
         sign(f_lower) == sign(f_upper) && iter < 20L) {
    upper <- upper * 2
    f_upper <- f(upper)
    iter <- iter + 1L
  }

  if (!is.finite(f_lower) || !is.finite(f_upper) || sign(f_lower) == sign(f_upper)) {
    cli::cli_abort(c(
      "Failed to bracket a site-index solution in {.fn si_kerbowling1991}.",
      "i" = "Check that age, height, and species are within model domain."
    ))
  }

  stats::uniroot(
    f,
    lower = lower,
    upper = upper,
    tol = .Machine$double.eps^0.25
  )$root
}
