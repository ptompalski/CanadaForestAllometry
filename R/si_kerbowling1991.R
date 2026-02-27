#' Ker and Bowling (1991) site index model
#'
#' Unified, vectorized implementation of the Ker and Bowling (1991)
#' polymorphic site-index equation (model 1, conditioned form; eq. 9) for New
#' Brunswick softwoods.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' This implementation uses the conditioned equation at index age 50 years
#' (breast-height age), so predicted height equals site index at `age = 50`.
#'
#' Predicting `height` from `si` is direct. Predicting `si` from `height` is
#' implicit (site index also appears in the exponent term), so there is no
#' closed-form algebraic inverse. For that mode, this implementation solves for
#' `si` numerically with `stats::uniroot()`.
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
      1.3 +
        (si - 1.3) *
        (1 - exp(-b2 * 50))^(-b3 * (si^b4)) *
        (1 - exp(-b2 * age))^(b3 * (si^b4))
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

  req <- c("Species", "b2", "b3", "b4")
  assert_required_cols(pars, req, object = "parameters_KerBowling1991")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$b2) || anyNA(out$b3) || anyNA(out$b4)) {
    bad <- unique(out$Species[
      is.na(out$b2) | is.na(out$b3) | is.na(out$b4)
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
.kerbowling1991_solve_si_one <- function(age, height, b2, b3, b4) {
  f <- function(si) {
    1.3 +
      (si - 1.3) *
      (1 - exp(-b2 * 50))^(-b3 * (si^b4)) *
      (1 - exp(-b2 * age))^(b3 * (si^b4)) -
      height
  }

  lower <- 1.300001
  upper <- max(60, height * 3)

  bracket <- NULL
  for (iter in seq_len(8)) {
    grid <- unique(c(lower, seq(1.5, upper, length.out = 250)))
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
      "Failed to bracket a site-index solution in {.fn si_kerbowling1991}.",
      "i" = "Check that age, height, and species are within model domain."
    ))
  }

  stats::uniroot(
    f,
    lower = bracket[[1]],
    upper = bracket[[2]],
    tol = .Machine$double.eps^0.25
  )$root
}
