#' Cieszewski and Bella (1991) site-index model for major Alberta tree species
#'
#' Vectorized implementation of the polymorphic, variable-age site-index (VASI)
#' model described by Cieszewski and Bella (1991) for four major Alberta tree
#' species.
#'
#' \strong{Model scope (species coverage):} \code{PINU.CON, PICE.GLA,
#' PICE.MAR, POPU.TRE}.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} site index is referenced to 50 years breast-height
#' age for all species in this implementation.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Top height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at
#'   breast height). If provided, `height` is predicted.
#' @param species Character vector of NFI species codes.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Cieszewski, C. J., & Bella, I. E. (1991). Polymorphic height and site index
#' curves for the major tree species in Alberta. \emph{Forest Management Note},
#' 51, 1-8.
#'
#' @examples
#' # Predict site index from age + height
#' si_cieszewskibella1991(
#'   age = c(50, 60, 70),
#'   height = c(14, 17, 20),
#'   species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
#' )
#'
#' # Predict height from age + site index
#' si_cieszewskibella1991(
#'   age = c(25, 50, 80),
#'   si = c(12, 16, 20),
#'   species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
#' )
#'
#' @export
si_cieszewskibella1991 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .cieszewskibella1991_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .cieszewskibella1991_height_one(
          age = df$age[[i]],
          si = df$si[[i]],
          a = df$a[[i]],
          b = df$b[[i]],
          base_age_bh = df$base_age_bh[[i]]
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_cieszewskibella1991}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_cieszewskibella1991}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .cieszewskibella1991_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        a = df$a[[i]],
        b = df$b[[i]],
        base_age_bh = df$base_age_bh[[i]]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_cieszewskibella1991}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_cieszewskibella1991}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.cieszewskibella1991_prepare <- function(age, x, species, x_name) {
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
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 1.3, allow_na = FALSE)

  species_std <- standardize_species_code(species)
  pars <- .cieszewskibella1991_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$a) || anyNA(out$b)) {
    bad <- unique(out$Species[is.na(out$a) | is.na(out$b)])
    cli::cli_abort(
      "No CieszewskiBella1991 parameters found for species: {paste(bad, collapse = ', ')}."
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
.cieszewskibella1991_height_one <- function(age, si, a, b, base_age_bh) {
  s <- si - 1.3
  if (!is.finite(s) || s <= 0) {
    return(NaN)
  }

  j <- -1 - a
  d <- 20 * b * (base_age_bh^j)
  root <- d + 2 * s
  denom <- 2 + (80 * b * (age^j)) / (root - d)

  if (!is.finite(denom) || denom == 0) {
    return(NaN)
  }

  (root + d) / denom + 1.3
}


# internal
.cieszewskibella1991_si_one <- function(age, height, a, b, base_age_bh) {
  q <- height - 1.3
  if (!is.finite(q) || q <= 0) {
    return(NaN)
  }

  j <- -1 - a
  d <- 20 * b * (base_age_bh^j)
  root <- q + sqrt((q - d)^2 + (80 * b * q * (age^j)))

  1.3 + (root - d) / 2
}


# internal
.cieszewskibella1991_parameters <- function() {
  pars <- .get_internal_data("parameters_CieszewskiBella1991") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c("Species", "a", "b", "base_age_bh")
  assert_required_cols(pars, req, object = "parameters_CieszewskiBella1991")
  pars
}
