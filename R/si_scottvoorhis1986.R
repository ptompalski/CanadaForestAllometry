#' Scott and Voorhis (1986) site index model
#'
#' Unified, vectorized implementation of the Scott and Voorhis (1986)
#' polymorphic site-index equation for northeastern species.
#'
#' \strong{Model scope (species coverage):} this implementation includes
#' parameter sets for 19 species:
#' \code{ABIE.BAL, PICE.GLA, PICE.MAR, PICE.RUB, PINU.BAN, PINU.ECH,
#' PINU.RES, PINU.STR, PINU.TAE, PINU.VIR, THUJ.OCC, ACER.SAC, BETU.ALL,
#' BETU.PAP, FRAX.AME, LIQU.STY, LIRI.TUL, POPU.GRA, QUER.ALB}.
#'
#' \strong{Geographic use (Canada):} the model is intended for northeastern
#' forest conditions. In Canada, it is most defensible for species/populations
#' in eastern regions (notably Atlantic Canada and adjacent central Canada where
#' these species occur). Use caution outside that domain.
#'
#' \strong{Age definition note:} `age` is breast-height age (years). The model
#' internally computes age-to-breast-height and uses total age in the height
#' equation.
#'
#' \strong{Base-age note:} site index in this model is referenced to base age
#' 50 years, but the fitted equations were not constrained to pass exactly
#' through SI at base age for every species.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' In this model, total age is the sum of breast-height age and age-to-breast-height.
#' Age-to-breast-height is itself a function of site index, so predicting `si` from
#' `height` is implicit and solved numerically with `stats::uniroot()`.
#'
#' Inputs/outputs are metric; the original equations are in imperial units, so
#' the function converts internally.
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at
#'   breast height). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"ABIE.BAL"`).
#' @param convert_to_total_age Logical scalar. If `TRUE`, converts
#'   breast-height age to total age internally using the Scott and Voorhis
#'   age-to-breast-height relation (source-consistent behavior). If `FALSE`
#'   (default), uses `age` directly in the height equation.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Scott, C. T., & Voorhis, N. G. (1986). Northeastern forest survey site
#' index equations and site productivity classes. \emph{Northern Journal of
#' Applied Forestry}, 3(4), 144-148.
#'
#' @examples
#' # Predict site index from age + height
#' si_scottvoorhis1986(
#'   age = c(25, 40, 60),
#'   height = c(8, 14, 20),
#'   species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
#' )
#'
#' # Predict height from age + site index
#' si_scottvoorhis1986(
#'   age = c(25, 40, 60),
#'   si = c(11, 13, 16),
#'   species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN"),
#'   convert_to_total_age = TRUE
#' )
#'
#' @export
si_scottvoorhis1986 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  convert_to_total_age = FALSE
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (
    !is.logical(convert_to_total_age) ||
      length(convert_to_total_age) != 1L ||
      is.na(convert_to_total_age)
  ) {
    cli::cli_abort("{.arg convert_to_total_age} must be TRUE or FALSE.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .scottvoorhis1986_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    total_age <- df$age
    if (isTRUE(convert_to_total_age)) {
      bh_age <- .scottvoorhis1986_bhage(
        si_ft = df$si_ft,
        b1 = df$b1,
        b2 = df$b2,
        b3 = df$b3,
        b4 = df$b4,
        b5 = df$b5
      )
      total_age <- total_age + bh_age
    }

    h_ft <- with(
      df,
      (b1 + b2 * si_ft) * (1 - exp(-b3 * total_age))^(b4 * (si_ft^b5))
    )

    if (any(!is.finite(h_ft))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_scottvoorhis1986}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_scottvoorhis1986}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h_ft / 3.28084))
  }

  si_ft_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .scottvoorhis1986_solve_si_one(
        age = df$age[[i]],
        height_ft = df$height_ft[[i]],
        b1 = df$b1[[i]],
        b2 = df$b2[[i]],
        b3 = df$b3[[i]],
        b4 = df$b4[[i]],
        b5 = df$b5[[i]],
        convert_to_total_age = convert_to_total_age
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_ft_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_scottvoorhis1986}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_ft_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_scottvoorhis1986}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_ft_est / 3.28084)
}


# internal
.scottvoorhis1986_prepare <- function(age, x, species, x_name) {
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

  pars <- .get_internal_data("parameters_ScottVoorhis1986") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c("Species", "b1", "b2", "b3", "b4", "b5")
  assert_required_cols(pars, req, object = "parameters_ScottVoorhis1986")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (
    anyNA(out$b1) ||
      anyNA(out$b2) ||
      anyNA(out$b3) ||
      anyNA(out$b4) ||
      anyNA(out$b5)
  ) {
    bad <- unique(out$Species[
      is.na(out$b1) |
        is.na(out$b2) |
        is.na(out$b3) |
        is.na(out$b4) |
        is.na(out$b5)
    ])
    cli::cli_abort(
      "No ScottVoorhis1986 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}


# internal
.scottvoorhis1986_bhage <- function(si_ft, b1, b2, b3, b4, b5) {
  den <- b1 + b2 * si_ft
  expo <- 1 / (b4 * (si_ft^b5))
  inside <- 1 - (4.5 / den)^expo

  bh_age <- -(1 / b3) * log(inside)

  bad <- !is.finite(bh_age) |
    !is.finite(inside) |
    !is.finite(den) |
    den <= 4.5 |
    inside <= 0 |
    inside >= 1

  bh_age[bad] <- NA_real_
  bh_age
}


# internal
.scottvoorhis1986_solve_si_one <- function(
  age,
  height_ft,
  b1,
  b2,
  b3,
  b4,
  b5,
  convert_to_total_age
) {
  f <- function(si_ft) {
    total_age <- age
    if (isTRUE(convert_to_total_age)) {
      bh_age <- .scottvoorhis1986_bhage(si_ft, b1, b2, b3, b4, b5)
      if (!is.finite(bh_age)) {
        return(NaN)
      }
      total_age <- total_age + bh_age
    }
    h_pred <- (b1 + b2 * si_ft) * (1 - exp(-b3 * total_age))^(b4 * (si_ft^b5))
    h_pred - height_ft
  }

  lower <- max(1e-6, (4.5 - b1) / b2 + 1e-6)
  upper <- max(60, height_ft * 3, lower * 2)

  f_lower <- f(lower)
  while (!is.finite(f_lower) && lower < upper) {
    lower <- lower * 1.5
    f_lower <- f(lower)
  }

  f_upper <- f(upper)
  iter <- 0L
  while (
    is.finite(f_lower) &&
      (!is.finite(f_upper) || sign(f_lower) == sign(f_upper)) &&
      iter < 30L
  ) {
    upper <- upper * 2
    f_upper <- f(upper)
    iter <- iter + 1L
  }

  if (
    !is.finite(f_lower) || !is.finite(f_upper) || sign(f_lower) == sign(f_upper)
  ) {
    cli::cli_abort(c(
      "Failed to bracket a site-index solution in {.fn si_scottvoorhis1986}.",
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
