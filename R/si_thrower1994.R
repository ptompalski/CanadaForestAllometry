#' Thrower et al. (1994) site index models for BC interior species
#'
#' Unified, vectorized implementation of the field-guide formulations in
#' Thrower, Nussbaum, and Di Lucca (1994) for interior British Columbia species.
#'
#' \strong{Model scope (species coverage):} this implementation includes
#' parameter sets for 11 species:
#' \code{PINU.CON, PICE.GLA, PSEU.MEN, ABIE.LAS, TSUG.HET, THUJ.PLI,
#' PINU.MON, PINU.PON, LARI.OCC, POPU.TRE, BETU.PAP}.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} site index in this model family is referenced to
#' height at 50 years breast-height age.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Top height (m). If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast height).
#'   If provided, `height` is predicted.
#' @param species Character vector of NFI species codes (e.g., `"PINU.CON"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Thrower, J.S., Nussbaum, A.F., and Di Lucca, C.M. (1994). Site index curves
#' and tables for British Columbia: interior species (2nd ed.).
#' B.C. Ministry of Forests, Land Management Handbook, Field Guide Insert 6.
#'
#' @examples
#' # Predict site index from age + height
#' si_thrower1994(
#'   age = c(25, 40, 70),
#'   height = c(8, 16, 24),
#'   species = c("PINU.CON", "PSEU.MEN", "THUJ.PLI")
#' )
#'
#' # Predict height from age + site index
#' si_thrower1994(
#'   age = c(25, 40, 70),
#'   si = c(12, 18, 24),
#'   species = c("PINU.CON", "PSEU.MEN", "THUJ.PLI")
#' )
#'
#' @export
si_thrower1994 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .thrower1994_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .thrower1994_height_one(
          age = df$age[[i]],
          si = df$si[[i]],
          pars = df[i, , drop = FALSE]
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_thrower1994}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_thrower1994}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .thrower1994_solve_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_thrower1994}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_thrower1994}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.thrower1994_prepare <- function(age, x, species, x_name) {
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
  pars <- .thrower1994_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$model_form)) {
    bad <- unique(out$Species[is.na(out$model_form)])
    cli::cli_abort(
      "No Thrower1994 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
    # nocov start
    # Unreachable via public API because `assert_numeric_vec(..., gt = 0)`
    # already rejects non-positive `si` before this defensive check.
    if (any(out$si <= 0)) {
      cli::cli_abort("{.arg si} must contain values > 0.")
    }
    # nocov end
  }

  out
}


# internal
.thrower1994_height_one <- function(age, si, pars) {
  form <- pars$model_form[[1]]

  if (form == "form_logexp_bratio") {
    si_shift <- si - pars$s_base[[1]]
    if (!is.finite(si_shift) || si_shift <= 0) {
      return(NaN)
    }

    b1 <- 1 + exp(
      pars$b1_const[[1]] +
        pars$b1_ln50_coef[[1]] * log(50) +
        pars$b1_lnS_coef[[1]] * log(si_shift)
    )
    b2 <- 1 + exp(
      pars$b2_const[[1]] +
        pars$b2_lnA_coef[[1]] * log(age) +
        pars$b2_lnS_coef[[1]] * log(si_shift)
    )

    return(pars$h_base[[1]] + (si - pars$s_base[[1]]) * (b1 / b2))
  }

  if (form == "form_rational_a2") {
    if (identical(pars$Species[[1]], "TSUG.HET")) {
      s_term <- si / pars$s_scale[[1]] - pars$s_base[[1]]
    } else {
      s_term <- si - pars$s_base[[1]]
    }
    if (!is.finite(s_term) || s_term <= 0) {
      return(NaN)
    }

    b1 <- pars$b1_const[[1]] + pars$b1_s_div_coef[[1]] / s_term
    b2 <- pars$b2_const[[1]] + pars$b2_s_div_coef[[1]] / s_term
    b3 <- pars$b3_const[[1]] + pars$b3_s_div_coef[[1]] / s_term

    base_h <- pars$h_base[[1]] + age^2 / (b1 + b2 * age + b3 * age^2)
    h <- pars$h_scale[[1]] * base_h

    if (identical(pars$Species[[1]], "ABIE.LAS")) {
      if (age < 50 && age * h < pars$adj_AH[[1]]) {
        h <- h + pars$adj_const[[1]] + pars$adj_AH_coef[[1]] * age * h
      }
    } else if (identical(pars$Species[[1]], "THUJ.PLI")) {
      if (age > pars$adj_age[[1]]) {
        h <- h + pars$adj_H_coef[[1]] * h + pars$adj_AH_coef[[1]] * age * h
      }
    }

    return(h)
  }

  if (form == "form_exp_exp_ratio") {
    s_ft <- si / pars$s_scale[[1]]
    s_term <- s_ft - pars$s_base[[1]]
    if (!is.finite(s_term) || s_term <= 0) {
      return(NaN)
    }

    if (identical(pars$Species[[1]], "PINU.MON")) {
      b3 <- pars$b3_const[[1]] + pars$b3_lnA_coef[[1]] * log(age)
      b4 <- pars$b4_const[[1]] +
        pars$b4_ln50_coef[[1]] * log(50) +
        pars$b4_lnA_coef[[1]] * log(age)

      b1 <- 1 - exp(-exp(b3 + pars$b1_lnS_coef[[1]] * log(s_ft)))
      b2 <- 1 - exp(-exp(b4 + pars$b2_lnS_coef[[1]] * log(s_ft)))
    } else {
      b3 <- pars$b3_const[[1]] + pars$b3_lnS_coef[[1]] * log(s_term)
      b1 <- 1 - exp(-exp(b3 + pars$b1_lnA_coef[[1]] * log(age)))
      b2 <- 1 - exp(-exp(b3 + pars$b2_ln50_coef[[1]] * log(50)))
    }

    base_h <- pars$h_base[[1]] + (s_term * (b1 / b2))
    return(pars$h_scale[[1]] * base_h)
  }

  if (form == "form_larch_additive") {
    b1 <- pars$b1_const[[1]] * (1 - exp(-pars$b1_rate[[1]] * age))^pars$b1_power[[1]]
    b2 <- pars$b2_const[[1]] * (1 - exp(-pars$b2_rate[[1]] * age))^pars$b2_power[[1]]
    base_h <- pars$h_base[[1]] + b1 + b2 * (si / pars$s_scale[[1]] - pars$s_center[[1]])
    return(pars$h_scale[[1]] * base_h)
  }

  NaN
}


# internal
.thrower1994_solve_si_one <- function(age, height, pars) {
  if (identical(pars$Species[[1]], "PSEU.MEN")) {
    return(pars$si_const[[1]] + pars$si_H_coef[[1]] * height + pars$si_H_over_A_coef[[1]] * height / age)
  }

  if (pars$model_form[[1]] == "form_larch_additive") {
    b1 <- pars$b1_const[[1]] * (1 - exp(-pars$b1_rate[[1]] * age))^pars$b1_power[[1]]
    b2 <- pars$b2_const[[1]] * (1 - exp(-pars$b2_rate[[1]] * age))^pars$b2_power[[1]]
    s_ft <- pars$s_center[[1]] + (height / pars$h_scale[[1]] - pars$h_base[[1]] - b1) / b2
    return(s_ft * pars$s_scale[[1]])
  }

  f <- function(si) {
    .thrower1994_height_one(age = age, si = si, pars = pars) - height
  }

  lower <- 1.300001
  upper <- max(60, height * 3)
  bracket <- NULL

  for (iter in seq_len(8)) {
    grid <- unique(c(lower, seq(lower + 0.01, upper, length.out = 400)))
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
      "Failed to bracket a site-index solution in {.fn si_thrower1994}.",
      "i" = "Check that age, height, and species are within model domain."
    ))
  }

  stats::uniroot(
    f,
    interval = bracket,
    tol = .Machine$double.eps^0.5
  )$root
}


# internal
.thrower1994_parameters <- function() {
  pars <- .get_internal_data("parameters_Thrower1994") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c(
    "Species",
    "model_form",
    "h_scale",
    "h_base",
    "s_scale",
    "s_base",
    "s_center",
    "b1_const",
    "b1_ln50_coef",
    "b1_lnA_coef",
    "b1_lnS_coef",
    "b1_s_div_coef",
    "b1_rate",
    "b1_power",
    "b2_const",
    "b2_ln50_coef",
    "b2_lnA_coef",
    "b2_lnS_coef",
    "b2_s_div_coef",
    "b2_rate",
    "b2_power",
    "b3_const",
    "b3_lnA_coef",
    "b3_lnS_coef",
    "b3_s_div_coef",
    "b4_const",
    "b4_ln50_coef",
    "b4_lnA_coef",
    "ytb_const",
    "ytb_s_div_coef",
    "ytb_lnS_coef",
    "ytb_s_linear_coef",
    "ytb_s_linear_scale",
    "adj_type",
    "adj_age",
    "adj_AH",
    "adj_const",
    "adj_H_coef",
    "adj_AH_coef",
    "si_const",
    "si_H_coef",
    "si_H_over_A_coef"
  )
  assert_required_cols(pars, req, object = "parameters_Thrower1994")

  pars
}
