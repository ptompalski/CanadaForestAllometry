#' Carmean (1996) site index models for northwest Ontario
#'
#' Implementation of the northwest Ontario site-index equations documented in
#' Carmean (1996) and the appendix equations transcribed from that report.
#'
#' \strong{Species coverage:} \code{PINU.BAN}, \code{PICE.MAR},
#' \code{PICE.GLA}, \code{ABIE.BAL}, \code{POPU.TRE}, \code{BETU.PAP},
#' \code{LARI.LAR}.
#'
#' \strong{Geographic use:} northwest Ontario.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} site index is defined as total height (m) at
#' 50 years breast-height age.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs and outputs are metric (m). Some source equations were published in
#' mixed unit form and are converted internally using the species-specific
#' metadata stored in the internal parameter table.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Total tree height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at
#'   breast-height age). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"PINU.BAN"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted total tree height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Carmean, W. H. (1996). Site-quality evaluation, site-quality maintenance,
#' and site-specific management for forest land in northwest Ontario.
#' Ontario Ministry of Natural Resources, Northwest Science and Technology,
#' Technical Report TR-105.
#'
#' @examples
#' si_carmean1996(
#'   age = c(20, 40, 60),
#'   si = c(10, 14, 18),
#'   species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
#' )
#'
#' si_carmean1996(
#'   age = c(20, 40, 60),
#'   height = c(6, 12, 19),
#'   species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
#' )
#'
#' @export
si_carmean1996 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .carmean1996_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .carmean1996_height_one(
          age = df$age[[i]],
          si = df$si[[i]],
          pars = df[i, , drop = FALSE]
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_carmean1996}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_carmean1996}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .carmean1996_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_carmean1996}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_carmean1996}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.carmean1996_prepare <- function(age, x, species, x_name) {
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

  pars <- .get_internal_data("parameters_Carmean1996") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c(
    "Species", "model_family", "si_base_age_bh",
    "source_length_factor", "source_height_offset", "source_si_offset",
    "h_a", "h_b", "h_c"
  )
  assert_required_cols(pars, req, object = "parameters_Carmean1996")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$model_family)) {
    bad <- unique(out$Species[is.na(out$model_family)])
    cli::cli_abort(
      "No Carmean1996 parameters found for species: {paste(bad, collapse = ', ')}."
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
.carmean1996_height_one <- function(age, si, pars) {
  family <- pars$model_family[[1]]

  if (family == "family_k") {
    return(.carmean1996_height_family_k(age = age, si = si, pars = pars))
  }
  if (family == "family_black_spruce_logit") {
    return(.carmean1996_height_black_spruce(age = age, si = si, pars = pars))
  }
  if (family == "family_metric_ft_no_subtract") {
    return(.carmean1996_height_metric_ft_no_subtract(age = age, si = si, pars = pars))
  }
  if (family == "family_metric_ft_subtract45") {
    return(.carmean1996_height_metric_ft_subtract45(age = age, si = si, pars = pars))
  }
  if (family == "family_metric_ft_subtract45_linear") {
    return(.carmean1996_height_metric_ft_subtract45_linear(age = age, si = si, pars = pars))
  }

  cli::cli_abort("Unsupported Carmean1996 model family: {.val {family}}.")
}


# internal
.carmean1996_si_one <- function(age, height, pars) {
  family <- pars$model_family[[1]]

  if (family == "family_k") {
    return(.carmean1996_si_family_k(age = age, height = height, pars = pars))
  }
  if (family == "family_black_spruce_logit") {
    return(.carmean1996_si_black_spruce(age = age, height = height, pars = pars))
  }
  if (family == "family_metric_ft_no_subtract") {
    return(.carmean1996_si_metric_ft_no_subtract(age = age, height = height, pars = pars))
  }
  if (family == "family_metric_ft_subtract45") {
    return(.carmean1996_si_metric_ft_subtract45(age = age, height = height, pars = pars))
  }
  if (family == "family_metric_ft_subtract45_linear") {
    return(.carmean1996_si_metric_ft_subtract45_linear(age = age, height = height, pars = pars))
  }

  cli::cli_abort("Unsupported Carmean1996 model family: {.val {family}}.")
}


# internal
.carmean1996_height_family_k <- function(age, si, pars) {
  offset <- pars$source_si_offset[[1]]
  base_age <- pars$si_base_age_bh[[1]]

  if (si <= offset) {
    cli::cli_abort(
      "{.arg si} must be > {format(offset, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  s <- si - offset
  a_term <- pars$h_a[[1]] * (s^pars$h_b[[1]])
  p_term <- pars$h_c[[1]] * (s^pars$h_d[[1]])
  k_term <- 1 - (s / a_term)^(1 / p_term)

  offset + a_term * (1 - k_term^(age / base_age))^p_term
}


# internal
.carmean1996_si_family_k <- function(age, height, pars) {
  offset <- pars$source_si_offset[[1]]

  if (height <= offset) {
    cli::cli_abort(
      "{.arg height} must be > {format(offset, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  f <- function(si) {
    .carmean1996_height_family_k(age = age, si = si, pars = pars) - height
  }

  lower <- offset + 1e-6
  upper <- max(60, height * 3)
  bracket <- NULL

  for (iter in seq_len(8)) {
    grid <- unique(c(lower, seq(lower + 0.25, upper, length.out = 300)))
    vals <- vapply(grid, f, numeric(1))
    keep <- is.finite(vals)
    grid <- grid[keep]
    vals <- vals[keep]

    if (length(vals) >= 2L) {
      exact <- which(abs(vals) < 1e-12)
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
      "Failed to bracket a site-index solution in {.fn si_carmean1996}.",
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


# internal
.carmean1996_height_black_spruce <- function(age, si, pars) {
  offset <- pars$source_si_offset[[1]]
  base_age <- pars$si_base_age_bh[[1]]

  if (si <= offset) {
    cli::cli_abort(
      "{.arg si} must be > {format(offset, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  s <- si - offset
  num <- 1 + exp(pars$h_a[[1]] - pars$h_b[[1]] * log(base_age) - pars$h_c[[1]] * log(s))
  den <- 1 + exp(pars$h_a[[1]] - pars$h_b[[1]] * log(age) - pars$h_c[[1]] * log(s))

  offset + s * num / den
}


# internal
.carmean1996_si_black_spruce <- function(age, height, pars) {
  offset <- pars$source_height_offset[[1]]

  if (height <= offset) {
    cli::cli_abort(
      "{.arg height} must be > {format(offset, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  h <- height - offset
  pars$si_a[[1]] +
    pars$si_b[[1]] * h +
    pars$si_c[[1]] * log(h) +
    pars$si_d[[1]] * log(age) +
    pars$si_e[[1]] * (log(age)^2) +
    pars$si_f[[1]] * h / age
}


# internal
.carmean1996_height_metric_ft_no_subtract <- function(age, si, pars) {
  lf <- pars$source_length_factor[[1]]
  h0 <- pars$source_height_offset[[1]]

  (h0 +
    pars$h_a[[1]] *
      (lf * si)^pars$h_b[[1]] *
      (1 - exp(-pars$h_c[[1]] * age))^(pars$h_d[[1]] * (lf * si)^pars$h_e[[1]])) / lf
}


# internal
.carmean1996_si_metric_ft_no_subtract <- function(age, height, pars) {
  lf <- pars$source_length_factor[[1]]
  h0 <- pars$source_height_offset[[1]]

  (h0 +
    pars$si_a[[1]] *
      (lf * height)^pars$si_b[[1]] *
      (1 - exp(-pars$si_c[[1]] * age))^(pars$si_d[[1]] * (lf * height)^pars$si_e[[1]])) / lf
}


# internal
.carmean1996_height_metric_ft_subtract45 <- function(age, si, pars) {
  lf <- pars$source_length_factor[[1]]
  h0 <- pars$source_height_offset[[1]]
  s0 <- pars$source_si_offset[[1]]

  if (lf * si <= s0) {
    cli::cli_abort(
      "{.arg si} must be > {format(s0 / lf, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  (h0 +
    pars$h_a[[1]] *
      (lf * si - s0)^pars$h_b[[1]] *
      (1 - exp(-pars$h_c[[1]] * age))^(pars$h_d[[1]] * (lf * si - s0)^pars$h_e[[1]])) / lf
}


# internal
.carmean1996_si_metric_ft_subtract45 <- function(age, height, pars) {
  lf <- pars$source_length_factor[[1]]
  h0 <- pars$source_height_offset[[1]]

  if (lf * height <= h0) {
    cli::cli_abort(
      "{.arg height} must be > {format(h0 / lf, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  (h0 +
    pars$si_a[[1]] *
      (lf * height - h0)^pars$si_b[[1]] *
      (1 - exp(-pars$si_c[[1]] * age))^(pars$si_d[[1]] * (lf * height - h0)^pars$si_e[[1]])) / lf
}


# internal
.carmean1996_height_metric_ft_subtract45_linear <- function(age, si, pars) {
  lf <- pars$source_length_factor[[1]]
  h0 <- pars$source_height_offset[[1]]
  s0 <- pars$source_si_offset[[1]]

  if (lf * si <= s0) {
    cli::cli_abort(
      "{.arg si} must be > {format(s0 / lf, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  (h0 +
    pars$h_a[[1]] *
      (lf * si - s0)^pars$h_b[[1]] *
      (1 - exp(-pars$h_c[[1]] * age))^pars$h_d[[1]]) / lf
}


# internal
.carmean1996_si_metric_ft_subtract45_linear <- function(age, height, pars) {
  lf <- pars$source_length_factor[[1]]
  h0 <- pars$source_height_offset[[1]]

  if (lf * height <= h0) {
    cli::cli_abort(
      "{.arg height} must be > {format(h0 / lf, trim = TRUE)} for species {.val {pars$Species[[1]]}}."
    )
  }

  (h0 +
    pars$si_a[[1]] *
      (lf * height - h0)^pars$si_b[[1]] *
      (1 - exp(-pars$si_c[[1]] * age))^pars$si_d[[1]]) / lf
}
