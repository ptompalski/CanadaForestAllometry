#' Huang et al. (2026) non-climate height-age models for Alberta
#'
#' Vectorized implementation of equations 3.2, 3.4, 3.6 and 3.8,
#' with the separately fitted non-climate coefficients in Table 3.4.
#' Covers aspen (`POPU.TRE`), white spruce (`PICE.GLA`), lodgepole pine
#' (`PINU.CON`), black spruce (`PICE.MAR`), balsam poplar (`POPU.BAL`),
#' balsam fir (`ABIE.BAL`), Douglas-fir (`PSEU.MEN`) and jack pine (`PINU.BAN`).
#'
#' Provide exactly one of `height` or `si`. Height is predicted directly from
#' age and site index; site index is recovered numerically from age and height.
#' Both age and the reference age use **total age**, not breast-height age.
#'
#' @details
#' The source's height index (HI), exposed here as `si`, is tree height at
#' 50 years total age. Models were fitted to stem-analysis trajectories of
#' dominant and codominant trees up to 180 total years. Predictions at older
#' ages generate a warning. Inversion from young trees, especially before
#' approximately 15--20 years, can be inaccurate.
#'
#' For aspen, balsam poplar and black spruce, the published logarithm requires
#' \eqn{b_0 SI^{b_1-1} > 1}. For white spruce and balsam fir, the square root
#' of \eqn{\ln(SI)} requires `si >= 1`. Impossible height-age pairs and multiple
#' site-index solutions under extrapolation raise explicit errors.
#'
#' The balsam poplar, balsam fir, Douglas-fir and jack pine fits pool data with
#' aspen, white spruce, lodgepole pine and lodgepole pine, respectively, but
#' each species has its own coefficients. No larch proxy is applied.
#'
#' These are not the report's climate-sensitive models evaluated at baseline
#' climate: the non-climate models were fitted separately. Source validation
#' uses plotted curves; no exact tabulated prediction benchmark is available.
#'
#' @param age Numeric vector. Positive total tree age (years).
#' @param height Optional numeric vector. Positive dominant/codominant tree
#'   height (m). If supplied, `si` is predicted.
#' @param si Optional numeric vector. Site index (m), equivalent to the source's
#'   height index at 50 years total age. If supplied, `height` is predicted.
#' @param species Character vector of NFI species codes; compact codes are
#'   normalized (e.g. `"PICEGLA"` becomes `"PICE.GLA"`).
#'
#' @return A tibble with one column: `height` (m) when `si` is supplied, or
#'   `si` (m) when `height` is supplied.
#'
#' @references
#' Huang, S., Stadt, K., Hossain, K., and Odell, R. (2026).
#' Improved Height Growth Models for Major Alberta Tree Species with and
#' without Climate Variables. Technical Report T/2026-RIBS01.
#' Alberta Ministry of Forestry and Parks, Edmonton, Alberta.
#'
#' @examples
#' si_huang2026(age = c(25, 50, 80), si = 15, species = "PICE.GLA")
#' si_huang2026(age = c(40, 70), height = c(12, 20), species = "POPU.TRE")
#'
#' @export
si_huang2026 <- function(age, height = NULL, si = NULL, species) {
  if (!xor(is.null(height), is.null(si))) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  predict_height <- is.null(height)
  x_name <- if (predict_height) "si" else "height"
  df <- .huang2026_prepare(
    age, if (predict_height) si else height, species, x_name
  )

  out <- vapply(seq_len(nrow(df)), function(i) {
    pars <- df[i, , drop = FALSE]
    if (!predict_height) {
      return(.huang2026_solve_si_one(df$age[[i]], df$x[[i]], pars))
    }
    log_si <- log(df$x[[i]])
    if (pars$model_form == "log_logistic_asymptote" &&
        log_si >= log(pars$b0) / (1 - pars$b1)) {
      cli::cli_abort("{.arg si} is outside the Huang2026 model domain for {pars$Species}.")
    }
    if (pars$model_form == "log_logistic_log_power" && log_si < 0) {
      cli::cli_abort("{.arg si} must be at least 1 m for {pars$Species}.")
    }
    exp(.huang2026_log_height(df$age[[i]], log_si, pars))
  }, numeric(1))

  if (any(!is.finite(out) | out <= 0)) {
    cli::cli_abort("Non-finite or non-positive prediction generated in {.fn si_huang2026}.")
  }
  if (predict_height) tibble::tibble(height = out) else tibble::tibble(si = out)
}

.huang2026_prepare <- function(age, x, species, x_name) {
  if (any(c(length(age), length(x), length(species)) == 0L)) {
    cli::cli_abort("Inputs must have length > 0.")
  }
  v <- assert_len_compat(age = age, x = x, species = species)
  assert_numeric_vec(v$age, "age", gt = 0)
  assert_numeric_vec(v$x, x_name, gt = 0)
  pars <- .get_internal_data("parameters_Huang2026")
  df <- tibble::tibble(
    age = v$age, x = v$x, Species = standardize_species_code(v$species)
  ) |>
    dplyr::left_join(pars, by = dplyr::join_by("Species"))
  if (anyNA(df$model_form)) {
    bad <- unique(df$Species[is.na(df$model_form)])
    cli::cli_abort(
      "No Huang2026 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }
  if (any(df$age > 180)) {
    cli::cli_warn(
      "Ages above 180 years extrapolate beyond the Huang2026 study.",
      class = "cfa_huang2026_extrapolation"
    )
  }
  df
}

.huang2026_log_height <- function(age, log_si, pars) {
  if (age == 50) {
    return(log_si)
  }
  if (pars$model_form == "log_logistic_asymptote") {
    log_k <- log(pars$b0) + pars$b1 * log_si
    eta <- log(expm1(log(pars$b0) + (pars$b1 - 1) * log_si)) +
      pars$b2 * (log1p(age) - log(51))
    return(log_k + stats::plogis(-eta, log.p = TRUE))
  }
  power <- if (pars$model_form == "log_logistic_log_power") 1.5 else 1
  common <- pars$b1 + pars$b3 * log_si^power
  eta50 <- common + pars$b2 * log(50 + pars$b4)
  eta <- common + pars$b2 * log(age + pars$b4)
  log_si + stats::plogis(-eta, log.p = TRUE) -
    stats::plogis(-eta50, log.p = TRUE)
}

.huang2026_solve_si_one <- function(age, height, pars) {
  asymptote <- pars$model_form == "log_logistic_asymptote"
  lower <- if (pars$model_form == "log_logistic_log_power") {
    0
  } else {
    log(.Machine$double.xmin) + log(.Machine$double.eps)
  }
  upper <- if (asymptote) log(pars$b0) / (1 - pars$b1) else log(.Machine$double.xmax)
  target <- log(height)
  f <- function(log_si) .huang2026_log_height(age, log_si, pars) - target
  if (asymptote && target >= upper) {
    cli::cli_abort("No site-index solution within the Huang2026 model domain.")
  }
  breaks <- c(lower, .huang2026_turning_points(age, pars, lower, upper), upper)
  # The upper endpoint of the asymptote form is an excluded limit, H -> SI.
  values <- if (asymptote) {
    c(f(lower), upper - target)
  } else {
    vapply(breaks, f, numeric(1))
  }
  exact <- which(values == 0)
  crossing <- which(
    sign(values[-length(values)]) * sign(values[-1]) < 0
  )
  n_roots <- length(exact) + length(crossing)
  if (n_roots == 0L) {
    cli::cli_abort("No site-index solution within the Huang2026 model domain.")
  }
  if (n_roots > 1L) {
    cli::cli_abort(
      "Multiple site-index solutions in {.fn si_huang2026}; the extrapolated height-age pair is ambiguous."
    )
  }
  if (length(exact) == 1L) {
    return(exp(breaks[[exact]]))
  }
  j <- crossing[[1]]
  root <- stats::uniroot(
    f, breaks[c(j, j + 1L)],
    f.lower = values[[j]], f.upper = values[[j + 1L]],
    tol = .Machine$double.eps^0.5, check.conv = TRUE
  )$root
  exp(root)
}

.huang2026_turning_points <- function(age, pars, lower, upper) {
  # Table 3.4's asymptote and sqrt-log fits are monotone in SI on their domains.
  if (age <= 50 || pars$model_form != "log_logistic_log") {
    return(numeric())
  }
  a <- pars$b1 + pars$b2 * log(50 + pars$b4)
  b <- pars$b1 + pars$b2 * log(age + pars$b4)
  if (b == a) {
    return(numeric())
  }
  # In v = log(SI), log of the negative elasticity contribution is
  # concave. Its zeros partition the curve into all its monotone branches.
  g <- function(v) {
    log(-pars$b3) + log(-expm1(b - a)) +
      stats::plogis(a + pars$b3 * v, log.p = TRUE) +
      stats::plogis(-b - pars$b3 * v, log.p = TRUE)
  }
  interval <- c(lower, upper)
  peak <- stats::optimize(g, interval, maximum = TRUE)
  if (peak$objective <= 0) {
    return(numeric())
  }
  brackets <- Filter(function(bounds) {
    sign(g(bounds[[1]])) * sign(g(bounds[[2]])) < 0
  }, list(
    c(interval[[1]], peak$maximum), c(peak$maximum, interval[[2]])
  ))
  roots <- vapply(brackets, function(bounds) {
    stats::uniroot(g, bounds, check.conv = TRUE, tol = .Machine$double.eps^0.5)$root
  }, numeric(1))
  roots
}
