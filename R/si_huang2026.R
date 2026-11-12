#' Huang et al. (2026) height-age models for Alberta with or without climate
#'
#' Vectorized implementation of the non-climate equations 3.2, 3.4, 3.6 and 3.8
#' (Table 3.4 coefficients), or climate equations 3.1, 3.3, 3.5 and 3.7
#' (Table 3.1 coefficients).
#' Covers aspen (`POPU.TRE`), white spruce (`PICE.GLA`), lodgepole pine
#' (`PINU.CON`), black spruce (`PICE.MAR`), balsam poplar (`POPU.BAL`),
#' balsam fir (`ABIE.BAL`), Douglas-fir (`PSEU.MEN`) and jack pine (`PINU.BAN`).
#'
#' Provide exactly one of `height` or `si`. Height is predicted directly from
#' age and site index. To estimate site index, the function finds the value
#' that reproduces the supplied height at the given age.
#' Both age and the reference age use **total age**, not breast-height age.
#'
#' @details
#' For the default non-climate version, `si` is tree height at 50 years total
#' age. Predictions beyond total age 180 generate a warning. Estimating site
#' index from young trees, especially before approximately 15--20 years,
#' can be inaccurate.
#'
#' For aspen, balsam poplar and black spruce, the published logarithm requires
#' \eqn{b_0 SI^{b_1-1} > 1}. For white spruce and balsam fir, the square root
#' of \eqn{\ln(SI)} requires `si >= 1`. Impossible height-age pairs and multiple
#' site-index solutions under extrapolation raise explicit errors.
#'
#' With `variant = "climate"`, `si` describes height before the climate
#' adjustment, not necessarily actual height at age 50. The climate equations
#' multiply the height curve by a factor \eqn{C}, so
#' \eqn{H(50) = C \times HI}, where \eqn{HI} is the supplied or estimated `si`.
#' Site-index values from the climate and non-climate versions therefore have
#' different meanings.
#'
#' Climate averages exclude the current year and use the previous 20 years for
#' aspen, balsam poplar, white spruce and balsam fir; previous 10 years for black
#' spruce; and all previous years since tree establishment for lodgepole pine,
#' Douglas-fir and jack pine. For young trees, use only previous years since
#' tree establishment, not climate before the tree existed. Supply averages
#' calculated over the correct years; the function cannot check those years
#' from an average alone. Do not omit missing years when calculating them.
#' The reference climate averages are for 1961--1990 at the location and remain
#' fixed between scenarios. Values outside the published climate ranges produce
#' a warning but are used as supplied.
#'
#' The function predicts height at a given age and climate setting, not a
#' year-by-year growth sequence. Changing climate inputs between ages while
#' holding `si` fixed can produce abrupt height changes. A continuous growth
#' projection would require recalculating `si` from the current height before
#' each new climate period; that procedure is not implemented here.
#'
#' The two model versions were fitted separately. Setting the climate factor
#' \eqn{C = 1} does not reproduce the non-climate model.
#'
#' @param age Numeric vector. Positive total tree age (years).
#' @param height Optional numeric vector. Positive dominant/codominant tree
#'   height (m). If supplied, `si` is predicted.
#' @param si Optional numeric vector. Site index (m) at 50 years total age for
#'   the non-climate version; height index before climate adjustment (m) for
#'   the climate version, as described above.
#'   If supplied, `height` is predicted.
#' @param species Character vector of NFI species codes; compact codes are
#'   normalized (e.g. `"PICEGLA"` becomes `"PICE.GLA"`).
#' @param MAP,MWMT Optional numeric vectors, required for the climate variant.
#'   Respectively the average annual precipitation (mm) and average mean warmest
#'   month temperature (degrees C) over the species-specific previous-years
#'   period described above. Both must be positive and finite. `MWMT` is not
#'   mean annual temperature; do not convert to kelvin.
#' @param MAP_normal,MWMT_normal Optional numeric vectors, required for the
#'   climate variant. The location's 1961--1990 averages for annual
#'   precipitation (mm) and mean warmest month temperature (degrees C).
#'   Both must be positive and finite; no provincial defaults are assumed.
#' @param variant Character scalar. `"non_climate"` (default) or `"climate"`.
#'   Climate inputs supplied to the non-climate variant raise an error.
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
#' si_huang2026(
#'   age = c(20, 50, 80), si = 16, species = "POPU.TRE",
#'   MAP = 400, MWMT = 18, MAP_normal = 500, MWMT_normal = 15.5,
#'   variant = "climate"
#' )
#'
#' @export
si_huang2026 <- function(age, height = NULL, si = NULL, species,
                         MAP = NULL, MWMT = NULL,
                         MAP_normal = NULL, MWMT_normal = NULL,
                         variant = c("non_climate", "climate")) {
  variant <- rlang::arg_match(variant)
  if (!xor(is.null(height), is.null(si))) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  predict_height <- is.null(height)
  x_name <- if (predict_height) "si" else "height"
  df <- .huang2026_prepare(
    age, if (predict_height) si else height, species, x_name, variant,
    list(MAP = MAP, MWMT = MWMT, MAP_normal = MAP_normal, MWMT_normal = MWMT_normal)
  )

  out <- vapply(seq_len(nrow(df)), function(i) {
    pars <- df[i, , drop = FALSE]
    if (!predict_height) {
      return(.huang2026_solve_si_one(
        df$age[[i]], df$x[[i]], pars, df$log_climate[[i]]
      ))
    }
    log_si <- log(df$x[[i]])
    if (pars$model_form == "log_logistic_asymptote" &&
        log_si >= log(pars$b0) / (1 - pars$b1)) {
      cli::cli_abort("{.arg si} is outside the Huang2026 model domain for {pars$Species}.")
    }
    if (pars$model_form == "log_logistic_log_power" && log_si < 0) {
      cli::cli_abort("{.arg si} must be at least 1 m for {pars$Species}.")
    }
    exp(.huang2026_log_height(df$age[[i]], log_si, pars) + df$log_climate[[i]])
  }, numeric(1))

  if (any(!is.finite(out) | out <= 0)) {
    cli::cli_abort("Non-finite or non-positive prediction generated in {.fn si_huang2026}.")
  }
  if (predict_height) tibble::tibble(height = out) else tibble::tibble(si = out)
}

.huang2026_prepare <- function(age, x, species, x_name, variant, climate) {
  supplied <- !vapply(climate, is.null, logical(1))
  if (variant == "climate" && !all(supplied)) {
    cli::cli_abort(
      'The climate variant requires all four climate inputs: {.arg MAP}, {.arg MWMT}, {.arg MAP_normal}, {.arg MWMT_normal}.'
    )
  }
  if (variant == "non_climate" && any(supplied)) {
    cli::cli_abort('Climate inputs are only accepted with {.code variant = "climate"}.')
  }
  if (variant == "non_climate") {
    climate <- list()
  }
  inputs <- c(list(age = age, x = x, species = species), climate)
  if (any(lengths(inputs) == 0L)) {
    cli::cli_abort("Inputs must have length > 0.")
  }
  v <- do.call(assert_len_compat, inputs)
  assert_numeric_vec(v$age, "age", gt = 0)
  assert_numeric_vec(v$x, x_name, gt = 0)
  for (arg in names(climate)) {
    assert_numeric_vec(v[[arg]], arg, gt = 0)
  }
  key <- if (variant == "climate") "parameters_Huang2026_climate" else "parameters_Huang2026"
  pars <- .get_internal_data(key)
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
  df$log_climate <- 0
  if (variant == "climate") {
    df <- dplyr::bind_cols(df, tibble::as_tibble(v[names(climate)]))
    outside <- character()
    for (arg in names(climate)) {
      if (any(df[[arg]] < df[[paste0(arg, "_min")]] |
              df[[arg]] > df[[paste0(arg, "_max")]])) {
        outside <- c(outside, arg)
      }
    }
    if (length(outside)) {
      cli::cli_warn(
        "Climate inputs outside the Huang2026 observed ranges (Table 3.3): {paste(outside, collapse = ', ')}. Predictions extrapolate without clipping.",
        class = "cfa_huang2026_climate_extrapolation"
      )
    }
    p <- df$c1
    use_b1 <- df$map_power == "1/b1"
    use_b0 <- df$map_power == "1/b0"
    p[use_b1] <- 1 / df$b1[use_b1]
    p[use_b0] <- 1 / df$b0[use_b0]
    df$log_climate <- p * (log(df$MAP) - log(df$MAP_normal)) +
      df$c2 * (log(df$MWMT) - log(df$MWMT_normal))
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

.huang2026_solve_si_one <- function(age, height, pars, log_climate) {
  asymptote <- pars$model_form == "log_logistic_asymptote"
  lower <- if (pars$model_form == "log_logistic_log_power") {
    0
  } else {
    log(.Machine$double.xmin) + log(.Machine$double.eps)
  }
  upper <- if (asymptote) log(pars$b0) / (1 - pars$b1) else log(.Machine$double.xmax)
  target <- log(height) - log_climate
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
  # Both tables' asymptote and sqrt-log fits are monotone in SI on their domains.
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
