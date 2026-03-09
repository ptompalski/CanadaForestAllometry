#' Huang et al. (1994) site index model for major Alberta tree species
#'
#' Implementation of the ecologically based, reference-age
#' invariant polymorphic site-index equations from Huang et al. (1994) for
#' Alberta.
#'
#'
#' \strong{Model scope (species coverage):} \code{PICE.GLA, PINU.CON,
#' POPU.TRE, PICE.MAR, PINU.BAN, POPU.BAL, ABIE.BAL, PSEU.MEN}.
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
#' @param subregion Character vector. Alberta natural-region grouping used by
#'   Huang et al. (1994). Defaults to `"All"` (provincial parameter set).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Huang, S., Titus, S. J., & Lakusta, T. W. (1994). Ecologically based site
#' index curves and tables for major Alberta tree species. Alberta
#' Environmental Protection, Land and Forest Service.
#'
#' @examples
#' # Predict site index from age + height, provincial parameters
#' si_huang1994(
#'   age = c(20, 35, 50),
#'   height = c(8, 14, 20),
#'   species = c("PICE.GLA", "PINU.CON", "POPU.TRE")
#' )
#'
#' # Predict height from age + site index with explicit subregions
#' si_huang1994(
#'   age = c(20, 35, 50),
#'   si = c(12, 16, 20),
#'   species = c("PICE.GLA", "PINU.CON", "POPU.TRE"),
#'   subregion = c("LF", "UF", "CM")
#' )
#'
#' # Pipe example: predict SI from a tibble of age/height/species inputs
#' dplyr::tibble(
#'   age = c(25, 35),
#'   height = c(11, 16),
#'   species = c("PICE.GLA", "ABIE.BAL")
#' ) |>
#'   dplyr::mutate(
#'     si = si_huang1994(
#'       age = age,
#'       height = height,
#'       species = species,
#'       subregion = "All"
#'     )
#'   ) |>
#'   tidyr::unnest(si)
#'
#' @export
si_huang1994 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  subregion = "All"
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .huang1994_prepare(
    age = age,
    x = x,
    species = species,
    subregion = subregion,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .huang1994_height_one(
          age = df$age[[i]],
          si = df$si[[i]],
          pars = df[i, , drop = FALSE]
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_huang1994}.",
        "i" = "Check inputs and species/subregion parameter combinations."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_huang1994}.",
        "i" = "Check inputs and species/subregion parameter combinations."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .huang1994_solve_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_huang1994}.",
      "i" = "Check inputs and species/subregion parameter combinations."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_huang1994}.",
      "i" = "Check inputs and species/subregion parameter combinations."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.huang1994_height_one <- function(age, si, pars) {
  # Source mapping:
  # - This block is intended to represent the generic "height from SI and age"
  #   form for Huang et al. (1994), i.e., eq. [4]/[6]/[8]/[10]/[12]/[14]/[16]/[18]
  #   (see PDF pages 15, 31, 50, 66, 76, 84, 92, 100).
  # - Coefficients come from Tables 1/6/12/18/22/24/26/28
  #   (PDF pages 16, 32, 51, 67, 77, 85, 93, 101).
  s <- si - 1.3
  if (!is.finite(s) || s <= 0) {
    return(NaN)
  }

  b0 <- pars$b0[[1]]
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]
  b5 <- pars$b5[[1]]
  t_base <- pars$base_age_bh[[1]]

  # Symbol mapping to your typed equations:
  # - SI -> si
  # - Tb -> age (tree BH age)
  # - Tr -> t_base (reference BH age, 50)
  #
  # Shared terms across eq. [4]/[6]/[8]/[10]/[12]/[14]/[16]/[18]
  # as entered at the end of this file by user.
  Tb <- age
  Tr <- t_base
  si_term <- b2^s

  # Eq groups from user transcription:
  # - Group A (eq. [4], [16], [18]): include "/ Tr" on si_term
  # - Group B (eq. [6], [8], [10], [12], [14]): no "/ Tr" on si_term
  # Shared inner form:
  #   inner = -((b0 * s)^b1) * (...) * T
  #   num/den = 1 - exp(inner_num/inner_den)
  if (pars$Species[[1]] %in% c("PICE.GLA", "ABIE.BAL", "PSEU.MEN")) {
    # Group A: use b2^(s/Tr) for the SI power term.
    si_term_group_a <- b2^(s / Tr)
    inner_num <- (-b0 * (s^b1)) * si_term_group_a * Tb
    inner_den <- (-b0 * (s^b1)) * si_term_group_a * Tr
    num <- 1 - exp(inner_num)
    den <- 1 - exp(inner_den)
  } else {
    inner_num <- (-b0 * (s^b1)) * ((si_term)) * Tb
    inner_den <- (-b0 * (s^b1)) * ((si_term)) * Tr
    num <- 1 - exp(inner_num)
    den <- 1 - exp(inner_den)
  }

  if (!is.finite(num) || !is.finite(den) || den == 0) {
    return(NaN)
  }

  power <- b3 * (s^b4) * (Tr^b5)
  ratio <- (num / den)^power
  if (!is.finite(ratio)) {
    return(NaN)
  }

  # Final assembled form from typed equations.
  1.3 + s * ratio
}


# internal
.huang1994_prepare <- function(age, x, species, subregion, x_name) {
  n <- max(length(age), length(x), length(species), length(subregion))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    species = species,
    subregion = subregion,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x
  species <- recycled$species
  subregion <- recycled$subregion

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  species_std <- standardize_species_code(species)

  if (!is.character(subregion)) {
    cli::cli_abort("{.arg subregion} must be character.")
  }
  if (anyNA(subregion)) {
    cli::cli_abort("{.arg subregion} cannot contain NA values.")
  }

  subregion_std <- .huang1994_std_subregion(subregion)
  pars <- .huang1994_parameters()

  req_tbl <- dplyr::tibble(
    .row_id = seq_len(n),
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std,
    subregion_req = subregion_std
  ) |>
    dplyr::rename(subregion_lookup = subregion_req)

  out <- req_tbl |>
    dplyr::left_join(
      pars,
      by = c("Species", "subregion_lookup")
    )

  ambig <- out |>
    dplyr::filter(!is.na(.data$b0)) |>
    dplyr::count(.data$.row_id, name = "n_matches") |>
    dplyr::filter(.data$n_matches > 1L)
  if (nrow(ambig) > 0L) {
    cli::cli_abort(c(
      "Ambiguous Huang1994 parameter match for one or more species/subregion inputs.",
      "i" = "Use {.val All}, an exact grouped subregion string from parameter tables, or a single Alberta subregion code."
    ))
  }

  if (anyNA(out$b0)) {
    bad_rows <- out |>
      dplyr::filter(is.na(.data$b0)) |>
      dplyr::distinct(.data$Species, .data$subregion_lookup)

    bad_txt <- paste0(
      bad_rows$Species,
      " / ",
      bad_rows$subregion_lookup,
      collapse = ", "
    )

    allowed_by_species <- bad_rows |>
      dplyr::distinct(.data$Species) |>
      dplyr::mutate(
        allowed = vapply(
          .data$Species,
          FUN.VALUE = character(1),
          FUN = function(sp) {
            vals <- pars |>
              dplyr::filter(.data$Species == sp) |>
              dplyr::distinct(.data$subregion_group) |>
              dplyr::pull(.data$subregion_group)
            paste(sort(unique(vals)), collapse = " | ")
          }
        )
      )
    allowed_txt <- paste0(
      allowed_by_species$Species,
      ": ",
      allowed_by_species$allowed,
      collapse = " | "
    )

    cli::cli_abort(
      c(
        "No Huang1994 parameters found for species/subregion: {bad_txt}.",
        "i" = "Allowed subregions by species: {allowed_txt}",
        "i" = "Aliases {.val provincial} and {.val province} are accepted as {.val All}."
      )
    )
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
    if (any(out$si <= 1.3)) {
      cli::cli_abort(
        "{.arg si} must contain values > 1.3 for {.fn si_huang1994}."
      )
    }
  }

  out |>
    dplyr::select(-.row_id)
}


# internal
.huang1994_std_subregion <- function(x) {
  out <- x |>
    stringr::str_to_upper() |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\s*,\\s*", ",") |>
    stringr::str_replace_all("\\s+", "")

  out[out %in% c("PROVINCIAL", "PROVINCE")] <- "ALL"
  out
}


# internal
.huang1994_solve_si_one <- function(age, height, pars) {
  # Source mapping:
  # - Numerical inversion of the site-index equations [4]/[6]/[8]/[10]/[12]/[14]/[16]/[18]
  #   (PDF pages 15, 31, 50, 66, 76, 84, 92, 100).
  # - The document also provides iterative procedures in Appendix 1/2
  #   (PDF pages 123-124); this uses a bracketing root solve equivalent in intent.
  # Appendix mapping:
  # - Appendix 1 (PDF p.123): iterative solution for SI from age + height.
  # - Here, `f(si) = H_model(si) - H_observed` and root-finding is used.
  f <- function(si) {
    .huang1994_height_one(age = age, si = si, pars = pars) - height
  }

  lower <- 1.300001
  upper <- max(60, height * 3)
  bracket <- NULL

  # Bracketing phase for Appendix-style SI inversion.
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
      "Failed to bracket a site-index solution in {.fn si_huang1994}.",
      "i" = "Check that age, height, species, and subregion are within model domain."
    ))
  }

  # Final 1D root solve for SI (numerical analogue of Appendix iterative procedure).
  stats::uniroot(
    f,
    interval = bracket,
    tol = .Machine$double.eps^0.5
  )$root
}


# internal
.huang1994_parameters <- function() {
  # Source mapping:
  # - Parameter object `parameters_Huang1994_si` is built from the extracted
  #   coefficient tables in the same report (Tables 1/6/12/18/22/24/26/28;
  #   PDF pages 16, 32, 51, 67, 77, 85, 93, 101).
  # - `natural_regions` stores the regional group labels used by the report.
  pars <- .get_internal_data("parameters_Huang1994_si") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .data$natural_regions, .keep_all = TRUE) |>
    dplyr::mutate(
      subregion_req = .huang1994_std_subregion(.data$natural_regions)
    )

  pars <- pars |>
    dplyr::mutate(
      subregion_group = .data$subregion_req,
      subregion_lookup = lapply(
        .data$subregion_req,
        function(sr) {
          if (identical(sr, "ALL")) {
            return("ALL")
          }
          parts <- unlist(strsplit(sr, ",", fixed = TRUE), use.names = FALSE)
          unique(c(sr, parts))
        }
      )
    ) |>
    tidyr::unnest_longer(subregion_lookup) |>
    dplyr::distinct(.data$Species, .data$subregion_lookup, .keep_all = TRUE)

  req <- c(
    "Species",
    "natural_regions",
    "base_age_bh",
    "b0",
    "b1",
    "b2",
    "b3",
    "b4",
    "b5",
    "subregion_req",
    "subregion_group",
    "subregion_lookup"
  )
  assert_required_cols(pars, req, object = "parameters_Huang1994_si")

  pars
}
