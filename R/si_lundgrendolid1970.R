#' Lundgren and Dolid (1970) site index model
#'
#' Unified, vectorized implementation of the Lundgren and Dolid (1970) site
#' index equations for Lake States species.
#'
#' Two model forms are available:
#' \itemize{
#'   \item \code{"exponential_monomolecular"} (default; preferred in the paper)
#'   \item \code{"monomolecular"}
#' }
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs/outputs are metric; the original equations are in imperial units, so
#' the function converts internally.
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast height). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"PICE.MAR"`).
#' @param model Character scalar. One of `"exponential_monomolecular"` or `"monomolecular"`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Lundgren, Allen L.; Dolid, William A. 1970. Biological growth functions describe published site index curves for Lake States timber species. Research Paper NC-36. St. Paul, MN: U.S. Dept. of Agriculture, Forest Service, North Central Forest Experiment Station
#'
#' @examples
#' # Predict site index from age + height (default model: exponential_monomolecular)
#' si_lundgrendolid1970(
#'   age = c(20, 40, 60),
#'   height = c(8, 16, 24),
#'   species = c("PICE.MAR", "PINU.BAN", "BETU.PAP")
#' )
#'
#' # Predict height from age + site index, using the monomolecular alternative
#' si_lundgrendolid1970(
#'   age = c(20, 40, 60),
#'   si = c(12, 18, 24),
#'   species = c("PICE.MAR", "PINU.BAN", "BETU.PAP")
#' )
#'
#' # Works with the pipe operator: predict site index from age + height
#' trees_h <- tibble::tibble(
#'   species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
#'   age = c(25, 45, 65),
#'   height = c(9, 18, 23)
#' )
#'
#' trees_h |>
#'   dplyr::mutate(
#'     si_pred = si_lundgrendolid1970(
#'       age = age,
#'       height = height,
#'       species = species
#'     )
#'   ) |>
#'   tidyr::unnest(si_pred)
#'
#' # With pipe: predict height from age + site index (monomolecular form)
#' trees_si <- tibble::tibble(
#'   species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
#'   age = c(25, 45, 65),
#'   si = c(14, 20, 22)
#' )
#'
#' trees_si |>
#'   dplyr::mutate(
#'     height_pred = si_lundgrendolid1970(
#'       age = age,
#'       si = si,
#'       species = species
#'     )
#'   ) |>
#'   tidyr::unnest(height_pred)
#'
#' @export
si_lundgrendolid1970 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  model = c("exponential_monomolecular", "monomolecular")
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  model <- rlang::arg_match(model)

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .lundgrendolid1970_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name,
    model = model
  )

  if (mode == "predict_height") {
    h_ft <- if (identical(model, "exponential_monomolecular")) {
      with(df, b1 * si_ft * (1 - exp(b2 * age))^b3)
    } else {
      with(df, si_ft * (a + b1 * exp(b2 * age)))
    }

    if (any(!is.finite(h_ft))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_lundgrendolid1970}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_lundgrendolid1970}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h_ft / 3.28084))
  }

  denom <- if (identical(model, "exponential_monomolecular")) {
    with(df, b1 * (1 - exp(b2 * age))^b3)
  } else {
    with(df, a + b1 * exp(b2 * age))
  }

  if (any(!is.finite(denom)) || any(denom <= 0)) {
    cli::cli_abort(c(
      "Invalid denominator generated in {.fn si_lundgrendolid1970}.",
      "i" = "Check age inputs and species-specific parameters."
    ))
  }

  si_ft <- df$height_ft / denom

  if (any(!is.finite(si_ft))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_lundgrendolid1970}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_ft < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_lundgrendolid1970}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_ft / 3.28084)
}


# internal
.lundgrendolid1970_prepare <- function(age, x, species, x_name, model) {
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

  pars <- .get_internal_data("parameters_LungrenDolid1970") |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$model == .env$model) |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c("Species", "model", "b1", "b2")
  if (identical(model, "exponential_monomolecular")) {
    req <- c(req, "b3")
  } else {
    req <- c(req, "a")
  }
  assert_required_cols(pars, req, object = "parameters_LungrenDolid1970")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (identical(model, "exponential_monomolecular")) {
    if (anyNA(out$b1) || anyNA(out$b2) || anyNA(out$b3)) {
      bad <- unique(out$Species[is.na(out$b1) | is.na(out$b2) | is.na(out$b3)])
      cli::cli_abort(
        "No LundgrenDolid1970 ({model}) parameters found for species: {paste(bad, collapse = ', ')}."
      )
    }
  } else {
    if (anyNA(out$a) || anyNA(out$b1) || anyNA(out$b2)) {
      bad <- unique(out$Species[is.na(out$a) | is.na(out$b1) | is.na(out$b2)])
      cli::cli_abort(
        "No LundgrenDolid1970 ({model}) parameters found for species: {paste(bad, collapse = ', ')}."
      )
    }
  }

  if (any(out$Species == "PICE.GLA")) {
    inform_once(
      c(
        "LundgrenDolid1970 was not recommended for white spruce in the source paper.",
        "i" = "Use results for {.val PICE.GLA} with caution."
      ),
      .id = paste0("si_lundgrendolid1970_white_spruce_", model)
    )
  }

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}
