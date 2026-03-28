#' Lafleche et al. (2013) ecological-site IQS curves for southern Quebec
#'
#' Implementation of the fixed ecological-site height curves published by
#' Lafleche et al. (2013) for major commercial tree species in southern Quebec.
#' Unlike the polymorphic `si_*` models in this package, these curves are
#' selected by ecological key and use fixed coefficients for each
#' species-region-(subregion)-type combination.
#'
#' \strong{Species coverage:} `ABIE.BAL`, `BETU.PAP`, `PICE.GLA`, `PICE.MAR`,
#' `PICE.RUB`, `PINU.BAN`, `PINU.STR`, `POPU.GRA`, `POPU.TRE`, `THUJ.OCC`.
#'
#' \strong{Geographic use:} ecological types in southern Quebec.
#'
#' \strong{Age definition note:} `age` is corrected age above 1 m height
#' (years).
#'
#' \strong{Height definition note:} the source curves predict stand height in
#' metres, constrained to pass through `(age = 0, height = 1)`.
#'
#' \strong{Base-age note:} IQS is defined as predicted height at 50 years above
#' 1 m. `base_age` defaults to `50`, but any positive scalar can be supplied.
#'
#' This function always predicts height from `age`; it also returns the
#' corresponding fixed-curve IQS value evaluated at `base_age`.
#'
#' `curve_set` chooses between the two curve families reported in the source:
#' `"potential"` corresponds to the fitted potential-height curves used to
#' derive `IQSstation` in the report, whereas `"observed"` corresponds to the
#' separate curves fitted to observed height growth (`IQSobserved`). In the
#' source tables these appear as `IQSstation` and `IQSobserved`, respectively.
#'
#' At present, `CR`, `WE`, and `LIN` equation forms are implemented directly
#' from the publication and validated against the published `IQSstation`
#' summaries. The single `LOGIST3` row is kept in the parameter data but raises
#' an error here because its printed equation/coefficients have not yet been
#' reconciled with the published table values.
#'
#' @param age Numeric vector. Corrected age above 1 m (years), with `age > 0`.
#' @param species Character vector of NFI species codes
#'   (for example `"PICE.GLA"`).
#' @param ecological_region Character vector of Quebec ecological-region codes
#'   (French source term: `region_ecologique`; for example `"4f"`).
#' @param ecological_type Character vector of ecological-type codes
#'   (French source term: `type_ecologique`; for example `"MS22"`).
#' @param ecological_subregion Optional character vector of ecological-subregion
#'   codes (French source term: `subregion_ecologique`) when needed, for
#'   example `"5eS"`. Defaults to `NULL`, which is treated as missing.
#' @param curve_set Character scalar. One of `"potential"` or `"observed"`.
#'   Defaults to `"potential"`. These map internally to the source-table curve
#'   sets `IQSstation` and `IQSobserved`, respectively.
#' @param base_age Positive numeric scalar. IQS reference age (years above
#'   1 m). Defaults to `50`.
#' @param include_metadata Logical scalar. If `TRUE`, append lookup metadata and
#'   fitted-curve metadata to the returned tibble.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m) at `age`.}
#'   \item{si}{Fixed-curve IQS value (m) at `base_age`.}
#' }
#' When `include_metadata = TRUE`, additional columns describing the matched
#' ecological key and parameter row are included.
#'
#' @references
#' Lafleche, V., S. Bernier, J.-P. Saucier, and C. Gagne. 2013. \emph{Indices
#' de qualite de station des principales essences commerciales en fonction des
#' types ecologiques du Quebec meridional}. Quebec, ministere des Ressources
#' naturelles, Direction des inventaires forestiers, 115 p.
#'
#' @examples
#' si_lafleche2013(
#'   age = 50,
#'   species = "PICE.GLA",
#'   ecological_region = "2a",
#'   ecological_type = "MJ11"
#' )
#'
#' si_lafleche2013(
#'   age = c(25, 50),
#'   species = "ABIE.BAL",
#'   ecological_region = "3c",
#'   ecological_type = "MJ12",
#'   curve_set = "potential",
#'   include_metadata = TRUE
#' )
#'
#' # Vectorized use with the pipe operator
#' trees_h <- tibble::tibble(
#'   species = c("ABIE.BAL", "PICE.GLA"),
#'   age = c(25, 50),
#'   ecological_region = c("3c", "2a"),
#'   ecological_type = c("MJ12", "MJ11")
#' )
#'
#' trees_h |>
#'   dplyr::mutate(
#'     iqs_pred = si_lafleche2013(
#'       age = age,
#'       species = species,
#'       ecological_region = ecological_region,
#'       ecological_type = ecological_type
#'     )
#'   ) |>
#'   tidyr::unnest(iqs_pred)
#'
#' @export
si_lafleche2013 <- function(
    age,
    species,
    ecological_region,
    ecological_type,
    ecological_subregion = NULL,
    curve_set = "potential",
    base_age = 50,
    include_metadata = FALSE
) {
  curve_set <- .lafleche2013_normalize_curve_set(curve_set)

  if (!is.numeric(base_age) || length(base_age) != 1L || is.na(base_age) || !is.finite(base_age) || base_age <= 0) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value > 0.")
  }
  if (!is.logical(include_metadata) || length(include_metadata) != 1L || is.na(include_metadata)) {
    cli::cli_abort("{.arg include_metadata} must be a single TRUE/FALSE value.")
  }

  df <- .lafleche2013_prepare(
    age = age,
    species = species,
    ecological_region = ecological_region,
    ecological_type = ecological_type,
    ecological_subregion = ecological_subregion,
    curve_set = curve_set
  )

  height <- .lafleche2013_height(
    age = df$age,
    equation_used = df$equation_used,
    b1 = df$b1,
    b2 = df$b2,
    b3 = df$b3
  )
  si <- .lafleche2013_height(
    age = rep_len(as.numeric(base_age), nrow(df)),
    equation_used = df$equation_used,
    b1 = df$b1,
    b2 = df$b2,
    b3 = df$b3
  )

  if (any(!is.finite(height))) {
    cli::cli_abort(c(
      "Non-finite height prediction generated in {.fn si_lafleche2013}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(height < 0)) {
    cli::cli_abort(c(
      "Negative height prediction generated in {.fn si_lafleche2013}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(!is.finite(si))) {
    cli::cli_abort(c(
      "Non-finite IQS prediction generated in {.fn si_lafleche2013}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si < 0)) {
    cli::cli_abort(c(
      "Negative IQS prediction generated in {.fn si_lafleche2013}.",
      "i" = "Check inputs and model domain."
    ))
  }

  out <- dplyr::tibble(height = height, si = si)
  if (isTRUE(include_metadata)) {
    out <- dplyr::bind_cols(
      out,
      dplyr::select(
        df,
        curve_set,
        species_qc,
        Species,
        ecological_region,
        ecological_region_description,
        ecological_subregion,
        ecological_type,
        ecological_type_description_fr,
        ecological_type_description_en,
        equation_used,
        n_trees,
        n_observations,
        pseudo_r2
      )
    )
  }

  out
}


# internal
.lafleche2013_prepare <- function(
    age,
    species,
    ecological_region,
    ecological_type,
    ecological_subregion,
    curve_set
) {
  if (length(age) == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  n_inputs <- list(
    age = age,
    species = species,
    ecological_region = ecological_region,
    ecological_type = ecological_type
  )
  if (!is.null(ecological_subregion)) {
    n_inputs$ecological_subregion <- ecological_subregion
  }

  n <- max(vapply(n_inputs, length, integer(1)))
  recycled <- do.call(
    assert_len_compat,
    c(n_inputs, list(.n = n, .recycle = TRUE))
  )

  age <- recycled$age
  species <- recycled$species
  ecological_region <- recycled$ecological_region
  ecological_type <- recycled$ecological_type
  if (is.null(ecological_subregion)) {
    ecological_subregion <- rep(NA_character_, n)
  } else {
    ecological_subregion <- recycled$ecological_subregion
  }

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  if (!is.character(species) || anyNA(species)) {
    cli::cli_abort("{.arg species} must be a character vector without NA values.")
  }
  if (!is.character(ecological_region) || anyNA(ecological_region)) {
    cli::cli_abort("{.arg ecological_region} must be a character vector without NA values.")
  }
  if (!is.character(ecological_type) || anyNA(ecological_type)) {
    cli::cli_abort("{.arg ecological_type} must be a character vector without NA values.")
  }
  if (!is.character(ecological_subregion)) {
    cli::cli_abort("{.arg ecological_subregion} must be character when supplied.")
  }

  pars <- parameters_QC_IQS2013
  species_std <- .lafleche2013_standardize_species(species)
  ecological_subregion_norm <- dplyr::na_if(trimws(ecological_subregion), "")

  lookup <- dplyr::tibble(
    row_id = seq_len(n),
    curve_set = curve_set,
    Species = species_std,
    ecological_region = trimws(ecological_region),
    ecological_subregion = ecological_subregion_norm,
    ecological_type = trimws(ecological_type),
    age = as.numeric(age)
  )

  pars_key <- pars |>
    dplyr::mutate(
      subregion_ecologique = dplyr::na_if(.data$subregion_ecologique, "")
    )

  dup_keys <- pars_key |>
    dplyr::count(
      .data$curve_set,
      .data$Species,
      .data$region_ecologique,
      .data$subregion_ecologique,
      .data$type_ecologique,
      name = "n"
    ) |>
    dplyr::filter(.data$n > 1L)

  if (nrow(dup_keys) > 0L) {
    cli::cli_abort("Internal QC IQS parameter table has duplicate ecological keys.")
  }

  out <- lookup |>
    dplyr::left_join(
      pars_key,
      by = c(
        "curve_set",
        "Species",
        "ecological_region" = "region_ecologique",
        "ecological_subregion" = "subregion_ecologique",
        "ecological_type" = "type_ecologique"
      )
    ) |>
    dplyr::left_join(
      qc_iqs_ecological_keys_2013,
      by = c(
        "ecological_region" = "region_ecologique",
        "ecological_subregion" = "subregion_ecologique",
        "ecological_type" = "type_ecologique"
      )
    ) |>
    dplyr::rename(
      ecological_region_description = region_description,
      ecological_type_description_fr = type_ecologique_description_fr,
      ecological_type_description_en = type_ecologique_description_en
    )

  if (anyNA(out$equation_used)) {
    bad_rows <- out |>
      dplyr::filter(is.na(.data$equation_used))

    bad_text <- apply(
      bad_rows[, c("Species", "ecological_region", "ecological_subregion", "ecological_type")],
      1,
      function(x) {
        parts <- x[!is.na(x) & nzchar(x)]
        paste(parts, collapse = " / ")
      }
    )

    cli::cli_abort(c(
      "No Lafleche2013 IQS curve found for one or more input combinations.",
      "i" = paste(unique(bad_text), collapse = "; "),
      "i" = "Check {.arg curve_set}, ecological keys, and whether {.arg ecological_subregion} is required."
    ))
  }

  out
}


# internal
.lafleche2013_standardize_species <- function(species) {
  species_chr <- trimws(as.character(species))
  qc_codes <- unique(parameters_QC_IQS2013$species_qc)
  is_qc_code <- toupper(species_chr) %in% qc_codes
  if (any(is_qc_code)) {
    bad <- unique(species_chr[is_qc_code])
    cli::cli_abort(
      paste0(
        "{.arg species} must use NFI species codes in {.fn si_lafleche2013}. ",
        "Quebec source codes are not accepted: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  standardize_species_code(species_chr)
}


# internal
.lafleche2013_normalize_curve_set <- function(curve_set) {
  if (!is.character(curve_set) || length(curve_set) != 1L || is.na(curve_set)) {
    cli::cli_abort("{.arg curve_set} must be a single character value.")
  }

  curve_set_up <- toupper(trimws(curve_set))
  if (curve_set_up == "POTENTIAL") {
    return("IQSstation")
  }
  if (curve_set_up == "OBSERVED") {
    return("IQSobserved")
  }

  cli::cli_abort(
    "{.arg curve_set} must be one of {.val potential} or {.val observed}."
  )
}


# internal
.lafleche2013_height <- function(age, equation_used, b1, b2, b3) {
  out <- rep(NA_real_, length(age))

  is_cr <- equation_used == "CR"
  if (any(is_cr)) {
    out[is_cr] <- 1 + b1[is_cr] * (1 - exp(-b2[is_cr] * age[is_cr]))^b3[is_cr]
  }

  is_we <- equation_used == "WE"
  if (any(is_we)) {
    out[is_we] <- 1 + b1[is_we] * (1 - exp(-b2[is_we] * age[is_we]^b3[is_we]))
  }

  is_lin <- equation_used == "LIN"
  if (any(is_lin)) {
    out[is_lin] <- 1 + b1[is_lin] * age[is_lin]
  }

  is_logist3 <- equation_used == "LOGIST3"
  if (any(is_logist3)) {
    cli::cli_abort(c(
      "The Lafleche2013 {.val LOGIST3} equation form is not yet implemented.",
      "i" = "The row is retained in the QC IQS parameter table.",
      "i" = "Its printed equation/coefficients have not yet been reconciled with the published `IQSstation` summary values.",
      "i" = "This currently affects one parameter row in the QC IQS table."
    ))
  }

  unknown <- !(equation_used %in% c("CR", "WE", "LIN", "LOGIST3"))
  if (any(unknown)) {
    cli::cli_abort(
      "Unsupported Lafleche2013 equation form(s): {.val {unique(equation_used[unknown])}}."
    )
  }

  out
}
