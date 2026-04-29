#' Huang et al. (2013) height-diameter model for Alberta tree species
#'
#' Estimates total tree height from diameter at breast height using the
#' population-average fixed/base height-diameter equations of Huang et al.
#' (2013) for major Alberta tree species.
#'
#' The model uses separate fixed forms for deciduous and coniferous species:
#' a Chapman-Richards form for deciduous species and a logistic-type form for
#' coniferous species. Plot-specific mixed-model calibration from the report is
#' not implemented in this function.
#'
#' Species must be supplied as NFI codes. Species with small sample sizes are
#' mapped to the grouped species used by the report: `PINU.ALB` and `PINU.FLE`
#' to `PINU.CON`, `PICE.ENG` to `PICE.GLA`, `ABIE.LAS` to `ABIE.BAL`, and
#' `LARI.OCC` and `LARI.LYA` to `LARI.LAR`.
#'
#' @param DBH Numeric vector. Diameter at breast height (cm), with `DBH > 0`.
#' @param species Character vector. NFI species code.
#' @param subregion Character vector. Alberta natural subregion code. Defaults
#'   to `"Province"` for the provincial parameter set. The aliases `"All"` and
#'   `"Provincial"` are also accepted. The package convention is used for
#'   Alberta natural subregions (`"DMW"`, `"M"`, and `"UB"`); report aliases
#'   (`"DM"`, `"MT"`, and `"UBH"`) are also accepted.
#'
#' @return A tibble with column `height`, predicted total tree height (m).
#'
#' @references
#' Huang, S., Yang, Y., & Aitkin, D. (2013). \emph{Population and
#' plot-specific individual tree height-diameter models for major Alberta tree
#' species}. Alberta Environment and Sustainable Resource Development,
#' Technical Report Pub. No. T/600.
#'
#' @examples
#' hd_huang2013(DBH = 20, species = "PICE.GLA")
#' hd_huang2013(
#'   DBH = c(20, 25),
#'   species = c("PICE.GLA", "POPU.TRE"),
#'   subregion = c("LF", "UF")
#' )
#'
#' @export
hd_huang2013 <- function(DBH, species, subregion = "Province") {
  n <- max(length(DBH), length(species), length(subregion))
  if (n == 0L) {
    cli::cli_abort("{.arg DBH} must have length > 0.")
  }

  recycled <- assert_len_compat(
    DBH = DBH,
    species = species,
    subregion = subregion,
    .n = n,
    .recycle = TRUE
  )
  DBH <- recycled$DBH
  species <- recycled$species
  subregion <- recycled$subregion

  assert_numeric_vec(DBH, "DBH", finite = TRUE, gt = 0, allow_na = FALSE)
  if (!is.character(species)) {
    cli::cli_abort("{.arg species} must be character.")
  }
  if (!is.character(subregion)) {
    cli::cli_abort("{.arg subregion} must be character.")
  }
  if (anyNA(species)) {
    cli::cli_abort("{.arg species} cannot contain NA values.")
  }
  if (anyNA(subregion)) {
    cli::cli_abort("{.arg subregion} cannot contain NA values.")
  }

  species_key <- .huang2013_hd_species_key(species)
  subregion_std <- .huang2013_hd_std_subregion(subregion)
  pars <- .huang2013_hd_match_params(species_key, subregion_std)

  h <- ifelse(
    pars$model == 1L,
    .huang2013_hd_eq1(DBH = DBH, b1 = pars$b1, b2 = pars$b2, b3 = pars$b3),
    .huang2013_hd_eq2(DBH = DBH, b1 = pars$b1, b2 = pars$b2, b3 = pars$b3)
  )

  if (any(!is.finite(h))) {
    cli::cli_abort(c(
      "Non-finite height prediction generated in {.fn hd_huang2013}.",
      "i" = "Check DBH, species, and subregion inputs."
    ))
  }
  if (any(h < 1.3)) {
    cli::cli_abort(c(
      "Predicted height below breast height generated in {.fn hd_huang2013}.",
      "i" = "Check DBH, species, and subregion inputs."
    ))
  }

  dplyr::tibble(height = as.numeric(h))
}


# internal
.huang2013_hd_eq1 <- function(DBH, b1, b2, b3) {
  1.3 + b1 * (1 - exp(-b2 * DBH))^b3
}


# internal
.huang2013_hd_eq2 <- function(DBH, b1, b2, b3) {
  1.3 + b1 / (1 + exp(b2 + b3 * log(DBH)))
}


# internal
.huang2013_hd_species_key <- function(species) {
  sp_std <- standardize_species_code(species, keep_all = FALSE)

  group_lookup <- c(
    "POPU.TRE" = "POPU.TRE",
    "POPU.BAL" = "POPU.BAL",
    "BETU.PAP" = "BETU.PAP",
    "PINU.CON" = "PINU.CON",
    "PINU.ALB" = "PINU.CON",
    "PINU.FLE" = "PINU.CON",
    "PINU.BAN" = "PINU.BAN",
    "PICE.GLA" = "PICE.GLA",
    "PICE.ENG" = "PICE.GLA",
    "PICE.MAR" = "PICE.MAR",
    "ABIE.BAL" = "ABIE.BAL",
    "ABIE.LAS" = "ABIE.BAL",
    "PSEU.MEN" = "PSEU.MEN",
    "LARI.LAR" = "LARI.LAR",
    "LARI.OCC" = "LARI.LAR",
    "LARI.LYA" = "LARI.LAR",
    "UNKN.HWD" = "UNKN.HWD",
    "UNKN.SWD" = "UNKN.SWD"
  )

  out <- unname(group_lookup[sp_std])
  bad <- unique(species[is.na(out)])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "No Huang2013 height-diameter parameters found for species.",
      "x" = paste0("Unsupported species: ", paste(bad, collapse = ", "))
    ))
  }

  out
}


# internal
.huang2013_hd_std_subregion <- function(subregion) {
  sr <- subregion |>
    stringr::str_squish() |>
    stringr::str_to_upper()

  sr <- stringr::str_replace_all(sr, "\\s+", "")

  province_alias <- c("PROVINCE", "PROV", "PROVINCIAL", "ALL", "ALBERTA", "AB")
  sr[sr %in% province_alias] <- "PROVINCE"

  alias <- c(
    DM = "DMW",
    MT = "M",
    UBH = "UB",
    "1-6,12-21" = "OTHERS",
    "1TO6,12TO21" = "OTHERS",
    "7-9" = "7 TO 9",
    "7TO9" = "7 TO 9",
    "7-10" = "7 TO 10",
    "7TO10" = "7 TO 10",
    "7-11" = "7 TO 11",
    "7TO11" = "7 TO 11"
  )

  idx <- sr %in% names(alias)
  sr[idx] <- unname(alias[sr[idx]])

  valid <- c(
    "PROVINCE", "OTHERS",
    "CM", "DMW", "NM", "BSA", "PAD", "LBH", "UB", "AP",
    "ALP", "SA", "M", "UF", "LF", "KU", "FP", "PRP", "CP",
    "DMG", "FF", "NF", "MG",
    "7 TO 9", "7 TO 10", "7 TO 11", "10", "11"
  )
  bad <- unique(subregion[!sr %in% valid])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Invalid Alberta natural subregion for {.fn hd_huang2013}.",
      "x" = paste0("Invalid subregion: ", paste(bad, collapse = ", ")),
      "i" = "Use {.val Province}, {.val Others}, a report code such as {.val LF}, or a grouped code such as {.val 7 to 9}."
    ))
  }

  sr
}


# internal
.huang2013_hd_match_params <- function(species_key, subregion_std) {
  pars <- .huang2013_hd_parameters()
  xwalk <- .huang2013_hd_subregion_lookup()

  out <- vector("list", length(species_key))

  for (i in seq_along(species_key)) {
    sp <- species_key[[i]]
    sr <- subregion_std[[i]]

    if (sp %in% c("UNKN.HWD", "UNKN.SWD")) {
      sr_group <- "Province"
    } else if (sr == "PROVINCE") {
      sr_group <- "Province"
    } else if (sp %in% c("BETU.PAP", "PSEU.MEN", "LARI.LAR", "PINU.BAN")) {
      sr_group <- "Province"
    } else if (sr %in% c("OTHERS", "7 TO 9", "7 TO 10", "7 TO 11", "10", "11")) {
      sr_group <- stringr::str_to_title(stringr::str_to_lower(sr))
    } else {
      match <- xwalk |>
        dplyr::filter(
          .data$Species == .env$sp,
          .data$NaturalSubregionCode == .env$sr
        )

      if (nrow(match) > 1L) {
        abort_row("hd_huang2013", i, "Ambiguous subregion match.", species = sp, subregion = sr)
      }

      if (nrow(match) == 1L) {
        sr_group <- match$Subregion[[1]]
      } else {
        sr_group <- "Others"
      }
    }

    row <- pars |>
      dplyr::filter(
        .data$Species == .env$sp,
        stringr::str_to_upper(.data$Subregion) == stringr::str_to_upper(.env$sr_group),
        .data$model %in% c(1L, 2L)
      )

    if (nrow(row) != 1L) {
      abort_row(
        "hd_huang2013",
        i,
        "Expected exactly one fixed/base parameter row.",
        species = sp,
        subregion = sr,
        parameter_subregion = sr_group
      )
    }

    out[[i]] <- row
  }

  dplyr::bind_rows(out)
}


# internal
.huang2013_hd_parameters <- function() {
  pars <- .get_internal_data("parameters_Huang2013_hd") |>
    dplyr::as_tibble()

  req <- c("Species", "model", "Subregion", "b1", "b2", "b3")
  assert_required_cols(pars, req, object = "parameters_Huang2013_hd")

  pars
}


# internal
.huang2013_hd_subregion_lookup <- function() {
  tibble::tribble(
    ~Species, ~Subregion, ~NaturalSubregionCode,
    "ABIE.BAL", "7 To 9", "ALP",
    "ABIE.BAL", "7 To 9", "SA",
    "ABIE.BAL", "7 To 9", "M",
    "ABIE.BAL", "10", "UF",
    "ABIE.BAL", "11", "LF",
    "PICE.GLA", "7 To 9", "ALP",
    "PICE.GLA", "7 To 9", "SA",
    "PICE.GLA", "7 To 9", "M",
    "PICE.GLA", "10", "UF",
    "PICE.GLA", "11", "LF",
    "PICE.MAR", "7 To 10", "ALP",
    "PICE.MAR", "7 To 10", "SA",
    "PICE.MAR", "7 To 10", "M",
    "PICE.MAR", "7 To 10", "UF",
    "PICE.MAR", "11", "LF",
    "PINU.CON", "7 To 9", "ALP",
    "PINU.CON", "7 To 9", "SA",
    "PINU.CON", "7 To 9", "M",
    "PINU.CON", "10", "UF",
    "PINU.CON", "11", "LF",
    "POPU.BAL", "7 To 11", "ALP",
    "POPU.BAL", "7 To 11", "SA",
    "POPU.BAL", "7 To 11", "M",
    "POPU.BAL", "7 To 11", "UF",
    "POPU.BAL", "7 To 11", "LF",
    "POPU.TRE", "7 To 10", "ALP",
    "POPU.TRE", "7 To 10", "SA",
    "POPU.TRE", "7 To 10", "M",
    "POPU.TRE", "7 To 10", "UF",
    "POPU.TRE", "11", "LF"
  )
}
