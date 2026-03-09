#' Site-index model registry
#'
#' Returns a registry (metadata table) describing site-index models available in
#' CanadaForestAllometry. The registry supports model discovery and can be used
#' by higher-level wrappers to select candidate models by jurisdiction/species
#' and required inputs.
#'
#' @return A tibble with one row per site-index model variant and metadata
#'   fields used for model selection.
#' @export
si_model_registry <- function() {
  tibble::tibble(
    model_id = c(
      "carmeanhahn1981",
      "kerbowling1991",
      "lundgrendolid1970_exponential_monomolecular",
      "lundgrendolid1970_monomolecular",
      "nigh2000",
      "nigh2000_gi",
      "nighcourtin1998_si25",
      "nighcourtin1998_si50",
      "huang1994",
      "payandeh1974",
      "scottvoorhis1986_bh_age",
      "scottvoorhis1986_total_age",
      "thrower1994"
    ),

    reference = c(
      "@CarmeanHahn1981",
      "@KerBowling1991",
      "@LundgrenDolid1970",
      "@LundgrenDolid1970",
      "@Nigh2000",
      "@Nigh2000",
      "@NighCourtin1998",
      "@NighCourtin1998",
      "@Huang1994si",
      "@Payandeh1974",
      "@ScottVoorhis1986",
      "@ScottVoorhis1986",
      "@Thrower1994"
    ),

    engine = c(
      "si_carmeanhahn1981",
      "si_kerbowling1991",
      "si_lundgrendolid1970",
      "si_lundgrendolid1970",
      "si_nigh2000",
      "si_nigh2000_gi",
      "si_nighcourtin1998",
      "si_nighcourtin1998",
      "si_huang1994",
      "si_payandeh1974",
      "si_scottvoorhis1986",
      "si_scottvoorhis1986",
      "si_thrower1994"
    ),

    # fixed arguments used to distinguish model variants sharing one engine
    fixed_args = list(
      list(),
      list(),
      list(model = "exponential_monomolecular"),
      list(model = "monomolecular"),
      list(),
      list(),
      list(si50 = FALSE),
      list(si50 = TRUE),
      list(),
      list(),
      list(convert_to_total_age = FALSE),
      list(convert_to_total_age = TRUE),
      list()
    ),

    # Inputs required by model interface
    requires_species = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    requires_gi = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),

    # Model capabilities
    supports_predict_si = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    supports_predict_height = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),

    scope = c(
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional"
    ),

    province_scope = list(
      c("ON"),
      c("NB"),
      c("ON"),
      c("ON"),
      c("BC"),
      c("BC"),
      c("BC"),
      c("BC"),
      c("AB"),
      c("ALL"),
      c("NB", "NS", "PE", "NL", "QC", "ON"),
      c("NB", "NS", "PE", "NL", "QC", "ON"),
      c("BC")
    ),

    # subregion metadata (for geography-aware selection)
    subregion_type = c(
      "none",
      "none",
      "none",
      "none",
      "bec_region",
      "bec_region",
      "bec_region",
      "bec_region",
      "ab_natural_region_group",
      "none",
      "none",
      "none",
      "bec_region"
    ),
    subregion_required = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    subregion_arg = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "subregion",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    subregion_scope = list(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      c("BC_INTERIOR"),
      c("BC_INTERIOR"),
      c("BC_COAST"),
      c("BC_COAST"),
      c("AB"),
      NA_character_,
      NA_character_,
      NA_character_,
      c("BC_INTERIOR")
    ),
    subregion_desc = c(
      "none",
      "none",
      "none",
      "none",
      "BC interior",
      "BC interior",
      "BC coast",
      "BC coast",
      "Alberta natural-region groups",
      "none",
      "none",
      "none",
      "BC interior"
    ),

    description = c(
      "Lake States site-index model for balsam fir and white spruce",
      "New Brunswick polymorphic site-index model for softwoods",
      "Lundgren and Dolid model (exponential monomolecular form)",
      "Lundgren and Dolid model (monomolecular form)",
      "Nigh (2000) polymorphic site-index model for interior western redcedar",
      "Nigh (2000) growth-intercept site-index model for interior western redcedar",
      "Nigh and Courtin (1998) red alder model, SI25 scale",
      "Nigh and Courtin (1998) red alder model, SI50 scale",
      "Huang et al. (1994) Alberta polymorphic site-index model set",
      "Payandeh (1974) nonlinear site-index equations",
      "Scott and Voorhis (1986) model using breast-height age directly",
      "Scott and Voorhis (1986) model with internal conversion to total age",
      "Thrower et al. (1994) BC interior species model set"
    ),

    rank = c(70, 80, 60, 55, 90, 85, 85, 84, 88, 50, 65, 66, 90),

    params_key = c(
      NA_character_,
      "parameters_KerBowling1991",
      "parameters_LungrenDolid1970",
      "parameters_LungrenDolid1970",
      NA_character_,
      "parameters_Nigh2000_gi",
      NA_character_,
      NA_character_,
      "parameters_Huang1994_si",
      "parameters_Payandeh1974",
      "parameters_ScottVoorhis1986",
      "parameters_ScottVoorhis1986",
      "parameters_Thrower1994"
    ),

    # optional manual species list for engines not backed by a parameter table
    species_manual = list(
      c("ABIE.BAL", "PICE.GLA"),
      NULL,
      NULL,
      NULL,
      c("THUJ.PLI"),
      c("THUJ.PLI"),
      c("ALNU.RUB"),
      c("ALNU.RUB"),
      NULL,
      NULL,
      NULL,
      NULL,
      c(
        "PINU.CON",
        "PICE.GLA",
        "PSEU.MEN",
        "ABIE.LAS",
        "TSUG.HET",
        "THUJ.PLI",
        "PINU.MON",
        "PINU.PON",
        "LARI.OCC",
        "POPU.TRE",
        "BETU.PAP"
      )
    )
  )
}

#' Site-index model registry with species coverage
#'
#' @return A tibble like `si_model_registry()` plus:
#'   - `species` (list-column of character vectors)
#'   - `n_species` (integer)
#'   - `species_text` (collapsed string for printing)
#' @keywords internal
si_model_registry_species <- function() {
  reg <- si_model_registry()

  # Backward-compatible defaults (in case older registry doesn't have them yet)
  if (!"subregion_required" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(subregion_required = FALSE)
  }
  if (!"subregion_arg" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(subregion_arg = NA_character_)
  }
  if (!"subregion_type" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(subregion_type = "none")
  }
  if (!"species_manual" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(species_manual = list(NULL))
  }

  reg |>
    dplyr::rowwise() |>
    dplyr::mutate(
      species = list({
        if (!is.null(species_manual) && length(species_manual) > 0) {
          sort(unique(as.character(species_manual)))
        } else if (!is.na(params_key) && nzchar(params_key)) {
          tryCatch(
            {
              params <- get_params_tbl(params_key)
              extract_species_from_params(params)
            },
            error = function(e) {
              stop(
                sprintf(
                  "Failed to build species list for model_id=%s (params_key=%s): %s",
                  model_id,
                  params_key,
                  conditionMessage(e)
                ),
                call. = FALSE
              )
            }
          )
        } else {
          character(0)
        }
      }),
      n_species = length(species),
      species_text = dplyr::if_else(
        n_species == 0L,
        NA_character_,
        paste(species, collapse = ", ")
      )
    ) |>
    dplyr::ungroup()
}
