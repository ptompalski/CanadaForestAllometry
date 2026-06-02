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
      "buckman2006",
      "carmean1989",
      "carmean1996",
      "carmeanhahn1981",
      "cieszewskibella1991",
      "kerbowling1991",
      "lundgrendolid1970_exponential_monomolecular",
      "lundgrendolid1970_monomolecular",
      "nigh2000",
      "nigh2000_gi",
      "nighcourtin1998_si25",
      "nighcourtin1998_si50",
      "huang1994",
      "augerward2021",
      "pregent2010",
      "pregent2016",
      "sharma2021",
      "sharmaparton2018a",
      "sharmaparton2018b",
      "sharmaparton2019",
      "sharma2015",
      "parresolvissage1998",
      "payandeh1974",
      "sharmareid2018",
      "sharma2022",
      "lafleche2013_potential",
      "lafleche2013_observed",
      "scottvoorhis1986_bh_age",
      "scottvoorhis1986_total_age",
      "thrower1994"
    ),
    reference = c(
      "@Buckman2006",
      "@Carmean1989",
      "@Carmean1996",
      "@CarmeanHahn1981",
      "@CieszewskiBella1991",
      "@KerBowling1991",
      "@LundgrenDolid1970",
      "@LundgrenDolid1970",
      "@Nigh2000",
      "@Nigh2000",
      "@NighCourtin1998",
      "@NighCourtin1998",
      "@Huang1994si",
      "@AugerWard2021",
      "@Pregent2010",
      "@Pregent2016",
      "@Sharma2021SI",
      "@SharmaParton2018a",
      "@SharmaParton2018b",
      "@SharmaParton2019",
      "@SharmaEtAl2015",
      "@ParresolVissage1998",
      "@Payandeh1974",
      "@SharmaReid2018",
      "@Sharma2022",
      "@LaflecheEtAl2013",
      "@LaflecheEtAl2013",
      "@ScottVoorhis1986",
      "@ScottVoorhis1986",
      "@Thrower1994"
    ),
    engine = c(
      "si_buckman2006",
      "si_carmean1989",
      "si_carmean1996",
      "si_carmeanhahn1981",
      "si_cieszewskibella1991",
      "si_kerbowling1991",
      "si_lundgrendolid1970",
      "si_lundgrendolid1970",
      "si_nigh2000",
      "si_nigh2000_gi",
      "si_nighcourtin1998",
      "si_nighcourtin1998",
      "si_huang1994",
      "si_augerward2021",
      "si_pregent2010",
      "si_pregent2016",
      "si_sharma2021",
      "si_sharmaparton2018a",
      "si_sharmaparton2018b",
      "si_sharmaparton2019",
      "si_sharma2015",
      "si_parresolvissage1998",
      "si_payandeh1974",
      "si_sharmareid2018",
      "si_sharma2022",
      "si_lafleche2013",
      "si_lafleche2013",
      "si_scottvoorhis1986",
      "si_scottvoorhis1986",
      "si_thrower1994"
    ),
    fixed_args = list(
      list(),
      list(),
      list(),
      list(),
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
      list(base_age = 50),
      list(base_age = 50),
      list(),
      list(),
      list(),
      list(),
      list(),
      list(),
      list(),
      list(),
      list(),
      list(curve_set = "potential"),
      list(curve_set = "observed"),
      list(convert_to_total_age = FALSE),
      list(convert_to_total_age = TRUE),
      list()
    ),
    plot_si_values = list(
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      c(4, 8, 12, 16),
      c(4, 8, 12, 16),
      c(4, 8, 12, 16),
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
    ),
    age_basis = c(
      "total_age",          # buckman2006
      "total_age",          # carmean1989
      "breast_height_age",  # carmean1996
      "total_age",          # carmeanhahn1981
      "breast_height_age",  # cieszewskibella1991
      "breast_height_age",  # kerbowling1991
      "breast_height_age",  # lundgrendolid1970_exponential_monomolecular
      "breast_height_age",  # lundgrendolid1970_monomolecular
      "breast_height_age",  # nigh2000
      "breast_height_age",  # nigh2000_gi
      "breast_height_age",  # nighcourtin1998_si25
      "breast_height_age",  # nighcourtin1998_si50
      "breast_height_age",  # huang1994
      "total_age",          # augerward2021
      "breast_height_age",  # pregent2010
      "breast_height_age",  # pregent2016
      "breast_height_age",  # sharma2021
      "breast_height_age",  # sharmaparton2018a
      "breast_height_age",  # sharmaparton2018b
      "breast_height_age",  # sharmaparton2019
      "breast_height_age",  # sharma2015
      "breast_height_age",  # parresolvissage1998
      "breast_height_age",  # payandeh1974
      "breast_height_age",  # sharmareid2018
      "breast_height_age",  # sharma2022
      "breast_height_age",  # lafleche2013_potential
      "breast_height_age",  # lafleche2013_observed
      "breast_height_age",  # scottvoorhis1986_bh_age
      "total_age",          # scottvoorhis1986_total_age
      "breast_height_age"   # thrower1994
    ),
    age_domain_max = c(
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      100,
      60,
      70,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_
    ),
    requires_species = c(
      FALSE, # buckman2006
      TRUE,  # carmean1989
      TRUE,  # carmean1996
      TRUE,  # carmeanhahn1981
      TRUE,  # cieszewskibella1991
      TRUE,  # kerbowling1991
      TRUE,  # lundgrendolid1970_exponential_monomolecular
      TRUE,  # lundgrendolid1970_monomolecular
      FALSE, # nigh2000
      FALSE, # nigh2000_gi
      FALSE, # nighcourtin1998_si25
      FALSE, # nighcourtin1998_si50
      FALSE, # huang1994
      TRUE,  # augerward2021
      FALSE, # pregent2010
      FALSE, # pregent2016
      TRUE,  # sharma2021
      FALSE, # sharmaparton2018a
      FALSE, # sharmaparton2018b
      FALSE, # sharmaparton2019
      TRUE,  # sharma2015
      FALSE, # parresolvissage1998
      TRUE,  # payandeh1974
      TRUE,  # sharmareid2018
      TRUE,  # sharma2022
      TRUE,  # lafleche2013_potential
      TRUE,  # lafleche2013_observed
      TRUE,  # scottvoorhis1986_bh_age
      TRUE,  # scottvoorhis1986_total_age
      TRUE   # thrower1994
    ),
    requires_gi = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
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
      FALSE,
      FALSE
    ),
    supports_predict_si = c(
      rep(TRUE, 25),
      FALSE,
      FALSE,
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
    scope = rep("regional", 30),
    province_scope = list(
      c("ON"),
      c("ON", "QC", "NB", "NS", "PE", "NL"),
      c("ON"),
      c("ON"),
      c("AB"),
      c("NB"),
      c("ON"),
      c("ON"),
      c("BC"),
      c("BC"),
      c("BC"),
      c("BC"),
      c("AB"),
      c("QC"),
      c("QC"),
      c("QC"),
      c("ON"),
      c("ON"),
      c("ON"),
      c("ON"),
      c("ON"),
      c("NB", "NS", "PE", "NL", "QC", "ON"),
      c("ALL"),
      c("ON"),
      c("ON"),
      c("QC"),
      c("QC"),
      c("NB", "NS", "PE", "NL", "QC", "ON"),
      c("NB", "NS", "PE", "NL", "QC", "ON"),
      c("BC")
    ),
    subregion_type = c(
      "none",
      "none",
      "none",
      "none",
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
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "qc_ecological_subregion",
      "qc_ecological_subregion",
      "none",
      "none",
      "bec_region"
    ),
    subregion_required = rep(FALSE, 30),
    subregion_arg = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
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
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "ecological_subregion",
      "ecological_subregion",
      NA_character_,
      NA_character_,
      NA_character_
    ),
    subregion_scope = list(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
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
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      c("QC"),
      c("QC"),
      NA_character_,
      NA_character_,
      c("BC_INTERIOR")
    ),
    subregion_desc = c(
      "none",
      "none",
      "none",
      "none",
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
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "none",
      "Quebec ecological subregion",
      "Quebec ecological subregion",
      "none",
      "none",
      "BC interior"
    ),
    description = c(
      "Buckman et al. (2006) piecewise red pine site-index model",
      "Carmean et al. (1989) eastern species site-index model set",
      "Carmean (1996) northwest Ontario site-index model set",
      "Lake States site-index model for balsam fir and white spruce",
      "Alberta polymorphic variable-age site-index model for four major tree species",
      "New Brunswick polymorphic site-index model for softwoods",
      "Lundgren and Dolid model (exponential monomolecular form)",
      "Lundgren and Dolid model (monomolecular form)",
      "Nigh (2000) polymorphic site-index model for interior western redcedar",
      "Nigh (2000) growth-intercept site-index model for interior western redcedar",
      "Nigh and Courtin (1998) red alder model, SI25 scale",
      "Nigh and Courtin (1998) red alder model, SI50 scale",
      "Huang et al. (1994) Alberta polymorphic site-index model set",
      "Auger and Ward (2021) Quebec plantation site-index model",
      "Pregent et al. (2010) white spruce plantation site-index model",
      "Pregent et al. (2016) Norway spruce plantation site-index model",
      "Sharma (2021) fixed-effects no-climate mixed-stand site-index model",
      "Sharma and Parton (2018) non-climate white spruce plantation model",
      "Sharma and Parton (2018) non-climate red pine plantation model",
      "Sharma and Parton (2019) non-climate white pine plantation model",
      "Sharma et al. (2015) no-climate plantation model for jack pine and black spruce",
      "Parresol and Vissage (1998) base-age invariant eastern white pine model",
      "Payandeh (1974) nonlinear site-index equations",
      "Sharma and Reid (2018) fixed-effects natural-stand site-index model",
      "Sharma (2022) fixed-effects no-climate mixed-stand site-index model",
      "Lafleche et al. (2013) Quebec ecological-site potential-height IQS curves",
      "Lafleche et al. (2013) Quebec ecological-site observed-height IQS curves",
      "Scott and Voorhis (1986) model using breast-height age directly",
      "Scott and Voorhis (1986) model with internal conversion to total age",
      "Thrower et al. (1994) BC interior species model set"
    ),
    rank = c(72, 67, 89, 70, 87, 80, 60, 55, 90, 85, 85, 84, 88, 88, 88, 88, 89, 86, 86, 86, 88, 68, 50, 89, 89, 87, 72, 65, 66, 90),
    params_key = c(
      NA_character_,
      "parameters_Carmean1989",
      "parameters_Carmean1996",
      NA_character_,
      "parameters_CieszewskiBella1991",
      "parameters_KerBowling1991",
      "parameters_LungrenDolid1970",
      "parameters_LungrenDolid1970",
      NA_character_,
      "parameters_Nigh2000_gi",
      NA_character_,
      NA_character_,
      "parameters_Huang1994_si",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "parameters_Payandeh1974",
      NA_character_,
      NA_character_,
      "parameters_QC_IQS2013",
      "parameters_QC_IQS2013",
      "parameters_ScottVoorhis1986",
      "parameters_ScottVoorhis1986",
      "parameters_Thrower1994"
    ),
    species_manual = list(
      c("PINU.RES"),
      c(
        "ACER.SAH",
        "BETU.ALL",
        "FAGU.GRA",
        "FRAX.AME",
        "FRAX.NIG",
        "PRUN.SER",
        "QUER.RUB",
        "TILI.AME",
        "ULMU.AME",
        "CHAM.THY",
        "TSUG.CAN"
      ),
      NULL,
      c("ABIE.BAL", "PICE.GLA"),
      NULL,
      NULL,
      NULL,
      NULL,
      c("THUJ.PLI"),
      c("THUJ.PLI"),
      c("ALNU.RUB"),
      c("ALNU.RUB"),
      NULL,
      c("PINU.BAN", "PICE.MAR"),
      c("PICE.GLA"),
      c("PICE.ABI"),
      c("PINU.BAN", "PICE.MAR"),
      c("PICE.GLA"),
      c("PINU.RES"),
      c("PINU.STR"),
      c("PINU.BAN", "PICE.MAR"),
      c("PINU.STR"),
      NULL,
      c("PINU.BAN", "PICE.MAR"),
      c("PICE.MAR", "POPU.TRE"),
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
