#' Site-index model registry
#'
#' Lists the available site-index models, their species and geographic
#' coverage, age and height definitions, and required inputs.
#' Used by [find_si_models()], [get_si_model_info()] and [si()].
#'
#' @details
#' `age_basis` describes the input age type, not the SI reference age type.
#' `total_age` includes years since planting for Auger-Ward and Prégent models;
#' their supplied ages and equations are unchanged and no nursery-age adjustment
#' is applied. `corrected_age_above_1m` means the source's corrected age above
#' 1 m, adjusted to remove periods of suppressed growth (not simply years
#' since reaching 1 m). `species_dependent` means the age type varies by
#' species; `unresolved` means it has not been established.
#'
#' Several columns contain a separate table for each model:
#' * `scientific_definitions`: the input age type, SI reference age, and whether
#'   height and SI are measured from the ground or above breast height.
#' * `applicability`: information about where and for which stands a model
#'   can be used.
#' * `metadata_evidence`: references supporting the recorded information,
#'   including assumptions and points that remain uncertain.
#' * `age_constraints`: supported ages and age-related restrictions.
#' An empty table means information is missing, not that there are no limits.
#'
#' Models not identified as plantation or mixed-origin models are recorded as
#' natural-stand models by assumption. This is noted in `metadata_evidence`;
#' it does not determine the stand type of a user's observations.
#'
#' Input age and SI reference age can use different age types. Likewise, the
#' height definition used for SI can differ from that used for predicted or
#' measured height. For models with `total_height`, that option changes height,
#' not the definition of SI. Site indices from different models should not
#' be treated as interchangeable.
#'
#' `fixed_args` and `fixed_arg_roles` distinguish settings that select a
#' model version (`variant`) from settings used only for plotting (`plot_only`).
#' The Prégent models use a reference age of 25 by default, although their
#' article plots use 50. `age_domain_max` is also used for plotting; it does
#' not decide whether a model can be selected.
#'
#' `operation_inputs` lists the inputs needed for each calculation, with roles
#' `required`, `optional`, `fixed` or `forbidden`. For ordinary age-height
#' models, supply height to estimate SI or SI to predict height, not both.
#' Growth-intercept models estimate SI from early height growth; ecological-site
#' curves predict height from age, species and ecological classification.
#'
#' `input_constraints` records supported ages, ecological-code combinations,
#' additional requirements and climate-input ranges. A `condition_id` links an
#' input to the relevant rule. All named values in a rule's `selectors` must
#' match; a missing ecological subregion matches a missing entry, not every
#' subregion. `engine_action` describes how the individual model handles that
#' rule. These tables describe requirements; they do not check observations.
#'
#' `native_defaults` contains each function's default argument values.
#' `variant_args` contains settings selecting a model version; `plot_args`
#' also includes plotting settings. The `nigh2009` model ID selects version 1,
#' and `huang2026` selects the version without climate inputs.
#'
#' @return A tibble with one row per site-index model variant and metadata
#'   fields used for model selection.
#' @export
si_model_registry <- function() {
  dplyr::bind_rows(.si_model_definitions()) |>
    .si_add_scientific_metadata() |>
    .si_add_operation_metadata()
}

.si_registry_row <- function(
  model_id,
  reference,
  engine,
  age_basis,
  requires_species,
  province_scope,
  description,
  rank,
  fixed_args = list(),
  plot_si_values = NULL,
  age_domain_max = NA_real_,
  requires_gi = FALSE,
  supports_predict_si = TRUE,
  supports_predict_height = TRUE,
  scope = "regional",
  subregion_type = "none",
  subregion_required = FALSE,
  subregion_arg = NA_character_,
  subregion_scope = NA_character_,
  subregion_desc = "none",
  params_key = NA_character_,
  species_manual = NULL
) {
  tibble::tibble(
    model_id = model_id,
    reference = reference,
    engine = engine,
    fixed_args = list(fixed_args),
    plot_si_values = list(plot_si_values),
    age_basis = age_basis,
    age_domain_max = age_domain_max,
    requires_species = requires_species,
    requires_gi = requires_gi,
    supports_predict_si = supports_predict_si,
    supports_predict_height = supports_predict_height,
    scope = scope,
    province_scope = list(province_scope),
    subregion_type = subregion_type,
    subregion_required = subregion_required,
    subregion_arg = subregion_arg,
    subregion_scope = list(subregion_scope),
    subregion_desc = subregion_desc,
    description = description,
    rank = rank,
    params_key = params_key,
    species_manual = list(species_manual)
  )
}

.si_model_definitions <- function() {
  list(
    .si_registry_row(
      model_id = "buckman2006",
      reference = "@Buckman2006",
      engine = "si_buckman2006",
      age_basis = "total_age",
      requires_species = FALSE,
      province_scope = "ON",
      description = "Buckman et al. (2006) piecewise red pine site-index model",
      rank = 72,
      species_manual = "PINU.RES"
    ),
    .si_registry_row(
      model_id = "carmean1989",
      reference = "@Carmean1989",
      engine = "si_carmean1989",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = c("ON", "QC", "NB", "NS", "PE", "NL"),
      description = "Carmean et al. (1989) eastern species site-index model set",
      rank = 67,
      params_key = "parameters_Carmean1989",
      species_manual = c(
        "ACER.SAH", "BETU.ALL", "FAGU.GRA", "FRAX.AME", "FRAX.NIG", "PRUN.SER",
        "QUER.RUB", "TILI.AME", "ULMU.AME", "CHAM.THY", "TSUG.CAN"
      )
    ),
    .si_registry_row(
      model_id = "carmean1996",
      reference = "@Carmean1996",
      engine = "si_carmean1996",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Carmean (1996) northwest Ontario site-index model set",
      rank = 89,
      params_key = "parameters_Carmean1996"
    ),
    .si_registry_row(
      model_id = "carmeanhahn1981",
      reference = "@CarmeanHahn1981",
      engine = "si_carmeanhahn1981",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Lake States site-index model for balsam fir and white spruce",
      rank = 70,
      species_manual = c("ABIE.BAL", "PICE.GLA")
    ),
    .si_registry_row(
      model_id = "cieszewskibella1991",
      reference = "@CieszewskiBella1991",
      engine = "si_cieszewskibella1991",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "AB",
      description = "Alberta polymorphic variable-age site-index model for four major tree species",
      rank = 87,
      params_key = "parameters_CieszewskiBella1991"
    ),
    .si_registry_row(
      model_id = "kerbowling1991",
      reference = "@KerBowling1991",
      engine = "si_kerbowling1991",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "NB",
      description = "New Brunswick polymorphic site-index model for softwoods",
      rank = 80,
      params_key = "parameters_KerBowling1991"
    ),
    .si_registry_row(
      model_id = "lundgrendolid1970_exponential_monomolecular",
      reference = "@LundgrenDolid1970",
      engine = "si_lundgrendolid1970",
      age_basis = "species_dependent",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Lundgren and Dolid model (exponential monomolecular form)",
      rank = 60,
      fixed_args = list(variant = "exponential_monomolecular"),
      params_key = "parameters_LungrenDolid1970"
    ),
    .si_registry_row(
      model_id = "lundgrendolid1970_monomolecular",
      reference = "@LundgrenDolid1970",
      engine = "si_lundgrendolid1970",
      age_basis = "species_dependent",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Lundgren and Dolid model (monomolecular form)",
      rank = 55,
      fixed_args = list(variant = "monomolecular"),
      params_key = "parameters_LungrenDolid1970"
    ),
    .si_registry_row(
      model_id = "nigh2000",
      reference = "@Nigh2000",
      engine = "si_nigh2000",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh (2000) polymorphic site-index model for interior western redcedar",
      rank = 90,
      subregion_type = "bec_region",
      subregion_scope = "BC_INTERIOR",
      subregion_desc = "BC interior",
      species_manual = "THUJ.PLI"
    ),
    .si_registry_row(
      model_id = "nigh2000_gi",
      reference = "@Nigh2000",
      engine = "si_nigh2000_gi",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh (2000) growth-intercept site-index model for interior western redcedar",
      rank = 85,
      age_domain_max = 50,
      requires_gi = TRUE,
      supports_predict_height = FALSE,
      subregion_type = "bec_region",
      subregion_scope = "BC_INTERIOR",
      subregion_desc = "BC interior",
      params_key = "parameters_Nigh2000_gi",
      species_manual = "THUJ.PLI"
    ),
    .si_registry_row(
      model_id = "nighcourtin1998_si25",
      reference = "@NighCourtin1998",
      engine = "si_nighcourtin1998",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh and Courtin (1998) red alder model, SI25 scale",
      rank = 85,
      fixed_args = list(si50 = FALSE),
      subregion_type = "bec_region",
      subregion_scope = "BC_COAST",
      subregion_desc = "BC coast",
      species_manual = "ALNU.RUB"
    ),
    .si_registry_row(
      model_id = "nighcourtin1998_si50",
      reference = "@NighCourtin1998",
      engine = "si_nighcourtin1998",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh and Courtin (1998) red alder model, SI50 scale",
      rank = 84,
      fixed_args = list(si50 = TRUE),
      subregion_type = "bec_region",
      subregion_scope = "BC_COAST",
      subregion_desc = "BC coast",
      species_manual = "ALNU.RUB"
    ),
    .si_registry_row(
      model_id = "huang1994",
      reference = "@Huang1994si",
      engine = "si_huang1994",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "AB",
      description = "Huang et al. (1994) Alberta polymorphic site-index model set",
      rank = 88,
      subregion_type = "ab_natural_region_group",
      subregion_arg = "natural_subregion",
      subregion_scope = "AB",
      subregion_desc = "Alberta natural-region groups",
      params_key = "parameters_Huang1994_si"
    ),
    .si_registry_row(
      model_id = "augerward2021",
      reference = "@AugerWard2021",
      engine = "si_augerward2021",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "QC",
      description = "Auger and Ward (2021) Quebec plantation site-index model",
      rank = 88,
      plot_si_values = c(4, 8, 12, 16),
      age_domain_max = 100,
      species_manual = c("PINU.BAN", "PICE.MAR")
    ),
    .si_registry_row(
      model_id = "pregent2010",
      reference = "@Pregent2010",
      engine = "si_pregent2010",
      age_basis = "total_age",
      requires_species = FALSE,
      province_scope = "QC",
      description = "Pregent et al. (2010) white spruce plantation site-index model",
      rank = 88,
      fixed_args = list(base_age = 50),
      plot_si_values = c(4, 8, 12, 16),
      age_domain_max = 60,
      species_manual = "PICE.GLA"
    ),
    .si_registry_row(
      model_id = "pregent2016",
      reference = "@Pregent2016",
      engine = "si_pregent2016",
      age_basis = "total_age",
      requires_species = FALSE,
      province_scope = "QC",
      description = "Pregent et al. (2016) Norway spruce plantation site-index model",
      rank = 88,
      fixed_args = list(base_age = 50),
      plot_si_values = c(4, 8, 12, 16),
      age_domain_max = 70,
      species_manual = "PICE.ABI"
    ),
    .si_registry_row(
      model_id = "sharma2021",
      reference = "@Sharma2021SI",
      engine = "si_sharma2021",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Sharma (2021) fixed-effects no-climate mixed-stand site-index model",
      rank = 89,
      species_manual = c("PINU.BAN", "PICE.MAR")
    ),
    .si_registry_row(
      model_id = "sharmaparton2018a",
      reference = "@SharmaParton2018a",
      engine = "si_sharmaparton2018a",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "ON",
      description = "Sharma and Parton (2018) non-climate white spruce plantation model",
      rank = 86,
      species_manual = "PICE.GLA"
    ),
    .si_registry_row(
      model_id = "sharmaparton2018b",
      reference = "@SharmaParton2018b",
      engine = "si_sharmaparton2018b",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "ON",
      description = "Sharma and Parton (2018) non-climate red pine plantation model",
      rank = 86,
      species_manual = "PINU.RES"
    ),
    .si_registry_row(
      model_id = "sharmaparton2019",
      reference = "@SharmaParton2019",
      engine = "si_sharmaparton2019",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "ON",
      description = "Sharma and Parton (2019) non-climate white pine plantation model",
      rank = 86,
      species_manual = "PINU.STR"
    ),
    .si_registry_row(
      model_id = "sharma2015",
      reference = "@SharmaEtAl2015",
      engine = "si_sharma2015",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Sharma et al. (2015) no-climate plantation model for jack pine and black spruce",
      rank = 88,
      species_manual = c("PINU.BAN", "PICE.MAR")
    ),
    .si_registry_row(
      model_id = "parresolvissage1998",
      reference = "@ParresolVissage1998",
      engine = "si_parresolvissage1998",
      age_basis = "total_age",
      requires_species = FALSE,
      province_scope = c("NB", "NS", "PE", "NL", "QC", "ON"),
      description = "Parresol and Vissage (1998) base-age invariant eastern white pine model",
      rank = 68,
      species_manual = "PINU.STR"
    ),
    .si_registry_row(
      model_id = "payandeh1974",
      reference = "@Payandeh1974",
      engine = "si_payandeh1974",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "ALL",
      description = "Payandeh (1974) nonlinear site-index equations",
      rank = 50,
      params_key = "parameters_Payandeh1974"
    ),
    .si_registry_row(
      model_id = "sharmareid2018",
      reference = "@SharmaReid2018",
      engine = "si_sharmareid2018",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Sharma and Reid (2018) fixed-effects natural-stand site-index model",
      rank = 89,
      species_manual = c("PINU.BAN", "PICE.MAR")
    ),
    .si_registry_row(
      model_id = "sharma2022",
      reference = "@Sharma2022",
      engine = "si_sharma2022",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Sharma (2022) fixed-effects no-climate mixed-stand site-index model",
      rank = 89,
      species_manual = c("PICE.MAR", "POPU.TRE")
    ),
    .si_registry_row(
      model_id = "lafleche2013_potential",
      reference = "@LaflecheEtAl2013",
      engine = "si_lafleche2013",
      age_basis = "corrected_age_above_1m",
      requires_species = TRUE,
      province_scope = "QC",
      description = "Lafleche et al. (2013) Quebec ecological-site potential-height IQS curves",
      rank = 87,
      fixed_args = list(curve_set = "potential"),
      supports_predict_si = FALSE,
      subregion_type = "qc_ecological_subregion",
      subregion_arg = "ecological_subregion",
      subregion_scope = "QC",
      subregion_desc = "Quebec ecological subregion",
      params_key = "parameters_QC_IQS2013"
    ),
    .si_registry_row(
      model_id = "lafleche2013_observed",
      reference = "@LaflecheEtAl2013",
      engine = "si_lafleche2013",
      age_basis = "unresolved",
      requires_species = TRUE,
      province_scope = "QC",
      description = "Lafleche et al. (2013) Quebec ecological-site observed-height IQS curves",
      rank = 72,
      fixed_args = list(curve_set = "observed"),
      supports_predict_si = FALSE,
      subregion_type = "qc_ecological_subregion",
      subregion_arg = "ecological_subregion",
      subregion_scope = "QC",
      subregion_desc = "Quebec ecological subregion",
      params_key = "parameters_QC_IQS2013"
    ),
    .si_registry_row(
      model_id = "scottvoorhis1986_bh_age",
      reference = "@ScottVoorhis1986",
      engine = "si_scottvoorhis1986",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = c("NB", "NS", "PE", "NL", "QC", "ON"),
      description = "Scott and Voorhis (1986) total-age input model (legacy bh_age ID; no internal conversion)",
      rank = 65,
      fixed_args = list(convert_to_total_age = FALSE),
      params_key = "parameters_ScottVoorhis1986"
    ),
    .si_registry_row(
      model_id = "scottvoorhis1986_total_age",
      reference = "@ScottVoorhis1986",
      engine = "si_scottvoorhis1986",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = c("NB", "NS", "PE", "NL", "QC", "ON"),
      description = "Scott and Voorhis (1986) BHA input model (legacy total_age ID; internal source YTBH conversion)",
      rank = 66,
      fixed_args = list(convert_to_total_age = TRUE),
      params_key = "parameters_ScottVoorhis1986"
    ),
    .si_registry_row(
      model_id = "thrower1994",
      reference = "@Thrower1994",
      engine = "si_thrower1994",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "BC",
      description = "Thrower et al. (1994) BC interior species model set",
      rank = 90,
      subregion_type = "bec_region",
      subregion_scope = "BC_INTERIOR",
      subregion_desc = "BC interior",
      params_key = "parameters_Thrower1994",
      species_manual = c(
        "PINU.CON", "PICE.GLA", "PSEU.MEN", "ABIE.LAS", "TSUG.HET", "THUJ.PLI",
        "PINU.MON", "PINU.PON", "LARI.OCC", "POPU.TRE", "BETU.PAP"
      )
    ),
    .si_registry_row(
      model_id = "hugarcia2009",
      reference = "@HuGarcia2009",
      engine = "si_hugarcia2009",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "BC",
      description = "Hu and Garcia (2009) interior spruce height-growth and site-index model (BC SBS zone)",
      rank = 88,
      subregion_type = "bec_region",
      subregion_scope = "BC_INTERIOR",
      subregion_desc = "BC interior",
      params_key = "parameters_HuGarcia2009",
      species_manual = c("PICE.GLA", "PICE.ENG")
    ),
    .si_registry_row(
      model_id = "nigh1997",
      reference = "@Nigh1997",
      engine = "si_nigh1997",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh (1997) logistic height-age (site index) model for Sitka spruce in coastal British Columbia",
      rank = 88,
      params_key = "parameters_Nigh1997",
      species_manual = "PICE.SIT"
    ),
    .si_registry_row(
      model_id = "nigh2002",
      reference = "@Nigh2002",
      engine = "si_nigh2002",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh et al. (2002) trembling aspen height-age (site index) model for British Columbia",
      rank = 86,
      subregion_type = "bec_region",
      subregion_arg = "bec_zone",
      subregion_scope = c("BWBS", "ICH", "IDF", "MS", "SBPS", "SBS"),
      subregion_desc = "BC biogeoclimatic zone (extended model)",
      params_key = "parameters_Nigh2002",
      species_manual = "POPU.TRE"
    ),
    .si_registry_row(
      model_id = "cieszewski1993",
      reference = "@Cieszewski1993",
      engine = "si_cieszewski1993",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "SK",
      description = "Cieszewski, Bella and Yeung (1993) preliminary variable-age site-index model for eleven Saskatchewan species",
      rank = 64,
      params_key = "parameters_Cieszewski1993",
      species_manual = c(
        "ABIE.BAL", "POPU.BAL", "PICE.MAR", "PINU.BAN", "PINU.CON", "ACER.NEG",
        "POPU.TRE", "LARI.LAR", "BETU.PAP", "ULMU.AME", "PICE.GLA"
      )
    ),
    .si_registry_row(
      model_id = "goudie1984",
      reference = "@Goudie1984",
      engine = "si_goudie1984",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "BC",
      description = "Goudie (1984) logistic height-age (site-index) model for lodgepole pine and white spruce in British Columbia (SAS-reference implementation; pine dry-site coefficients)",
      rank = 66,
      age_domain_max = 140,
      params_key = "parameters_Goudie1984",
      species_manual = c("PINU.CON", "PICE.GLA")
    ),
    .si_registry_row(
      model_id = "nigh2004",
      reference = "@Nigh2004",
      engine = "si_nigh2004",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "BC",
      description = "Nigh (2004) juvenile height-age (site index) model for lodgepole pine and interior spruce in British Columbia (province-wide and biogeoclimatic-zone parameter sets)",
      rank = 86,
      age_domain_max = 20,
      subregion_type = "bec_region",
      subregion_arg = "bec_zone",
      subregion_scope = c("BWBS", "ESSF", "ICH", "IDF", "MS", "SBS", "SBPS"),
      subregion_desc = "BC biogeoclimatic zone (optional; province-wide otherwise)",
      params_key = "parameters_Nigh2004",
      species_manual = c("PINU.CON", "PICE.GLA")
    ),
    .si_registry_row(
      model_id = "nigh2009",
      reference = "@Nigh2009",
      engine = "si_nigh2009",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh et al. (2009) paper birch log-logistic height-age (site index) model for British Columbia (base, operational, and zonal variants)",
      rank = 86,
      subregion_type = "bec_region",
      subregion_arg = "bec_zone",
      subregion_scope = c("ICH", "IDF", "SBS"),
      subregion_desc = "BC biogeoclimatic zone (zonal model 3)",
      params_key = "parameters_Nigh2009",
      species_manual = "BETU.PAP"
    ),
    .si_registry_row(
      model_id = "carmean2006",
      reference = "@Carmean2006",
      engine = "si_carmean2006",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "ON",
      description = "Carmean, Hazenberg and Deschamps (2006) polymorphic (Newnham) site-index model for black spruce and trembling aspen in northwest Ontario",
      rank = 85,
      age_domain_max = 100,
      params_key = "parameters_Carmean2006",
      species_manual = c("PICE.MAR", "POPU.TRE")
    ),
    .si_registry_row(
      model_id = "carmean2001",
      reference = "@Carmean2001",
      engine = "si_carmean2001",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "ON",
      description = "Carmean, Niznowski and Hazenberg (2001) polymorphic (Newnham) site-index model for jack pine in northern Ontario",
      rank = 66,
      age_domain_max = 100,
      params_key = "parameters_Carmean2001",
      species_manual = "PINU.BAN"
    ),
    .si_registry_row(
      model_id = "goelz1992",
      reference = "@Goelz1992",
      engine = "si_goelz1992",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "ON",
      description = "Goelz and Burk (1992) base-age invariant Chapman-Richards site-index model for jack pine in north central Ontario",
      rank = 66,
      params_key = "parameters_Goelz1992",
      species_manual = "PINU.BAN"
    ),
    .si_registry_row(
      model_id = "nigh2017",
      reference = "@Nigh2017",
      engine = "si_nigh2017",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh (2017) grounded-GADA (Chapman-Richards) height-age (site index) model for lodgepole pine in British Columbia",
      rank = 90,
      params_key = "parameters_Nigh2017",
      species_manual = "PINU.CON"
    ),
    .si_registry_row(
      model_id = "batho2014",
      reference = "@Batho2014",
      engine = "si_batho2014",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "BC",
      description = "Batho and Garcia (2014) polymorphic Bertalanffy-Richards height-age (site index) model for lodgepole pine in the Sub-Boreal Spruce zone of British Columbia",
      rank = 88,
      species_manual = "PINU.CON"
    ),
    .si_registry_row(
      model_id = "nigh1998",
      reference = "@Nigh1998",
      engine = "si_nigh1998",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh (1998) log-logistic height-age (site index) model for western hemlock in the interior of British Columbia",
      rank = 88,
      plot_si_values = c(5, 10, 15, 20, 25),
      species_manual = "TSUG.HET"
    ),
    .si_registry_row(
      model_id = "nigh1998_gi",
      reference = "@Nigh1998",
      engine = "si_nigh1998_gi",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Nigh (1998) growth-intercept site-index model for western hemlock in the interior of British Columbia",
      rank = 85,
      age_domain_max = 50,
      requires_gi = TRUE,
      supports_predict_height = FALSE,
      params_key = "parameters_Nigh1998_gi",
      species_manual = "TSUG.HET"
    ),
    .si_registry_row(
      model_id = "alemdag1991",
      reference = "@Alemdag1991",
      engine = "si_alemdag1991",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "ALL",
      description = "Alemdag (1991) national (Canada-wide) modified Chapman-Richards site-index and height-growth model for white spruce in natural stands",
      rank = 60,
      plot_si_values = c(5, 10, 15, 20, 25),
      age_domain_max = 150,
      scope = "national",
      params_key = "parameters_Alemdag1991",
      species_manual = "PICE.GLA"
    ),
    .si_registry_row(
      model_id = "brisco2002",
      reference = "@Brisco2002",
      engine = "si_brisco2002",
      age_basis = "breast_height_age",
      requires_species = FALSE,
      province_scope = "BC",
      description = "Brisco, Klinka and Nigh (2002) Chapman-Richards height-age (site index) model for western larch in British Columbia",
      rank = 88,
      plot_si_values = c(12, 16, 20, 24),
      age_domain_max = 134,
      params_key = "parameters_Brisco2002",
      species_manual = "LARI.OCC"
    ),
    .si_registry_row(
      model_id = "chenklinka2000",
      reference = "@ChenKlinka2000",
      engine = "si_chenklinka2000",
      age_basis = "breast_height_age",
      requires_species = TRUE,
      province_scope = "BC",
      description = "Chen and Klinka (2000) conditioned Chapman-Richards height-age (site index) model for subalpine fir, Engelmann spruce, and lodgepole pine in the ESSF zone of British Columbia",
      rank = 88,
      plot_si_values = c(8, 12, 16, 20, 24),
      age_domain_max = 150,
      params_key = "parameters_ChenKlinka1998",
      species_manual = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
    ),
    .si_registry_row(
      model_id = "huang2009_bh_age",
      reference = "@Huang2009gypsy",
      engine = "si_huang2009",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "AB",
      description = "Huang, Meng and Yang (2009) GYPSY top-height / site-index model for four Alberta tree species (site index at 50 years breast-height age)",
      rank = 88,
      fixed_args = list(index_age = "breast_height"),
      params_key = "parameters_Huang2009",
      species_manual = c("POPU.TRE", "PICE.MAR", "PINU.CON", "PICE.GLA")
    ),
    .si_registry_row(
      model_id = "huang2009_total_age",
      reference = "@Huang2009gypsy",
      engine = "si_huang2009",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "AB",
      description = "Huang, Meng and Yang (2009) GYPSY top-height / site-index model for four Alberta tree species (site index at 50 years total age)",
      rank = 88,
      fixed_args = list(index_age = "total"),
      params_key = "parameters_Huang2009",
      species_manual = c("POPU.TRE", "PICE.MAR", "PINU.CON", "PICE.GLA")
    ),
    .si_registry_row(
      model_id = "huang2026",
      reference = "@HuangEtAl2026",
      engine = "si_huang2026",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "AB",
      description = "Huang et al. (2026) non-climate height-age model for eight Alberta species (height index at 50 years total age)",
      rank = 88,
      age_domain_max = 180,
      params_key = "parameters_Huang2026"
    ),
    .si_registry_row(
      model_id = "huang2026_climate",
      reference = "@HuangEtAl2026",
      engine = "si_huang2026",
      age_basis = "total_age",
      requires_species = TRUE,
      province_scope = "AB",
      description = "Huang et al. (2026) climate height-age model for eight Alberta species (inherent HI; requires species-specific previous-years MAP and MWMT averages and location-specific 1961-1990 MAP_normal and MWMT_normal)",
      rank = 88,
      fixed_args = list(variant = "climate"),
      age_domain_max = 180,
      params_key = "parameters_Huang2026_climate"
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
