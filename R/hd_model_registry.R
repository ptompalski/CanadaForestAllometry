# DBH–height (hd_) model registry and discovery helpers.
#
# Single source of truth for predictors: `.hd_model_predictors()` (one row per
# model_id x predictor argument). The registry's `required_predictors` /
# `optional_predictors` list-columns are derived from it.
#
# To add a model: append a `.hd_registry_row()` call to `.hd_model_definitions()`
# and its predictor rows to `.hd_model_predictors()`. `species` is not a
# predictor (coverage is derived from the params table / `species_manual`).

# internal
.hd_predictor_levels <- c("tree", "plot")

# internal
.hd_model_predictors <- function() {
  tibble::tribble(
    ~model_id, ~arg, ~level, ~units, ~required, ~definition, ~source_locator,
    "huang1994", "DBH", "tree", "cm", TRUE,
    "D: diameter (cm) at breast height (1.3 m), outside bark.",
    "T/291 p. 2",
    "huang1994", "natural_subregion", "plot", "code (1-21 or e.g. 'CM')", FALSE,
    "Alberta natural region (1994 regions 1-20 = 2006 natural subregion codes; 21 uses region 6); selects the natural-region-group model. NULL = provincial model.",
    "T/291 Table 1 (p. 5); App. 2 (p. 25)",
    "huang2016", "DBH", "tree", "cm", TRUE,
    "Tree diameter (cm) measured at a breast height of 1.3 m above the ground.",
    "T/609 p. 6 (eqs. [1]-[2])",
    "huang2016", "natural_subregion", "plot", "code (1-21 or e.g. 'CM')", FALSE,
    "Alberta 2006 natural subregion (Natural Regions Committee 2006); selects the natural-subregion-group model. NULL = provincial model.",
    "T/609 Table 2 (p. 2); Table 4 (p. 7)",
    "huang2016b", "DBH", "tree", "cm", TRUE,
    "Tree diameter (cm) at breast height.",
    "T/610 p. 4 (eqs. [1]-[2])",
    "huang2016b", "HDOM", "plot", "m", TRUE,
    "H_DC: average height of the dominant and co-dominant trees for the species (or deciduous / coniferous / all species combined); all dominant/co-dominant trees with height measurements are used.",
    "T/610 p. 4; Section 6.2 (p. 22)",
    "huang2016b", "BA", "plot", "m2/ha", TRUE,
    "Species-specific basal area per ha (or total for deciduous / coniferous / all species combined), summed over trees taller than 1.3 m.",
    "T/610 p. 4; Section 6.1 (p. 22)",
    "sharmaparton2007", "DBH", "tree", "cm", TRUE,
    "D = diameter at breast height (DBH) (cm).",
    "FEM 249:187-198, p. 190 (eq. 6)",
    "sharmaparton2007", "HDOM", "plot", "m", TRUE,
    "Dominant stand height (SHT): mean height of sampled dominant and codominant trees in a plot, irrespective of species.",
    "FEM 249:187-198, p. 188; p. 190",
    "sharmaparton2007", "BA", "plot", "m2/ha", TRUE,
    "Total tree basal area (BA/ha); minimum DBH for the total not stated.",
    "FEM 249:187-198, p. 188; p. 190",
    "sharmaparton2007", "TPH", "plot", "stems/ha", TRUE,
    "Number of stems (N/ha), calculated from the trees with DBH greater than or equal to 2.5 cm.",
    "FEM 249:187-198, p. 188",
    "eslamdoust2026_m4", "DBH", "tree", "cm", TRUE,
    "DBH_ij is diameter at breast height (cm) of jth tree in ith plot.",
    "FST 22:217-230, p. 220 (eq. 4)",
    "eslamdoust2026_m5", "DBH", "tree", "cm", TRUE,
    "DBH_ij is diameter at breast height (cm) of jth tree in ith plot.",
    "FST 22:217-230, p. 220 (eq. 5)",
    "eslamdoust2026_m5", "HDOM", "plot", "m", TRUE,
    "Stand height (SHT): mean height of dominant and co-dominant trees; species basis not stated.",
    "FST 22:217-230, p. 219; Table 5 (p. 225)",
    "eslamdoust2026_m5", "BA", "plot", "m2/ha", TRUE,
    "Stand basal area (BAH, m2/ha) of the plot; minimum DBH and species basis not stated.",
    "FST 22:217-230, p. 219; p. 220",
    "eslamdoust2026_m5", "TPH", "plot", "stems/ha", TRUE,
    "Number of trees per hectare (n/ha) with DBH >= 2.5 cm of the plot.",
    "FST 22:217-230, p. 220 (eq. 5)",
    "huang2009_m9", "DBH", "tree", "cm", TRUE,
    "Tree diameter at breast height (1.3 m above ground), cm.",
    "MAS 3(4):3-18, p. 4 (eq. 9); Table 1 (p. 12)",
    "huang2009_m10", "DBH", "tree", "cm", TRUE,
    "Tree diameter at breast height (1.3 m above ground), cm.",
    "MAS 3(4):3-18, p. 5 (eq. 10); Table 1 (p. 12)",
    "huang2009_m10", "HTOP", "plot", "m", TRUE,
    "Top height (TopH): 'the average height of the 100 largest DBH trees per ha'.",
    "MAS 3(4):3-18, p. 5; Table 1 (p. 12)",
    "huang2009_m11", "DBH", "tree", "cm", TRUE,
    "Tree diameter at breast height (1.3 m above ground), cm.",
    "MAS 3(4):3-18, p. 5 (eq. 11); Table 1 (p. 12)",
    "huang2009_m11", "HTOP", "plot", "m", TRUE,
    "Top height (TopH): 'the average height of the 100 largest DBH trees per ha'.",
    "MAS 3(4):3-18, p. 5; Table 1 (p. 12)",
    "huang2009_m11", "BA", "plot", "m2/ha", TRUE,
    "Basal area per ha (BA, m2/ha) as a measure of stand density; minimum DBH and species basis not stated.",
    "MAS 3(4):3-18, p. 5; Table 1 (p. 12)",
    "auger2016", "DBH", "tree", "cm", TRUE,
    "DHP (diam\u00e8tre \u00e0 hauteur de poitrine, 1.3 m) de l'arbre (cm).",
    "NRF 146, p. 4 (eq. [1])",
    "auger2016", "BA", "plot", "m2/ha", TRUE,
    "ST: surface terri\u00e8re marchande dans la placette (m2/ha; trees with DBH > 9 cm, all species).",
    "NRF 146, p. 4; Tableau 3 (p. 6)",
    "auger2016", "QMD", "plot", "cm", FALSE,
    "DHP moyen quadratique des arbres de la placette (cm); used as RDHP = DBH / QMD. Required for 21 species.",
    "NRF 146, Tableau 3 (p. 6); Tableau 5 (pp. 10-11)",
    "auger2016", "bioclim_subdomain", "plot", "code ('1', '2est' ... '6ouest')", FALSE,
    "Sous-domaine bioclimatique (11 classes). Required for 11 species.",
    "NRF 146, Tableau 3 (p. 6); Annexe 3",
    "auger2016", "physical_env", "plot", "code (0-9)", FALSE,
    "Milieu physique (10 classes; MRN 2013). Required for 10 species.",
    "NRF 146, Tableau 3 (p. 6); Annexe 2",
    "auger2016", "potential_veg", "plot", "code (e.g. 'RS2')", FALSE,
    "V\u00e9g\u00e9tation potentielle (three-character code). Required for 15 species.",
    "NRF 146, Tableau 3 (p. 6); Annexe 4",
    "auger2016", "MAT", "plot", "degC", FALSE,
    "Temp\u00e9rature annuelle moyenne sur la p\u00e9riode 1981-2010 (\u00b0C). Required for 7 species.",
    "NRF 146, Tableau 3 (p. 6)",
    "auger2016", "MAP", "plot", "mm", FALSE,
    "Pr\u00e9cipitations totales annuelles sur la p\u00e9riode 1981-2010 (mm). Required for 10 species.",
    "NRF 146, Tableau 3 (p. 6)",
    "auger2016", "elevation", "plot", "m", FALSE,
    "Altitude (m). Required for 4 species.",
    "NRF 146, Tableau 3 (p. 6)",
    "auger2016", "disturbance", "plot", "code (anthropique/naturelle/aucune)", FALSE,
    "Occurrence d'une perturbation l\u00e9g\u00e8re (3 classes): anthropique, naturelle, aucune; English 'anthropogenic', 'natural', 'none' accepted. Required for 5 species.",
    "NRF 146, Tableau 3 (p. 6); Annexe 2",
    "auger2025_dbh", "DBH", "tree", "cm", TRUE,
    "Diam\u00e8tre \u00e0 hauteur de poitrine (mesur\u00e9 \u00e0 1,3 m, DHP), cm.",
    "SSS-10 p. 2; eq. 10 (p. 11)",
    "auger2025_plot_mean_dbh", "DBH", "tree", "cm", TRUE,
    "Diam\u00e8tre \u00e0 hauteur de poitrine (mesur\u00e9 \u00e0 1,3 m, DHP), cm.",
    "SSS-10 p. 2; eqs. 11-12 (p. 14)",
    "auger2025_plot_mean_dbh", "DMEAN", "plot", "cm", TRUE,
    "DMOY: 'le DHP moyen de tous les arbres de la placette' (arithmetic mean DBH of all trees in the plot).",
    "SSS-10 eq. 2 (p. 6)",
    "auger2025_dominant", "DBH", "tree", "cm", TRUE,
    "Diam\u00e8tre \u00e0 hauteur de poitrine (mesur\u00e9 \u00e0 1,3 m, DHP), cm.",
    "SSS-10 p. 2; eqs. 13-14 (p. 16)",
    "auger2025_dominant", "HDOM", "plot", "m", TRUE,
    "HDOM: mean height of the dominant trees of the plot, i.e. 'les 8 plus hauts arbres plant\u00e9s faisant partie de l'\u00e9tage des arbres dominants et bien r\u00e9partis dans la placette (200 arbres/ha)' (400 m2 plots).",
    "SSS-10 eq. 3 (p. 7); p. 2",
    "auger2025_dominant", "DDOM", "plot", "cm", TRUE,
    "DDOM: mean DBH of the same dominant trees of the plot.",
    "SSS-10 eq. 3 (p. 7); p. 2",
    "zhang2002_ecoregion", "DBH", "tree", "cm", TRUE,
    "DBH is tree diameter at breast height (cm); outside bark, measured 1.3 m above ground.",
    "For. Chron. 78:530-538, p. 532 (eq. [1]); p. 530",
    "zhang2002_ecoregion", "ecoregion", "plot", "code ('3E','3S','3W','4E','4S','4W','5E')", TRUE,
    "Ontario ecoregion (ELC Working Group 2000; Hayden et al. 1995): 3E, 3S, 3W, 4S, 4W (Boreal region) and 4E, 5E (Central region).",
    "For. Chron. 78:530-538, p. 532; Fig. 1 (p. 531); Table 2 (p. 533)",
    "zhang2002_regional", "DBH", "tree", "cm", TRUE,
    "DBH is tree diameter at breast height (cm); outside bark, measured 1.3 m above ground.",
    "For. Chron. 78:530-538, p. 532 (eq. [1]); p. 530",
    "zhang2002_regional", "forest_region", "plot", "code ('boreal','central')", TRUE,
    "Ontario forest region: Boreal (ecoregions 3E, 3S, 3W, 4S, 4W) or Central (Great Lakes-St. Lawrence; ecoregions 4E, 5E). Used when ecoregion is not supplied.",
    "For. Chron. 78:530-538, p. 532; Table 2 (p. 533)",
    "zhang2002_overall", "DBH", "tree", "cm", TRUE,
    "DBH is tree diameter at breast height (cm); outside bark, measured 1.3 m above ground.",
    "For. Chron. 78:530-538, p. 532 (eq. [1]); p. 530",
    "temesgen2004_m2", "DBH", "tree", "cm", TRUE,
    "D is the observed diameter outside bark at breast height in centimetres.",
    "EJFR 123:45-51, p. 46 (Model 2); Table 5 (p. 51)",
    "temesgen2004_m7", "DBH", "tree", "cm", TRUE,
    "D is the observed diameter outside bark at breast height in centimetres.",
    "EJFR 123:45-51, p. 47 (Model 7); Table 4 (p. 50)",
    "temesgen2004_m7", "BAL", "tree", "m2/ha", TRUE,
    "BAL is the basal area in larger trees (m2/ha); species basis and minimum DBH not stated.",
    "EJFR 123:45-51, p. 47 (Model 7); Table 4 (p. 50)",
    "temesgen2004_m7", "TPH", "plot", "stems/ha", TRUE,
    "SPH is number of trees per hectare; minimum DBH not stated (trees < 2.0 cm DBH not recorded).",
    "EJFR 123:45-51, p. 47 (Model 7); p. 46",
    "temesgen2004_m7", "BA", "plot", "m2/ha", TRUE,
    "BA is basal area per hectare (m2/ha); minimum DBH not stated.",
    "EJFR 123:45-51, p. 47 (Model 7); Table 4 (p. 50)",
    "staudhammerlemay2000_dbh", "DBH", "tree", "cm", TRUE,
    "dbh (cm), recorded for all trees above 2 cm dbh.",
    "For. Chron. 76:303-309, p. 304; eq. [2] (p. 305); Table 5 (p. 308)",
    "staudhammerlemay2000_density", "DBH", "tree", "cm", TRUE,
    "dbh (cm), recorded for all trees above 2 cm dbh.",
    "For. Chron. 76:303-309, p. 304; eqs. [2a]-[2b] (p. 305); Table 5 (p. 308)",
    "staudhammerlemay2000_density", "TPH", "plot", "stems/ha", FALSE,
    "TSTEMSHA: stems per hectare for all species combined (trees > 2 cm dbh; live/dead basis not stated). Required for red alder (eq. 2a).",
    "For. Chron. 76:303-309, p. 304; Table 5 (p. 308)",
    "staudhammerlemay2000_density", "BA", "plot", "m2/ha", FALSE,
    "TBAHA: basal area per hectare for all species combined (trees > 2 cm dbh; live/dead basis not stated). Required for red alder (eq. 2a).",
    "For. Chron. 76:303-309, p. 304; Table 5 (p. 308)",
    "staudhammerlemay2000_density", "DMAX", "plot", "cm", FALSE,
    "Maximum dbh of all species on the plot, used for REL_dbh = 'dbh divided by the maximum dbh for all species'. Required for western redcedar, Douglas-fir and western hemlock (eq. 2b).",
    "For. Chron. 76:303-309, p. 304; Table 1 (p. 304); Table 5 (p. 308)"
  )
}

# internal
.hd_registry_row <- function(
  model_id,
  engine,
  reference,
  scope,
  province_scope,
  stand_origin,
  equation_family,
  description,
  fixed_args = list(),
  region_arg = NA_character_,
  prediction_types = "population",
  calibration_published = FALSE,
  params_key = NA_character_,
  species_manual = NULL
) {
  assert_chr_scalar(model_id, "model_id")
  assert_chr_scalar(engine, "engine")
  assert_chr_scalar(reference, "reference")
  assert_choice(scope, "scope", c("provincial", "regional"))
  assert_choice(stand_origin, "stand_origin", c("natural", "plantation", "both"))
  assert_chr_scalar(equation_family, "equation_family")
  assert_chr_scalar(description, "description")
  assert_chr_scalar(region_arg, "region_arg", allow_na = TRUE)
  assert_chr_scalar(params_key, "params_key", allow_na = TRUE)
  assert_choice(
    prediction_types,
    "prediction_types",
    c("population", "calibrated"),
    multiple = TRUE
  )
  if (!is.character(province_scope) || length(province_scope) == 0L) {
    cli::cli_abort("{.arg province_scope} must be a non-empty character vector.")
  }
  if (!is.list(fixed_args)) {
    cli::cli_abort("{.arg fixed_args} must be a (named) list.")
  }
  if (!is.logical(calibration_published) || length(calibration_published) != 1L ||
    is.na(calibration_published)) {
    cli::cli_abort("{.arg calibration_published} must be TRUE or FALSE.")
  }

  tibble::tibble(
    model_id = model_id,
    engine = engine,
    fixed_args = list(fixed_args),
    reference = reference,
    scope = scope,
    province_scope = list(province_scope),
    stand_origin = stand_origin,
    equation_family = equation_family,
    region_arg = region_arg,
    prediction_types = list(prediction_types),
    calibration_published = calibration_published,
    params_key = params_key,
    species_manual = list(species_manual),
    description = description
  )
}

# internal
# One `.hd_registry_row()` per model variant.
.hd_model_definitions <- function() {
  list(
    .hd_registry_row(
      model_id = "huang1994",
      engine = "hd_huang1994",
      reference = "@Huang1994hd",
      scope = "provincial",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards",
      description = "Alberta T/291 ecologically based DBH-height models (weighted NLS); provincial (Table 2) or natural-region-group (Table 1) variants, incl. softwood/hardwood group models.",
      region_arg = "natural_subregion",
      calibration_published = FALSE,
      params_key = "parameters_hd_Huang1994"
    ),
    .hd_registry_row(
      model_id = "huang2016",
      engine = "hd_huang2016",
      reference = "@Huang2016hd",
      scope = "provincial",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards; log_logistic",
      description = "Alberta T/609 population DBH-height models (base fits, Table 4); provincial or natural-subregion-group variants.",
      region_arg = "natural_subregion",
      calibration_published = TRUE,
      params_key = "parameters_hd_Huang2016"
    ),
    .hd_registry_row(
      model_id = "huang2016b",
      engine = "hd_huang2016b",
      reference = "@Huang2016hdExpanded",
      scope = "provincial",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards; log_logistic",
      description = "Alberta T/610 expanded population DBH-height models with species-specific basal area and dominant/co-dominant height (base fits, Table A1); balsam fir excluded (source error).",
      calibration_published = TRUE,
      params_key = "parameters_hd_Huang2016b"
    ),
    .hd_registry_row(
      model_id = "sharmaparton2007",
      engine = "hd_sharmaparton2007",
      reference = "@SharmaParton2007",
      scope = "regional",
      province_scope = "ON",
      stand_origin = "both",
      equation_family = "chapman_richards_stand",
      description = "Northern Ontario boreal population DBH-height models with dominant height, basal area and stems/ha (Method 2 fixed effects, Table 6).",
      calibration_published = TRUE,
      params_key = "parameters_hd_SharmaParton2007"
    ),
    .hd_registry_row(
      model_id = "eslamdoust2026_m4",
      engine = "hd_eslamdoust2026",
      reference = "@Eslamdoust2026",
      scope = "provincial",
      province_scope = "ON",
      stand_origin = "natural",
      equation_family = "chapman_richards",
      description = "Ontario boreal/GLSL population DBH-height models, DBH only (M4 fixed effects, Table 4); used when HDOM, BA and TPH are not supplied.",
      calibration_published = TRUE,
      params_key = "parameters_hd_Eslamdoust2026"
    ),
    .hd_registry_row(
      model_id = "eslamdoust2026_m5",
      engine = "hd_eslamdoust2026",
      reference = "@Eslamdoust2026",
      scope = "provincial",
      province_scope = "ON",
      stand_origin = "natural",
      equation_family = "chapman_richards_stand",
      description = "Ontario boreal/GLSL population DBH-height models with stand height, basal area and stems/ha (M5 fixed effects, Table 5); used when HDOM, BA and TPH are supplied.",
      calibration_published = TRUE,
      params_key = "parameters_hd_Eslamdoust2026"
    ),
    .hd_registry_row(
      model_id = "huang2009_m9",
      engine = "hd_huang2009",
      reference = "@Huang2009hd",
      scope = "regional",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards",
      description = "Alberta (Lower Foothills) black spruce population DBH-height model, DBH only (base model 9, Table 2); used when HTOP and BA are not supplied.",
      calibration_published = TRUE,
      params_key = "parameters_hd_Huang2009"
    ),
    .hd_registry_row(
      model_id = "huang2009_m10",
      engine = "hd_huang2009",
      reference = "@Huang2009hd",
      scope = "regional",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards",
      description = "Alberta (Lower Foothills) black spruce population DBH-height model with top height (model 10, Table 2); used when only HTOP is supplied.",
      calibration_published = TRUE,
      params_key = "parameters_hd_Huang2009"
    ),
    .hd_registry_row(
      model_id = "huang2009_m11",
      engine = "hd_huang2009",
      reference = "@Huang2009hd",
      scope = "regional",
      province_scope = "AB",
      stand_origin = "natural",
      equation_family = "chapman_richards",
      description = "Alberta (Lower Foothills) black spruce population DBH-height model with top height and basal area (model 11, Table 2); used when HTOP and BA are supplied.",
      calibration_published = TRUE,
      params_key = "parameters_hd_Huang2009"
    ),
    .hd_registry_row(
      model_id = "auger2016",
      engine = "hd_auger2016",
      reference = "@Auger2016",
      scope = "provincial",
      province_scope = "QC",
      stand_origin = "natural",
      equation_family = "log_quadratic",
      description = "Quebec population DBH-height models for 27 species with basal area and species-specific relative DBH, climate, altitude and ecological-classification effects (NRF 146, Annexes 1-4).",
      region_arg = "bioclim_subdomain",
      calibration_published = TRUE,
      params_key = "parameters_hd_Auger2016"
    ),
    .hd_registry_row(
      model_id = "auger2025_dbh",
      engine = "hd_auger2025",
      reference = "@Auger2025",
      scope = "provincial",
      province_scope = "QC",
      stand_origin = "plantation",
      equation_family = "korf",
      description = "Quebec black spruce and jack pine plantation population DBH-height models, DBH only (Korf, eq. 10, Tableau 7; SSS-10).",
      fixed_args = list(variant = "dbh"),
      calibration_published = FALSE,
      params_key = "parameters_hd_Auger2025"
    ),
    .hd_registry_row(
      model_id = "auger2025_plot_mean_dbh",
      engine = "hd_auger2025",
      reference = "@Auger2025",
      scope = "provincial",
      province_scope = "QC",
      stand_origin = "plantation",
      equation_family = "chapman_richards; hossfeld_ii",
      description = "Quebec black spruce and jack pine plantation DBH-height models with plot mean DBH (eqs. 11-12, Tableau 8; SSS-10).",
      fixed_args = list(variant = "plot_mean_dbh"),
      calibration_published = FALSE,
      params_key = "parameters_hd_Auger2025"
    ),
    .hd_registry_row(
      model_id = "auger2025_dominant",
      engine = "hd_auger2025",
      reference = "@Auger2025",
      scope = "provincial",
      province_scope = "QC",
      stand_origin = "plantation",
      equation_family = "chapman_richards; hossfeld_ii",
      description = "Quebec black spruce and jack pine plantation DBH-height models with dominant height and dominant DBH (eqs. 13-14, Tableau 9; SSS-10).",
      fixed_args = list(variant = "dominant"),
      calibration_published = FALSE,
      params_key = "parameters_hd_Auger2025"
    ),
    .hd_registry_row(
      model_id = "zhang2002_ecoregion",
      engine = "hd_zhang2002",
      reference = "@Zhang2002hd",
      scope = "regional",
      province_scope = "ON",
      stand_origin = "both",
      equation_family = "chapman_richards",
      description = "Ontario jack pine DBH-height models by ELC ecoregion (3E, 3S, 3W, 4E, 4S, 4W, 5E; Table 2); used when ecoregion is supplied.",
      region_arg = "ecoregion",
      calibration_published = FALSE,
      params_key = "parameters_hd_Zhang2002"
    ),
    .hd_registry_row(
      model_id = "zhang2002_regional",
      engine = "hd_zhang2002",
      reference = "@Zhang2002hd",
      scope = "regional",
      province_scope = "ON",
      stand_origin = "both",
      equation_family = "chapman_richards",
      description = "Ontario jack pine DBH-height models for the Boreal and Central forest regions (Table 2); used when only forest_region is supplied.",
      region_arg = "forest_region",
      calibration_published = FALSE,
      params_key = "parameters_hd_Zhang2002"
    ),
    .hd_registry_row(
      model_id = "zhang2002_overall",
      engine = "hd_zhang2002",
      reference = "@Zhang2002hd",
      scope = "provincial",
      province_scope = "ON",
      stand_origin = "both",
      equation_family = "chapman_richards",
      description = "Ontario jack pine DBH-height model fitted to all data (Overall, Table 2); used when neither ecoregion nor forest_region is supplied.",
      calibration_published = FALSE,
      params_key = "parameters_hd_Zhang2002"
    ),
    .hd_registry_row(
      model_id = "temesgen2004_m2",
      engine = "hd_temesgen2004",
      reference = "@Temesgen2004",
      scope = "regional",
      province_scope = "BC",
      stand_origin = "natural",
      equation_family = "weibull",
      description = "Interior BC (IDF zone) complex-stand DBH-height models, DBH only (Model 2, Table 5); used when BAL, TPH and BA are not supplied.",
      calibration_published = FALSE,
      params_key = "parameters_hd_Temesgen2004"
    ),
    .hd_registry_row(
      model_id = "temesgen2004_m7",
      engine = "hd_temesgen2004",
      reference = "@Temesgen2004",
      scope = "regional",
      province_scope = "BC",
      stand_origin = "natural",
      equation_family = "weibull_stand",
      description = "Interior BC (IDF zone) complex-stand DBH-height models with basal area in larger trees, stems/ha and basal area (Model 7, Table 4); used when BAL, TPH and BA are supplied.",
      calibration_published = FALSE,
      params_key = "parameters_hd_Temesgen2004"
    ),
    .hd_registry_row(
      model_id = "staudhammerlemay2000_dbh",
      engine = "hd_staudhammerlemay2000",
      reference = "@StaudhammerLeMay2000",
      scope = "regional",
      province_scope = "BC",
      stand_origin = "both",
      equation_family = "weibull",
      description = "Coastal BC (UBC Malcolm Knapp Research Forest, CWH zone) DBH-height models for western redcedar, red alder, Douglas-fir and western hemlock, DBH only (eq. 2, Table 5).",
      fixed_args = list(variant = "dbh"),
      calibration_published = FALSE,
      params_key = "parameters_hd_StaudhammerLeMay2000"
    ),
    .hd_registry_row(
      model_id = "staudhammerlemay2000_density",
      engine = "hd_staudhammerlemay2000",
      reference = "@StaudhammerLeMay2000",
      scope = "regional",
      province_scope = "BC",
      stand_origin = "both",
      equation_family = "weibull_parameter_prediction",
      description = "Coastal BC (UBC Malcolm Knapp Research Forest) DBH-height models with stand density: stems/ha and basal area for red alder (eq. 2a), relative DBH for western redcedar, Douglas-fir and western hemlock (eq. 2b; Table 5).",
      fixed_args = list(variant = "density"),
      calibration_published = FALSE,
      params_key = "parameters_hd_StaudhammerLeMay2000"
    )
  )
}

#' DBH–height model registry
#'
#' Returns a registry (metadata table) of the DBH–height (`hd_*()`) models
#' available in CanadaForestAllometry, one row per model variant. Use it with
#' [get_hd_model_info()] and [find_hd_models()] to discover which model suits
#' your data, then call the corresponding `hd_*()` function directly. There is
#' no automatic model selection.
#'
#' All models predict total tree height (m) from DBH (cm) at the population
#' level (random effects set to zero for mixed-effects sources).
#'
#' @return A tibble with one row per model variant and columns:
#' \describe{
#'   \item{`model_id`}{Stable model identifier (with variant suffix).}
#'   \item{`engine`}{Name of the `hd_*()` function implementing the model.}
#'   \item{`fixed_args`}{List-column of arguments selecting the variant.}
#'   \item{`reference`}{BibTeX key of the source publication.}
#'   \item{`scope`}{`"provincial"` or `"regional"`.}
#'   \item{`province_scope`}{List-column of jurisdiction codes.}
#'   \item{`stand_origin`}{`"natural"`, `"plantation"` or `"both"`.}
#'   \item{`equation_family`}{Model form (e.g. `"chapman_richards"`).}
#'   \item{`required_predictors`, `optional_predictors`}{List-columns of
#'     argument names (excluding `species`).}
#'   \item{`region_arg`}{Name of the model-specific region argument, or `NA`.}
#'   \item{`prediction_types`}{List-column; currently `"population"`.}
#'   \item{`calibration_published`}{Whether the source publishes the variance
#'     components needed for local (plot) calibration.}
#'   \item{`params_key`}{Internal parameter table name.}
#'   \item{`species_manual`}{List-column of species, used only when
#'     coefficients are not stored in a parameter table.}
#'   \item{`description`}{One-line description.}
#' }
#'
#' @seealso [get_hd_model_info()], [find_hd_models()]
#' @examples
#' hd_model_registry()
#' @export
hd_model_registry <- function() {
  rows <- .hd_model_definitions()
  reg <- if (length(rows) == 0L) {
    # zero-row prototype with the correct column types
    .hd_registry_row(
      model_id = "x", engine = "x", reference = "x", scope = "provincial",
      province_scope = "ALL", stand_origin = "both", equation_family = "x",
      description = "x"
    )[0, ]
  } else {
    dplyr::bind_rows(rows)
  }

  preds <- .hd_model_predictors()
  pick <- function(id, req) {
    preds$arg[preds$model_id == id & preds$required == req]
  }
  reg$required_predictors <- lapply(reg$model_id, pick, req = TRUE)
  reg$optional_predictors <- lapply(reg$model_id, pick, req = FALSE)

  dplyr::relocate(
    reg,
    "required_predictors", "optional_predictors",
    .after = "equation_family"
  )
}

#' DBH–height model registry with species coverage
#'
#' @return A tibble like `hd_model_registry()` plus `species` (list-column),
#'   `n_species` (integer) and `species_text` (character).
#' @keywords internal
#' @noRd
hd_model_registry_species <- function(reg = hd_model_registry()) {
  species_of <- function(model_id, params_key, species_manual) {
    if (length(species_manual) > 0L) {
      return(sort(unique(as.character(species_manual))))
    }
    if (is.na(params_key) || !nzchar(params_key)) {
      return(character(0))
    }
    tryCatch(
      extract_species_from_params(get_params_tbl(params_key)),
      error = function(e) {
        cli::cli_abort(
          "Failed to build species list for model_id={model_id} (params_key={params_key}): {conditionMessage(e)}"
        )
      }
    )
  }

  reg$species <- purrr::pmap(
    list(reg$model_id, reg$params_key, reg$species_manual),
    species_of
  )
  reg$n_species <- lengths(reg$species)
  reg$species_text <- vapply(
    reg$species,
    function(s) if (length(s) == 0L) NA_character_ else paste(s, collapse = ", "),
    character(1)
  )
  reg
}

#' Get information about a DBH–height model
#'
#' Returns the registry metadata for one `hd_*()` model variant together with
#' its predictor table (argument, level, units, whether required, and the
#' source's definition of the predictor).
#'
#' @param model_id Single model identifier, as in `hd_model_registry()$model_id`.
#'
#' @return A named list with the registry fields of the model (list-columns
#'   unwrapped), its `species` coverage, and `predictors`, a tibble with columns
#'   `arg`, `level`, `units`, `required`, `definition` and `source_locator`.
#'
#' @seealso [hd_model_registry()], [find_hd_models()]
#' @examples
#' ids <- hd_model_registry()$model_id
#' if (length(ids) > 0) get_hd_model_info(ids[[1]])
#' @export
get_hd_model_info <- function(model_id) {
  assert_chr_scalar(model_id, "model_id")

  reg <- hd_model_registry()
  if (!model_id %in% reg$model_id) {
    avail <- if (nrow(reg) == 0L) "none (no models registered yet)" else
      paste(reg$model_id, collapse = ", ")
    cli::cli_abort(c(
      "Unknown DBH\u2013height model {.val {model_id}}.",
      "i" = "Available model_id values: {avail}."
    ))
  }

  row <- hd_model_registry_species(reg[reg$model_id == model_id, ])
  info <- lapply(as.list(row), function(col) if (is.list(col)) col[[1]] else col)
  info$n_species <- NULL
  info$species_text <- NULL

  preds <- .hd_model_predictors()
  info$predictors <- preds[preds$model_id == model_id, setdiff(names(preds), "model_id")]
  info
}

#' Find DBH–height models usable with your data
#'
#' Filters [hd_model_registry()] by jurisdiction and species, and reports which
#' required predictors each candidate model needs that you do not have. Models
#' with missing predictors are kept (as near-misses) so you can see what extra
#' data would make them usable. No model is selected automatically.
#'
#' @param jurisdiction Optional character vector of province/territory codes
#'   (e.g. `"ON"`). Models covering any of them are kept. `NULL` (default)
#'   applies no filter.
#' @param species Optional character vector of NFI species codes
#'   (e.g. `"PICE.MAR"`). Models covering any of them are kept. `NULL`
#'   (default) applies no filter.
#' @param available Character vector of predictor argument names you can
#'   supply (e.g. `c("DBH", "BA", "TPH")`). Names follow the `hd_*()` argument
#'   names, which are case-sensitive.
#'
#' @return The matching rows of `hd_model_registry()` with species coverage
#'   (`species`, `n_species`, `species_text`), plus `missing_predictors`
#'   (list-column of required predictors not in `available`) and `usable`
#'   (logical). Usable models are listed first.
#'
#' @seealso [hd_model_registry()], [get_hd_model_info()]
#' @examples
#' find_hd_models(jurisdiction = "ON", species = "PICE.MAR",
#'                available = c("DBH", "BA", "TPH"))
#' @export
find_hd_models <- function(jurisdiction = NULL, species = NULL, available = "DBH") {
  if (!is.null(jurisdiction)) {
    if (!is.character(jurisdiction) || length(jurisdiction) == 0L || anyNA(jurisdiction)) {
      cli::cli_abort("{.arg jurisdiction} must be a character vector without NA.")
    }
    jurisdiction <- standardize_jurisdiction_code(jurisdiction)
  }
  if (!is.null(species)) {
    if (!is.character(species) || length(species) == 0L || anyNA(species)) {
      cli::cli_abort("{.arg species} must be a character vector without NA.")
    }
    species <- standardize_species_code(species, keep_all = FALSE)
  }
  if (!is.character(available) || anyNA(available)) {
    cli::cli_abort("{.arg available} must be a character vector without NA.")
  }

  reg <- hd_model_registry_species()

  if (!is.null(jurisdiction)) {
    keep <- vapply(
      reg$province_scope,
      function(p) any(jurisdiction %in% normalize_province_scope(p)),
      logical(1)
    )
    reg <- reg[keep, ]
  }
  if (!is.null(species)) {
    keep <- vapply(reg$species, function(s) any(species %in% s), logical(1))
    reg <- reg[keep, ]
  }

  reg$missing_predictors <- lapply(reg$required_predictors, setdiff, available)
  reg$usable <- lengths(reg$missing_predictors) == 0L
  reg[order(!reg$usable, lengths(reg$missing_predictors)), ]
}

# internal
# Returns a character vector describing registry / predictor-table
# inconsistencies (empty when consistent). Checked by the test suite.
.hd_registry_problems <- function(
  reg = hd_model_registry(),
  preds = .hd_model_predictors(),
  env = asNamespace("CanadaForestAllometry")
) {
  out <- character()

  dup <- unique(reg$model_id[duplicated(reg$model_id)])
  if (length(dup)) out <- c(out, paste0("Duplicated model_id: ", dup))

  unknown <- setdiff(preds$model_id, reg$model_id)
  if (length(unknown)) out <- c(out, paste0("Predictors for unregistered model_id: ", unknown))

  key <- paste(preds$model_id, preds$arg)
  dup_p <- unique(key[duplicated(key)])
  if (length(dup_p)) out <- c(out, paste0("Duplicated predictor: ", dup_p))

  bad_lvl <- unique(preds$level[!preds$level %in% .hd_predictor_levels])
  if (length(bad_lvl)) out <- c(out, paste0("Invalid predictor level: ", bad_lvl))

  if ("species" %in% preds$arg) {
    out <- c(out, "`species` must not be listed as a predictor.")
  }

  for (i in seq_len(nrow(reg))) {
    id <- reg$model_id[[i]]
    fn <- get0(reg$engine[[i]], envir = env, mode = "function")
    if (is.null(fn)) {
      out <- c(out, paste0(id, ": engine `", reg$engine[[i]], "` not found."))
      next
    }
    fmls <- formals(fn)
    args <- c(reg$required_predictors[[i]], reg$optional_predictors[[i]])

    if (!"DBH" %in% reg$required_predictors[[i]]) {
      out <- c(out, paste0(id, ": `DBH` must be a required predictor."))
    }
    not_formal <- setdiff(c(args, names(reg$fixed_args[[i]])), names(fmls))
    if (length(not_formal)) {
      out <- c(out, paste0(id, ": not arguments of the engine: ", paste(not_formal, collapse = ", ")))
    }
    no_default <- names(fmls)[vapply(fmls, function(x) identical(x, quote(expr = )), logical(1))]
    undeclared <- setdiff(no_default, c(reg$required_predictors[[i]], "species"))
    if (length(undeclared)) {
      out <- c(out, paste0(
        id, ": engine arguments without default not declared as required predictors: ",
        paste(undeclared, collapse = ", ")
      ))
    }
    if (!is.na(reg$region_arg[[i]]) && !reg$region_arg[[i]] %in% args) {
      out <- c(out, paste0(id, ": region_arg `", reg$region_arg[[i]], "` is not a predictor."))
    }
  }

  out
}
