# Scientific metadata; empty applicability tables are unknown, not unrestricted.
.si_scientific_definitions <- function() {
  defs <- tibble::tribble(
    ~model_id, ~input_age_basis, ~reference_age, ~reference_age_basis, ~productivity_quantity, ~si_datum, ~height_datum, ~reference_age_role, ~limitations,
    "augerward2021", "plantation_age", NA_real_, "plantation_age", "site_index", "total_height", "total_height", "configurable_engine_default",
    "Years since planting, not germination; no nursery-age conversion is implied.",
    "pregent2010", "plantation_age", NA_real_, "plantation_age", "site_index", "total_height", "total_height", "configurable_engine_default",
    "Native SI25; registry base_age=50 is a plotting override only.",
    "pregent2016", "plantation_age", NA_real_, "plantation_age", "site_index", "total_height", "total_height", "configurable_engine_default",
    "Native SI25; registry base_age=50 is a plotting override only.",
    "sharma2015", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "total_height", "configurable_engine_default",
    "Source reference is 25 years; native engine default is 50. total_height changes height only.",
    "sharma2021", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "total_height", "configurable_engine_default",
    "SI remains above breast height even when height is total height.",
    "sharmaparton2018a", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "total_height", "configurable_engine_default",
    "Source reference is 25 years; native engine default is 50. total_height changes height only.",
    "sharmaparton2018b", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "total_height", "configurable_engine_default",
    "Source reference is 25 years; native engine default is 50. total_height changes height only.",
    "sharmaparton2019", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "total_height", "configurable_engine_default",
    "Source illustrates reference age 25; native engine default is 50. total_height changes height only.",
    "sharma2022", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "above_breast_height", "configurable_engine_default",
    "Source uses height from breast height; engine applies no total-height offset.",
    "sharmareid2018", "breast_height_age", NA_real_, "breast_height_age", "site_index", "above_breast_height", "above_breast_height", "configurable_engine_default",
    "Figure 2 (p. 38) explicitly reports Equation 1 heights from breast height (1.3 m); SI uses the same datum. No total-height offset is applied; do not substitute the antecedent Carmean models' total-height SI.",
    "lafleche2013_potential", "corrected_age_above_1m", NA_real_, "corrected_age_above_1m", "ecological_iqs", "total_height", "total_height", "configurable_engine_default",
    "Evaluates an ecological curve, not an arbitrary SI trajectory or inversion of measured height. Corrected age removes growth suppression, not just years below 1 m.",
    "lafleche2013_observed", "unresolved", NA_real_, "unresolved", "ecological_iqs", "total_height", "total_height", "configurable_engine_default",
    "Engine documents corrected age above 1 m; observed-family source clock is unresolved. Evaluates a fixed ecological curve, not an arbitrary SI trajectory.",
    "nigh2000_gi", "breast_height_age", 50, "breast_height_age", "site_index", "total_height", "unresolved", "fixed_model_scale",
    "Age-specific growth intercept (cm/year) predicts SI only; no height output.",
    "nigh1998_gi", "breast_height_age", 50, "breast_height_age", "site_index", "total_height", "unresolved", "fixed_model_scale",
    "Age-specific growth intercept (cm/year) predicts SI only; no height output or interpolation between coefficient ages.",
    "nighcourtin1998_si25", "breast_height_age", 25, "breast_height_age", "site_index", "total_height", "total_height", "fixed_model_scale",
    "Native SI25 scale.",
    "nighcourtin1998_si50", "breast_height_age", 50, "breast_height_age", "site_index", "total_height", "total_height", "converted_index",
    "Source Equation 7 uses SI50=-0.4063+1.313*SI25; rounded conversion does not guarantee H(50)=SI50.",
    "huang2009_bh_age", "total_age", 50, "breast_height_age", "site_index", "total_height", "total_height", "fixed_model_scale",
    "Input is total age despite the legacy ID suffix; engine converts the SI reference scale.",
    "huang2009_total_age", "total_age", 50, "total_age", "site_index", "total_height", "total_height", "fixed_model_scale",
    "Input and SI reference use total age.",
    "huang2026", "total_age", 50, "total_age", "height_index", "total_height", "total_height", "fixed_model_scale",
    "Realized dominant/codominant tree height index; separately fitted from the climate model.",
    "huang2026_climate", "total_age", 50, "total_age", "inherent_height_index", "total_height", "total_height", "nominal_parameter_reference",
    "H(50)=C * HI, not necessarily HI. Requires MAP, MWMT, MAP_normal and MWMT_normal; aggregate values cannot verify the climate windows.",
    "scottvoorhis1986_bh_age", "total_age", 50, "total_age", "site_index", "total_height", "total_height", "nominal_parameter_reference",
    "Legacy ID retained: convert_to_total_age=FALSE accepts total age directly. SI50 uses total age; fitted curves are not exactly constrained at the reference age.",
    "scottvoorhis1986_total_age", "breast_height_age", 50, "total_age", "site_index", "total_height", "total_height", "nominal_parameter_reference",
    "Legacy ID retained: convert_to_total_age=TRUE accepts BHA and adds source-estimated YTBH internally. SI50 still uses total age; no generic wrapper conversion.",
    "parresolvissage1998", "total_age", NA_real_, "total_age", "site_index", "total_height", "total_height", "configurable_engine_default",
    "Frothingham (1914) antecedent data use total age; no BHA conversion. Native reference age is 50; age and base_age must be at least 10.",
    "nigh2004", "total_age", 50, "breast_height_age", "site_index", "total_height", "total_height", "nominal_parameter_reference",
    "Juvenile total-age model using GI-derived SI50 BHA. Sampled ages are about <=15 pine / <=20 spruce; not a mature trajectory or exact H(50)=SI model. Managed stands do not establish origin; the default origin is an explicit assumption.",
    "brisco2002", "breast_height_age", 50, "breast_height_age", "site_index", "total_height", "total_height", "nominal_parameter_reference",
    "Unconstrained fitted model: H(50) is not exactly SI.",
    "nigh2017", "breast_height_age", 50, "breast_height_age", "site_index", "total_height", "total_height", "nominal_parameter_reference",
    "Approximate SI-to-parameter conversion; no exact base-age or round-trip identity."
  )

  configurable <- defs$reference_age_role == "configurable_engine_default"
  defs$reference_age[configurable] <- vapply(defs$model_id[configurable], function(id) {
    engine <- get(paste0("si_", sub("_potential$|_observed$", "", id)),
                  envir = asNamespace("CanadaForestAllometry"))
    as.numeric(formals(engine)$base_age)
  }, numeric(1))
  defs$species <- NA_character_
  defs$condition <- "native"

  offset <- defs$model_id %in% c(
    "sharma2015", "sharma2021", "sharmaparton2018a",
    "sharmaparton2018b", "sharmaparton2019"
  )
  defs$condition[offset] <- "total_height=TRUE"
  above <- defs[offset, ]
  above$condition <- "total_height=FALSE"
  above$height_datum <- "above_breast_height"

  payandeh <- .get_internal_data("parameters_Payandeh1974") |>
    dplyr::transmute(
      model_id = "payandeh1974",
      input_age_basis = "total_age",
      reference_age = as.numeric(.data$index_age),
      reference_age_basis = "total_age",
      productivity_quantity = "site_index",
      si_datum = "total_height",
      height_datum = "total_height",
      reference_age_role = "fixed_model_scale",
      limitations = paste(
        dplyr::case_when(
          .data$Species %in% c("ABIE.BAL", "PICE.GLA") ~
            "Total age is assumed by an approved implementation decision, not publication-verified; the antecedent full text was not obtained.",
          .data$Species == "PSEU.MEN" ~
            "Total age is corroborated by McArdle's 1949/1961 revisions; the cited 1941 revision was not verified.",
          .data$Species == "PINU.MON" ~
            "Haig (1932) defines total stand age using the oldest dominant tree, not an average dominant/codominant age.",
          .data$Species == "PINU.CON" ~
            "Smithers (1961) uses total age; SI combines site quality and density and is not suitable for stands radically changed by thinning.",
          .data$Species == "PICE.RUB" ~
            "Meyer (1929) uses total age; Payandeh converted the original SI65 to SI50 without changing the age clock.",
          TRUE ~ "Meyer (1937/1938) uses total age of sample dominant/codominant trees."
        ),
        "Species-specific reference age; no generic age conversion.",
        "Separately fitted directions are not exact inverses."
      ),
      species = .data$Species,
      condition = "native"
    )
  lundgren <- .get_internal_data("parameters_LungrenDolid1970") |>
    dplyr::distinct(.data$Species, .data$model) |>
    dplyr::transmute(
      model_id = paste0("lundgrendolid1970_", .data$model),
      input_age_basis = dplyr::if_else(
        .data$Species %in% c("ABIE.BAL", "PICE.GLA"), "breast_height_age", "total_age"
      ),
      reference_age = 50, reference_age_basis = .data$input_age_basis,
      productivity_quantity = "site_index", si_datum = "total_height",
      height_datum = "total_height", reference_age_role = "fixed_model_scale",
      limitations = dplyr::case_when(
        .data$Species == "PICE.GLA" ~
          "Neither fitted form is recommended for white spruce (Tables 2-3); excluded automatically, explicit use warns.",
        .data$Species == "ABIE.BAL" & .data$model == "exponential_monomolecular" ~
          "The exponential-monomolecular balsam-fir form is recommended only at BHA >=20; younger automatic use is excluded and explicit use warns.",
        TRUE ~ "Species-specific source age clock; no generic BHA/total-age conversion."
      ),
      species = .data$Species, condition = "native"
    )
  dplyr::bind_rows(defs, above, payandeh, lundgren, .si_native_scale_definitions()) |>
    dplyr::mutate(definition_id = paste(.data$model_id, .data$condition, .data$species, sep = ":"))
}

.si_native_scale_definitions <- function() {
  tibble::tribble(
    ~model_id, ~input_age_basis, ~reference_age_basis,
    "buckman2006", "total_age", "total_age",
    "carmean1989", "total_age", "total_age",
    "carmean1996", "breast_height_age", "breast_height_age",
    "carmeanhahn1981", "total_age", "total_age",
    "cieszewskibella1991", "breast_height_age", "breast_height_age",
    "kerbowling1991", "breast_height_age", "breast_height_age",
    "nigh2000", "breast_height_age", "breast_height_age",
    "huang1994", "breast_height_age", "breast_height_age",
    "thrower1994", "breast_height_age", "breast_height_age",
    "hugarcia2009", "breast_height_age", "breast_height_age",
    "nigh1997", "breast_height_age", "breast_height_age",
    "nigh2002", "breast_height_age", "breast_height_age",
    "cieszewski1993", "breast_height_age", "breast_height_age",
    "goudie1984", "breast_height_age", "breast_height_age",
    "nigh2009", "breast_height_age", "breast_height_age",
    "carmean2006", "breast_height_age", "breast_height_age",
    "carmean2001", "breast_height_age", "breast_height_age",
    "goelz1992", "breast_height_age", "breast_height_age",
    "batho2014", "breast_height_age", "breast_height_age",
    "nigh1998", "breast_height_age", "breast_height_age",
    "alemdag1991", "breast_height_age", "breast_height_age",
    "chenklinka2000", "breast_height_age", "breast_height_age"
  ) |>
    dplyr::mutate(
      reference_age = 50, productivity_quantity = "site_index",
      si_datum = "total_height", height_datum = "total_height",
      reference_age_role = "fixed_model_scale",
      limitations = paste(
        "Engine-documented native scale, not independent publication verification.",
        "No exact inverse, base-age identity or unrestricted population/sampling applicability is implied."
      ),
      species = NA_character_, condition = "native"
    )
}

.si_model_applicability <- function() {
  tibble::tribble(
    ~model_id, ~stand_origin, ~stand_structure, ~composition_definition, ~geographic_definition, ~height_sampling_definition, ~source_locator,
    "augerward2021", "plantation", "unresolved", "Jack pine or black spruce plantations", "Quebec", "Mean of the 100 tallest planted trees/ha", "R/si_augerward2021.R:3-18",
    "pregent2010", "plantation", "unresolved", "White spruce plantations", "Quebec", "Mean of the 100 tallest trees/ha", "R/si_pregent2010.R:3-19",
    "pregent2016", "plantation", "unresolved", "Norway spruce plantations", "Quebec", "Mean of the 100 tallest trees/ha", "R/si_pregent2016.R:3-19",
    "sharma2015", "plantation", "unresolved", "Jack pine or black spruce plantations", "Northern Ontario", "Dominant/codominant height; mean of three sampled trees/plot", "Sharma et al. 2015, methods, pp. 3-4",
    "sharmaparton2018a", "plantation", "Even-aged", "Monospecific white spruce plantations", "Ontario", "Average of three dominant/codominant trees/site", "Sharma and Parton 2018 white spruce paper, pp. 2-4",
    "sharmaparton2018b", "plantation", "unresolved", "Red pine plantations", "Ontario", "Largest suitable planted tree/plot; three plots/site", "Sharma and Parton 2018 red pine paper, pp. 2-3",
    "sharmaparton2019", "plantation", "unresolved", "White pine plantations", "Ontario", "Sampled dominant/codominant height trajectories", "R/si_sharmaparton2019.R:3-9; source methods, pp. 2-4",
    "sharma2021", "natural", "unresolved", "Mixed jack pine/black spruce stands; not a universal composition threshold", "Ontario", "Sampled dominant/codominant height", "R/si_sharma2021.R:1-10; source methods, p. 2",
    "sharmareid2018", "natural", "Even-aged, fully stocked, undisturbed", "Jack pine or black spruce", "Northern Ontario", "Dominant/codominant height above breast height (1.3 m)", "Sharma and Reid 2018, Forest Science 64:33-40, pp. 34-35; p. 38, Figure 2",
    "sharma2022", "natural", "unresolved", "Mixed black spruce/aspen stands", "Ontario", "Mean dominant/codominant height; three sampled trees/species/site", "Sharma 2022, Forests 13:430, pp. 2-3",
    "nigh1997", "unresolved", "unresolved", "Sitka spruce", "Coastal British Columbia", "Site/top height; suitable largest-diameter trees", "R/si_nigh1997.R:1-4; Nigh 1997, pp. 363, 366",
    "nigh1998", "unresolved", "unresolved", "Western hemlock", "Interior British Columbia", "Source-specific site height; not an unrestricted sampling rule", "R/si_nigh1998.R:1-5",
    "nigh1998_gi", "unresolved", "unresolved", "Western hemlock", "Interior British Columbia", "Age-specific growth intercept, not measured-height inversion", "R/si_nigh1998_gi.R:1-24",
    "nigh2000", "unresolved", "unresolved", "Western redcedar", "Interior British Columbia", "Four largest suitable cedar trees/0.04-ha plot, averaged by BHA", "Nigh 2000 RR18, methods, PDF pp. 10-12",
    "nigh2000_gi", "unresolved", "unresolved", "Western redcedar", "Interior British Columbia", "Age-specific growth intercept from the source cedar population", "Nigh 2000 RR18, methods and eq. 1, PDF pp. 10-12",
    "hugarcia2009", "unresolved", "unresolved", "Interior spruce complex", "British Columbia SBS zone", "Suitable largest-DBH tree/0.01 ha; not a largest-100/ha stand average", "Hu and Garcia 2009, methods, pp. 2, 4",
    "batho2014", "natural_and_plantation", "unresolved", "Lodgepole pine", "British Columbia SBS zone", "Suitable largest tree/0.01 ha; not a largest-100/ha stand average", "Batho and Garcia 2014, methods, p. 2",
    "chenklinka2000", "unresolved", "unresolved", "Subalpine fir, Engelmann spruce and lodgepole pine", "British Columbia ESSF zone", "Sampling not fully encoded", "R/si_chenklinka2000.R:1-6",
    "huang2009_bh_age", "unresolved", "unresolved", "Natural and post-harvest populations; origin not inferred from harvest status", "Alberta", "Mean height of the 100 largest-DBH trees/ha", "R/si_huang2009.R:38-46",
    "huang2009_total_age", "unresolved", "unresolved", "Natural and post-harvest populations; origin not inferred from harvest status", "Alberta", "Mean height of the 100 largest-DBH trees/ha", "R/si_huang2009.R:38-46",
    "huang2026", "unresolved", "unresolved", "Eight species with separately fitted coefficients", "Alberta", "Dominant/codominant tree height, not a standardized largest-DBH stand mean", "R/si_huang2026.R:3-29,60-79",
    "huang2026_climate", "unresolved", "unresolved", "Eight species with separately fitted coefficients", "Alberta", "Dominant/codominant tree height, not a standardized largest-DBH stand mean", "R/si_huang2026.R:3-58,60-79"
  )
}

.si_metadata_evidence <- function() {
  tibble::tribble(
    ~model_id, ~subject, ~evidence_kind, ~locator, ~status, ~note,
    "augerward2021", "input_and_reference_age", "engine", "R/si_augerward2021.R:11-23,33-43", "verified", "Plantation age, native SI25.",
    "pregent2010", "input_and_reference_age", "engine", "R/si_pregent2010.R:11-25,35-42", "verified", "Plantation age, native SI25; plot SI50 is not the engine default.",
    "pregent2016", "input_and_reference_age", "engine", "R/si_pregent2016.R:11-25,35-42", "verified", "Plantation age, native SI25; plot SI50 is not the engine default.",
    "lafleche2013_potential", "input_and_reference_age", "publication", "Lafleche et al. 2013, section 4.6.3, PDF p. 31; corrected-age figures pp. 24-25", "verified", "IQSstation uses corrected age above 1 m, reference 50.",
    "lafleche2013_observed", "input_age_basis", "engine", "R/si_lafleche2013.R:14-24,38-59", "verified", "Engine documents corrected age above 1 m for both curve sets.",
    "lafleche2013_observed", "input_and_reference_age", "publication", "Lafleche et al. 2013, sections 4.6.3 and 4.7", "unresolved", "Potential-family corrected age is not proof of the observed-family source clock.",
    "huang1994", "requires_species", "engine", "R/si_huang1994.R:75-81", "verified", "species has no default.",
    "batho2014", "requires_species", "engine", "R/si_batho2014.R:67", "verified", "Single-species engine nevertheless requires species.",
    "nigh2000_gi", "capabilities_and_age_support", "engine", "R/si_nigh2000_gi.R:30,48-59,109", "verified", "Age and GI return SI only; integer ages 1 through 50.",
    "nigh1998_gi", "capabilities_and_age_support", "engine", "R/si_nigh1998_gi.R:19-24,58-72", "verified", "Age and GI return SI only; six discrete coefficient ages.",
    "sharma2015", "si_and_height_datum", "engine", "R/si_sharma2015.R:36-53,125,205-210", "verified", "SI is above BH; total_height affects height only. Source reference 25, engine default 50.",
    "sharma2015", "source_reference_age", "publication", "Sharma et al. 2015, Reference Age for Site Index, p. 4", "verified", "Source selects 25 years BHA; this does not change the native engine default.",
    "sharma2021", "si_and_height_datum", "engine", "R/si_sharma2021.R:35-52,137,217-222", "verified", "SI is above BH; total_height affects height only.",
    "sharmaparton2018a", "si_and_height_datum", "publication", "Sharma and Parton 2018 white spruce paper, pp. 2-4; R/si_sharmaparton2018a.R:37-52,178-183", "verified", "Source uses above-BH height and SI25; engine defaults to total height and SI50.",
    "sharmaparton2018b", "si_and_height_datum", "engine", "R/si_sharmaparton2018b.R:37-52,177-182", "verified", "SI is above BH; total_height affects height only. Source reference 25, engine default 50.",
    "sharmaparton2018b", "source_reference_age", "publication", "Sharma and Parton 2018 red pine paper, methods pp. 2-3; Figure 4", "verified", "Source SI is mean height of three sampled dominant/codominant trees/site at 25 years BHA.",
    "sharmaparton2019", "si_and_height_datum", "engine", "R/si_sharmaparton2019.R:20-24,37-52,185-190", "verified", "SI is above BH; total_height affects height only. Source illustrates 25, engine default 50.",
    "sharmaparton2019", "source_reference_age", "publication", "Sharma and Parton 2019, methods pp. 2-4; Figure 6", "verified", "Source averages three sampled dominant/codominant trees/site at BHA25 and illustrates SI25 curves.",
    "sharma2022", "si_and_height_datum", "publication", "Sharma 2022, Forests 13:430, methods p. 3; SI analysis p. 5", "verified", "Height and age from breast height used for analyses; engine applies no offset.",
    "sharmareid2018", "si_and_height_datum", "engine", "R/si_sharmareid2018.R, height/SI definitions and Equation 1 calls", "verified", "Height and SI are above BH; no offset is applied.",
    "sharmareid2018", "si_and_height_datum", "publication", "Sharma and Reid 2018, Forest Science 64:33-40, p. 37; p. 38, Figure 2 (PDF p. 6)", "verified", "Figure 2 explicitly states that all stand height values are from breast height and identifies Equation 1 for both species. Heights at BHA50 define SI on the same above-BH datum.",
    "sharmareid2018", "input_and_reference_age", "publication", "Sharma and Reid 2018, Forest Science 64:33-40, pp. 34, 37", "verified", "Input age is breast-height age; the recommended SI reference is BHA50, the native engine default.",
    "payandeh1974", "reference_age", "publication", "Payandeh 1974 p. 194; data-raw/Payandeh_1974_parameters.csv:1-10", "verified", "Species references 50/80/100; compiled index_age is authoritative.",
    "payandeh1974", "input_and_reference_age_basis", "publication", "Haig 1932 pp. 2,7; Smithers 1961 pp. 54,143; Meyer 1929 p. 5, 1937 pp. 7-8, 1938 pp. 7-8; Payandeh 1974 p. 194", "verified", "Total age established for PINU.MON, PINU.CON, PICE.RUB, PICE.SIT, TSUG.HET and PINU.PON; Payandeh converts red spruce SI65 to SI50, not to BHA.",
    "payandeh1974", "age_basis_assumption", "assumption", "si_improvements_plan.md, Payandeh total-age implementation decision", "unresolved", "Total input and reference age are assumed for ABIE.BAL and PICE.GLA by an approved implementation decision; not publication-verified. This accepted contract does not establish a generic BHA conversion.",
    "payandeh1974", "antecedent_edition", "publication", "McArdle et al., 1949 revision pp. 10-11; 1961 slight revision pp. 10-11", "secondary_only", "PSEU.MEN uses total age and SI100 in later editions; the exact 1941 revision cited by Payandeh was not verified. The implementation adopts total age with this edition caveat.",
    "parresolvissage1998", "input_and_reference_age_basis", "publication", "Parresol and Vissage 1998 RP-SRS-10 p. 1; Frothingham 1914 USDA Bulletin 13 p. 19, Tables 3-5 pp. 21-22", "verified", "Antecedent ages include annual growth from germination; input and SI-reference clocks are total age.",
    "scottvoorhis1986_bh_age", "input_and_reference_age_basis", "publication", "Scott and Voorhis 1986 pp. 145-146, Equation 4 and Table 2; appendix pp. 147-148", "verified", "FALSE accepts total input age directly; SI50 uses total age. Legacy ID and selector unchanged.",
    "scottvoorhis1986_total_age", "input_and_reference_age_basis", "publication", "Scott and Voorhis 1986 pp. 145-146, Equation 4 and Table 2; appendix pp. 147-148", "verified", "TRUE accepts BHA and adds source YTBH internally; SI50 uses total age. Legacy ID and selector unchanged.",
    "lundgrendolid1970_exponential_monomolecular", "species_age_basis_and_recommendation", "publication", "Lundgren and Dolid 1970 Table 1 pp. 3-4, Tables 2-3 footnotes pp. 4,6; Gevorkiantz species notes and Cooley paper-birch TN541 reverse-side age definitions", "verified", "Fir/white spruce use BHA; other nine species use total age. Both forms are not recommended for white spruce; exponential balsam fir requires age >=20.",
    "lundgrendolid1970_monomolecular", "species_age_basis_and_recommendation", "publication", "Lundgren and Dolid 1970 Table 1 pp. 3-4, Tables 2-3 footnotes pp. 4,6; Gevorkiantz species notes and Cooley paper-birch TN541 reverse-side age definitions", "verified", "Fir/white spruce use BHA; other nine species use total age. Neither fitted form is recommended for white spruce.",
    "nigh2004", "reference_age", "publication", "Nigh 2004 RR25 pp. 2-3,17-18; Nigh 1997 EN11 p. 2 eq. 2; Nigh 2004 TR013 pp. 2-3; Nigh 1999 EN37 p. 2", "verified", "Juvenile total-age observations; GI lineage establishes total-height SI50 at breast-height age. No mature-age conditioning identity.",
    "nighcourtin1998_si50", "converted_index", "engine", "R/si_nighcourtin1998.R:27-31", "verified", "Affine SI25/SI50 conversion, not an exact H(50) conditioning guarantee.",
    "nighcourtin1998_si50", "conversion_provenance", "publication", "Nigh and Courtin 1998 p. 66 (PDF p. 8), Equation 7; pp. 61-62", "verified", "Source explicitly publishes SI50=-0.4063+1.313*SI25 on the total-height/BHA scale; retain rounded conversion, not exact H(50) conditioning.",
    "huang2009_bh_age", "input_and_reference_age", "engine", "R/si_huang2009.R:10-23,38-46", "verified", "Total input age; breast-height reference age.",
    "huang2009_total_age", "input_and_reference_age", "engine", "R/si_huang2009.R:10-23,38-46", "verified", "Total input and reference age.",
    "huang2026", "productivity_quantity", "engine", "R/si_huang2026.R:10-29,60-79", "verified", "Realized tree HI at 50 total years; climate arguments forbidden.",
    "huang2026_climate", "productivity_quantity", "engine", "R/si_huang2026.R:31-58,68-79,149-158", "verified", "Inherent HI; H(50)=C*HI. All four climate aggregates required, not conventional SI fallback.",
    "huang2026_climate", "climate_aggregation", "engine", "R/si_huang2026.R:39-48", "verified", "Previous 20 years: aspen, poplar, white spruce, fir; 10: black spruce; all previous years since initiation: lodgepole pine, Douglas-fir, jack pine. Exclude current and pre-initiation years; normals are location-specific 1961-1990. Caller aggregation cannot be verified numerically.",
    "brisco2002", "reference_conditioning", "test", "tests/testthat/test-si_brisco2002.R:45-70", "verified", "Nominal reference, not exact H(50)=SI.",
    "nigh2017", "reference_conditioning", "engine", "R/si_nigh2017.R:11-30; tests/testthat/test-si_nigh2017.R:48-70", "verified", "Approximate SI-to-parameter conversion."
  )
}

.si_age_constraints <- function() {
  tibble::tibble(
    model_id = c("nigh2000_gi", "nigh1998_gi"),
    operation = "infer_si_from_gi",
    arg = "age",
    kind = "discrete_support",
    values = list(1:50, c(5L, 10L, 20L, 30L, 40L, 50L)),
    engine_action = "error",
    definition = c(
      "Integer breast-height ages 1 through 50; no interpolation.",
      "Only the six tabulated breast-height ages; no interpolation."
    ),
    source_locator = c("R/si_nigh2000_gi.R:48-59", "R/si_nigh1998_gi.R:58-72")
  )
}

.si_add_scientific_metadata <- function(reg) {
  definitions <- .si_scientific_definitions()
  applicability <- .si_model_applicability()
  evidence <- .si_metadata_evidence()
  constraints <- .si_age_constraints()
  reg$fixed_arg_roles <- lapply(seq_len(nrow(reg)), function(i) {
    args <- reg$fixed_args[[i]]
    roles <- stats::setNames(rep("variant", length(args)), names(args))
    if (reg$model_id[[i]] %in% c("pregent2010", "pregent2016")) {
      roles["base_age"] <- "plot_only"
    }
    roles
  })
  reg$scientific_definitions <- lapply(reg$model_id, function(id) {
    definitions[definitions$model_id == id, ]
  })
  reg$applicability <- lapply(reg$model_id, function(id) {
    applicability[applicability$model_id == id, ]
  })
  reg$metadata_evidence <- lapply(reg$model_id, function(id) {
    evidence[evidence$model_id == id, ]
  })
  for (i in seq_len(nrow(reg))) {
    app <- reg$applicability[[i]]
    assumed <- nrow(app) == 0L || any(app$stand_origin == "unresolved")
    if (!assumed) next
    if (nrow(app) == 0L) {
      app <- tibble::tibble(
        model_id = reg$model_id[[i]], stand_origin = "natural",
        stand_structure = "unresolved", composition_definition = "unresolved",
        geographic_definition = "unresolved", height_sampling_definition = "unresolved",
        source_locator = "Approved non-plantation natural-origin default; source details not fully encoded"
      )
    }
    app$stand_origin[app$stand_origin == "unresolved"] <- "natural"
    reg$applicability[[i]] <- app
    reg$metadata_evidence[[i]] <- dplyr::bind_rows(
      reg$metadata_evidence[[i]],
      tibble::tibble(
        model_id = reg$model_id[[i]], subject = "stand_origin_assumption",
        evidence_kind = "assumption",
        locator = "si_improvements_plan.md, approved non-plantation natural-origin policy",
        status = "unresolved",
        note = paste(
          "Stand origin is assumed natural under the approved non-plantation default,",
          "not independently publication-verified. No broader sampling, geographic",
          "or stand-structure applicability is implied."
        )
      )
    )
  }
  reg$age_constraints <- lapply(reg$model_id, function(id) {
    constraints[constraints$model_id == id, ]
  })
  reg
}

.si_complete_scientific_metadata <- function(reg) {
  sources <- .si_engine_contracts()
  clocks <- c(
    total_age = "Total age from germination/seed, not breast-height age.",
    breast_height_age = "Breast-height age under the engine's documented convention.",
    plantation_age = "Years since planting; no nursery-age or germination conversion is implied.",
    corrected_age_above_1m = "Source-corrected age above 1 m, removing suppression; not simply elapsed years above 1 m.",
    species_dependent = "Species-specific age clock; consult the species definition.",
    unresolved = "Source age clock is unresolved; do not infer scientific compatibility."
  )
  for (i in seq_len(nrow(reg))) {
    source <- sources$source_locator[match(reg$engine[[i]], sources$engine)]
    defs <- reg$scientific_definitions[[i]]
    if (nrow(defs) == 0L) {
      cli::cli_abort("No scientific definition declared for SI model {.val {reg$model_id[[i]]}}.")
    }
    defs$age_definition <- unname(clocks[defs$input_age_basis])
    defs$height_definition <- paste(
      "Height datum:", defs$height_datum,
      "; engine-specific measurement/sampling convention, not a universal top-height definition.",
      "Condition:", defs$condition, ". Consult applicability and unresolved evidence."
    )
    defs$source_locator <- source
    defs$evidence_kind <- "engine"
    unresolved <- defs$input_age_basis == "unresolved" |
      defs$reference_age_basis == "unresolved" |
      defs$si_datum == "unresolved" | defs$height_datum == "unresolved"
    defs$status <- ifelse(unresolved, "unresolved", "verified")
    reg$scientific_definitions[[i]] <- defs
    reg$metadata_evidence[[i]] <- dplyr::bind_rows(
      reg$metadata_evidence[[i]],
      tibble::tibble(
        model_id = reg$model_id[[i]], subject = "operation_input_contract",
        evidence_kind = "engine", locator = source, status = "verified",
        note = paste(
          "Current engine signature, operation and native scale only.",
          "Does not resolve conflicting publication claims or establish scientific eligibility."
        )
      )
    )
  }
  reg
}
