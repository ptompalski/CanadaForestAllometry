testthat::test_that("si_model_registry has expected structure and key models", {
  reg <- CanadaForestAllometry::si_model_registry()

  testthat::expect_s3_class(reg, "tbl_df")
  testthat::expect_true(all(
    c(
      "model_id", "engine", "rank", "subregion_type",
      "subregion_scope", "subregion_required", "age_domain_max",
      "plot_si_values"
    ) %in% names(reg)
  ))
  testthat::expect_true(all(c(
    "nigh2000",
    "nighcourtin1998_si25",
    "lafleche2013_potential",
    "lafleche2013_observed",
    "parresolvissage1998",
    "sharma2015",
    "sharmareid2018",
    "sharma2022",
    "augerward2021",
    "pregent2010",
    "pregent2016",
    "sharmaparton2018a",
    "sharmaparton2018b",
    "sharmaparton2019",
    "sharma2021",
    "thrower1994",
    "huang1994"
  ) %in% reg$model_id))
})

testthat::test_that("SI registry rows keep multi-valued and empty fields nested", {
  args <- list(
    model_id = "example",
    reference = "@Example",
    engine = "si_example",
    age_basis = "total_age",
    requires_species = TRUE,
    province_scope = c("ON", "QC"),
    description = "Example registry row",
    rank = 80
  )
  populated <- list(
    fixed_args = list(variant = "example", base_age = 50),
    plot_si_values = c(5, 10, 15),
    subregion_scope = c("region1", "region2"),
    species_manual = c("PINU.BAN", "PICE.MAR")
  )
  empty <- do.call(.si_registry_row, args)
  full <- do.call(.si_registry_row, c(args, populated))
  combined <- dplyr::bind_rows(empty, full)

  testthat::expect_equal(nrow(combined), 2L)
  for (column in names(populated)) {
    testthat::expect_identical(
      combined[[column]], list(empty[[column]][[1]], populated[[column]]),
      info = column
    )
  }
  testthat::expect_identical(combined$province_scope, rep(list(args$province_scope), 2))
  testthat::expect_identical(combined$fixed_args[[1]], list())
  testthat::expect_null(combined$plot_si_values[[1]])
  testthat::expect_null(combined$species_manual[[1]])
  testthat::expect_identical(combined$subregion_scope[[1]], NA_character_)
})

testthat::test_that("si_model_registry fixed arguments match engine signatures", {
  reg <- CanadaForestAllometry::si_model_registry()
  for (i in seq_len(nrow(reg))) {
    engine <- get(reg$engine[[i]], envir = asNamespace("CanadaForestAllometry"))
    testthat::expect_true(
      all(names(reg$fixed_args[[i]]) %in% names(formals(engine))),
      info = reg$model_id[[i]]
    )
  }
})

testthat::test_that("SI input age categories and required species match engines", {
  reg <- si_model_registry()
  plantation <- reg$model_id %in% c("augerward2021", "pregent2010", "pregent2016")
  testthat::expect_true(all(reg$age_basis[plantation] == "total_age"))
  testthat::expect_identical(
    reg$age_basis[reg$model_id == "lafleche2013_potential"],
    "corrected_age_above_1m"
  )
  testthat::expect_identical(
    reg$age_basis[reg$model_id == "lafleche2013_observed"], "unresolved"
  )
  for (i in seq_len(nrow(reg))) {
    engine <- get(reg$engine[[i]], asNamespace("CanadaForestAllometry"))
    required <- "species" %in% names(formals(engine)) &&
      identical(formals(engine)$species, quote(expr = ))
    testthat::expect_identical(reg$requires_species[[i]], required, info = reg$model_id[[i]])
  }
})

testthat::test_that("plot-only SI arguments do not redefine native reference ages", {
  reg <- si_model_registry()
  for (i in seq_len(nrow(reg))) {
    testthat::expect_identical(names(reg$fixed_arg_roles[[i]]), names(reg$fixed_args[[i]]))
  }
  for (id in c("pregent2010", "pregent2016")) {
    row <- reg[reg$model_id == id, ]
    testthat::expect_identical(row$fixed_args[[1]], list(base_age = 50))
    testthat::expect_identical(row$fixed_arg_roles[[1]], c(base_age = "plot_only"))
    testthat::expect_equal(row$scientific_definitions[[1]]$reference_age, 25)
    testthat::expect_equal(row$scientific_definitions[[1]]$reference_age_basis, "total_age")
  }
  alder <- reg[reg$model_id == "nighcourtin1998_si50", ]
  testthat::expect_identical(alder$fixed_arg_roles[[1]], c(si50 = "variant"))
  testthat::expect_identical(
    alder$scientific_definitions[[1]]$reference_age_role, "converted_index"
  )
})

testthat::test_that("SI reference clocks, datums and quantities are independent", {
  reg <- si_model_registry()
  definition <- function(id) reg$scientific_definitions[[match(id, reg$model_id)]]
  bh <- definition("huang2009_bh_age")
  testthat::expect_identical(bh$input_age_basis, "total_age")
  testthat::expect_identical(bh$reference_age_basis, "breast_height_age")
  testthat::expect_identical(
    definition("huang2009_total_age")$reference_age_basis, "total_age"
  )
  for (id in c("sharma2015", "sharma2021", "sharmaparton2018a",
               "sharmaparton2018b", "sharmaparton2019")) {
    defs <- definition(id)
    testthat::expect_identical(defs$condition, c("total_height=TRUE", "total_height=FALSE"))
    testthat::expect_identical(defs$height_datum, c("total_height", "above_breast_height"))
    testthat::expect_true(all(defs$si_datum == "above_breast_height"))
    testthat::expect_true(all(defs$reference_age == 50))
  }
  testthat::expect_identical(definition("sharma2022")$height_datum, "above_breast_height")
  testthat::expect_identical(definition("huang2026")$productivity_quantity, "height_index")
  climate <- definition("huang2026_climate")
  testthat::expect_identical(climate$productivity_quantity, "inherent_height_index")
  testthat::expect_identical(climate$reference_age_role, "nominal_parameter_reference")
  testthat::expect_match(climate$limitations, "C \\* HI")
  potential <- definition("lafleche2013_potential")
  testthat::expect_identical(potential$productivity_quantity, "ecological_iqs")
  testthat::expect_match(potential$limitations, "not.*arbitrary")
})

testthat::test_that("Payandeh uses total age with disclosed source gaps and species reference ages", {
  reg <- si_model_registry()
  row <- reg[reg$model_id == "payandeh1974", ]
  defs <- row$scientific_definitions[[1]]
  testthat::expect_equal(nrow(defs), 9L)
  testthat::expect_equal(defs$reference_age[match(
    c("ABIE.BAL", "PINU.CON", "PSEU.MEN"), defs$species
  )], c(50, 80, 100))
  testthat::expect_identical(row$age_basis, "total_age")
  testthat::expect_true(all(defs$reference_age_basis == "total_age"))
  testthat::expect_true(all(defs$input_age_basis == "total_age"))
  assumed <- defs$species %in% c("ABIE.BAL", "PICE.GLA")
  testthat::expect_true(all(grepl("assumed", defs$limitations[assumed])))
  testthat::expect_match(defs$limitations[defs$species == "PSEU.MEN"], "1941")
  testthat::expect_match(defs$limitations[defs$species == "PINU.MON"], "oldest dominant")
  testthat::expect_match(defs$limitations[defs$species == "PINU.CON"], "density")
})

testthat::test_that("GI capabilities and discrete age constraints match engine support", {
  reg <- si_model_registry()
  for (id in c("nigh2000_gi", "nigh1998_gi")) {
    row <- reg[reg$model_id == id, ]
    testthat::expect_true(row$supports_predict_si)
    testthat::expect_false(row$supports_predict_height)
    testthat::expect_equal(row$age_domain_max, 50)
    constraint <- row$age_constraints[[1]]
    testthat::expect_identical(constraint$operation, "infer_si_from_gi")
    testthat::expect_identical(constraint$kind, "discrete_support")
    testthat::expect_identical(constraint$engine_action, "error")
    expected <- if (id == "nigh2000_gi") 1:50 else c(5L, 10L, 20L, 30L, 40L, 50L)
    testthat::expect_identical(constraint$values[[1]], expected)
    engine <- get(row$engine[[1]], asNamespace("CanadaForestAllometry"))
    testthat::expect_named(engine(age = expected, gi = 20), "si")
    testthat::expect_error(engine(age = 7.5, gi = 20))
  }
})

testthat::test_that("conventional clock disputes are reconciled without erasing ecological uncertainty", {
  reg <- si_model_registry()
  for (id in c("scottvoorhis1986_bh_age", "scottvoorhis1986_total_age",
               "lundgrendolid1970_monomolecular", "lundgrendolid1970_exponential_monomolecular",
               "parresolvissage1998", "nigh2004", "nighcourtin1998_si50")) {
    row <- reg[reg$model_id == id, ]
    testthat::expect_true(all(row$scientific_definitions[[1]]$status == "verified"), info = id)
    testthat::expect_length(.si_disputed_fields(.si_unresolved_evidence(row)), 0L)
  }
  for (id in "lafleche2013_observed") {
    evidence <- reg$metadata_evidence[[match(id, reg$model_id)]]
    testthat::expect_true(any(evidence$status == "unresolved"), info = id)
    testthat::expect_true(all(nzchar(evidence$locator)), info = id)
  }
  testthat::expect_false(any(grepl("nigh2009_", reg$model_id)))
})

testthat::test_that("Sharma-Reid's above-BH scale is publication-supported", {
  reg <- si_model_registry()
  row <- reg[reg$model_id == "sharmareid2018", ]
  defs <- row$scientific_definitions[[1]]
  testthat::expect_identical(defs$si_datum, "above_breast_height")
  testthat::expect_identical(defs$height_datum, "above_breast_height")
  testthat::expect_identical(defs$reference_age, 50)
  testthat::expect_identical(defs$input_age_basis, "breast_height_age")
  testthat::expect_identical(defs$reference_age_basis, "breast_height_age")
  testthat::expect_identical(defs$status, "verified")
  evidence <- row$metadata_evidence[[1]]
  datum <- evidence[evidence$subject == "si_and_height_datum" &
                      evidence$evidence_kind == "publication", ]
  testthat::expect_equal(nrow(datum), 1L)
  testthat::expect_identical(datum$status, "verified")
  testthat::expect_match(datum$locator, "p. 38.*Figure 2")
  testthat::expect_identical(row$applicability[[1]]$stand_origin, "natural")
})

testthat::test_that("SI applicability is distinct from geographic engine arguments", {
  reg <- si_model_registry()
  for (id in c("pregent2010", "pregent2016", "augerward2021", "sharmaparton2018a")) {
    app <- reg$applicability[[match(id, reg$model_id)]]
    testthat::expect_identical(app$stand_origin, "plantation")
  }
  batho <- reg$applicability[[match("batho2014", reg$model_id)]]
  testthat::expect_match(batho$geographic_definition, "SBS")
  testthat::expect_match(batho$height_sampling_definition, "0.01")
  testthat::expect_identical(batho$stand_origin, "natural_and_plantation")
  testthat::expect_match(
    reg$applicability[[match("nigh1997", reg$model_id)]]$geographic_definition, "Coastal"
  )
  testthat::expect_match(
    reg$applicability[[match("nigh1998", reg$model_id)]]$geographic_definition, "Interior"
  )
  expanded <- si_model_registry_species()
  for (column in c("scientific_definitions", "applicability",
                   "metadata_evidence", "age_constraints", "fixed_arg_roles")) {
    testthat::expect_identical(expanded[[column]], reg[[column]], info = column)
  }
})

testthat::test_that("SI article consumers label age types and height definitions", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("patchwork")
  article <- testthat::test_path("..", "..", "vignettes", "articles", "Site-Index-Models.qmd")
  testthat::skip_if_not(file.exists(article), "Article source not available")
  text <- readLines(article)
  first <- match("si_age_axis_label <- function(model_row) {", text)
  last <- match("reg <- CanadaForestAllometry:::si_model_registry_species() |>", text) - 1L
  env <- new.env(parent = asNamespace("CanadaForestAllometry"))
  eval(parse(text = text[first:last]), envir = env)
  reg <- si_model_registry_species()
  row <- function(id) reg[reg$model_id == id, ]

  testthat::expect_identical(
    env$si_age_axis_label(row("pregent2010")), "Total age (years since planting)"
  )
  testthat::expect_identical(
    env$si_age_axis_label(row("lafleche2013_potential")), "Corrected age above 1 m (years)"
  )
  for (id in "lafleche2013_observed") {
    testthat::expect_identical(env$si_age_axis_label(row(id)), "Age (years)")
  }
  testthat::expect_identical(env$si_age_axis_label(row("payandeh1974")), "Total age (years)")
  testthat::expect_identical(env$si_age_axis_label(row("scottvoorhis1986_bh_age")), "Total age (years)")
  testthat::expect_identical(env$si_age_axis_label(row("scottvoorhis1986_total_age")), "Breast-height age (years)")
  testthat::expect_match(env$si_age_axis_label(row("lundgrendolid1970_monomolecular")), "varies by species")
  for (basis in c("species_dependent", "unresolved")) {
    example <- row("carmean1989")
    example$age_basis <- basis
    expected <- if (basis == "species_dependent") {
      "Age (years; type varies by species)"
    } else {
      "Age (years)"
    }
    testthat::expect_identical(env$si_age_axis_label(example), expected)
  }
  above <- env$plot_si_curves(
    row("sharma2022"), si_values = 15, age_min = 49, age_max = 50
  )
  testthat::expect_identical(above$plot[[1]]$labels$y, "Height above breast height (m)")
  climate <- env$plot_si_curves(
    row("huang2026"), si_values = 15, age_min = 49, age_max = 50
  )
  testthat::expect_identical(climate$plot[[1]]$labels$y, "Dominant/codominant tree height (m)")
  for (id in c("lafleche2013_potential", "lafleche2013_observed")) {
    eco <- env$plot_lafleche_examples(row(id))
    testthat::expect_identical(eco$plot[[1]]$labels$x, env$si_age_axis_label(row(id)))
  }
})

testthat::test_that("si_model_registry includes AugerWard2021 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "augerward2021")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_augerward2021")
  testthat::expect_identical(s$reference[[1]], "@AugerWard2021")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_true(is.na(s$params_key[[1]]))
  testthat::expect_identical(s$age_domain_max[[1]], 100)
  testthat::expect_identical(s$plot_si_values[[1]], c(4, 8, 12, 16))
})

testthat::test_that("si_model_registry includes Pregent2010 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "pregent2010")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_pregent2010")
  testthat::expect_identical(s$reference[[1]], "@Pregent2010")
  testthat::expect_identical(s$species_manual[[1]], "PICE.GLA")
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_identical(s$fixed_args[[1]], list(base_age = 50))
  testthat::expect_true(is.na(s$params_key[[1]]))
  testthat::expect_identical(s$age_domain_max[[1]], 60)
  testthat::expect_identical(s$plot_si_values[[1]], c(4, 8, 12, 16))
})

testthat::test_that("si_model_registry includes Pregent2016 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "pregent2016")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_pregent2016")
  testthat::expect_identical(s$reference[[1]], "@Pregent2016")
  testthat::expect_identical(s$species_manual[[1]], "PICE.ABI")
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_identical(s$fixed_args[[1]], list(base_age = 50))
  testthat::expect_true(is.na(s$params_key[[1]]))
  testthat::expect_identical(s$age_domain_max[[1]], 70)
  testthat::expect_identical(s$plot_si_values[[1]], c(4, 8, 12, 16))
})

testthat::test_that("si_model_registry includes Sharma2022 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharma2022")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharma2022")
  testthat::expect_identical(s$reference[[1]], "@Sharma2022")
  testthat::expect_identical(s$species_manual[[1]], c("PICE.MAR", "POPU.TRE"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes Lafleche2013 potential metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "lafleche2013_potential")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_lafleche2013")
  testthat::expect_identical(s$reference[[1]], "@LaflecheEtAl2013")
  testthat::expect_identical(s$fixed_args[[1]], list(curve_set = "potential"))
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_identical(s$supports_predict_si[[1]], FALSE)
  testthat::expect_identical(s$supports_predict_height[[1]], TRUE)
  testthat::expect_identical(s$subregion_type[[1]], "qc_ecological_subregion")
  testthat::expect_identical(s$subregion_arg[[1]], "ecological_subregion")
  testthat::expect_identical(s$params_key[[1]], "parameters_QC_IQS2013")
})

testthat::test_that("si_model_registry includes Lafleche2013 observed metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "lafleche2013_observed")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_lafleche2013")
  testthat::expect_identical(s$reference[[1]], "@LaflecheEtAl2013")
  testthat::expect_identical(s$fixed_args[[1]], list(curve_set = "observed"))
  testthat::expect_identical(s$province_scope[[1]], "QC")
  testthat::expect_identical(s$supports_predict_si[[1]], FALSE)
  testthat::expect_identical(s$supports_predict_height[[1]], TRUE)
  testthat::expect_identical(s$params_key[[1]], "parameters_QC_IQS2013")
})

testthat::test_that("si_model_registry includes Sharma2021 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharma2021")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharma2021")
  testthat::expect_identical(s$reference[[1]], "@Sharma2021SI")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaReid2018 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmareid2018")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmareid2018")
  testthat::expect_identical(s$reference[[1]], "@SharmaReid2018")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaEtAl2015 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharma2015")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharma2015")
  testthat::expect_identical(s$reference[[1]], "@SharmaEtAl2015")
  testthat::expect_identical(s$species_manual[[1]], c("PINU.BAN", "PICE.MAR"))
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaParton2019 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmaparton2019")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmaparton2019")
  testthat::expect_identical(s$reference[[1]], "@SharmaParton2019")
  testthat::expect_identical(s$species_manual[[1]], "PINU.STR")
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaParton2018a metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmaparton2018a")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmaparton2018a")
  testthat::expect_identical(s$reference[[1]], "@SharmaParton2018a")
  testthat::expect_identical(s$species_manual[[1]], "PICE.GLA")
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes SharmaParton2018b metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  s <- reg |>
    dplyr::filter(.data$model_id == "sharmaparton2018b")

  testthat::expect_equal(nrow(s), 1L)
  testthat::expect_identical(s$engine[[1]], "si_sharmaparton2018b")
  testthat::expect_identical(s$reference[[1]], "@SharmaParton2018b")
  testthat::expect_identical(s$species_manual[[1]], "PINU.RES")
  testthat::expect_identical(s$province_scope[[1]], "ON")
  testthat::expect_true(is.na(s$params_key[[1]]))
})

testthat::test_that("si_model_registry includes ParresolVissage1998 metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  p <- reg |>
    dplyr::filter(.data$model_id == "parresolvissage1998")

  testthat::expect_equal(nrow(p), 1L)
  testthat::expect_identical(p$engine[[1]], "si_parresolvissage1998")
  testthat::expect_identical(p$reference[[1]], "@ParresolVissage1998")
  testthat::expect_identical(p$species_manual[[1]], "PINU.STR")
  testthat::expect_true(is.na(p$params_key[[1]]))
})

testthat::test_that("si_model_registry includes Huang1994 subregion metadata", {
  reg <- CanadaForestAllometry::si_model_registry()

  h <- reg |>
    dplyr::filter(.data$model_id == "huang1994")

  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_identical(h$subregion_type[[1]], "ab_natural_region_group")
  testthat::expect_identical(h$subregion_arg[[1]], "natural_subregion")
  testthat::expect_identical(h$params_key[[1]], "parameters_Huang1994_si")
})

testthat::test_that("si_model_registry includes subregion metadata for BC-specific models", {
  reg <- CanadaForestAllometry::si_model_registry()

  bc_focus <- reg |>
    dplyr::filter(.data$model_id %in% c("nigh2000", "nighcourtin1998_si25", "thrower1994"))

  testthat::expect_true(all(bc_focus$subregion_type == "bec_region"))
  testthat::expect_true(all(lengths(bc_focus$subregion_scope) >= 1L))
  testthat::expect_true(all(!bc_focus$subregion_required))
})

testthat::test_that("si_model_registry marks both NighCourtin1998 variants as supporting height prediction", {
  reg <- CanadaForestAllometry::si_model_registry()

  nc <- reg |>
    dplyr::filter(.data$model_id %in% c("nighcourtin1998_si25", "nighcourtin1998_si50")) |>
    dplyr::arrange(.data$model_id)

  testthat::expect_equal(nrow(nc), 2L)
  testthat::expect_true(all(nc$supports_predict_height))
})

testthat::test_that("si_model_registry_species returns species metadata", {
  out <- CanadaForestAllometry:::si_model_registry_species()
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("species", "n_species", "species_text") %in% names(out)))
  testthat::expect_true(all(out$n_species >= 0))
})

testthat::test_that("si_model_registry_species prefers manual species over params lookup", {
  reg_one <- tibble::tibble(
    model_id = "m1",
    params_key = "dummy_params",
    species_manual = list(c("ABIE.BAL", "PICE.GLA"))
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::si_model_registry_species()
    },
    si_model_registry = function() reg_one,
    get_params_tbl = function(params_key) stop("should not be called"),
    .package = "CanadaForestAllometry"
  )

  testthat::expect_identical(out$species[[1]], c("ABIE.BAL", "PICE.GLA"))
  testthat::expect_identical(out$n_species[[1]], 2L)
})

testthat::test_that("si_model_registry_species applies backward-compatible defaults", {
  minimal_reg <- tibble::tibble(
    model_id = "m1",
    params_key = "dummy_params"
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::si_model_registry_species()
    },
    si_model_registry = function() minimal_reg,
    get_params_tbl = function(params_key) tibble::tibble(Species = c("ABIE.BAL", "PICE.GLA")),
    .package = "CanadaForestAllometry"
  )

  testthat::expect_true("subregion_required" %in% names(out))
  testthat::expect_true("subregion_arg" %in% names(out))
  testthat::expect_true("subregion_type" %in% names(out))
  testthat::expect_true("species_manual" %in% names(out))
  testthat::expect_identical(out$subregion_required[[1]], FALSE)
  testthat::expect_true(is.na(out$subregion_arg[[1]]))
  testthat::expect_identical(out$subregion_type[[1]], "none")
})

testthat::test_that("si_model_registry_species wraps get_params_tbl errors with model context", {
  minimal_reg <- tibble::tibble(
    model_id = "model_x",
    params_key = "missing_x",
    species_manual = list(NULL),
    subregion_required = FALSE,
    subregion_arg = NA_character_,
    subregion_type = "none"
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      {
        CanadaForestAllometry:::si_model_registry_species()
      },
      si_model_registry = function() minimal_reg,
      get_params_tbl = function(params_key) rlang::abort("boom"),
      .package = "CanadaForestAllometry"
    ),
    "Failed to build species list for model_id=model_x",
    fixed = FALSE
  )
})

testthat::test_that("si_model_registry_species returns empty species when no source is provided", {
  reg_none <- tibble::tibble(
    model_id = "m_none",
    params_key = NA_character_,
    species_manual = list(NULL),
    subregion_required = FALSE,
    subregion_arg = NA_character_,
    subregion_type = "none"
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::si_model_registry_species()
    },
    si_model_registry = function() reg_none,
    .package = "CanadaForestAllometry"
  )

  testthat::expect_identical(out$species[[1]], character(0))
  testthat::expect_identical(out$n_species[[1]], 0L)
  testthat::expect_true(is.na(out$species_text[[1]]))
})
