# Operation contracts describe engine inputs, not automatic scientific eligibility.
.si_engine_contracts <- function() {
  tibble::tribble(
    ~engine, ~source_locator,
    "si_buckman2006", "R/si_buckman2006.R:19-130",
    "si_carmean1989", "R/si_carmean1989.R:7-129",
    "si_carmean1996", "R/si_carmean1996.R:12-138",
    "si_carmeanhahn1981", "R/si_carmeanhahn1981.R:12-128",
    "si_cieszewskibella1991", "R/si_cieszewskibella1991.R:10-130",
    "si_kerbowling1991", "R/si_kerbowling1991.R:23-165",
    "si_lundgrendolid1970", "R/si_lundgrendolid1970.R:21-167",
    "si_nigh2000", "R/si_nigh2000.R:10-134",
    "si_nigh2000_gi", "R/si_nigh2000_gi.R:6-102",
    "si_nighcourtin1998", "R/si_nighcourtin1998.R:9-132",
    "si_huang1994", "R/si_huang1994.R:11-153",
    "si_augerward2021", "R/si_augerward2021.R:11-147",
    "si_pregent2010", "R/si_pregent2010.R:11-130",
    "si_pregent2016", "R/si_pregent2016.R:11-130",
    "si_sharma2021", "R/si_sharma2021.R:10-165",
    "si_sharmaparton2018a", "R/si_sharmaparton2018a.R:9-148",
    "si_sharmaparton2018b", "R/si_sharmaparton2018b.R:9-147",
    "si_sharmaparton2019", "R/si_sharmaparton2019.R:9-155",
    "si_sharma2015", "R/si_sharma2015.R:9-153",
    "si_parresolvissage1998", "R/si_parresolvissage1998.R:9-138",
    "si_payandeh1974", "R/si_payandeh1974.R:6-155",
    "si_sharmareid2018", "R/si_sharmareid2018.R:10-136",
    "si_sharma2022", "R/si_sharma2022.R:10-148",
    "si_lafleche2013", "R/si_lafleche2013.R:14-199",
    "si_scottvoorhis1986", "R/si_scottvoorhis1986.R:17-176",
    "si_thrower1994", "R/si_thrower1994.R:11-126",
    "si_hugarcia2009", "R/si_hugarcia2009.R:12-150",
    "si_nigh1997", "R/si_nigh1997.R:12-100",
    "si_nigh2002", "R/si_nigh2002.R:10-112",
    "si_cieszewski1993", "R/si_cieszewski1993.R:17-119",
    "si_goudie1984", "R/si_goudie1984.R:10-124",
    "si_nigh2004", "R/si_nigh2004.R:12-133",
    "si_nigh2009", "R/si_nigh2009.R:10-160",
    "si_carmean2006", "R/si_carmean2006.R:11-145",
    "si_carmean2001", "R/si_carmean2001.R:10-131",
    "si_goelz1992", "R/si_goelz1992.R:9-128",
    "si_nigh2017", "R/si_nigh2017.R:11-116",
    "si_batho2014", "R/si_batho2014.R:11-140",
    "si_nigh1998", "R/si_nigh1998.R:10-113",
    "si_nigh1998_gi", "R/si_nigh1998_gi.R:8-90",
    "si_alemdag1991", "R/si_alemdag1991.R:3-105",
    "si_brisco2002", "R/si_brisco2002.R:10-107",
    "si_chenklinka2000", "R/si_chenklinka2000.R:11-144",
    "si_huang2009", "R/si_huang2009.R:10-166",
    "si_huang2026", "R/si_huang2026.R:10-214"
  )
}

.si_operations <- function(engine) {
  if (engine %in% c("si_nigh2000_gi", "si_nigh1998_gi")) {
    "infer_si_from_gi"
  } else if (engine == "si_lafleche2013") {
    "evaluate_ecological_curve"
  } else {
    c("infer_si_from_height", "predict_height_from_si")
  }
}

.si_native_defaults <- function(engine, env = asNamespace("CanadaForestAllometry")) {
  fn <- get(engine, envir = env)
  defaults <- as.list(formals(fn))
  defaults <- defaults[!vapply(defaults, identical, logical(1), y = quote(expr = ))]
  defaults <- lapply(defaults, eval, envir = environment(fn))
  for (arg in intersect(c("variant", "index_age"), names(defaults))) {
    defaults[[arg]] <- defaults[[arg]][[1]]
  }
  defaults
}

.si_variant_args <- function(row) {
  args <- row$fixed_args[[1]][row$fixed_arg_roles[[1]] == "variant"]
  if (length(args) == 0L) args <- list()
  # These legacy IDs denote the native fit, not every mode of their engines.
  if (row$model_id == "nigh2009") args$variant <- 1
  if (row$model_id == "huang2026") args$variant <- "non_climate"
  args
}

.si_input_row <- function(model_id, operation, arg, role, units, definition,
                          source_locator, condition_id = NA_character_, value = NULL,
                          evidence_kind = "engine") {
  list(
    model_id = model_id, operation = operation, arg = arg, role = role,
    units = units, definition = definition, condition_id = condition_id,
    value = list(value), evidence_kind = evidence_kind, status = "verified",
    source_locator = source_locator
  )
}

.si_model_predictors <- function(reg = si_model_registry(),
                                 constraints = .si_model_constraints(reg)) {
  sources <- .si_engine_contracts()
  options <- tibble::tribble(
    ~arg, ~units, ~definition,
    "base_age", "years", "Configurable native reference age; changes the SI reference, not the input age clock.",
    "total_height", "logical", "TRUE uses total height; FALSE uses height above breast height. SI datum is unchanged.",
    "si50", "logical", "Selects the native SI25 or converted SI50 scale; not a universal base-age identity.",
    "variant", "selector", "Engine fit selector; fixed values define the registered variant.",
    "index_age", "selector", "SI reference-age clock; does not change the total input age clock.",
    "convert_to_total_age", "logical", "FALSE accepts total age directly; TRUE accepts BHA and adds source-estimated YTBH internally. SI50 always uses total age; legacy IDs do not identify the input clock.",
    "natural_subregion", "code", "Alberta natural-region group; All selects provincial parameters, not unrestricted applicability.",
    "bec_zone", "code", "Optional BEC coefficient mode for Nigh2002/2004; ignored with a warning by the fixed Nigh2009 base fit.",
    "ecological_region", "code", "Quebec ecological region; part of an exact joint species-region-subregion-type key.",
    "ecological_type", "code", "Quebec ecological type; part of the exact joint ecological key.",
    "ecological_subregion", "code", "Quebec ecological subregion; required only for keys without a missing-subregion parameter row.",
    "curve_set", "selector", "Potential or observed fixed ecological-curve family; not a supplied SI trajectory.",
    "include_metadata", "logical", "Append matched ecological-key and fitted-curve metadata.",
    "MAP", "mm", "Mean annual precipitation over the species-specific previous-years window; excludes current/pre-initiation years.",
    "MWMT", "degrees C", "Mean warmest-month temperature, not MAT, over the species-specific previous-years window.",
    "MAP_normal", "mm", "Location-specific annual precipitation normal for 1961-1990; fixed in future scenarios.",
    "MWMT_normal", "degrees C", "Location-specific mean warmest-month temperature normal for 1961-1990; fixed in future scenarios."
  )
  rows <- lapply(seq_len(nrow(reg)), function(i) {
    row <- reg[i, ]
    source <- sources$source_locator[match(row$engine, sources$engine)]
    fn <- get(row$engine, envir = asNamespace("CanadaForestAllometry"))
    fixed <- .si_variant_args(row)
    defs <- row$scientific_definitions[[1]]
    age_definition <- paste(unique(defs$age_definition), collapse = " ")
    height_definition <- paste(unique(defs$height_definition), collapse = " ")
    si_definition <- paste(
      "Native", paste(unique(defs$productivity_quantity), collapse = "/"),
      "in metres;", paste(unique(defs$si_datum), collapse = "/"),
      "datum. Reference ages/clocks and conditional/species definitions are in scientific_definitions.",
      "No cross-model SI interchangeability or exact inverse is implied."
    )
    dplyr::bind_rows(lapply(.si_operations(row$engine), function(operation) {
      add <- function(arg, role, units, definition, condition_id = NA_character_,
                      source_locator = source, evidence_kind = "engine") {
        .si_input_row(
          row$model_id, operation, arg, role, units, definition, source_locator,
          condition_id, if (role == "fixed") fixed[[arg]] else NULL, evidence_kind
        )
      }
      required <- switch(operation,
        infer_si_from_height = "height",
        predict_height_from_si = "si",
        infer_si_from_gi = "gi",
        evaluate_ecological_curve = character()
      )
      common <- dplyr::bind_rows(
        add("age", "required", "years", age_definition),
        add("height", if ("height" %in% required) "required" else "forbidden",
            "m", height_definition),
        add("si", if ("si" %in% required) "required" else "forbidden", "m", si_definition),
        add("gi", if ("gi" %in% required) "required" else "forbidden", "cm/year",
            paste(
              "Age-specific mean annual height growth immediately above breast height:",
              "100 * (H_A - 1.3) / (A - A_p), where A_p is the fraction",
              if (row$engine == "si_nigh1998_gi") {
                "of the growing season between BHA 0 and 1 spent below breast height."
              } else {
                "of height growth between BHA 0 and 1 occurring below breast height."
              },
              "Use the source's site-tree height averaging, not an arbitrary annual increment."
            ),
            source_locator = if (row$engine == "si_nigh2000_gi") {
              "Nigh 2000, RR18 p. 5 eq. 1 (top heights from suitable cedar site trees)"
            } else if (row$engine == "si_nigh1998_gi") {
              "Nigh 1998, Forestry Chronicle 74:588-596, eq. 3 (site-tree top height)"
            } else source,
            evidence_kind = if (row$requires_gi) "publication" else "engine")
      )
      if (row$requires_species) {
        common <- dplyr::bind_rows(
          common, add("species", "required", "NFI code",
                      "Species selecting the engine coefficients; discovery coverage also applies to single-species engines.")
        )
      }
      opts <- options[options$arg %in% names(formals(fn)), ]
      extra <- lapply(seq_len(nrow(opts)), function(j) {
        arg <- opts$arg[[j]]
        role <- if (arg %in% names(fixed)) "fixed" else "optional"
        if (arg %in% c("ecological_region", "ecological_type")) role <- "required"
        if (arg %in% c("MAP", "MWMT", "MAP_normal", "MWMT_normal")) {
          role <- if (row$model_id == "huang2026_climate") "required" else "forbidden"
        }
        add(arg, role, opts$units[[j]], opts$definition[[j]])
      })
      conditional <- constraints[
        constraints$model_id == row$model_id & constraints$operation == operation &
          constraints$kind == "conditional_key_requirement",
      ]
      conditional_inputs <- lapply(seq_len(nrow(conditional)), function(j) {
        add(conditional$arg[[j]], "required", "code",
            conditional$definition[[j]], conditional$condition_id[[j]])
      })
      dplyr::bind_rows(common, extra, conditional_inputs)
    }))
  })
  dplyr::bind_rows(rows)
}

.si_constraint_row <- function(model_id, operation, condition_id, arg, kind, values,
                               definition, source_locator, engine_action = "error",
                               selectors = list()) {
  list(
    model_id = model_id, operation = operation, condition_id = condition_id,
    arg = arg, kind = kind, selectors = list(selectors), values = list(values),
    definition = definition, engine_action = engine_action,
    evidence_kind = "engine", status = "verified", source_locator = source_locator
  )
}

.si_model_constraints <- function(reg = si_model_registry()) {
  sources <- .si_engine_contracts()
  age_rules <- .si_age_constraints()
  rows <- lapply(seq_len(nrow(reg)), function(i) {
    row <- reg[i, ]
    source <- sources$source_locator[match(row$engine, sources$engine)]
    dplyr::bind_rows(lapply(.si_operations(row$engine), function(operation) {
      add <- function(condition_id, arg, kind, values, definition,
                      engine_action = "error", selectors = list(),
                      source_locator = source, evidence_kind = "engine") {
        rule <- .si_constraint_row(
          row$model_id, operation, condition_id, arg, kind, values, definition,
          source_locator, engine_action, selectors
        )
        rule$evidence_kind <- evidence_kind
        rule
      }
      out <- list()
      if (row$engine == "si_lundgrendolid1970") {
        out[[length(out) + 1L]] <- add(
          "white-spruce-not-recommended", "species", "published_recommendation",
          list(allowed = FALSE),
          "The authors do not recommend either fitted form for white spruce (PICE.GLA); automatic use is excluded and explicit use warns.",
          "not_enforced", selectors = list(species = "PICE.GLA"),
          source_locator = "Lundgren and Dolid 1970 Tables 2-3, footnote 3, pp. 4,6",
          evidence_kind = "publication"
        )
        if (row$model_id == "lundgrendolid1970_exponential_monomolecular") {
          out[[length(out) + 1L]] <- add(
            "balsam-fir-minimum-recommended-age", "age", "published_recommendation",
            list(lower = 20),
            "The exponential-monomolecular balsam-fir (ABIE.BAL) form is recommended only at BHA >=20; younger automatic use is excluded and explicit use warns.",
            "not_enforced", selectors = list(species = "ABIE.BAL"),
            source_locator = "Lundgren and Dolid 1970 Table 2, p. 4, balsam-fir age recommendation",
            evidence_kind = "publication"
          )
        }
      }
      if (row$requires_gi) {
        rule <- age_rules[age_rules$model_id == row$model_id, ]
        out[[length(out) + 1L]] <- add(
          "gi-age-support", "age", "discrete_support", rule$values[[1]],
          rule$definition, source_locator = rule$source_locator
        )
      }
      if (row$model_id %in% c("nigh2002", "nigh2004")) {
        out[[length(out) + 1L]] <- add(
          "zone-coefficients", "bec_zone", "discrete_support", row$subregion_scope[[1]],
          "When supplied, use one of these BEC coefficient sets; NULL retains the native base/province-wide fit."
        )
      }
      if (row$engine == "si_lafleche2013") {
        pars <- .get_internal_data("parameters_QC_IQS2013")
        family <- if (row$fixed_args[[1]]$curve_set == "potential") "IQSstation" else "IQSobserved"
        pars <- pars[pars$curve_set == family, ]
        keys <- pars |>
          dplyr::transmute(
            species = .data$Species, ecological_region = .data$region_ecologique,
            ecological_subregion = dplyr::na_if(.data$subregion_ecologique, ""),
            ecological_type = .data$type_ecologique, equation_used = .data$equation_used
          )
        out[[length(out) + 1L]] <- add(
          "ecological-key-support", "ecological_region", "discrete_support",
          keys[keys$equation_used != "LOGIST3", setdiff(names(keys), "equation_used")],
          "Exact joint keys, including explicit missing subregions; not a Cartesian product of allowed codes.",
          source_locator = "R/si_lafleche2013.R:264-325; parameters_QC_IQS2013"
        )
        groups <- keys |>
          dplyr::summarise(
            required = all(!is.na(.data$ecological_subregion)),
            subregions = list(unique(.data$ecological_subregion)),
            .by = c("species", "ecological_region", "ecological_type")
          )
        groups <- groups[groups$required, ]
        for (j in seq_len(nrow(groups))) {
          key <- as.list(groups[j, c("species", "ecological_region", "ecological_type")])
          out[[length(out) + 1L]] <- add(
            paste("subregion", paste(unlist(key), collapse = ":"), sep = ":"),
            "ecological_subregion", "conditional_key_requirement", groups$subregions[[j]],
            "This species-region-type key requires an exact ecological_subregion match.",
            selectors = key, source_locator = "R/si_lafleche2013.R:264-325; parameters_QC_IQS2013"
          )
        }
        unsupported <- keys[keys$equation_used == "LOGIST3", ]
        for (j in seq_len(nrow(unsupported))) {
          out[[length(out) + 1L]] <- add(
            paste0("unsupported-logist3-", j), "ecological_type", "unsupported_key_form",
            "LOGIST3", "The retained LOGIST3 parameter row raises an error; no reconciled equation is implemented.",
            selectors = as.list(unsupported[j, setdiff(names(unsupported), "equation_used")]),
            source_locator = "R/si_lafleche2013.R:397-405; parameters_QC_IQS2013"
          )
        }
      }
      if (row$engine == "si_huang2026") {
        out[[length(out) + 1L]] <- add(
          "age-study-range", "age", "calibration_range", c(NA_real_, 180),
          "Upper studied age 180; lower calibration bound is not encoded. Older ages warn without clipping.",
          "warning", source_locator = "R/si_huang2026.R:207-213"
        )
        if (row$model_id == "huang2026_climate") {
          climate_args <- c("MAP", "MWMT", "MAP_normal", "MWMT_normal")
          for (arg in climate_args) {
            out[[length(out) + 1L]] <- add(
              paste0("positive-", arg), arg, "mathematical_domain",
              list(lower = 0, lower_inclusive = FALSE, finite = TRUE),
              "Required positive finite numeric aggregate.",
              source_locator = "R/si_huang2026.R:149-169"
            )
          }
          pars <- .get_internal_data("parameters_Huang2026_climate")
          for (j in seq_len(nrow(pars))) {
            for (arg in climate_args) {
              out[[length(out) + 1L]] <- add(
                paste("climate-observed-range", pars$Species[[j]], arg, sep = ":"),
                arg, "calibration_range",
                c(pars[[paste0(arg, "_min")]][[j]], pars[[paste0(arg, "_max")]][[j]]),
                "Observed species-specific climate range (Table 3.3); warns without clipping.",
                "warning", selectors = list(species = pars$Species[[j]]),
                source_locator = "R/si_huang2026.R:185-196; parameters_Huang2026_climate"
              )
            }
            for (arg in c("MAP", "MWMT")) {
              out[[length(out) + 1L]] <- add(
                paste("climate-window", pars$Species[[j]], arg, sep = ":"),
                arg, "semantic_prerequisite",
                list(
                  window = pars$climate_window[[j]], years = pars$climate_years[[j]],
                  include_current_year = FALSE, include_pre_initiation = FALSE,
                  normal_period = c(1961L, 1990L), normals_fixed = TRUE
                ),
                paste(
                  "Species-specific prior-years window, shortened to previous years since initiation for young trees.",
                  "Normals are location-specific 1961-1990 and fixed in scenarios.",
                  "Aggregate numeric values cannot verify the history; missing years must not be silently omitted."
                ),
                "not_enforced", selectors = list(species = pars$Species[[j]]),
                source_locator = "R/si_huang2026.R:39-48; parameters_Huang2026_climate"
              )
            }
          }
        }
      }
      dplyr::bind_rows(out)
    }))
  })
  dplyr::bind_rows(rows)
}

.si_add_operation_metadata <- function(reg) {
  reg <- .si_complete_scientific_metadata(reg)
  reg$native_defaults <- lapply(reg$engine, .si_native_defaults)
  reg$variant_args <- lapply(seq_len(nrow(reg)), function(i) .si_variant_args(reg[i, ]))
  reg$plot_args <- lapply(seq_len(nrow(reg)), function(i) {
    args <- reg$variant_args[[i]]
    args[names(reg$fixed_args[[i]])] <- reg$fixed_args[[i]]
    args
  })
  constraints <- .si_model_constraints(reg)
  inputs <- .si_model_predictors(reg, constraints)
  reg$operation_inputs <- lapply(reg$model_id, function(id) inputs[inputs$model_id == id, ])
  reg$input_constraints <- lapply(reg$model_id, function(id) constraints[constraints$model_id == id, ])
  reg
}
