.si_registry_problems <- function(
  reg = si_model_registry(),
  inputs = dplyr::bind_rows(reg$operation_inputs),
  constraints = dplyr::bind_rows(reg$input_constraints),
  env = asNamespace("CanadaForestAllometry")
) {
  out <- character()
  report <- function(text) out <<- c(out, text)
  missing <- setdiff(c(
    "model_id", "engine", "fixed_args", "fixed_arg_roles", "native_defaults",
    "variant_args", "plot_args", "requires_species", "requires_gi",
    "supports_predict_si", "supports_predict_height", "scientific_definitions",
    "metadata_evidence", "age_basis", "params_key", "species_manual"
  ), names(reg))
  if (length(missing)) {
    return(paste0("Registry missing columns: ", paste(missing, collapse = ", ")))
  }
  missing <- setdiff(c(
    "model_id", "operation", "arg", "role", "units", "definition", "condition_id",
    "value", "evidence_kind", "status", "source_locator"
  ), names(inputs))
  if (length(missing)) {
    return(paste0("Predictors missing columns: ", paste(missing, collapse = ", ")))
  }
  missing <- setdiff(c(
    "model_id", "operation", "condition_id", "arg", "kind", "selectors", "values",
    "definition", "engine_action", "evidence_kind", "status", "source_locator"
  ), names(constraints))
  if (length(missing)) {
    return(paste0("Constraints missing columns: ", paste(missing, collapse = ", ")))
  }
  nonempty <- function(x) !is.na(x) & nzchar(trimws(x))
  dup <- unique(reg$model_id[duplicated(reg$model_id)])
  if (length(dup)) report(paste0("Duplicated model_id: ", dup))
  if (any(!nonempty(reg$model_id))) return(c(out, "Missing model_id."))
  for (table in list(inputs, constraints)) {
    unknown <- setdiff(table$model_id, reg$model_id)
    if (length(unknown)) report(paste0("Metadata for unregistered model_id: ", unknown))
    if (any(!nonempty(table$definition) | !nonempty(table$source_locator) |
            !table$evidence_kind %in% c("engine", "publication", "test", "extraction_spec",
                                       "reference_implementation", "plot_configuration") |
            !table$status %in% c("verified", "secondary_only", "unresolved", "contradicted"))) {
      report("Missing or invalid metadata evidence/definition.")
    }
  }
  if (any(!nonempty(inputs$operation) | !nonempty(inputs$arg) | is.na(inputs$role)) ||
      any(!nonempty(constraints$operation) | !nonempty(constraints$arg) |
            is.na(constraints$kind) | is.na(constraints$engine_action))) {
    return(c(out, "Missing operation, argument, role or constraint kind/action."))
  }
  key <- paste(inputs$model_id, inputs$operation, inputs$arg, inputs$condition_id)
  if (anyDuplicated(key)) report(paste0("Duplicated predictor: ", unique(key[duplicated(key)])))
  key <- paste(constraints$model_id, constraints$operation,
               constraints$condition_id, constraints$arg)
  if (anyDuplicated(key)) report(paste0("Duplicated constraint: ", unique(key[duplicated(key)])))
  if (any(!inputs$role %in% c("required", "optional", "fixed", "forbidden"))) {
    report("Invalid predictor role.")
  }
  if (any(!nonempty(inputs$units))) report("Missing predictor units.")
  kinds <- c(
    "mathematical_domain", "discrete_support", "calibration_range",
    "published_recommendation", "conditional_key_requirement",
    "unsupported_key_form", "semantic_prerequisite"
  )
  if (any(!constraints$kind %in% kinds)) report("Invalid constraint kind.")
  if (any(!constraints$engine_action %in% c("error", "warning", "not_enforced", "not_applicable"))) {
    report("Invalid constraint engine_action.")
  }
  if (any(!nonempty(constraints$condition_id))) report("Missing constraint condition_id.")

  for (i in seq_len(nrow(reg))) {
    row <- reg[i, ]
    id <- row$model_id[[1]]
    fn <- get0(row$engine[[1]], envir = env, mode = "function")
    if (is.null(fn)) {
      report(paste0(id, ": engine not found."))
      next
    }
    fmls <- formals(fn)
    declared <- inputs[inputs$model_id == id, ]
    rules <- constraints[constraints$model_id == id, ]
    operations <- .si_operations(row$engine)
    if (!setequal(unique(declared$operation), operations) ||
        any(!rules$operation %in% operations)) {
      report(paste0(id, ": missing or incompatible operations."))
    }
    expected_gi <- identical(operations, "infer_si_from_gi")
    expected_eco <- identical(operations, "evaluate_ecological_curve")
    if (!identical(row$requires_gi[[1]], expected_gi) ||
        !identical(row$supports_predict_si[[1]], !expected_eco) ||
        !identical(row$supports_predict_height[[1]], !expected_gi)) {
      report(paste0(id, ": incompatible legacy capabilities."))
    }
    no_default <- names(fmls)[vapply(fmls, identical, logical(1), y = quote(expr = ))]
    if (!identical(row$requires_species[[1]], "species" %in% no_default)) {
      report(paste0(id, ": requires_species conflicts with engine signature."))
    }
    raw <- row$fixed_args[[1]]
    roles <- row$fixed_arg_roles[[1]]
    if ((length(raw) && (is.null(names(raw)) || any(!nonempty(names(raw))))) ||
        anyDuplicated(names(raw)) || !identical(names(raw), names(roles)) ||
        any(!roles %in% c("variant", "plot_only")) ||
        (id %in% c("pregent2010", "pregent2016") &&
         !identical(roles, c(base_age = "plot_only")))) {
      report(paste0(id, ": invalid fixed-argument roles."))
    }
    fixed <- row$variant_args[[1]]
    plot_args <- fixed
    plot_args[names(raw)] <- raw
    if (!identical(fixed, .si_variant_args(row)) ||
        (length(fixed) && (is.null(names(fixed)) || any(!nonempty(names(fixed))))) ||
        anyDuplicated(names(fixed)) ||
        !identical(row$plot_args[[1]], plot_args)) {
      report(paste0(id, ": fixed/variant/plot argument conflict."))
    }
    if (!identical(row$native_defaults[[1]], .si_native_defaults(row$engine, env))) {
      report(paste0(id, ": native defaults differ from engine formals."))
    }
    allowed_forbidden <- declared$role == "forbidden" &
      declared$arg %in% c("height", "si", "gi")
    illegal <- setdiff(c(declared$arg[!allowed_forbidden], names(raw), names(fixed),
                         rules$arg), names(fmls))
    if (length(illegal)) {
      report(paste0(id, ": not arguments of the engine: ", paste(illegal, collapse = ", ")))
    }
    for (operation in operations) {
      op <- declared[declared$operation == operation, ]
      base <- op[is.na(op$condition_id), ]
      op_rules <- rules[rules$operation == operation, ]
      undeclared <- setdiff(names(fmls), base$arg)
      if (length(undeclared)) {
        report(paste0(id, " / ", operation, ": engine arguments not declared: ",
                      paste(undeclared, collapse = ", ")))
      }
      required <- c("age", switch(operation,
        infer_si_from_height = "height",
        predict_height_from_si = "si",
        infer_si_from_gi = "gi",
        evaluate_ecological_curve = c("ecological_region", "ecological_type")
      ))
      if (row$requires_species) required <- c(required, "species")
      for (arg in required) {
        if (!any(base$arg == arg & base$role %in% c("required", "fixed"))) {
          report(paste0(id, " / ", operation, ": missing required input ", arg, "."))
        }
      }
      unsatisfied <- setdiff(no_default, base$arg[base$role %in% c("required", "fixed")])
      if (length(unsatisfied)) {
        report(paste0(id, " / ", operation, ": engine arguments without default not required: ",
                      paste(unsatisfied, collapse = ", ")))
      }
      for (arg in c("height", "si", "gi")) {
        role <- if (arg %in% required) "required" else "forbidden"
        if (!identical(base$role[base$arg == arg], role)) {
          report(paste0(id, " / ", operation, ": height/SI exclusivity or GI contract conflicts."))
        }
      }
      for (arg in names(fixed)) {
        entry <- base[base$arg == arg, ]
        if (nrow(entry) != 1L || entry$role != "fixed" ||
            !identical(entry$value[[1]], fixed[[arg]])) {
          report(paste0(id, " / ", operation, ": fixed input conflicts for ", arg, "."))
        }
      }
      unexpected_fixed <- setdiff(base$arg[base$role == "fixed"], names(fixed))
      if (length(unexpected_fixed)) report(paste0(id, ": undeclared fixed inputs."))
      if (any(vapply(seq_len(nrow(op)), function(j) {
        op$role[[j]] != "fixed" && !is.null(op$value[[j]])
      }, logical(1)))) {
        report(paste0(id, ": non-fixed input has a fixed value."))
      }
      for (j in which(!is.na(op$condition_id))) {
        rule <- op_rules[op_rules$condition_id == op$condition_id[[j]] &
                           op_rules$arg == op$arg[[j]] &
                           op_rules$kind == "conditional_key_requirement", ]
        if (nrow(rule) != 1L || op$role[[j]] != "required") {
          report(paste0(id, ": Unknown condition or invalid conditional input."))
        }
      }
      for (j in seq_len(nrow(op_rules))) {
        rule <- op_rules[j, ]
        selectors <- rule$selectors[[1]]
        selector_args <- intersect(names(fmls), c(
          "species", "variant", "si50", "index_age", "curve_set", "total_height",
          "convert_to_total_age", "bec_zone", "natural_subregion",
          "ecological_region", "ecological_type", "ecological_subregion"
        ))
        if (!is.list(selectors) || (length(selectors) &&
            (is.null(names(selectors)) || anyDuplicated(names(selectors)) ||
             any(!names(selectors) %in% selector_args) ||
             any(lengths(selectors) != 1L)))) {
          report(paste0(id, ": invalid conditional selector."))
        }
        for (arg in intersect(names(selectors), names(fixed))) {
          if (!fixed[[arg]] %in% selectors[[arg]]) {
            report(paste0(id, ": condition contradicts fixed selector ", arg, "."))
          }
        }
        values <- rule$values[[1]]
        if (rule$kind == "discrete_support" &&
            (length(values) == 0L || (is.data.frame(values) && nrow(values) == 0L) ||
             anyNA(values) || anyDuplicated(values) ||
             (is.numeric(values) && any(!is.finite(values))))) {
          # Missing subregion is an exact ecological key, not an unknown wildcard.
          ecological_keys <- is.data.frame(values) &&
            identical(names(values), c("species", "ecological_region",
                                       "ecological_subregion", "ecological_type")) &&
            nrow(values) > 0L && !anyDuplicated(values) &&
            !anyNA(values[setdiff(names(values), "ecological_subregion")])
          if (!ecological_keys) report(paste0(id, ": invalid discrete support."))
        }
        if (rule$kind == "conditional_key_requirement" &&
            (length(selectors) == 0L || length(values) == 0L || anyNA(values) ||
             !any(op$condition_id == rule$condition_id & op$arg == rule$arg &
                    op$role == "required", na.rm = TRUE))) {
          report(paste0(id, ": incomplete conditional key requirement."))
        }
        if (id == "huang2026_climate" && rule$kind == "mathematical_domain" &&
            rule$arg %in% c("MAP", "MWMT", "MAP_normal", "MWMT_normal") &&
            (!is.list(values) || !isTRUE(values$finite) ||
             !identical(values$lower, 0) || !identical(values$lower_inclusive, FALSE))) {
          report(paste0(id, ": invalid positive-finite mathematical domain."))
        }
      }
      if (expected_gi) {
        support <- op_rules[op_rules$kind == "discrete_support" & op_rules$arg == "age", ]
        expected <- .si_age_constraints()
        expected <- expected$values[[match(id, expected$model_id)]]
        if (nrow(support) != 1L || !identical(support$values[[1]], expected)) {
          report(paste0(id, ": missing or incorrect GI discrete support."))
        }
      }
      if (row$engine == "si_huang2026") {
        climate_args <- c("MAP", "MWMT", "MAP_normal", "MWMT_normal")
        role <- if (id == "huang2026_climate") "required" else "forbidden"
        if (!setequal(base$arg[base$role == role & base$arg %in% climate_args], climate_args)) {
          report(paste0(id, ": incompatible climate requirements."))
        }
        if (id == "huang2026_climate") {
          domains <- op_rules[op_rules$kind == "mathematical_domain", ]
          windows <- op_rules[op_rules$kind == "semantic_prerequisite", ]
          pars <- .get_internal_data("parameters_Huang2026_climate")
          if (!setequal(domains$arg, climate_args)) {
            report(paste0(id, ": missing climate domain metadata."))
          }
          for (species in pars$Species) {
            window <- windows[vapply(windows$selectors, function(x) {
              identical(x$species, species)
            }, logical(1)), ]
            p <- pars[pars$Species == species, ]
            expected <- list(
              window = p$climate_window[[1]], years = p$climate_years[[1]],
              include_current_year = FALSE, include_pre_initiation = FALSE,
              normal_period = c(1961L, 1990L), normals_fixed = TRUE
            )
            if (!setequal(window$arg, c("MAP", "MWMT")) ||
                !all(vapply(window$values, identical, logical(1), y = expected)) ||
                any(window$engine_action != "not_enforced")) {
              report(paste0(id, ": missing or incorrect species climate aggregation metadata."))
            }
          }
        }
      }
      if (expected_eco) {
        key_support <- op_rules[op_rules$kind == "discrete_support", ]
        if (nrow(key_support) != 1L || !is.data.frame(key_support$values[[1]]) ||
            !identical(names(key_support$values[[1]]),
                       c("species", "ecological_region", "ecological_subregion", "ecological_type"))) {
          report(paste0(id, ": missing exact ecological key support."))
        }
        if (row$fixed_args[[1]]$curve_set == "potential" &&
            !any(op_rules$kind == "unsupported_key_form" & op_rules$engine_action == "error")) {
          report(paste0(id, ": missing unsupported ecological form metadata."))
        }
      }
    }
    defs <- row$scientific_definitions[[1]]
    fields <- c(
      "model_id", "definition_id", "species", "condition", "input_age_basis",
      "reference_age", "reference_age_basis", "productivity_quantity",
      "si_datum", "height_datum", "reference_age_role", "limitations",
      "age_definition", "height_definition", "source_locator", "evidence_kind", "status"
    )
    if (nrow(defs) == 0L || !all(fields %in% names(defs))) {
      report(paste0(id, ": missing scientific definitions."))
      next
    }
    if (!is.numeric(defs$reference_age) ||
        any(!nonempty(defs$model_id) | !nonempty(defs$definition_id) |
              !nonempty(defs$condition))) {
      report(paste0(id, ": invalid scientific identifiers/reference-age type."))
      next
    }
    if (any(defs$model_id != id) || anyDuplicated(defs$definition_id) ||
        anyDuplicated(paste(defs$condition, defs$species))) {
      report(paste0(id, ": ambiguous scientific definitions."))
    }
    species <- row$species_manual[[1]]
    if (length(species) == 0L && !is.na(row$params_key)) {
      species <- extract_species_from_params(get_params_tbl(row$params_key[[1]]))
    }
    for (condition in unique(defs$condition)) {
      sp <- defs$species[defs$condition == condition]
      if (anyNA(sp) && length(sp) > 1L) report(paste0(id, ": ambiguous species definitions."))
      if ((all(!is.na(sp)) || row$age_basis %in% "species_dependent") &&
          (!setequal(species, sp) || anyNA(sp))) {
        report(paste0(id, ": incomplete species definition coverage."))
      }
    }
    if (any(!nonempty(defs$age_definition) | !nonempty(defs$height_definition) |
            !nonempty(defs$source_locator) | !nonempty(defs$limitations)) ||
        any(!defs$status %in% c("verified", "secondary_only", "unresolved", "contradicted"))) {
      report(paste0(id, ": missing scientific evidence/definitions."))
    }
    clocks <- c("total_age", "breast_height_age",
                "corrected_age_above_1m", "species_dependent", "unresolved")
    datums <- c("total_height", "above_breast_height", "unresolved")
    if (!row$age_basis %in% clocks || any(!defs$input_age_basis %in% clocks) ||
        any(!defs$reference_age_basis %in% clocks) ||
        any(!defs$si_datum %in% datums) || any(!defs$height_datum %in% datums) ||
        any(!defs$productivity_quantity %in% c("site_index", "height_index",
                                               "inherent_height_index", "ecological_iqs")) ||
        any(!defs$reference_age_role %in% c(
          "fixed_model_scale", "configurable_engine_default", "converted_index",
          "nominal_parameter_reference"
        )) ||
        any(!is.na(defs$reference_age) &
              (!is.finite(defs$reference_age) | defs$reference_age <= 0))) {
      report(paste0(id, ": invalid scientific clock/datum/quantity; unknown is not ALL."))
    }
    expected_quantity <- if (expected_eco) {
      "ecological_iqs"
    } else if (id == "huang2026_climate") {
      "inherent_height_index"
    } else if (id == "huang2026") {
      "height_index"
    } else {
      "site_index"
    }
    if (any(!defs$productivity_quantity %in% expected_quantity) ||
        (!row$age_basis %in% "species_dependent" &&
         any(!defs$input_age_basis %in% row$age_basis))) {
      report(paste0(id, ": incompatible scientific quantity/input clock."))
    }
    if ("base_age" %in% names(row$native_defaults[[1]]) &&
        any(defs$reference_age_role == "configurable_engine_default" &
              defs$reference_age != row$native_defaults[[1]]$base_age, na.rm = TRUE)) {
      report(paste0(id, ": native reference age conflicts with scientific definitions."))
    }
    evidence <- row$metadata_evidence[[1]]
    if (nrow(evidence) == 0L || any(!nonempty(evidence$locator)) ||
        !any(evidence$subject %in% "operation_input_contract" & evidence$status %in% "verified")) {
      report(paste0(id, ": missing scientific evidence."))
    }
  }
  unique(out)
}
