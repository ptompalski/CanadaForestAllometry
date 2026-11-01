# Registry and wiring

After the parameters exist and the R function is written, wire the model into the
package's discovery and documentation surfaces.

## Model registry

Add a row to the family's registry so the model is discoverable and so the
comparison-value generator can find it.

- Site index: `R/si_model_registry.R` (`si_model_registry()`)
- Volume: `R/volume_model_registry.R` (`volume_model_registry()`)
- DBH–height: `R/hd_model_registry.R` (`hd_model_registry()`) — row-wise; see
  the section below instead of the parallel-vector instructions.

Read the existing registry function and add your model consistently across **all**
parallel columns (they are constructed as parallel vectors, so every vector must gain
one element in the same position). Typical columns for the SI registry:

| Column | Meaning |
|--------|---------|
| `model_id` | Stable id used by metadata/helpers (e.g. `"thrower1994"`; append a variant suffix like `_si50` when a function has variants) |
| `reference` | BibTeX key, e.g. `"@Thrower1994"` — must exist in `REFERENCES.bib` |
| `engine` | The R function name that implements it (e.g. `"si_thrower1994"`) |
| `age_basis` | `"total"` or `"breast_height"` |
| `fixed_args` | Named list of args identifying a variant, or empty |
| `species_manual` | Character vector of covered NFI codes, when listed directly |

Match the exact column set of the registry you are editing; do not invent columns.

### DBH–height registry (`R/hd_model_registry.R`)

The `hd_` registry is built **row-wise**, not from parallel vectors:

1. Append one `.hd_registry_row(...)` per model **variant** to the list returned
   by `.hd_model_definitions()` (e.g. `eslamdoust2026_m4`, `eslamdoust2026_m5`).
2. Append the model's predictor rows to `.hd_model_predictors()` — the single
   source of truth from which the registry's `required_predictors` /
   `optional_predictors` list-columns are derived. Do **not** list `species`
   (coverage comes from the params table or `species_manual`).

| Column | Meaning |
|--------|---------|
| `model_id` | Stable id with variant suffix |
| `engine` | `hd_*` function name |
| `fixed_args` | Named list of args selecting the variant (may be empty) |
| `reference` | BibTeX key, e.g. `"@Huang2016"` |
| `scope` | `"provincial"` / `"regional"` |
| `province_scope` | Jurisdiction codes (`"ALL"` allowed) |
| `stand_origin` | `"natural"` / `"plantation"` / `"both"` |
| `equation_family` | `"chapman_richards"`, `"korf"`, `"log_linear_mixed"`, ... |
| `required_predictors`, `optional_predictors` | Derived; do not set by hand |
| `region_arg` | Model-specific region arg name, or `NA` |
| `prediction_types` | `"population"` in v1 |
| `calibration_published` | `TRUE` if the source publishes variance components |
| `params_key` | `parameters_hd_<Model>` |
| `species_manual` | Only when coefficients are not in a params table |
| `description` | One line |

Predictor table columns: `model_id`, `arg`, `level` (`"tree"`/`"plot"`),
`units`, `required`, `definition` (source definition, verbatim where
practical), `source_locator` (table/page).

`.hd_registry_problems()` checks consistency and is asserted empty by
`tests/testthat/test-hd_model_registry.R`: every predictor is a formal of the
engine, every engine argument without a default (except `species`) is a
required predictor, `DBH` is required, `region_arg` is a predictor, and ids are
unique. Fix the registry, not the test, if it fails.

## Bibliography

Add an entry to `REFERENCES.bib` keyed to match the registry `reference` column
(e.g. `@Thrower1994`). Include full citation details from the source document.

## Vignette (optional)

If the model belongs in a family vignette, mention it in the relevant
`vignettes/articles/*.qmd` (e.g. `Site-Index-Models.qmd`, `Tree-Volume-Models.qmd`,
`Height-Diameter-Models.qmd`).

## NEWS.md

Add a **single brief sentence** under the current development version naming the new
model. Keep it to one sentence; do not enumerate parameters, species, or validation
details there (record those in the model spec instead). If the model shipped with
only a plausibility check, note "no source benchmark" in the model spec — not NEWS.

Example: `* Added \`si_nigh2002\`, a site index model for interior BC lodgepole pine.`

## Documentation build

Run `devtools::document()` to regenerate `man/*.Rd` and `NAMESPACE` from the roxygen
block. Never edit `man/*.Rd` by hand.

## Verification checklist

- [ ] `source("data-raw/preprocess_data.R")` run, R restarted, `load_all()` clean
- [ ] `devtools::document()` regenerates docs without warnings
- [ ] `devtools::test()` passes, including the new test file
- [ ] Coverage of the new `R/<fn>.R` is 100% (`covr::file_coverage()`); unreachable lines marked `# nocov`
- [ ] New model compared against similar existing function(s); discrepancies reported
- [ ] Registry row present and internally consistent
- [ ] `REFERENCES.bib` entry added
- [ ] `NEWS.md` updated with a single brief sentence
- [ ] `devtools::check()` run if the change is substantial
