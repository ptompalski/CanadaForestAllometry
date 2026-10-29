# Parameters CSV convention

Model coefficients live as CSV files in `data-raw/`, are assembled by
`data-raw/preprocess_data.R`, and are compiled into `R/sysdata.rda` (internal data).
User-facing functions read them via accessors like `get_volume_params()` or a
model-specific `.<model>_prepare()` helper.

## CSV structure

- **One row per parameter set.** For most models this is one row per species (and,
  where relevant, per subregion/jurisdiction or model variant).
- **Species identified by NFI code** in a column (e.g. `nfi_species` = `PICE.MAR`).
  Keep the source's own species label in a separate column too, so the mapping is
  auditable.
- **One column per coefficient**, named to match the model's own symbols where
  reasonable (e.g. `b1`, `b2`, `a0`; or `b1_const`, `b1_lnS_coef` for structured
  forms). Keep names stable and lowercase/snake_case.
- **Provenance columns** are encouraged and already used in existing files:
  a `page` column and a `source_short` (or similar) column. See
  `data-raw/Thrower1994_parameters.csv` for a worked example.
- Empty cells are allowed for coefficients that don't apply to a given row (the
  Thrower1994 table uses this heavily across its several model forms).

## Look at an existing example first

Before creating a new CSV, read a comparable existing one to match style:

- Structured multi-form SI table: `data-raw/Thrower1994_parameters.csv`
- Simple per-species coefficient table: `data-raw/Huang1994_parameters.csv`,
  `data-raw/Payandeh_1974_parameters.csv`
- Volume/taper coefficients: `data-raw/kozak1994_parameters.csv`

## Wiring into preprocess_data.R

`data-raw/preprocess_data.R` reads each CSV and builds one object per model. The
pattern (see the Thrower1994 block) is:

```r
parameters_<Model> <- readr::read_csv(
  "data-raw/<Model>_parameters.csv",
  show_col_types = FALSE
)
# ...any light post-processing (type coercion, code standardization,
#    renaming the NFI species column to `Species`)...
```

Then **add the object name to the `internal_objs` vector** near the end of the
file (section "combine all into one"). The script finishes with

```r
save(list = internal_objs, file = "R/sysdata.rda", compress = "bzip2")
```

which writes **only** the objects listed in `internal_objs` (and stops if any is
missing). An object not added to `internal_objs` silently never reaches
`R/sysdata.rda`. Do **not** call `usethis::use_data(..., internal = TRUE)` — those
calls are commented out in the file and would overwrite `sysdata.rda` with only
the objects passed to them. Run the script top-to-bottom.

## DBH–height (`hd_`) parameter tables

- **Naming:** `data-raw/<Model>_hd_parameters.csv` → object
  `parameters_hd_<Model>` (e.g. `parameters_hd_Huang2016`), used as the
  registry `params_key`.
- **Keys:** `Species` (NFI code) plus any region/class key column named after
  the model's region argument (e.g. `natural_subregion`, `ecoregion`), and a
  variant column when one table holds several variants (e.g. `variant` =
  `m4` / `m5`). Keep the source's own species/region labels in separate
  columns. Long source tables (species × region × coefficient) may be stored
  long and pivoted at build time with `tidyr::pivot_wider()`.
- **Applicability limits:** `<arg>_min` / `<arg>_max` columns using the
  argument name (e.g. `DBH_min`, `DBH_max`, `BA_min`, `HDOM_max`); empty when
  not published. These feed `.hd_warn_out_of_range()`.
- **Variance components:** transcribe them into clearly prefixed columns
  (e.g. `var_u1`, `var_u2`, `cov_u1_u2`, `sigma2`) in the same table, or in a
  separate `parameters_hd_<Model>_varcomp` object when the structure does not
  fit one row per species. They are not used for v1 (population) prediction.
- Region/class lookup tables (e.g. code → group mapping) are separate internal
  objects, also added to `internal_objs`.

## After editing parameters

1. `source("data-raw/preprocess_data.R")`
2. **Restart R** (internal data is loaded at package load).
3. `devtools::load_all()`

Never hand-edit `R/sysdata.rda`.
