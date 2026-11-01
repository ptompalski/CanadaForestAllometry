# Model spec: <fn_name>

> This is the human-reviewed extraction artifact. Fill every section from the
> source document. Flag anything uncertain with WARN — do not guess. Write this
> file under `data-raw/` so it is version-controlled (the `sources/` tree, where
> the source PDF lives, is gitignored).

## Source

- **Citation:** <full citation>
- **BibTeX key:** @<Author><Year>
- **Document in `sources/`:** `sources/<file>.pdf`
- **Model family:** <vol | si | agb | ytbh | hd>
- **Target function:** `<fn_name>`

## Scope / domain of applicability

- **Jurisdiction / region:** <e.g. BC interior>
- **Species covered (NFI codes):** <PICE.MAR, ...>
- **Valid input ranges:** <DBH, height, age ranges if stated>
- **Age basis:** <total age | breast-height age>  (SI models)
- **Base age:** <e.g. 50 yr at breast height>  (SI models)
- **Caveats / assumptions from the source:** <...>

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| DBH | diameter at breast height | cm |
| ... | ... | ... |

## Model form(s)

Transcribe every equation exactly. Note direction(s) supported.

```
<equation(s), e.g. H = 1.3 + b1 * (1 - exp(-b2 * A))^b3 >
```

- **Direction(s):** <predict height from age+SI; invert to predict SI from age+height; ...>
- **Notes on form:** <piecewise? per-species form differences? adjustment terms?>

## DBH–height (hd) models only

Delete this section for other families.

- **Height convention:** <total height | height above breast height>;
  breast-height offset <1.3 | 1.37 m> (where it enters the formula)
- **Fit type:** <OLS/NLS fixed-effects | nonlinear mixed | linear mixed>
- **Coefficients used for population prediction:** <Table X fixed fit | Table Y
  mixed fixed effects with u = 0>; source recommendation: <quote + page, or
  "none stated → fixed-effects fit by default">
- **Random-effects structure:** <which parameters carry random effects; grouping
  level (plot/stand/source); residual correlation>
- **Variance components transcribed:** <Table/page; columns in CSV> (not used in v1)
- **Back-transformation:** <none | exp(...) with/without correction, as source>
- **Variants and how they are selected:** <inferred from supplied predictors
  (hierarchy) | explicit `variant` arg (disjoint inputs)>
- **Source species mappings:** <e.g. PW, PF → PL (Table 1, p. 4) | none>

### Predictor table

One row per argument (excluding `species`); becomes `.hd_model_predictors()`.

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DBH | tree | cm | TRUE | <...> | p. X |
| HDOM | <...> | plot | m | <TRUE/FALSE> | <top-height definition> | p. X |
| BA | <...> | plot | m2/ha | <...> | <threshold; all-species vs species-specific> | p. X |

### Applicability ranges

<per species / variant: DBH, height and stand-predictor ranges, Table/page, or
"not published">

## Parameters

Coefficients copied **digit-for-digit** from the source. One row per parameter set.
Record the source table/page for each. Put the actual values in the CSV
(`data-raw/<Model>_parameters.csv`); summarize provenance here.

- **Source table(s):** <Table 3, p. 7; Table 4, p. 11>
- **Species → NFI mapping:** <Pli → PINU.CON, Sw → PICE.GLA, ...>
- **⚠ Flagged / uncertain values:** <list any illegible or ambiguous cells, or "none">

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** <yes/no>
  - If yes: <source table/example to reproduce; inputs and expected outputs>
- **If no fidelity values — plausibility (Tier 2) comparison:**
  - Existing same-family functions to compare against: <si_xxx, si_yyy>
  - Expected relationship: <similar magnitude, monotonic in age/DBH, ±X%>
  - ⚠ This model will ship with **no source benchmark** — note in NEWS.
