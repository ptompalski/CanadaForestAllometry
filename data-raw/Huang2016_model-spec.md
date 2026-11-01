# Model spec: hd_huang2016

> Human-reviewed extraction artifact (Stage 1). Page numbers are **PDF pages**
> (report page = PDF page − 6). Items marked ⚠ need a reviewer decision.

## Source

- **Citation:** Huang, S., Yang, Y., and Aitkin, D. 2016. *Updated population
  and plot-specific individual tree height-diameter models for major Alberta
  tree species.* Report #4, Tech. Rep. Pub. No. T/609. Forestry Division,
  Alberta Agriculture and Forestry, Edmonton, AB. 82 pp.
  ISBN 978-1-4601-2646-2 (online).
- **BibTeX key:** @Huang2016_T609 (proposed)
- **Document in `sources/`:** `sources/height-diameter/implemented/huang2016_t609_updated_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_huang2016`

## Scope / domain of applicability

- **Jurisdiction / region:** Alberta (all 21 natural subregions, Natural
  Regions Committee 2006 classification; Table 2, p. 8).
- **Species covered (NFI codes):** POPU.TRE (AW), POPU.BAL (PB), BETU.PAP (BW),
  PINU.CON (PL), PICE.GLA (SW), PICE.MAR (SB), ABIE.BAL (FB), PSEU.MEN (FD),
  LARI.LAR (LT), PINU.BAN (PJ). Through the Table 1 mapping: PINU.ALB/PINU.FLE → PL,
  PICE.ENG → SW, ABIE.LAS → FB, LARI.OCC/LARI.LYA → LT. Combined groups:
  Decid, Conif, All (⚠ exposure TBD).
- **Valid input ranges:** Table 3, p. 10 (per species × subregion group; see
  CSV columns `DBH_min/max`, `height_min/max`). Overall DBH range is 0.1–80.8 cm.
- **Caveats from the source:**
  - The fit is weighted NLS with weight 1/DBH (p. 12).
  - Population predictions from the mixed-model fixed effects alone are biased,
    so they are not recommended (§4.5, p. 25).
  - Base (Table 4) models are recommended when a plot has no height measurements
    (§6 Rec. 1, p. 44).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| DBH | "tree diameter (cm) measured at a breast height of 1.3 m above the ground" (p. 12) | cm |
| H | "total tree height (m)" (p. 12) | m |
| b1, b2, b3 | fixed parameters (Table 4) | – |
| u1, u2 | plot-level random effects on b1, b2 (mixed models only) | – |

## Model form(s)

```
[1] AW, PB, BW, Decid:
    H = 1.3 + b1 * (1 - exp(-b2 * DBH))^b3
[2] PL, SW, SB, FB, FD, LT, PJ, Conif, All:
    H = 1.3 + b1 / (1 + exp(b2 + b3 * ln(DBH + 0.01)))
[4]/[5] mixed versions: b1 -> (b1 + u1), b2 -> (b2 + u2)   (not used in v1)
```

- **Direction(s):** height from DBH only.
- **Notes on form:**
  - The form depends on the species group: [1] for deciduous, [2] for conifers
    (stored in CSV column `model_eq`).
  - Form [2] adds +0.01 inside the log so that DBH = 0 is defined (p. 12).
  - All three [1]/[2] equations on p. 12 and [4]/[5] on p. 13 were checked
    against the rendered images.

## DBH–height (hd) models only

- **Height convention:** total height; the 1.3 m offset is the additive
  intercept of both forms.
- **Fit type:** weighted NLS (WF = 1/DBH) for the base models (Table 4).
  Weighted first-order nonlinear mixed models with plot random effects for
  Table 5.
- **Coefficients used for population prediction:** **Table 4** (base models,
  "Type = B-U-Pop").
  - Quote, §6 Rec. 1, p. 44: "For any species in a plot without any height
    measurement, base models with the coefficients listed in Table 4 can be
    used".
  - §4.5 (p. 25) states that population predictions from the mixed-model fixed
    parameters are biased (Meng & Huang 2009).
  - This follows plan §8.1 #10.
- **Random-effects structure:** plot-level u1, u2 on b1, b2. The
  variance–covariance matrix is var(u1), cov(u1,u2), var(u2), plus residual σ².
- **Variance components transcribed:** Table 5, p. 14. CSV columns
  `mixed_b1..3`, `var_u1`, `cov_u1_u2`, `var_u2`, `sigma2`, `aic` and `bic` are
  stored for the future EBLUP calibration phase and are **not used in v1**.
- **Back-transformation:** none.
- **Variants and how they are selected:** a hierarchy chosen by
  `natural_subregion`. The subregion-group model is used when one is supplied
  and exists for the species. Otherwise the provincial model is used (with
  `inform_once` on fallback).
  - Subregion groups by species (Table 4):
    - AW: 7–10 / 11 / 1–6,12–21
    - PB: 7–11 / 1–6,12–21
    - PL, SW, FB: 7–9 / 10 / 11 / 1–6,12–21
    - SB: 7–10 / 11 / 1–6,12–21
    - BW, FD, LT, PJ, Decid, Conif, All: provincial only.
- **Source species mappings:** Table 1, p. 7: PW, PF → PL; SE → SW; FA → FB;
  LW, LA → LT. Stored in `Huang2016_hd_species_map.csv`.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DBH | tree | cm | TRUE | "tree diameter (cm) measured at a breast height of 1.3 m above the ground" | p. 12 |
| natural_subregion | natural subregion (NSR) | plot | code (1–21 or CM, DM, …) | FALSE | "2006 natural subregions" of Alberta (Table 2) | p. 8 |

### Applicability ranges

- Per species × subregion group, from Table 3 (p. 10). Stored in CSV columns
  `DBH_mean/min/max/sd`, `height_mean/min/max/sd`, `n_trees` and `n_plots`.
- Proposal: warn (`.hd_warn_out_of_range`) when DBH falls outside
  `[DBH_min, DBH_max]` of the selected row. Height ranges are informational.

## Parameters

- **Source tables:**
  - Table 4 (base, p. 13): `b1, b2, b3`.
  - Table 5 (mixed, p. 14): `mixed_*`, the variance components, AIC and BIC.
  - Table 3 (data summary, p. 10): the range columns.
  - Table 2 (p. 8): subregion codes, in `Huang2016_hd_natural_subregions.csv`.
- **Files:**
  - `data-raw/Huang2016_hd_parameters.csv`: 33 rows, one per species group ×
    subregion group.
  - `data-raw/Huang2016_hd_species_map.csv`
  - `data-raw/Huang2016_hd_natural_subregions.csv`
- **Verification:**
  - Tables 1–5 and 9 were checked against images rendered at 200 dpi.
  - An automated string match of every CSV tuple against the PDF text layer
    found 0 mismatches across 33 rows (Tables 3, 4 and 5).
- **Species → NFI mapping:**
  - AW = POPU.TRE, PB = POPU.BAL, BW = BETU.PAP
  - PL = PINU.CON, PJ = PINU.BAN
  - SW = PICE.GLA, SB = PICE.MAR
  - FB = ABIE.BAL, FD = PSEU.MEN, LT = LARI.LAR
- **⚠ Flagged:**
  1. PW → PINU.ALB is not in the AB `species_code_lookup`, but it is present in
     `species_dictionary`.
  2. Decid, Conif and All have no NFI code. How to expose them is TBD.
  3. Plan §6 PR 1 cites Table 18 as a benchmark. Table 18 is actually the SAS
     IML EBLUP calibration program, so it is deferred to the calibration phase.

## Benchmark plan

- **Fidelity (Tier 1): yes, but conifer form [2] only.**
  - §5.1 worked example (p. 31): SW provincial, DBH 32.5 → H = 25.4846.
    Reproduced exactly.
  - Table 9 column 5 (p. 30): B-U-Pop predictions for 28 SW trees, given to
    2 dp. All 28 are reproduced at 2 dp. Grand mean 20.9276 (published 20.93).
  - Bonus: Table 12 column 5 (p. 30–31), fixed-only predictions from the Table 5
    SW mixed model. The first rows (24.16, 11.73, 25.55, 23.18) are reproduced,
    which checks the Table 5 transcription. These are not v1 outputs.
  - Plan: `tmp/generate_hd_huang2016_comparison_values.R` writes the CSV, and
    tests compare against it at tolerance 0.005 (2 dp) and 1e-4 (§5.1).
- **Deciduous form [1]: plausibility only (Tier 2).**
  - There is no published example.
  - Checks: monotonic in DBH, H → 1.3 as DBH → 0, and asymptote b1 + 1.3.
  - Sanity values: AW provincial at DBH 0.1 / 10 / 30 / 60 gives 1.35 / 13.08 /
    24.21 / 27.26 m.
  - No in-package hd_ model exists yet for cross-comparison.
  - ⚠ NEWS should note that the deciduous forms have no source benchmark.

## Review decisions (Stage 1 sign-off)

- Coefficients and spec are approved. Population coefficients come from Table 4 (B-U-Pop).
- Decid/Conif/All are exposed through special `species` values (e.g. `"DECID"`, `"CONIF"`, `"ALL"`).
- `natural_subregion` accepts numeric codes 1–21 and character codes (CM, LF, …). `NULL` means provincial.
- The registry has a single `huang2016` row. The variant is chosen by hierarchy.
- DBH validation is `DBH >= 0`. Form [2] handles 0 through `+0.01`.
- PW maps to PINU.ALB.

## Implementation notes (Stage 2)

- Internal objects: `parameters_hd_Huang2016` and `hd_huang2016_natural_subregions`.
  - The parameters table is expanded to one row per species key × natural subregion
    (229 rows). Provincial rows have `natural_subregion = NA`.
  - Mapped species are duplicated from their group's rows.
  - Combined models have `Species = NA` and `species_key` set to DECID, CONIF or ALL.
- `keep_model_id = TRUE` adds `model_id` (`"huang2016"`) and `subregion_group`.
- Registry `stand_origin = "natural"`. ⚠ The source does not state stand origin.
  The data are pre-1981 Alberta PSPs plus stem analysis, with no plantation data
  mentioned.
- Benchmarks:
  - Fidelity tests (§5.1; Table 9 at 2 dp; grand mean 20.9276) are in
    `tests/testthat/test-hd_huang2016.R`.
  - The grid is in `tmp/hd_huang2016_comparison_values.csv`.
- ⚠ No source benchmark exists for the deciduous form [1]; it has plausibility checks only.
