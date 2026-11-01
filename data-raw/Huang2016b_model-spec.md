# Model spec: hd_huang2016b

> Human-reviewed extraction artifact (Stage 1). Page numbers are **report pages**,
> with the PDF page in parentheses (PDF page = report page + 6). Items marked ⚠
> need a reviewer decision or are source issues.

## Source

- **Citation:** Huang, S., and Yang, Y. 2016. *Expanded individual tree height
  prediction models from tree diameter and other variables.* Report #5, Tech.
  Rep. Pub. No. T/610. Forestry Division, Alberta Agriculture and Forestry,
  Edmonton, AB. ISBN 978-1-4601-2648-6 (online).
- **BibTeX key:** @Huang2016hdExpanded (proposed). The existing `@Huang2016hd` key
  is T/609.
- **Document in `sources/`:** `sources/height-diameter/huang2016b_t610_expanded_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_huang2016b`

## Triage verdict: GO WITH GAPS → accepted by the user

- **Model form:** eqs. [1]–[2] (base) and [3]–[6] (mixed) are given on p. 4 (PDF p10).
- **Parameters:** Table A1 (pp. 38–40) covers all 10 species groups plus Decid,
  Conif and All.
- **Predictors:**
  - BA is defined in §6.1 (p. 22).
  - HDC is defined in §6.2 (p. 22).
  - Ranges are in Table 2 (p. 3).
- **Fidelity:** the AW worked example in Table 6 (p. 9) is reproduced to 2 dp
  (all 8 trees).
- **⚠ Gap:** the FB base coefficients are not usable. See Flagged item 1.
  - User decision: **exclude** the FB base model, which covers FB and FA.
- **User decisions:**
  - Function name: `hd_huang2016b`.
  - H_DC maps to argument `HDOM`.
  - Scope: **base models only** (B-U-Pop). Mixed coefficients and variance
    components are stored for future calibration.

## Scope / domain of applicability

- **Jurisdiction / region:** Alberta, provincial (no subregion variants). Data
  come from Alberta government PSPs. For AW, PB, PL, SW, SB and FB, only PSPs
  first established before 1981 were used (§2.1, p. 3).
- **Species covered (NFI codes):**
  - POPU.TRE (AW), POPU.BAL (PB), BETU.PAP (BW), PINU.CON (PL),
    PICE.GLA (SW), PICE.MAR (SB), PSEU.MEN (FD), LARI.LAR (LT), PINU.BAN (PJ).
  - Combined groups DECID, CONIF and ALL.
  - ⚠ ABIE.BAL (FB) is excluded from the base model. See Flagged item 1.
- **Species mapping (Table 1, p. 2):** PW, PF → PL; SE → SW; FA → FB; LW, LA → LT.
  - It is identical to T/609, so `data-raw/Huang2016_hd_species_map.csv` is reused.
  - ABIE.LAS (FA → FB) is therefore excluded too.
- **Valid input ranges:** Table 2 (p. 3), per species group: DBH, H, BA and HDC
  min/max. Stored in the CSV.
- **Caveats from the source:**
  - Weighted NLS with WF = 1/DBH (eq. [7], p. 5).
  - BA tagging limit: see the predictor table.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | "total tree height (m)" (p. 4) | m |
| DBH | "tree diameter (cm) at breast height" (p. 4) | cm |
| BA | "basal area (m2/ha) of the species (or species combination)" (p. 4) | m²/ha |
| H_DC | "average height (m) of the dominant and co-dominant trees for the species" (p. 4) | m |
| b1–b5 | population parameters | – |

## Model form(s)

```
[1] AW, PB, BW, Decid:
    H = 1.3 + (b1 + b2*BA + b3*HDC) * [1 - exp(-b4*DBH)]^b5
[2] PL, SW, SB, FB, FD, LT, PJ, Conif, All:
    H = 1.3 + (b1 + b2*BA + b3*HDC) / (1 + exp[b4 + b5*ln(DBH + 0.01)])
```

Mixed forms (not used in v1; plot-level random effects u):

```
[3] AW, PB, Decid:              b1->b1+u1, b3->b3+u2, b4->b4+u3   (in form [1])
[4] BW:                         b1->b1+u1, b5->b5+u2              (in form [1])
[5] PL, SW, SB, FB, Conif, All: b1->b1+u1, b3->b3+u2, b4->b4+u3   (in form [2])
[6] FD, LT, PJ:                 b1->b1+u1, b4->b4+u2              (in form [2])
```

- **Direction(s):** height from DBH, BA and HDC only.
- **Notes on form:**
  - The +0.01 inside ln() "is a constant used to prevent ln(0) … when DBH
    equals zero" (p. 4).
  - The asymptote term (b1 + b2·BA + b3·HDC) can in principle be ≤ 0. This
    happens for PB, because b2 < 0, only at implausibly high BA. ⚠ Proposal:
    abort or NA when height ≤ 0; see Open question B.

## DBH–height (hd) models only

- **Height convention:** total height. The 1.3 m offset is the additive term,
  so H → 1.3 as DBH → 0.
- **Fit type:**
  - Base models: weighted NLS (WF = 1/DBH).
  - Mixed models: first-order nonlinear mixed models with plot random effects.
- **Coefficients used for population prediction:** Table A1, base models [1]/[2]
  (Type = B-U-Pop). §5 Rec. 1 (p. 21): "For any species in a plot without any
  height measurement, base models with the coefficients listed in Table A1 of
  Appendix 1 can be used for height prediction (Type=B-U-Pop)."
- **Random-effects structure:** see the mixed forms above. CSV column
  `re_params` lists the parameters that carry u1, u2 and u3, in that order.
- **Variance components transcribed:** Table A1, pp. 38/40 (PDF p44/46).
  - CSV columns: `var_u1`, `cov_u1_u2`, `var_u2`, `cov_u1_u3`, `cov_u2_u3`,
    `var_u3`, `sigma2`, `aic`, `bic`.
  - The source symbols map as σ1², σ12, σ2², σ13, σ23, σ3², σ².
  - Not used in v1.
- **Back-transformation:** none.
- **Variants:** a single provincial model per species group. The registry has
  one row, `huang2016b`. No `variant` argument.
- **Optional PAR adjustment** (Option B, §5, p. 21):
  PAR = mean(H_obs) / mean(Ĥ) per plot. It needs observed heights, so it is
  out of scope for v1.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DBH | tree | cm | TRUE | "tree diameter (cm) at breast height" | p. 4 (PDF p10) |
| BA | BA | plot | m2/ha | TRUE | "BA is species-specific basal area per hectare (m2/ha). In cases where a deciduous combined, a coniferous combined, or an all species combined model was fitted, BA is the total basal area for deciduous combined, coniferous combined, or all species combined, respectively. In this study, BA is the summation of the basal areas for trees taller than 1.3 m." | §6.1 p. 22 (PDF p28) |
| HDOM | H_DC | plot | m | TRUE | "HDC is the average height (m) of the dominant and co-dominant trees for the species" … "all dominant/co-dominant trees with height measurements were used in this study to compute the dominant/co-dominant height, by species or species combination." | p. 4; §6.2 p. 22 (PDF p28) |

- **⚠ BA tagging limit:** §6.1 says users with BA at other tagging limits
  (≥ 5.1 or ≥ 9.1 cm) can "assume that the BA were measured at the 1.3 m
  tagging limit, and put it into the models". The source text on converting BA
  between limits is advisory; the package will not do it.
- Both BA and HDOM are **species-specific** (that is, for the species group of
  the tree, or for the combination when DECID/CONIF/ALL is used). This differs
  from `hd_sharmaparton2007`, whose HDOM is irrespective of species. It must be
  stated in `@param`.

### Applicability ranges

- Table 2, p. 3 (PDF p9), per species group.
- Stored in CSV columns `DBH_*`, `height_*`, `BA_*` and `HDOM_*` (mean/min/max/sd).
- Proposal: warn through `.hd_warn_out_of_range()` when DBH, BA or HDOM falls
  outside [min, max].

## Parameters

- **File:** `data-raw/Huang2016b_hd_parameters.csv` (13 rows: 10 species groups
  + Decid, Conif, All).
- **Source tables:**
  - Table A1 part 1, p. 38 (PDF p44): model [1] base and [3]/[4] mixed.
  - Table A1 part 2, p. 39 (PDF p45): model [2] base.
  - Table A1 part 3, p. 40 (PDF p46): models [5]/[6] mixed.
  - Table 2, p. 3 (PDF p9): ranges.
- **Verification:**
  - All values were transcribed from 200-dpi page renders.
  - Base and mixed b1–b5 were cross-checked automatically against the PDF text
    layer, with 0 mismatches across 13 rows.
  - Table 2 was checked visually against the render.
- **Species → NFI mapping:**
  - AW = POPU.TRE, PB = POPU.BAL, BW = BETU.PAP
  - PL = PINU.CON, PJ = PINU.BAN
  - SW = PICE.GLA, SB = PICE.MAR
  - FB = ABIE.BAL, FD = PSEU.MEN, LT = LARI.LAR
  - The groups have an empty `nfi_species`, as in T/609.
- **⚠ Flagged / uncertain values:**
  1. **FB base coefficients — source error.**
     - In Table A1 part 2 the FB column is identical to SB in every coefficient
       (11.1383, 0.0992, 0.6271, 3.7403, −1.4773) and every fit statistic. Only
       N (8807) and m (626) differ.
     - Table 5 (p. 7) gives different FB B-U-Pop statistics: SD 1.92, MSE 3.67,
       R² 0.92. The SB values are SD 1.67, MSE 2.79, R² 0.86, and they match
       Table A1.
     - The true FB base coefficients are therefore not published. The CSV leaves
       FB `b1`–`b5` empty with `base_status = excluded_source_error`.
     - FB mixed (Model [5]) coefficients are distinct and stored.
  2. PB b2 (−0.0158), PB mixed b2 (−0.00066) and FB mixed b2 (−0.08725) are
     negative as printed, which is correct.
  3. The Model [3]/[4]/[5] mixed coefficients carry 4–5 significant digits, as
     printed. They are not trimmed or padded.

## Benchmark plan

- **Fidelity (Tier 1), form [1]: yes.** Table 6 (p. 9), AW base, population-based Ĥ:
  - Plot 1 (BA 36.8225, HDC 29.9333):
    - DBH 29.7, 26.9, 37.5, 1.6, 4.0 → 27.20, 26.58, 28.26, 3.14, 7.09
  - Plot 2 (BA 47.0862, HDC 16.0000):
    - DBH 13.9, 15.3, 15.3 → 16.44, 17.37, 17.37
  - The worked example on p. 9 gives 27.2046 to 4 dp.
  - All are reproduced by the transcribed coefficients (computed:
    27.2046, 26.5772, 28.2593, 3.1417, 7.0866, 16.4368, 17.3715, 17.3715).
  - Tolerance: 0.005 for the 2-dp values and 1e-4 for the 4-dp value.
- **Bonus transcription check (not a v1 output):**
  - Table 9 column 4 (p. 11) gives AW mixed fixed-only predictions (u = 0):
    28.87, 28.23, 29.92, 2.92, 6.95, 14.27, 15.10, 15.10.
  - Table 18 (pp. 35–36) gives ht_fix to 4 dp: 28.8727 … 15.1012.
  - These verify the AW Model [3] transcription.
- **Form [2] (conifers, CONIF, ALL): no source example → plausibility (Tier 2).**
  - Check monotonicity in DBH, and in HDOM where b3 > 0.
  - Check that H → 1.3 as DBH → 0.
  - Cross-compare with `hd_huang2016` (T/609 base, same data source and species)
    at the Table 2 mean BA and HDC per species. Similar magnitude is expected,
    and discrepancies will be reported.
  - Also compare with `hd_sharmaparton2007` where species overlap.
  - ⚠ The conifer form ships with no source benchmark.

## Open questions for review

Resolved at Stage 1 sign-off: all proposals A–C accepted as written.

- **A. Species-group input.** Proposal: same API as `hd_huang2016`. NFI codes
  are mapped through Table 1, and `"DECID"`, `"CONIF"` and `"ALL"` select the
  combined models.
  - FB/FA rows: proposal is to abort with a message citing the source error.
    The alternative is NA plus a warning.
- **B. Non-positive asymptote** (b1 + b2·BA + b3·HDC ≤ 0, i.e. H ≤ 1.3).
  Proposal: compute as published and warn only on out-of-range inputs.
  No clamping.
- **C. Validation.** Proposal: DBH ≥ 0; BA ≥ 0; HDOM > 0. All are required with
  no defaults, and NA propagates to NA height.

## Implementation notes (Stage 2)

- Signature: `hd_huang2016b(DBH, species, HDOM, BA, keep_model_id = FALSE)`.
- Internal object `parameters_hd_Huang2016b`: 17 rows, one per species key
  (Table 1 mapped species + DECID/CONIF/ALL). FB rows (`base_status =
  excluded_source_error`) and the species mapped to FB (ABIE.BAL, ABIE.LAS)
  are dropped; requesting them aborts with a message naming the source error.
- Registry: single row `huang2016b` (provincial, AB, `stand_origin = "natural"`
  as for T/609), reference `@Huang2016hdExpanded`.
- No clamping of heights: computed as published (open question B).
- Tests: `tests/testthat/test-hd_huang2016b.R`; fidelity against the p. 9
  worked example (1e-4) and Table 6 (2 dp); 100% line coverage.
- Comparison values: `tmp/generate_hd_huang2016b_comparison_values.R` ->
  `tmp/hd_huang2016b_comparison_values.csv` (benchmarks, grid, and a
  cross-comparison with `hd_huang2016` at the Table 2 mean HDOM/BA).
- Cross-comparison with `hd_huang2016` (T/609) at Table 2 means: within about
  ±10% for most groups at DBH 10–40 cm. Large divergences for LT (−31% to −45%
  at DBH 20–40) and PJ (−20% to −25% at DBH 30–40), and PL/BW/SB at DBH 40
  (−10% to −16%): at low mean HDOM the T/610 asymptote (b1 + b2·BA + b3·HDOM)
  caps height near the stand's dominant height, whereas T/609 has no such
  constraint. Consistent with the model form, not a transcription issue.
- ⚠ Conifer form [2] has no source benchmark (plausibility only).
