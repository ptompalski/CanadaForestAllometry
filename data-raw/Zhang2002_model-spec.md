# Model spec: hd_zhang2002

> Human-reviewed extraction artifact (Stage 1). Page numbers are given as
> printed journal page and PDF page (printed = PDF + 529). Items marked ⚠
> need a reviewer decision.

## Review decisions (maintainer)

- Equations and coefficients: **approved** by user.
- ⚠1 Inconsistent `ecoregion` + `forest_region` on a row → **abort**.
- ⚠2 `PINU.SPP` → **abort** (not mapped).
- ⚠3 `forest_region` (Rowe forest region; optional): keep. Accepted values
  `"boreal"`, `"central"`; aliases `"glsl"` and
  `"great lakes-st. lawrence"` → Central (case-insensitive). Derivable from
  `ecoregion`; used only when `ecoregion` is NULL/NA.
- Ecoregion codes not in the paper (anything other than 3E, 3S, 3W, 4E, 4S,
  4W, 5E) → **abort**.
- DOI 10.5558/tfc78530-4: **confirmed** by maintainer.
- ⚠5 Registry: **three rows**: `zhang2002_ecoregion` (regional),
  `zhang2002_regional` (regional), `zhang2002_overall` (provincial); all
  `province_scope = "ON"`, `stand_origin = "both"` (origin not stated in
  source; consistent with `sharmaparton2007`, same OMNR PSP program).
- ⚠6 `keep_model_id = TRUE` adds `model_id`, `variant`, `parameter_set`:
  **approved**.

## Source

- **Citation:** Zhang, L., Peng, C., Huang, S., and Zhou, X. 2002. Development
  and evaluation of ecoregion-based jack pine height-diameter models for
  Ontario. *The Forestry Chronicle* 78(4): 530–538. doi:10.5558/tfc78530-4
  (citation confirmed from rendered p.530 header and running footers; ⚠ DOI
  follows the journal's pattern but is not printed in the PDF — verify before
  adding to `REFERENCES.bib`).
- **BibTeX key:** @Zhang2002hd (proposed; not yet in `REFERENCES.bib`)
- **Document in `sources/`:** `sources/height-diameter/zhang2002_forchron_jackpine_ecoregion_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_zhang2002`

## Feasibility triage — verdict: GO WITH GAPS (accepted by maintainer)

| Check | Result |
|---|---|
| Direction | Height from DBH. ✔ |
| Model form | Eq. [1], p.532 (PDF p3), verified on rendered image. ✔ |
| Parameters present | Table 2, p.533 (PDF p4): a, b, c, MSE for 7 ecoregions + Boreal, Central, Overall. ✔ |
| Units | H m, DBH cm (p.532). ✔ |
| Height convention | Total height; additive 1.3 m offset; DBH at 1.3 m outside bark (p.530, p.532). ✔ |
| Extra predictors | None beyond the ecoregion / forest-region class. ✔ |
| Fit type | Fixed-effects NLS (SAS PROC NLIN, p.532). ✔ |
| Applicability ranges | Table 1, p.532 (PDF p3): DBH and H mean/STD/min/max per ecoregion, region, overall. ✔ |
| Validation | **Gap:** no worked example. Tables 4–6 give only aggregate mean observed/predicted H per ecoregion over unpublished PSP data → not reproducible. **Plausibility only (Tier 2); no source benchmark.** Accepted by maintainer. |
| Legibility | Text PDF; Tables 1–2 and Eq. [1] read from 300-dpi renders (`tmp/zhang2002/t1.png`, `p4_top.png`, `p3_bottom.png`). ✔ |

## Scope / domain of applicability

- **Jurisdiction / region:** Ontario. Five ecoregions in the Boreal forest
  region (3E, 3S, 3W, 4S, 4W) and two in the Central (Great Lakes–St.
  Lawrence) forest region (4E, 5E) (p.532; Hayden et al. 1995; ELC Working
  Group 2000; Fig. 1 p.531).
- **Species covered (NFI codes):** PINU.BAN (jack pine, *Pinus banksiana*
  Lamb.) only.
- **Data:** 10 727 trees from "new permanent sample plots" (OMNR PSPs);
  trees with forks and/or damaged tops excluded (p.532). Stand origin not
  stated → registry `stand_origin = "both"` (approved, ⚠5).
- **Valid input ranges:** Table 1 (see Applicability ranges).
- **Caveats from the source:**
  - "an unusually large estimate for the asymptotic parameter occurred in the
    ecoregion 3S. We suspect that this is mainly due to lack of large-sized
    jack pine trees in the observations." (p.533) — 3S: a = 79.0082,
    b = 0.0082, c = 0.8275; extrapolation beyond DBH 35.3 cm is unreliable
    (e.g. DBH 60 cm → ≈ 37.4 m).
  - Applying the Overall/regional models, or another ecoregion's model, to an
    ecoregion produces significant bias (Tables 4–6, p.534–536; Conclusion
    p.537). Ecoregion models are the authors' preferred models.
  - Ecoregion boundaries are those of the ELC Working Group (2000) map
    (updated map supplied by P. Uhlig, Acknowledgements p.537). Later Ontario
    ecoregion revisions (Crins et al. 2009) may differ — document in
    `@details`.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | total tree height | m |
| DBH | diameter at breast height (1.3 m) outside bark | cm |
| a | asymptotic parameter | m |
| b | rate parameter | cm⁻¹ |
| c | shape parameter | – |
| MSE | model mean squared error | m² |

## Model form(s)

Read from rendered image (p.532, PDF p3).

```
Eq. [1]:  H = 1.3 + a · (1 − exp(−b · DBH))^c
```

- **Direction:** height from DBH only.
- Same form for all 10 parameter sets. Eqs. [2]–[3] are dummy-variable
  full models for F-tests only (no coefficients published) → not implemented.
- H → 1.3 as DBH → 0 for all sets (c > 0). Monotone increasing in DBH
  (a, b, c > 0). c < 1 for 3S and 4E (no inflection point).
- Identical in form to Huang et al. (2016) Eq. [1] and Eslamdoust (2026) M4;
  implemented inline (`1.3 + a * (1 - exp(-b * DBH))^c`) as in
  `.huang2016_height()` — no new helper in `R/hd_utils.R` needed.

## DBH–height (hd) models only

- **Height convention:** total height; additive 1.3 m offset.
- **Fit type:** SAS PROC NLIN, fixed-effects (p.532).
- **Coefficients used for population prediction:** Table 2 (only fit
  published). Source recommendation: use the ecoregion-specific model
  (Conclusion, p.537).
- **Random-effects structure:** none.
- **Variance components transcribed:** MSE only (Table 2 → CSV `mse`). Not
  used in v1.
- **Back-transformation:** none.
- **Variants and how they are selected** (maintainer choice
  `ecoregion_with_fallback`), per row:
  1. `ecoregion` non-NA → ecoregion fit (`variant = "ecoregion"`).
  2. `ecoregion` NULL/NA and `forest_region` non-NA → Boreal or Central
     regional fit (`variant = "regional"`).
  3. both NULL/NA → Overall fit (`variant = "overall"`), with a one-time
     `inform_once()` message (as `hd_huang2016`).
  - ⚠1 If both `ecoregion` and `forest_region` are supplied on a row, the
    ecoregion fit is used. Proposed: **abort** when they are inconsistent
    (e.g. `"3E"` + `"central"`), since the source fixes 3E/3S/3W/4S/4W →
    Boreal and 4E/5E → Central (p.532).
- **Source species mappings:** none. Species other than PINU.BAN → abort
  (as `hd_huang2016`). ⚠2 Genus-level `PINU.SPP` → abort (not mapped).

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DBH | tree | cm | TRUE | "Outside bark diameter at breast height (DBH)"; "DBH is tree diameter at breast height (cm)"; "diameter at breast height (i.e., 1.3 m above ground) outside bark" | p.532; p.530 |
| ecoregion | Ecoregion | plot | – | FALSE | Ontario ecoregion code, one of 3E, 3S, 3W, 4E, 4S, 4W, 5E (ELC Working Group 2000; Hayden et al. 1995) | p.532, Fig. 1 p.531 |
| forest_region | Boreal / Central region | plot | – | FALSE | "Boreal region (3E, 3S, 3W, 4S and 4W) and two in the Central region (4E and 5E)" | p.532 |

- ⚠3 Accepted input codes for `ecoregion`: `"3E"` … `"5E"`, case- and
  whitespace-insensitive (Table 1 prints `"3 E"`, Table 2 `"3E"`); numeric
  not accepted. `forest_region`: `"boreal"` / `"central"`, case-insensitive.
  Approved: also `"glsl"` / `"great lakes-st. lawrence"` → Central.

### Applicability ranges

- Table 1 (p.532, PDF p3) per ecoregion / region / overall: DBH and H mean,
  STD, min, max → CSV `DBH_*`, `height_*`.
- Proposal: one warning per call (`.hd_warn_out_of_range()`) for rows with
  DBH outside `[DBH_min, DBH_max]` of the parameter set actually used. Height
  ranges informational only.

## Parameters

- **Source tables:**
  - Table 2, p.533 (PDF p4): N, a, b, c, MSE.
  - Table 1, p.532 (PDF p3): N, DBH and H mean/STD/min/max.
- **File:** `data-raw/Zhang2002_hd_parameters.csv`, 10 rows (7 ecoregion +
  2 regional + 1 overall) → proposed object `parameters_hd_Zhang2002` (add to
  `internal_objs`). Keys: `nfi_species`, `variant`, `ecoregion`,
  `forest_region`. `forest_region` is filled on ecoregion rows (from p.532) so
  ⚠1 consistency can be checked from the table.
- **Verification:**
  - Tables 1 and 2 transcribed from 300-dpi renders.
  - Automated match of every numeric CSV column against the PDF text layer as
    contiguous column runs (7 ecoregion / 2 regional / 1 overall), which also
    confirms row-to-ecoregion alignment: **0 mismatches**.
  - Image spot checks: 3S a = 79.0082, b = 0.0082; 4W c = 1.9955;
    Central MSE = 6.2790; Overall b = 0.0808; Table 1 4E DBH min 3.10,
    3E DBH max 51.30, 5E H min 5.12.
  - N in Table 1 equals N in Table 2 for every row.
- **Species → NFI mapping:** jack pine *Pinus banksiana* Lamb. → PINU.BAN.

### ⚠ Flagged / uncertain values

- No illegible or ambiguous coefficients.
- ⚠4 Precision: b is printed to 4 decimals (e.g. 3S b = 0.0082, 2
  significant figures). Stored as printed; the published rounding limits
  achievable precision (3S at DBH 30 cm: ±0.00005 in b ≈ ±0.1 m in H).

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** **No.** No worked
  example or prediction grid. Tables 4–6 report mean observed and mean
  predicted H per ecoregion over the unpublished PSP data — not reproducible
  without the tree list.
- ⚠ This model ships with **no source benchmark** (accepted).
- **Plausibility (Tier 2) comparison** for PINU.BAN:
  - `hd_sharmaparton2007` (northern Ontario boreal; requires HDOM, BA, TPH —
    compare at typical stand values from that spec's Table 1 means).
  - `hd_huang2016` (Alberta provincial PINU.BAN, Chapman–Richards) — same
    form, different province; expect similar magnitude.
  - `hd_auger2016` (Québec natural stands; requires BA) — magnitude check.
  - Expected: monotone increasing in DBH over 1.3–51.3 cm; H(0) = 1.3;
    heights within ~±20–25 % of comparators over DBH 5–35 cm; Central
    (4E, 5E) curves above Boreal at mid DBH (consistent with Tables 4–5:
    the Overall/Boreal models under-predict in 4E/5E); 4S lowest of the
    boreal ecoregions at large DBH.
- **Cross-check results** (`tmp/generate_hd_zhang2002_comparison_values.R`,
  PINU.BAN, DBH 5–35 cm):
  - Overall vs `hd_huang2016` (AB): within ~5 % at 10–35 cm.
  - Overall vs `hd_sharmaparton2007` (HDOM 20, BA 25, TPH 1500): ~−34 % at
    10 cm, ~−11 % at 20 cm, ≈0 % at 30 cm. The stand-level model is taller
    for small trees.
  - Overall vs `hd_auger2025` (QC plantations): far lower at 5–15 cm and
    agrees around 25 cm. Plantation curves are much more linear.
  - `hd_auger2016`: not comparable, because PINU.BAN requires more
    predictors than BA.
  - Central 4E/5E are 23–32 % above Overall at 10 cm. 3S (a = 79) keeps
    rising to 26.3 m at 35 cm, beyond its data range of 35.3 cm. 4S/4W
    are below Overall at 35 cm.
- **Internal consistency checks (computed, not source benchmarks):**
  hand-calculated Eq. [1] values for each of the 10 parameter sets at a few
  DBH values (stored via `tmp/generate_hd_zhang2002_comparison_values.R`).

## Proposed API (for review)

```r
hd_zhang2002(DBH, species, ecoregion = NULL, forest_region = NULL,
             keep_model_id = FALSE)
```

- Returns tibble `height` (m); with `keep_model_id = TRUE` adds `model_id`
  (`"zhang2002"`) and `variant` (`"ecoregion"`/`"regional"`/`"overall"`)
  — ⚠6 or a single `parameter_set` column (`"3E"`, …, `"boreal"`,
  `"central"`, `"overall"`); proposed: `variant` + `parameter_set`.
- Registry (approved): three rows `zhang2002_ecoregion` / `zhang2002_regional`
  (`scope = "regional"`) and `zhang2002_overall` (`scope = "provincial"`),
  `province_scope = "ON"`, `stand_origin = "both"`,
  `equation_family = "chapman_richards"`, `region_arg = "ecoregion"`,
  `calibration_published = FALSE`, `params_key = "parameters_hd_Zhang2002"`.
  Predictor rows per model_id: `_ecoregion` → `DBH`, `ecoregion` (required);
  `_regional` → `DBH`, `forest_region` (required); `_overall` → `DBH`.
