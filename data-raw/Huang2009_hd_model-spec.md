# Model spec: hd_huang2009

> Human-reviewed extraction artifact (Stage 1). Page numbers are given as
> printed journal page and PDF page (printed = PDF + 2).

## Review decisions (maintainer, 2026-10-30)

- Equations and coefficients: **approved**.
- ⚠1 Variants: **all three** (m9/m10/m11), inferred from supplied predictors.
- ⚠2 TopH argument name: **`HTOP`** (distinct from `HDOM`).
- ⚠4 Applicability ranges: **fitting data** (Table 1).
- ⚠6 Registry `scope`: **`"regional"`**.
- Benchmark: test m10/m11 with back-solved HTOP = 14.5, BA = 7.75 (±0.002 m).
- ⚠3 (BA threshold not stated) and ⚠5 (no H_max, calibration or ratio
  correction in v1) accepted as proposed.

## Source

- **Citation:** Huang, S., Meng, S.X., and Yang, Y. 2009. Using nonlinear mixed
  model technique to determine the optimal tree height prediction model for
  black spruce. *Modern Applied Science* 3(4): 3–18. (Confirmed from rendered
  p.3 header / title block and running heads "Vol. 3, No. 4 … April, 2009".)
- **BibTeX key:** @Huang2009hd (proposed; `@Huang2009gypsy` already exists for
  the GYPSY report used by `si_huang2009`).
- **Document in `sources/`:** `sources/height-diameter/huang2009a_mas_blackspruce_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_huang2009` (no clash: the existing `si_huang2009`
  has a different prefix). Data-raw files use `Huang2009_hd_*` to avoid
  colliding with `Huang2009_model-spec.md` / `parameters_Huang2009` (GYPSY SI).

## Feasibility triage — verdict: GO

| Check | Result |
|---|---|
| Direction | Height from DBH (+ TopH, BA). ✔ |
| Model form | Eqs. (9), (10), (11), pp.4–5 (PDF p2–3). ✔ |
| Parameters present | Table 2 p.13 (PDF p11): all three variants, fixed effects + variance components. ✔ |
| Parameter coverage | Single species (black spruce). ✔ |
| Units | DBH cm, H m, TopH m, BA m²/ha (p.5, Table 1). ✔ |
| Height convention | Total height; 1.3 m additive offset. ✔ |
| Extra predictors defined | TopH = mean height of the 100 largest-DBH trees per ha; BA = stand basal area per ha (pp.4–5). BA size threshold / species basis not stated (⚠3). ✔ |
| Fit type | NLMM, FO method (SAS %NLINMIX); only mixed fixed effects published (no separate NLS fit). ✔ |
| Applicability ranges | Table 1 p.12 (PDF p10): tree DBH/H and plot BA/TopH, fitting and validation sets. ✔ |
| Validation | **Tier 1**: Table 3 p.14 (PDF p12) worked example, H_fix for all three models. ✔ |
| Legibility | Text PDF; Table 2/3 values checked on 300-dpi renders (`tmp/huang2009hd/p-11.png`, `p-12.png`) and against the text layer — identical. ✔ |

## Scope / domain of applicability

- **Jurisdiction / region:** Alberta. Provincial PSPs; **fitting data from
  the Lower Foothills** natural region only (93 plots, 917 trees); validated
  on Boreal Mixedwood + Upper Foothills (71 plots, 786 trees) (p.4, PDF p2).
  PSP sizes 200–2000 m²; plots measured repeatedly — only one measurement per
  plot retained (p.4).
- **Species covered:** black spruce (*Picea mariana*) → `PICE.MAR`.
- **Stand origin:** PSPs in natural stands (not explicitly stated) → registry
  `stand_origin = "natural"` (⚠6).
- **Valid input ranges (Table 1, fitting data):** DBH 1.3–39.4 cm;
  H 2.3–26.2 m; BA 0.643–54.608 m²/ha; TopH 5.500–23.550 m.
  Validation data: DBH 1.4–35.8; H 2.0–27.4; BA 0.515–45.344;
  TopH 7.067–27.400.
- **Caveats from the source:**
  - The paper's main conclusion: (11) ranks best by AIC/BIC, but on validation
    with calibration (fixed + random) the base model (9) is most accurate;
    "fixed only" (population-averaged) predictions are biased —
    Table 4: validation Bias% −7.5 (9), −2.1 (10), −3.3 (11); modelling data
    +3.4, +2.2, +0.8 (p.14, PDF p12).
  - A ratio-estimate correction r (eq. 29–30, p.8–9) is proposed to remove
    population bias, but **no numeric r is published** → not implementable.
  - Negative FO predictions set to zero (p.7, PDF p5) — irrelevant for
    population predictions (always ≥ 1.3 m).
  - Eq. (26) H_max = 1.3 + 34.3451[1 − exp(−0.04892·DBH)]^0.9042 (p.7) is an
    upper-bound curve used only for the spaghetti-plot discussion (⚠5).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | total tree height | m |
| DBH | diameter at breast height (1.3 m) | cm |
| TopH | top height: mean height of the 100 largest-DBH trees per ha | m |
| BA | stand basal area per ha | m²/ha |
| β1–β5 | fixed parameters (Table 2) | – |
| b1i, b2i | plot random effects on β1 (asymptote) and β3 (shape) | – |
| σ²b1, σb1b2, σ²b2 | random-effect var–cov (Table 2) | – |
| σ² | residual variance (Table 2; used as R̂ = 1.2931·I in Table 3) | m² |

## Model form(s)

```
Eq. (9),  p.4 (PDF p2) [Chapman-Richards base, mixed]:
  H_ij = 1.3 + (β1 + b1i) · [1 − exp(−β2 · DBH_ij)]^(β3 + b2i) + ε_ij

Eq. (10), p.5 (PDF p3):
  H_ij = 1.3 + (β1 + β4·TopH_i + b1i) · [1 − exp(−β2 · DBH_ij)]^(β3 + b2i) + ε_ij

Eq. (11), p.5 (PDF p3):
  H_ij = 1.3 + (β1 + β4·TopH_i + β5·BA_i + b1i) · [1 − exp(−β2 · DBH_ij)]^(β3 + b2i) + ε_ij

Population ("typical", fixed only) prediction, p.7–8 (PDF p5–6): b = 0
  H = 1.3 + (β1 [+ β4·HTOP] [+ β5·BA]) · [1 − exp(−β2 · DBH)]^β3
```

- Note: eq. (9) is the plot-vector form of the scalar mixed Chapman-Richards
  eq. (8) (p.4, PDF p2); the paper uses "(9)" in Table 2 and thereafter.
- **Direction:** height from DBH (+ TopH, BA) only.
- H → 1.3 as DBH → 0 (β3 > 0). All Table 2 estimates for (9)–(11) are
  positive (verified on image).
- Other base models (5)–(7) in Table 2 are ranked worse and **not extracted**.

## DBH–height (hd) models only

- **Height convention:** total height; 1.3 m additive offset.
- **Fit type:** nonlinear mixed model, FO method (SAS %NLINMIX), plot random
  effects on β1 and β3, unstructured 2×2 D, R = σ²I (no heteroscedasticity
  found, p.5).
- **Coefficients for population prediction:** Table 2 mixed-model fixed
  effects with b = 0 — the only estimates published, and exactly what the
  paper uses for "fixed only"/H_fix predictions (p.7–8; Table 3).
- **Variance components transcribed:** Table 2 → CSV `var_b1`, `cov_b1_b2`,
  `var_b2`, `sigma2`. Not used in v1 (consistent with the other hd_ models).
  EBLUP (p.8, PDF p6): b̂ = D̂ Ẑ′(Ẑ D̂ Ẑ′ + R̂)⁻¹(H − H_fix), with
  der_b1 = [1 − exp(−β2 DBH)]^β3,
  der_b2 = (β1[+β4 TopH][+β5 BA])·ln[1 − exp(−β2 DBH)]·[1 − exp(−β2 DBH)]^β3;
  H_ss = H_fix + Ẑ b̂ (FO linearisation).
- **Back-transformation:** none.
- **Variants and selection (proposed):** inferred from supplied predictors,
  following `hd_eslamdoust2026`: none → `m9`; `HTOP` → `m10`;
  `HTOP` + `BA` → `m11`; `BA` without `HTOP` → error. `keep_model_id` flag.
  Registry: one row per variant (`huang2009_m9`, `_m10`, `_m11`). (⚠1)
- **Source species mappings:** none.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DBH | tree | cm | TRUE | "tree diameter at breast height (1.3 m above ground)" | Table 1 note p.12 (PDF p10); p.4 (PDF p2) |
| HTOP | TopH | plot | m | m10, m11 | "Top height is defined as the average height of the 100 largest DBH trees per ha" ("top height (i.e., mean height of the 100 largest DBH trees per ha)") | p.5 (PDF p3); p.4 (PDF p2) |
| BA | BA | plot | m²/ha | m11 | "basal area per ha (BA, m2/ha) … a simple and objective measure of the stand density"; minimum DBH and species basis **not stated** | p.5 (PDF p3) |

- ⚠2 `HDOM` vs a distinct name for TopH: implementation plan §3.4 lists
  `HDOM` as "per-model definition (top height, mean dom./co-dom., etc.)" and
  reserves a distinct name (`HDC`) for quantities that differ from top
  height. The source itself contrasts top height with dominant/co-dominant
  height "Hdc" and deliberately chose top height (pp.4–5).
  TopH *is* the classic top height → proposed `HDOM`; **decided `HTOP`**; the per-function
  doc states the 100-largest-DBH definition.
- ⚠3 BA: likely all live trees, all species, in the PSP (trees > 1.3 m tall
  were measured, p.4), but not stated. Document as "not stated".

### Applicability ranges

- Proposed: **fitting data** (Table 1) → `DBH_min/max` 1.3/39.4,
  `height_*` informational, `HTOP_min/max` 5.500/23.550 (m10, m11),
  `BA_min/max` 0.643/54.608 (m11). (⚠4: fit vs validation ranges.)

## Parameters

- **Source tables:** Table 2 p.13 (PDF p11): β1–β5, σ²b1, σb1b2, σ²b2, σ²,
  P, m, −2LL, AIC, BIC for models (9)–(11). Table 1 p.12 (PDF p10): ranges.
- **File:** `data-raw/Huang2009_hd_parameters.csv`, 3 rows (`variant` =
  m9/m10/m11) → proposed object `parameters_hd_Huang2009`.

| Variant | β1 | β2 | β3 | β4 | β5 | σ²b1 | σb1b2 | σ²b2 | σ² |
|---|---|---|---|---|---|---|---|---|---|
| m9 (9) | 25.4440 | 0.04734 | 1.0748 | – | – | 10.7688 | 0.2834 | 0.02546 | 1.2931 |
| m10 (10) | 9.1617 | 0.07668 | 1.2266 | 0.6745 | – | 0.8135 | 0.08974 | 0.03450 | 1.2864 |
| m11 (11) | 8.9909 | 0.07127 | 1.1617 | 0.6752 | 0.03388 | 1.1145 | 0.1277 | 0.02895 | 1.2923 |

- **Verification:** transcribed from the 300-dpi render
  (`tmp/huang2009hd/p-11.png`) and matched against the text layer: 0
  mismatches. Independent check: the m9 coefficients reproduce Table 3
  H_fix exactly (8.577, 10.738, 16.455), and the BLUP with Table 2 D, σ²
  reproduces b̂ = [−2.0180, 0.06947] and H_ss = 7.411 / 9.384 / 14.745.
- **Species → NFI mapping:** Black spruce → `PICE.MAR`.
- **⚠ Flagged values:** none illegible. Table 2 row label for the residual
  renders as "σ" but the note defines it as σ² (variance); confirmed by
  Table 3 (R̂ = 1.2931·I) → stored as `sigma2`.

## Benchmark plan

- **Fidelity (Tier 1):** Table 3 p.14 (PDF p12), DBH = 7.9, 10.7, 20.3 cm:

  | DBH | m9 H_fix | m10 H_fix | m11 H_fix |
  |---|---|---|---|
  | 7.9 | 8.577 | 8.497 | 8.454 |
  | 10.7 | 10.738 | 10.597 | 10.479 |
  | 20.3 | 16.455 | 15.467 | 15.244 |

  - m9: exact inputs → assert `tolerance = 5e-4` (3 dp).
  - m10/m11: the plot's TopH and BA are **not published**. Back-solved from
    the H_fix values: TopH = 14.50 m (consistent across the 3 trees to ±0.002),
    BA ≈ 7.75 m²/ha (7.738–7.758). With TopH = 14.5 m10 reproduces 8.497 /
    10.597 / 15.467. Proposal: test m10 with HTOP = 14.5 and m11 with
    HTOP = 14.5, BA = 7.75 at 3-dp tolerance (±0.002), documenting the
    back-solve in `tmp/generate_hd_huang2009_comparison_values.R`.
- **Plausibility (Tier 2):** monotone in DBH; H(0) = 1.3; ↑HTOP → ↑H
  (β4 > 0); ↑BA → ↑H (β5 > 0, p.7 "both positive").
- **Cross-model comparison:** `hd_huang2016` PICE.MAR (provincial and
  natural-subregion group incl. Lower Foothills) over DBH 5–35; report only.

## Open decisions for the reviewer

1. ⚠1 Include all three variants (m9/m10/m11), selected from supplied
   predictors? (proposed: yes)
2. ⚠2 TopH argument name: `HDOM` (proposed) vs distinct name.
3. ⚠3 BA threshold: document as not stated.
4. ⚠4 Applicability ranges: fitting-data Table 1 (proposed).
5. ⚠5 Eq. (26) H_max curve: not implemented (proposed); calibration (EBLUP)
   and ratio correction r: out of scope for v1 (r not published).
6. ⚠6 Registry: `scope = "regional"` (fit on Lower Foothills only) vs
   `"provincial"`; `province_scope = "AB"`; `stand_origin = "natural"`;
   `calibration_published = TRUE`; `equation_family = "chapman_richards"`
   (m9) / `"chapman_richards_stand"` (m10, m11).

## Implementation outcome (Stage 2)

- Function `hd_huang2009(DBH, species, HTOP = NULL, BA = NULL,
  keep_model_id = FALSE)`; object `parameters_hd_Huang2009`; registry rows
  `huang2009_m9`, `_m10`, `_m11` (regional, AB, natural, chapman_richards).
- Validation tier: **Tier 1 fidelity** (Table 3 H_fix). Max |diff|: m9
  < 0.0005 m (exact inputs); m10/m11 ≤ 0.002 m with back-solved
  HTOP = 14.5, BA = 7.75. Generator:
  `tmp/generate_hd_huang2009_comparison_values.R` →
  `tmp/hd_huang2009_comparison_values.csv`.
- Cross-check vs `hd_huang2016` PICE.MAR (m9, DBH 5–35): provincial
  −0.7 to +0.6 m; LF natural subregion −1.1 to +0.4 m (Huang 2009 lower above
  ~10 cm). Different data and fits; consistent shape and magnitude.
