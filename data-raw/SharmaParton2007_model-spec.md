# Model spec: hd_sharmaparton2007

> Human-reviewed extraction artifact (Stage 1). Page numbers are given as
> printed journal page and PDF page (printed = PDF + 186). Items marked ⚠ need
> a reviewer decision.

## Review decisions (maintainer, 2026-10-29)

- Equations and coefficients: **approved**.
- ⚠1 Population coefficients: **Method 2** fixed effects with u_i = 0.
- ⚠2 SHT argument name: **`HDOM`**.
- ⚠3 BA threshold: document as not stated in source.
- ⚠4 DBH applicability ranges: **fit (model) data set** (Table 1).
- ⚠6 Registry `stand_origin`: **`"both"`**.
- Shared evaluator `.hd_cr_stand()` in `R/hd_utils.R`: **yes** (reuse in
  Eslamdoust 2026 M5).

## Source

- **Citation:** Sharma, M., and Parton, J. 2007. Height–diameter equations for
  boreal tree species in Ontario using a mixed-effects modeling approach.
  *Forest Ecology and Management* 249(3): 187–198.
  doi:10.1016/j.foreco.2007.05.006 (confirmed from rendered p.187 header).
- **BibTeX key:** @SharmaParton2007 (proposed; not yet in `REFERENCES.bib`)
- **Document in `sources/`:** `sources/height-diameter/Sharma and Parton - 2007 - Height–diameter equations for boreal tree species in Ontario using a mixed-effects modeling approach.pdf`
- **Model family:** hd
- **Target function:** `hd_sharmaparton2007`

## Feasibility triage — verdict: GO

| Check | Result |
|---|---|
| Direction | Height from DBH (+ stand variables). ✔ |
| Model form | Eq. (6) p.190 / Eq. (8) p.192, verified on rendered images. ✔ |
| Parameters present | Table 6 p.195, all 8 species, both fit methods. ✔ |
| Units | H m, D cm, BA m²/ha, TPH trees/ha, SHT m (pp.188, 190). ✔ |
| Height convention | Total height, 1.3 m additive offset ("total height equals breast height (1.3 m) when DBH = 0", p.190). ✔ |
| Extra predictors defined | SHT, BA, TPH defined pp.188, 190 (BA size threshold not stated — see ⚠3). ✔ |
| Fit type | NLMIXED; Method 1 (no RE) and Method 2 (random asymptote). ✔ |
| Applicability ranges | Per species DBH/H (Table 1); plot ranges pooled (text p.188). ✔ |
| Validation | No worked example; Fig. 2 (p.196) is readable → figure-read check + plausibility. |
| Legibility | Text PDF; text layer drops minus signs and Greek letters, so equations/tables were read from 300-dpi renders. ✔ |

## Scope / domain of applicability

- **Jurisdiction / region:** northern Ontario boreal forest ("commercially
  operable forest of northern Ontario", p.188). FESC + OMNR permanent growth
  plots, 400 m², 5498 plots, mostly measured once.
- **Species covered (NFI codes):** ABIE.BAL, POPU.BAL, PICE.MAR, PINU.BAN,
  PINU.RES, POPU.TRE, BETU.PAP, PICE.GLA.
- **Valid input ranges:**
  - Plot level, pooled over all plots (p.188): BA 5.03–85.32 m²/ha;
    TPH 50–17,625 trees/ha; SHT 3.80–33.10 m.
  - Tree level, per species: Table 1 p.189 (model/fit data set) → CSV
    `DBH_*`, `height_*`. DBH min is 2.50 cm for every species.
- **Stand origin:** tree-level origin (natural/planted) was recorded (p.188),
  but the paper does not say whether planted trees were excluded. Registry
  `stand_origin` → ⚠ proposed `"natural"`? see ⚠6.
- **Caveats from the source:**
  - Population ("fixed-effects response") prediction sets the random
    parameter to zero when no local heights are available (§2.4, p.191).
  - Prediction accuracy showed no trend across species proportion of plot BA
    (p.195): "the species-specific height–diameter relationship is
    independent of the species sampled to calculate dominant stand height".

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H, h_ij | total tree height | m |
| D, D_ij | diameter at breast height (DBH) | cm |
| SHT | dominant stand height | m |
| BA | stand basal area | m²/ha |
| TPH | stand density | trees/ha |
| θ, δ, β, φ, γ | fixed parameters (Table 6) | – |
| u_i | random plot×species effect on the asymptote, u_i ~ N(0, τ) | m-scale multiplier |
| σ²_e, σ²_u | residual and random-effect variances (Table 6) | – |

## Model form(s)

Read from rendered images (text layer omits "−" and Greek letters).

```
Eq. (6), p.190 (fixed form):
  H = 1.3 + α · SHT^δ · (1 − exp(−β · (TPH/BA)^φ · D))^γ

Eq. (8), p.192 (mixed form, α = θ + u_i):
  h_ij = 1.3 + (θ + u_i) · SHT_i^δ · (1 − exp(−β · (TPH_i/BA_i)^φ · D_ij))^γ + e_ij
  u_i ~ N(0, τ),  e_ij ~ N(0, σ²_e),  D = σ²_u (scalar)

Population prediction (v1): u_i = 0
  H = 1.3 + θ · SHT^δ · (1 − exp(−β · (TPH/BA)^φ · D))^γ
```

- Table 6 column headers are θ, δ, β, φ, γ for both methods (so α ≡ θ).
- All Table 6 estimates are positive (verified on image; no hidden minus
  signs).
- **Direction(s):** height from DBH + SHT + BA + TPH only.
- Asymptote = 1.3 + θ·SHT^δ; rate = β·(TPH/BA)^φ (TPH/BA "analogous to
  quadratic mean diameter", p.190).
- H → 1.3 as D → 0 for all species (γ > 0).

## DBH–height (hd) models only

- **Height convention:** total height; 1.3 m additive offset.
- **Fit type:** SAS NLMIXED (Methods 1 and 2; Method 1 estimates identical to
  NLIN, p.193). No weighting, no transformation.
- **Coefficients used for population prediction — ⚠1 (decision needed):**
  - **Proposed: Method 2** fixed effects with u_i = 0.
  - Evidence:
    1. §2.4 p.191: "height can be predicted (i) by assuming the random
       parameters are zero if no local height information is available
       (fixed-effects response)" — i.e. the mixed model with u = 0.
    2. p.194: validation "fixed-effects response" = Eq. (8) "ignoring the
       random parameter"; Table 7 "Fixed" column is this response.
    3. Fig. 2 ("Eq. (8) without the random parameter", p.196) numerically
       matches **Method 2**, not Method 1 (pixel-calibrated, 20.68 px/m, at
       DBH = 40, BA = 25, TPH = 2500, SHT = 20):

       | Species | Fig. 2 read | Method 2 | Method 1 |
       |---|---|---|---|
       | Balsam fir | ≈20.1 | 20.10 | 21.16 |
       | White birch | ≈19.5 | 19.51 | 20.70 |
       | White spruce (top curve) | ≈23.4 | 23.40 | 23.68 |
       | Jack pine at DBH 2 (top) | ≈6.1 | 6.12 | 5.79 |
  - Plan §8.1 #10 says follow the source's stated recommendation; the source
    has no explicit "use Method X" sentence, but its prediction section and
    its own figure use Method 2. Method 1 (pure fixed-effects fit) remains the
    fallback default if the reviewer reads this as "no recommendation".
  - Both methods are stored in the CSV (`method` = 1/2), so the choice is a
    one-line filter.
- **Random-effects structure:** single random effect u_i on the asymptote
  parameter, grouping = plot × species combination (p.192). Within-plot
  covariance σ²_e·I; no autocorrelation (p.193).
- **Variance components transcribed:** Table 6 → CSV `sigma2_e` (both
  methods), `sigma2_u` (Method 2). Not used in v1. EBLUP for later
  calibration: Eq. (7) p.191, b̂_i = D Ẑᵢᵀ (R̂ᵢ + Ẑᵢ D Ẑᵢᵀ)⁻¹ êᵢ with D = σ²_u,
  R̂ᵢ = σ²_e I.
- **Back-transformation:** none.
- **Variants:** none (single form; species selects the row).
- **Source species mappings:** none.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | D | tree | cm | TRUE | "D = diameter at breast height (DBH) (cm)" | p.190 (PDF p4) |
| HDOM | SHT | plot | m | TRUE | "The mean height of sampled dominant and codominant trees in a plot, irrespective of species, was used as the dominant stand height for the plot." Also: "stand dominant height (calculated from a sample of dominant and codominant trees with measured heights)" | p.190 (PDF p4); p.188 (PDF p2) |
| BA | BA | plot | m²/ha | TRUE | "total tree basal area (BA/ha)"; "BA = stand basal area (m²/ha)" | p.188 (PDF p2); p.190 (PDF p4) |
| TPH | TPH | plot | trees/ha | TRUE | "number of stems (N/ha, calculated from the trees with DBH greater than or equal to 2.5 cm)"; "TPH = stand density (trees/ha)" | p.188 (PDF p2); p.190 (PDF p4) |

- `HDOM` naming: SHT is mean height of dominant + codominant trees (all
  species), which fits the shared `HDOM` meaning per plan §3.4 (per-function
  doc states the definition). ⚠2 confirm `HDOM` vs a distinct name.
- ⚠3 BA: no DBH threshold is stated for BA ("total tree basal area"); likely
  the same ≥2.5 cm trees and all species, live only, but **not stated**. Doc
  will quote the source and say the threshold is not specified.

### Applicability ranges

- `DBH_min/max`, `height_min/max` per species: Table 1 **model (fit) data
  set** rows. ⚠4 The validation set sometimes extends further (e.g. aspen
  DBH max 64.80 vs 62.20; balsam poplar height 2.50 vs 2.59). Proposal: use
  fit-set ranges.
- `BA_min/max`, `TPH_min/max`, `HDOM_min/max`: pooled plot ranges (same for
  every species): 5.03–85.32, 50–17625, 3.80–33.10.
- Proposal (plan §3.4 #9, §8.1 #8): one warning per call listing rows outside
  `[DBH_min, DBH_max]`, BA, TPH or HDOM ranges. Height ranges informational.

## Parameters

- **Source tables:**
  - Table 6, p.195 (PDF p9): θ, δ, β, φ, γ, σ²_e, σ²_u, AIC for Methods 1
    and 2. Standard errors not transcribed.
  - Table 1, p.189 (PDF p3): n trees, DBH and height mean/SD/min/max (model
    data set).
  - Text p.188 (PDF p2): plot-level ranges.
- **File:** `data-raw/SharmaParton2007_hd_parameters.csv`, 16 rows
  (8 species × 2 methods). → proposed object `parameters_hd_SharmaParton2007`.
- **Verification:**
  - Table 6 transcribed from a 300-dpi render (crops in
    `tmp/sharmaparton2007/t6_left.png`, `t6_right.png`, `t6_ws.png`).
  - Automated match of every CSV value against the PDF text layer:
    **0 mismatches** (16 Table 6 rows incl. row-to-species order; 8 Table 1
    DBH + height rows).
  - Spot checks on image: BF M2 θ = 4.7942; PJ M2 σ²_u = 0.0026; SW M1
    θ = 9.4916; RP M1 δ = 1.0165.
- **Species → NFI mapping:** Balsam fir = ABIE.BAL, Balsam poplar = POPU.BAL,
  Black spruce = PICE.MAR, Jack pine = PINU.BAN, Red pine = PINU.RES,
  Trembling aspen = POPU.TRE, White birch = BETU.PAP, White spruce = PICE.GLA.
  Latin names confirmed on p.188.
- **⚠ Flagged values:**
  - ⚠5 Jack pine Method 2 σ²_u = 0.0026 with SE printed "(0.00001)" —
    transcribed as shown; SE implausibly small but SEs are not stored.
  - Balsam fir Method 2 γ prints "1.5166 0.0389)" (missing "(" — typo in
    SE only; estimate 1.5166 unaffected).
  - No illegible values.

## Benchmark plan

- **Fidelity (Tier 1): none exact.** No worked numeric example or prediction
  table in the paper.
- **Figure-read check (weak Tier 1, stated tolerance):** Fig. 2 p.196 —
  predicted H vs DBH 2–40 cm at BA = 25, TPH = 2500, SHT = 20, all 8
  species. Proposal: digitize endpoints (DBH 2 and 40) per species where
  markers are separable and assert |pred − read| ≤ 0.5 m; store readings in
  `tmp/generate_hd_sharmaparton2007_comparison_values.R` → CSV. Overlapping
  markers (AT/BP/SB/RP cluster near 22–23 m) are read only where
  unambiguous. Also confirms the Method 1 vs 2 choice.
- **Plausibility (Tier 2):**
  - Monotone increasing in DBH; H(DBH→0) → 1.3; asymptote 1.3 + θ·SHT^δ.
  - ↑HDOM → ↑height (δ > 0 all species).
  - ↑TPH/BA → ↑height at a given DBH (φ > 0 all species) — matches the
    source's interpretation (denser/smaller-QMD stands, trees taller for DBH).
  - Text checks (p.195): with Fig. 2 inputs, SW shortest then BF and SB up to
    ≈22 cm; BW shortest then BF at ≈32 cm; at 40 cm SW tallest, then SB.
    (Computed M2: at 40 cm SW 23.40 > SB 23.12 ✔; at 10 cm SW 9.83 < BF 10.58
    < SB 11.94 ✔.)
- **Cross-model comparison:** `hd_huang2016` (AB, DBH-only, provincial) for
  ABIE.BAL, POPU.BAL, PICE.MAR, PINU.BAN, POPU.TRE, BETU.PAP, PICE.GLA over
  DBH 5–40 at typical stand values; report discrepancies, do not assert.
- ⚠ Ships with **no exact source benchmark** (figure-read + plausibility).

## Implementation notes for Stage 2 (proposals)

- Signature (plan §3.2): `hd_sharmaparton2007(DBH, species, HDOM, BA, TPH)`;
  returns `tibble(height)`. No variants → no `keep_model_id`.
- Validation: DBH > 0; HDOM > 0; BA > 0; TPH > 0 (TPH/BA must be finite);
  NA predictors → NA height (§8.1 #14).
- Shared evaluator: Eslamdoust 2026 M5 uses the identical form
  (a0·SHT^a1·(1 − exp(−a2·(TPH/BAH)^a3·D))^a4). Proposal: add
  `.hd_cr_stand()` to `R/hd_utils.R` now and reuse in PR 3.
- Registry: `model_id = "sharmaparton2007"`, `equation_family =
  "chapman_richards_stand"`, `province_scope = "ON"`, `scope = "regional"`
  (northern ON boreal), `calibration_published = TRUE`,
  `params_key = "parameters_hd_SharmaParton2007"`.

## Implementation outcome (Stage 2)

- Validation tier: **figure-read + plausibility; no exact source benchmark.**
- Fig. 2 digitized readings (BF@40 20.13, BW@40 19.55, SW@40 23.45, PJ@2 6.10)
  vs Method 2 predictions: max |diff| 0.046 m (tolerance 0.5 m).
- Text-order checks (p.195) pass at 10 cm and 40 cm.
- DBH validated as `>= 0` (H = 1.3 m at DBH 0), consistent with `hd_huang2016`.
- Cross-check vs `hd_huang2016` (BA 25, TPH 1500, DBH 5–40): mean difference
  within about ±2.5 m at HDOM 20 for most species (PJ +1.9, SW −2.3). Differences
  scale with HDOM, as expected: −2 to −4 m at HDOM 15, +2 to +6 m at HDOM 25.
  These reflect different regions and predictors, not a defect.
