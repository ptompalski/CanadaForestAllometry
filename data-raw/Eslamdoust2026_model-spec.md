# Model spec: hd_eslamdoust2026

> Human-reviewed extraction artifact (Stage 1). Page numbers are given as
> printed journal page and PDF page (printed = PDF + 215; PDF p1 is the T&F
> cover sheet). Items marked ⚠ need a reviewer decision.

## Review decisions (maintainer, 2026-10-29)

- Equations and coefficients: **approved**.
- ⚠1 La M5 α2: **use 0.09793** (printed 0.9793 treated as a misprint with a
  missing leading zero; evidence below). Flag the correction in the CSV
  `source_params`, this spec, and the roxygen `@details`, and in the tests
  (plausibility check against Fig. 3b). Contact the author to confirm.
- ⚠2 α3 = 0 "ns": **use α3 = 0 as published**. BA and TPH have no effect for
  La and Bd. M5 still **requires HDOM, BA and TPH together** for all species
  (one rule; NA in BA/TPH → NA height, propagated explicitly despite
  `NA^0 == 1`).
- ⚠3 By/He: **use M5 whenever stand variables are supplied**; document the
  authors' M4 note.
- ⚠4 SHT → **`HDOM`**; BA threshold and species basis documented as not
  stated; TPH threshold DBH ≥ 2.5 cm (Eq. 5).
- Registry: **two rows** (`eslamdoust2026_m4`, `eslamdoust2026_m5`),
  `provincial`, ON, `natural`.

## Source

- **Citation:** Eslamdoust, J., Vasiliauskas, S., Little, T., and Ashiq, M.W.
  2026. Height–diameter modeling of tree species in boreal and mixed forests
  using a mixed-effects approach and stand-level variables. *Forest Science and
  Technology* 22(1): 217–230. doi:10.1080/21580103.2025.2571222 (confirmed
  from rendered p.217 header; published online 15 Oct 2025; Open Access,
  CC BY-NC 4.0, © Crown Copyright).
- **BibTeX key:** @Eslamdoust2026 (proposed; not yet in `REFERENCES.bib`)
- **Document in `sources/`:** `sources/height-diameter/eslamdoust2026_fst_boreal_mixed_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_eslamdoust2026`
- **Plan reference:** `implementation_plan.md` §6 PR 3 (models.csv ids
  `eslamdoust2026_m4`, `eslamdoust2026_m5`).

## Feasibility triage — verdict: GO (one species-level gap, ⚠1)

| Check | Result |
|---|---|
| Direction | Height from DBH (M4) and DBH + stand variables (M5). ✔ |
| Model form | Eq. (4) and Eq. (5), p.220 (PDF p5), verified on rendered image. ✔ |
| Parameters present | Table 4 (M4) and Table 5 (M5), p.225 (PDF p10), all 8 species. ✔ (but see ⚠1: La M5 α2) |
| Units | H m, DBH cm, SHT m, BAH m² ha⁻¹, TPH n ha⁻¹ (p.219, p.220). ✔ |
| Height convention | Total height, additive 1.3 m offset in every model form. ✔ |
| Extra predictors defined | SHT (mean height of dominant and co-dominant trees), BAH, TPH (DBH ≥ 2.5 cm) — p.219, p.220. BA size threshold and species basis of SHT/BAH not stated (⚠4). |
| Fit type | `nlme` NLME, single random plot effect on α0; only mixed fixed effects published (no fixed-only fit coefficients). ✔ |
| Applicability ranges | Per species DBH, H, SHT, BAH, TPH mean (min–max), Table 1 p.219. ✔ |
| Validation | No worked example. Fig. 3a (M4 fixed response) readable → figure-read check. Fig. 3b (M5) not usable (stand values not stated). |
| Legibility | Text PDF; text layer OK for numbers but drops equation layout. Tables 1, 2, 4, 5 and Eqs. 1–5 read from 300-dpi renders. ✔ |

M1–M3 (Eqs. 1–3) have fit statistics only (Table 2) — no coefficients → not
implemented (`models.csv` `eslamdoust2026_m1_m3`, NO-GO).

## Scope / domain of applicability

- **Jurisdiction / region:** Ontario, boreal and mixed (Great Lakes–St.
  Lawrence) forests. OMNR growth-and-yield PSPs (6400 m²) and PGPs (400 m²)
  plus CFS NFI plots "distributed across Ontario" (p.219, p.226). Fig. 1
  (p.219) shows plots concentrated in central/southern Ontario for the
  hardwoods and hemlock; Pw, Cw, La extend into the boreal.
- **Species covered (NFI codes):** PINU.STR, THUJ.OCC, ACER.SAH, BETU.ALL,
  FAGU.GRA, LARI.LAR, TILI.AME, TSUG.CAN.
- **Stand origin:** "All species have natural origins" (Table 1 caption, p.219).
- **Plot selection:** BAH ≥ 5 m² ha⁻¹ (disturbance filter); species ≥ 3 % of
  plot BA; ≥ 3 stems of the species per plot (p.219). 48,454 trees, 1,867
  plots.
- **Valid input ranges:** Table 1 per species (see Applicability ranges).
- **Caveats from the source:**
  - "M4 may be considered a more parsimonious and operationally practical
    model in cases where stand-level variables are unavailable or unreliable
    or when SHT measurements/calculations are not reliably updated" (p.225).
  - Bias increased for DBH > 50 cm, particularly Pw, Mh, Be, He (p.225).
  - Repeated measurements treated as independent (autocorrelation ignored,
    p.220).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H_ij | total height of tree j in plot i | m |
| DBH_ij | diameter at breast height | cm |
| SHT_i | stand height: mean height of dominant and co-dominant trees | m |
| BAH_i | stand basal area | m² ha⁻¹ |
| TPH_i | trees per hectare with DBH ≥ 2.5 cm | n ha⁻¹ |
| α0…α4 | fixed-effect parameters (Tables 4, 5) | – |
| u_i | random plot effect on α0, u_i ~ N(0, var(u_i)) | – |
| σ²_e, σ²_u | residual and random-effect variances (Tables 4, 5) | – |

## Model form(s)

Read from rendered images (p.220, PDF p5).

```
M4, Eq. (4):
  H_ij = 1.3 + (α0 + u_i) · (1 − exp(−α1 · DBH_ij))^α2 + e_ij

M5, Eq. (5):
  H_ij = 1.3 + (α0 + u_i) · SHT_i^α1 · (1 − exp(−α2 · (TPH_i/BAH_i)^α3 · DBH_ij))^α4 + e_ij

Population prediction (v1): u_i = 0
  M4: H = 1.3 + α0 · (1 − exp(−α1 · DBH))^α2
  M5: H = 1.3 + α0 · SHT^α1 · (1 − exp(−α2 · (TPH/BAH)^α3 · DBH))^α4
```

- **Parameter roles differ between M4 and M5** (same symbols, different
  meaning): in M4 α1 is the rate and α2 the shape; in M5 α2 is the rate, α3
  the TPH/BAH exponent and α4 the shape (p.220). CSV columns `a0`–`a4` keep
  the source's symbols per variant.
- M5 is algebraically identical to Sharma & Parton (2007) Eq. (8)
  (α0↔θ, α1↔δ, α2↔β, α3↔φ, α4↔γ) → reuse existing `.hd_cr_stand()` in
  `R/hd_utils.R` (verified: `1.3 + a0 * HDOM^a1 * (1 - exp(-a2 * (TPH / BA)^a3 * DBH))^a4`).
- **Direction:** height from DBH (+ stand variables) only.
- H → 1.3 as DBH → 0 for every species/variant (all shape exponents > 0).

## DBH–height (hd) models only

- **Height convention:** total height; additive 1.3 m offset.
- **Fit type:** R `nlme::nlme()`; M5 start values from `nlsLM()` without
  u_i (p.220). Final parameters estimated on the full data set after 5-fold
  80/20 validation (p.220). No transformation, no weighting reported.
- **Coefficients used for population prediction:** Tables 4/5 fixed effects
  with u_i = 0. These are the only coefficients published for M4/M5 (M2, the
  fixed-only Chapman–Richards fit, has no published coefficients), so the
  §8.1 #10 choice does not arise. Supported by Fig. 3 caption: "fitted lines
  of fixed response model based on M4 and M5" (p.226).
- **Random-effects structure:** single random effect u_i on α0, grouping =
  plot (Eqs. 4–5). Residual e_ij independent; no autocorrelation.
- **Variance components transcribed:** Table 4/5 → CSV `sigma2_e`,
  `sigma2_u` (both variants). Not used in v1. Note σ²_u scale differs: M4
  u_i is additive on α0 in metres (σ²_u 4.2–18.1); M5 u_i is additive on α0,
  which multiplies SHT^α1 (σ²_u 0.02–0.26).
- **Back-transformation:** none.
- **Variants and how they are selected:** inferred from supplied predictors
  (plan §8.1 #11): `HDOM`, `BA`, `TPH` all `NULL` → M4; all supplied → M5;
  partial supply → abort (`.hd_check_predictor_group()`). Variant named via
  `inform_once()`; `keep_model_id = FALSE` argument (§8.1 #12).
- **Source species mappings:** none.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DBH | tree | cm | TRUE | "diameter at breast height (DBH – cm)"; "DBH_ij is diameter at breast height (cm) of jth tree in ith plot" | p.219 (PDF p4); p.220 (PDF p5) |
| HDOM | SHT | plot | m | FALSE (M5 group) | "stand height (mean height of dominant and co-dominant trees, SHT – m)"; Table 1: "SHT: mean stand height (m) based on dominant or codominant tree heights per plot"; Table 5: "SHT: stand height, referring to the height of dominant or codominant trees in each plot (m)" | p.219 (PDF p4); p.225 (PDF p10) |
| BA | BAH | plot | m²/ha | FALSE (M5 group) | "basal area (BAH – m² ha⁻¹)"; "BAH_i is basal area (m² ha⁻¹) of ith plot"; Table 5: "BAH: stand basal area (m² ha⁻¹)" | p.219; p.220; p.225 |
| TPH | TPH | plot | stems/ha | FALSE (M5 group) | "TPH_i is number of trees per hectare (n ha⁻¹) with DBH≥ 2.5cm of ith plot" | p.220 (PDF p5) |

- `HDOM` naming: SHT has the same definition as Sharma & Parton (2007) SHT,
  already mapped to `HDOM` (approved in that spec). Proposed: `HDOM`.
- ⚠4 Definitions not stated in the source: (a) BA tree-size threshold;
  (b) whether SHT and BAH are all-species or species-specific (wording
  "stand height"/"stand basal area" implies all species); (c) TPH threshold
  wording conflicts: "DBH≥ 2.5cm" (p.220, Eq. 5 definition) vs "DBH> 2.5cm"
  (p.222, Eq. 11–15 definitions). Proposed doc: quote Eq. 5 (≥ 2.5 cm), state
  BA threshold and species basis as not specified.

### Applicability ranges

- Table 1 (p.219, PDF p4), per species, fit data set (= full data set; final
  parameters fitted on all data, p.220): DBH, height, SHT, BAH, TPH mean
  (min – max). Image-verified incl. the Bd row (truncated in the text layer).
- CSV: `DBH_*`, `height_*` on both variants; `HDOM_*`, `BA_*`, `TPH_*` on M5
  rows only (empty on M4 rows so M4 never warns on stand variables).
- Proposal (§8.1 #8): one warning per call for rows outside DBH (both
  variants) or HDOM/BA/TPH (M5) ranges. Height ranges informational.
- Note Table 1 BAH min (5.04–14.61) reflects the BAH ≥ 5 filter.

## Parameters

- **Source tables:**
  - Table 4, p.225 (PDF p10): M4 α0, α1, α2, σ²_e, σ²_u (+ MAE/RMSE/rRMSE,
    not stored).
  - Table 5, p.225 (PDF p10): M5 α0–α4, σ²_e, σ²_u (+ fit stats and variable
    contributions, not stored).
  - Table 2, p.222 (PDF p7): AIC for M4, M5 (stored as `aic`).
  - Table 1, p.219 (PDF p4): n plots, n trees, means and ranges.
- **File:** `data-raw/Eslamdoust2026_hd_parameters.csv`, 16 rows
  (8 species × `variant` m4/m5) → proposed object
  `parameters_hd_Eslamdoust2026` (add to `internal_objs`).
- **Verification:**
  - Tables 1, 2, 4, 5 transcribed from 300-dpi renders (crops in
    `tmp/eslamdoust2026/t1.png`, `t2.png`, `t4.png`, `t5.png`; equations
    `eq_left.png`, `eq_right.png`; Fig. 3 `fig3.png`).
  - Automated match of every numeric CSV cell against the PDF text layer,
    including column-to-species order for every Table 4/5 parameter row:
    **0 mismatches**.
  - Image spot checks: Pw M4 α1 = 0.05122; Bd M4 α1 = 0.07795; Cw M5
    α3 = −0.1683; He M5 σ²_u = 0.0592; Bd Table 1 TPH 1630 (321 – 6091).
- **Species → NFI mapping** (Table 1, scientific names p.219; all codes
  exist in `species_code_lookup`):
  Pw *Pinus strobus* = PINU.STR; Cw *Thuja occidentalis* = THUJ.OCC;
  Mh *Acer saccharum* = ACER.SAH; By *Betula alleghaniensis* = BETU.ALL;
  Be *Fagus grandifolia* = FAGU.GRA; La *Larix laricina* = LARI.LAR;
  Bd *Tilia americana* = TILI.AME; He *Tsuga canadensis* = TSUG.CAN.

### ⚠ Flagged / uncertain values

- **⚠1 Tamarack (La) M5 α2 = 0.9793 — suspected misprint in Table 5.**
  Printed clearly (image and text layer agree), but:
  - It is 11–19× every other species' M5 α2 (0.0507–0.0908).
  - With α3 = 0 the curve saturates almost immediately: at Table 1 mean
    SHT = 14.02, H = 16.1 m at DBH 2.5 cm and 18.7 m from DBH ≈ 8 cm onward
    (La mean H is 11.70 m at mean DBH 12.93; min H 1.73 m).
  - The paper's own Fig. 3b La curve starts ≈ 4 m at DBH ≈ 2.5 and rises to
    ≈ 14.2 m at DBH 17 and ≈ 17.5 m at DBH 31. Achieving H(2.5) ≈ 4 m with
    α2 = 0.9793 would need SHT < 1 m (Table 1 min SHT 5.14).
  - A least-squares fit of α2 and α0·SHT^α1 to four Fig. 3b La points gives
    α2 ≈ 0.12 with implied SHT ≈ 13 m (close to the La mean) — consistent
    with a dropped/garbled digit, but the true value **cannot be recovered**
    from the figure.
  - Transcribed as printed; CSV `source_params` carries a WARN tag.
  - **Candidate correction α2 = 0.09793** (reviewer's hypothesis: missing
    leading zero). Behaviour at La Table 1 mean SHT = 14.02 (α3 = 0, so BA/TPH
    have no effect):

    | DBH | M5 (0.9793) | M5 (0.09793) | M4 |
    |---|---|---|---|
    | 2.5 | 16.14 | 2.45 | 3.82 |
    | 5 | 18.51 | 4.52 | 6.33 |
    | 10 | 18.74 | 8.84 | 10.34 |
    | 12.93 (mean) | 18.74 | 10.97 | 12.10 |
    | 20 | 18.74 | 14.60 | 14.98 |
    | 30 | 18.74 | 17.13 | 17.02 |
    | 40 | 18.74 | 18.12 | 17.90 |

    - With 0.09793, fitting only SHT to the four Fig. 3b La points gives
      SHT = 14.72 (Table 1 mean 14.02), RMSE 0.82 m (coarse reads); with
      0.9793 the best is SHT = 8.13, RMSE 4.94 m.
    - Stage 2 re-digitization (full-resolution line, 9 points DBH 8–37):
      0.09793 → best SHT 15.2, RMSE 0.93 m (max 1.7 m at DBH 8); 0.9793 →
      RMSE 3.15 m. A free fit of α2 gives ≈ 0.126 (RMSE 0.28 m). Control
      species basswood (α3 = 0, unambiguous α2 = 0.0883): free fit 0.0765,
      published value RMSE 0.61 m. Fig. 3b therefore recovers α2 only to
      ±15–30 %; 0.09793 lies within that noise, 0.9793 does not.
    - Mean-DBH prediction 10.97 m vs Table 1 mean height 11.70 m; monotone in
      DBH; tracks M4 within 0.4 m for DBH ≥ 20.
    - Conclusion: 0.09793 is consistent with the paper's figure and data;
      0.9793 is not. Still an **inferred** value, not published.
- **⚠2 α3 = "0 ns" for La and Bd (Table 5).** Caption: superscript ns =
  "variables associated with the parameter is/are not statistically
  significant"; variable contributions for BAH/TPH shown as "–" for La, Bd;
  text p.223/p.226: "BAH and TPH were not significant for tamarack and
  basswood" / "α3 ... was not significant for tamarack and basswood". Stored
  as `a3 = 0`, `a3_ns = TRUE`. With α3 = 0, (TPH/BA)^0 = 1, so BA and TPH
  have **no effect** for these species. Note for Stage 2: in R, `NA^0 == 1`,
  so NA in BA/TPH must be propagated explicitly (§8.1 #14).
  - **Answer to reviewer ("can they be ignored?")**: effectively yes, but
    the table already does this. "ns" means the estimate was not
    significantly different from zero. The authors report α3 as exactly 0
    and show "–" for the BAH/TPH contributions. That indicates the term was
    dropped and the other La/Bd coefficients were estimated without it.
    With α3 = 0, (TPH/BA)^0 = 1, so M5 for La and Bd reduces to
    H = 1.3 + α0·SHT^α1·(1 − exp(−α2·DBH))^α4. Setting α3 = 0 is therefore
    the faithful choice. Dropping a *non-zero* ns coefficient would not be
    faithful, because the other coefficients were fitted with it, but that
    case does not arise here.
  - Consequence for the API: BA and TPH do not change La/Bd heights. HDOM is
    still required. Whether BA/TPH must still be supplied is the open
    sub-question.
- **⚠3 Minor internal inconsistencies (not affecting coefficients):**
  - Text (p.222, p.225) says M4 had lower AIC than M5 for By and He; Table 2
    shows He M4 3954 < M5 3971 ✔ but By M4 7977 > M5 7969 ✗. Table 3 log-
    likelihoods (LL M5 < LL M4 for "Yb" and He; LR negative) support the
    text. Table 3 also labels By as "Yb".
  - Text p.222 lists "yellow birch, basswood, and eastern hemlock" as better
    with M4; p.223 says M4 vs M5 not significant for Bd (p = 0.1428).
- Sign of α3: negative for Pw, Cw, He (higher TPH/BA → slower rate → shorter
  at a given DBH); positive for Mh, By, Be. Opposite in sign to Sharma &
  Parton (all φ > 0) for three species. Transcribed as printed; source
  behaviour, not an error. Magnitude of effect is tiny (BAH ≤ 0.36 %,
  TPH ≤ 0.01 % contribution, Table 5).

## Benchmark plan

- **Fidelity (Tier 1) exact reference values:** none (no worked example or
  prediction table).
- **Figure-read check (weak Tier 1) — M4 only:** Fig. 3a p.226 (PDF p11) is
  the M4 fixed response (u = 0) vs DBH; no stand inputs needed. Preliminary
  coarse pixel reads (7.4 px/cm, 17.93 px/m) vs M4 predictions:

  | Species | DBH | Fig. 3a read | M4 pred |
  |---|---|---|---|
  | Pw | 60.5 | 26.4 | 26.48 |
  | Cw | 44.1 | 15.1 | 15.20 |
  | Mh | 63.2 | 24.4 | 24.62 |
  | La | 16.9 | 13.9 | 13.91 |
  | La | 31.4 | 16.8 | 17.19 |
  | Bd | 23.4 | 20.0 | 19.58 |
  | He | 31.6 | 18.2 | 18.72 |
  | He | 60.8 | 22.5 | 22.92 |

  (By, Be markers overlap other curves; excluded.)
  **Implemented:** 24 points re-digitized at full resolution (marker
  centroids + unobstructed line positions; DBH 0 → px 445.5, 80 → px 1179;
  H 0 → px 1000.5, 30 → px 334) in
  `tmp/generate_hd_eslamdoust2026_comparison_values.R`. Pw, Cw, Mh, La, Bd
  within 0.51 m; the He curve lies a consistent 0.4–0.6 m below the Table 4
  prediction along its whole length (max 0.62 m), so the test tolerance is
  **0.75 m**. Swapped or mistyped coefficients would differ by metres.
- **M5:** no benchmark. Fig. 3b curves are jagged (apparently drawn through
  plot-specific SHT/BAH/TPH) and do not match predictions at Table 1 means
  (e.g. Cw −2.1 m, By +2.2 m) → not usable. Only use of Fig. 3b: evidence
  for ⚠1.
- **Plausibility (Tier 2):**
  - Monotone increasing in DBH; H(DBH → 0) → 1.3 (both variants).
  - ↑HDOM → ↑height (α1 > 0 all species).
  - TPH/BA response sign follows α3 (species-dependent; zero for La, Bd).
  - M4 vs M5 at Table 1 mean stand values agree within ~3 m for 7 species
    (DBH 2.5–50; largest By, M4 20.6 vs M5 23.3 at 50 cm). With the
    corrected La α2 = 0.09793, La M5 also agrees with M4 (within 1.9 m;
    within 0.4 m for DBH ≥ 20). Test: Fig. 3b La line at HDOM = 15.2,
    RMSE < 1 m; La M5 H(2.5) < 5 m at Table 1 means.
- **Cross-model comparison:** only overlap with implemented models is
  LARI.LAR in `hd_huang2016` (AB, LT). No overlap with
  `hd_sharmaparton2007`. Report La M4 vs `hd_huang2016` LT; do not assert.
- ⚠ M5 ships with **no source benchmark** (plausibility only); M4 with a
  figure-read check only.

## Implementation notes for Stage 2 (proposals)

- Signature (plan §3.2 + §8.1 #12):
  `hd_eslamdoust2026(DBH, species, HDOM = NULL, BA = NULL, TPH = NULL, keep_model_id = FALSE)`;
  returns `tibble(height)` (+ `model_id` when requested).
- Validation: DBH ≥ 0 (as in `hd_sharmaparton2007`); HDOM, BA, TPH > 0;
  NA → NA height; species outside the 8 aborts with supported list.
- Evaluation: M4 inline (or a small `.hd_cr()` helper only if another model
  needs it); M5 via existing `.hd_cr_stand()`.
- Registry (plan §3.5): two rows,
  - `eslamdoust2026_m4`: required DBH; `equation_family = "chapman_richards"`.
  - `eslamdoust2026_m5`: required DBH, HDOM, BA, TPH;
    `equation_family = "chapman_richards_stand"`.
  - Both: `engine = "hd_eslamdoust2026"`, `reference = "@Eslamdoust2026"`,
    `scope = "provincial"`, `province_scope = "ON"`,
    `stand_origin = "natural"`, `calibration_published = TRUE`,
    `params_key = "parameters_hd_Eslamdoust2026"`, `fixed_args` = variant.
