# Model spec: hd_auger2025

> Human-reviewed extraction artifact (Stage 1). The page numbers are the
> printed "n/31" page numbers of SSS-10, which match the PDF page numbers.
> Every equation and coefficient was read from 300-dpi renders (`pdftoppm`)
> and checked against the PDF text layer. The text layer garbles the math
> glyphs but has the numbers right. Reviewer decisions are recorded under "Review decisions (maintainer)".

## Source

- **Citation (recommended citation, p.29):**
  - Auger, I., J. DeBlois and M.-C. Lambert, 2025. *Relations
    hauteur-diamètre, tarifs de cubage et équations de défilement pour
    l'épinette noire et le pin gris en plantation au Québec.*
  - Gouvernement du Québec, ministère des Ressources naturelles et des Forêts,
    Direction de la recherche forestière. Avis technique SSS-10. 31 p.
  - <https://doi.org/10.82204/dcwg-5v65>
- **BibTeX key:** @Auger2025 (proposed; no clash in `REFERENCES.bib`)
- **Document in `sources/`:**
  `sources/height-diameter/auger2025_sss10_qc_plantation_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_auger2025`
- **Plan reference:** `implementation_plan.md` §6 **PR 5**. The models.csv ids
  are `auger2025_korf`, `auger2025_dhpm` and `auger2025_hdom`. The request
  called this "PR 1", but §6 lists PR 1 as `hd_huang2016`, and PRs 0–4 are
  already committed on `feature/hd-models`.
- **Out of scope:** the same report also contains volume tables (eqs 15–16)
  and taper equations. They are not part of this PR.

## Feasibility triage: verdict **GO**

| Check | Result |
|---|---|
| Direction | Height from DBH. ✓ |
| Model form | Eqs 10–14 are explicit (pp.11, 14, 16) and match the Tableau 3 candidate forms. ✓ |
| Parameters | Tableaux 7–9 give all coefficients for both species. ✓ |
| Units | H in m, DHP in cm (eq. 1 text p.5); DMOY, DDOM in cm; HDOM in m (Tableau 1). ✓ |
| Height convention | Total height, with an explicit `1,3 +` offset in every form. ✓ |
| Extra predictors | DMOY, HDOM and DDOM are defined in the text (pp.6–7, p.2), with one interpretation gap (resolved by review decision 2). |
| Fit type | Fixed-effects NLS fitted with NLMIXED. A plot random effect "n'améliorait pas l'ajustement", so none was included (pp.5–7). There is no population-vs-mixed choice to make. ✓ |
| Ranges | Tableau 1 (calibration) and Tableau 2 (validation), p.4. ✓ |
| Validation | No worked example or table of predictions, so Tier 2 only (see the benchmark plan). |
| Legibility | Text PDF; tables are clean in the renders. ✓ |

## Scope / domain of applicability

- **Jurisdiction / region:** Québec (QC), **plantations only**. The data come
  from monospecific EPN and PIG plantations in the DRF "réseau de suivi des
  effets réels des plantations" (p.2, Fig. 1).
- **Species:** PIG = *Pinus banksiana* → `PINU.BAN`; EPN = *Picea mariana* →
  `PICE.MAR` (p.1).
- **Data (p.2–3):**
  - Plots are 400 m².
  - From 2005, 14 sample trees were measured per plot: the 8 tallest planted
    dominant trees, well distributed in the plot (200/ha), plus 6 random
    planted trees (2 codominant, 2 intermediate, 2 suppressed).
  - Before 2005 (1995–2004), all live planted trees were measured.
  - Felled taper-study trees from 5–42-year-old plantations were added to the
    calibration set.
  - Calibration used one random measurement per tree: PIG 3,735 trees on 512
    plots; EPN 6,160 trees on 509 plots.
- **Calibration ranges (Tableau 1, p.4):**

  | Sp | Variable | n | mean | min | max |
  |---|---|---|---|---|---|
  | PIG | H (m) | 3735 | 7.4 | 1.3 | 25.0 |
  | PIG | DHP (cm) | 3735 | 9.3 | 0.3 | 34.8 |
  | PIG | DHP moyen (cm) | 837 | 7.3 | 0.1 | 25.7 |
  | PIG | DHP dominants (cm) | 1086 | 10.2 | 0.9 | 30.9 |
  | PIG | Hauteur dominante (m) | 1086 | 8.0 | 1.6 | 23.8 |
  | EPN | H (m) | 6160 | 5.4 | 1.3 | 18.4 |
  | EPN | DHP (cm) | 6160 | 6.9 | 0.2 | 27.0 |
  | EPN | DHP moyen (cm) | 1607 | 5.4 | 0.0 | 18.5 |
  | EPN | DHP dominants (cm) | 1664 | 8.1 | 0.1 | 23.4 |
  | EPN | Hauteur dominante (m) | 1664 | 6.3 | 1.2 | 17.8 |

  For DHP moyen, DHP dominants and hauteur dominante, n counts plot
  measurements. The DHP moyen n is smaller because the full tree list is not
  always available for the peripheral taper-study trees (Tableau 1 note †).
- **Validation ranges (Tableau 2, p.4), recorded for information only:**
  - PIG: H 1.3–18.3, DHP 0.3–25.7, DHP moyen 0.0–19.5, DDOM 0.2–23.8,
    HDOM 1.2–17.7.
  - EPN: H 1.3–15.8, DHP 0.1–22.6, DHP moyen 0.0–16.0, DDOM 0.1–21.2,
    HDOM 0.9–14.9.
- **Source caveats:**
  - Variant 1 (DBH only) "estime la hauteur moyenne de la population pour un
    DHP donné". Used as a plot-specific curve, its shape is "fort improbable",
    and the generalized forms are more appropriate for that (p.4–5).
  - The authors chose Korf partly for its "comportement en extrapolation"
    (p.11).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | total tree height | m |
| DHP | DBH, measured at 1.3 m | cm |
| DMOY | DHP moyen of all trees of the plot | cm |
| HDOM | mean height of the plot's dominant trees | m |
| DDOM | mean DHP of the same dominant trees | cm |

## Model form(s)

All forms are verified from the images: eq. 10 p.11; eqs 11–12 p.14; eqs
13–14 p.16. Tableau 3 (p.6) gives the base forms.

```
Variant "dbh" (eq. 10, Korf; both species):
  H = 1.3 + a1 * exp(-a2 * DBH^(-a3))

Variant "plot_mean_dbh":
  PIG (eq. 11, Chapman-Richards):
    H = 1.3 + (a10 + a11*DMOY) * (1 - exp(-(a20 + a21*DMOY) * DBH))^a3
  EPN (eq. 12, Hossfeld II):
    H = 1.3 + (a10 + a11*DMOY) * DBH^a3 / ((a20 + a21*DMOY) + DBH^a3)

Variant "dominant":
  PIG (eq. 13, Chapman-Richards):
    H = 1.3 + (a10 + a11*HDOM) * (1 - exp(-(a20 + a22*DDOM) * DBH))^a3
  EPN (eq. 14, Hossfeld II):
    H = 1.3 + (a10 + a11*HDOM) * DBH^a3 / ((a20 + a22*DDOM) + DBH^a3)
```

- **Direction:** height from DBH plus plot predictors. There is no inversion.
- **Parameter naming:** follows the general eqs 2–3 (pp.6–7):
  - In the plot-mean-DBH variant, α1 = α10 + α11·DMOY and α2 = α20 + α21·DMOY.
  - In the dominant variant, α1 = α10 + α11·HDOM + α12·DDOM and
    α2 = α20 + α21·HDOM + α22·DDOM.
  - The AIC simplification kept only one of HDOM or DDOM per parameter (p.7).
    The published result is α1 on HDOM (α11) and α2 on DDOM (α22). α12, α21
    (dominant), α30, α31 and α32 were dropped, and α3 is a constant.
- **Limits:**
  - All forms give H → 1.3 as DBH → 0.
  - Korf is checked numerically: PIG gives 1.31 m at DBH 0.1 cm.
- **Huge Korf α1:** the asymptotes are PIG 1,630,094 m (SE 3,805,823) and EPN
  101,489 m (SE 92,640). Within the data the curve is close to a power curve;
  it is unbounded in practice. Printed digits are integers, so this is
  faithful. Out-of-range DBH is covered by the 8.1 #8 warning.
- **Sign checks:**
  - PIG eq. 13: α2 = 0.1944 − 0.0031·DDOM becomes ≤ 0 at DDOM ≥ 62.7 cm, which
    is far beyond the maximum of 30.9.
  - Eq. 13/14: α1 = α10 + α11·HDOM is < 0 for HDOM < 0.209 m (PIG) and
    < 0.265 m (EPN), below every observed HDOM (heights stay > 1 m).
  - Implementation: inputs that make PIG eq. 13 undefined (DDOM ≥ 62.7 cm)
    abort with an informative error; other out-of-range inputs are computed
    with the range warning.

## DBH–height (hd) models only

- **Height convention:** total height. The breast-height offset is 1.3 m,
  added explicitly in every form.
- **Fit type:** weighted fixed-effects NLS (SAS NLMIXED, no random effects):
  - eq. 1 (Korf): residual variance σ²/poids, with
    poids = 1/(p1·DHP + p2·DHP² + p3·DHP³).
  - eq. 2 (DMOY): σ²·DHP^γ.
  - eq. 3 (dominant): homogeneous σ².
- **Coefficients for population prediction:** Tableaux 7–9 as published.
  These are the only fit, so 8.1 #10 needs no choice.
- **Random-effects structure:** none (p.5, p.6, p.7).
- **Variance components transcribed** (not used in v1):
  - `sigma2` for every row.
  - `p1`, `p2`, `p3` for the Korf rows (Tableau 7).
  - `gamma` for the DMOY rows (Tableau 8).
- **Back-transformation:** none (the response is untransformed H).
- **Variants:** an explicit `variant` argument (8.1 #11, which names SSS-10
  explicitly), because the variants need disjoint inputs.
- **Source species mappings:** none.

### Predictor table

Argument names per review decision 1.

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DHP | tree | cm | TRUE | « diamètre à hauteur de poitrine (mesuré à 1,3 m, DHP) »; « le DHP (cm) de l'arbre j de la placette i » | p.2, p.5 |
| `mean_DBH` | DMOY_i | plot | cm | variant `plot_mean_dbh` | « DMOY_i est le DHP moyen de tous les arbres de la placette i » | eq. 2, p.6 |
| HDOM | HDOM_i | plot | m | variant `dominant` | « HDOM_i et DDOM_i sont respectivement la hauteur moyenne et le DHP moyen des arbres dominants de la placette i »; dominant trees = « les 8 plus hauts arbres plantés faisant partie de l'étage des arbres dominants et bien répartis dans la placette (200 arbres/ha) » (400 m² plots) | eq. 3, p.7; §2.1 p.2 |
| `dominant_DBH` | DDOM_i | plot | cm | variant `dominant` | (same sentence as HDOM): mean DHP of those same dominant trees | eq. 3, p.7 |

HDOM fits the shared `HDOM` vocabulary: it is a top-height-type definition of
the 200 tallest planted stems/ha. There is no shared name for DMOY or DDOM.
DMOY is **not** QMD, since the source says "DHP moyen", not "moyen
quadratique".

### Applicability ranges

`Auger2025_hd_parameters.csv` holds the calibration ranges from Tableau 1, in
`*_mean/_min/_max` columns:

- `DBH` and `height` on every row.
- `mean_DBH` on the `plot_mean_dbh` rows.
- `dominant_DBH` and `HDOM` on the `dominant` rows.

These feed `.hd_warn_out_of_range()`.

## Parameters

- **Table:** `data-raw/Auger2025_hd_parameters.csv`, one row per species ×
  variant (6 rows).
  - The `equation` column holds `korf`, `chapman_richards` or `hossfeld_ii`.
  - Coefficient columns are `a1 a2 a3` (Korf) and `a10 a11 a20 a21 a22 a3`
    (generalized). Cells that do not apply are empty.
  - Variance columns are `sigma2 p1 p2 p3 gamma`.
  - Also included: `n_trees`, `n_plot_meas`, the ranges, and the
    `source_params` / `source_ranges` locators.
- **Source tables:** Tableau 7 p.12, Tableau 8 p.14, Tableau 9 p.17,
  Tableau 1 p.4.
- **Values (digit-for-digit, decimal commas converted):**

  | Variant | Sp | a1 | a2 | a3 | a10 | a11 | a20 | a21 | a22 | σ² | other |
  |---|---|---|---|---|---|---|---|---|---|---|---|
  | dbh | PIG | 1630094 | 15.3007 | 0.0900 | | | | | | 0.8700 | p1 0.2820, p2 −0.0265, p3 0.0015 |
  | dbh | EPN | 101489 | 12.1844 | 0.0974 | | | | | | 0.6726 | p1 0.1822, p2 −0.0207, p3 0.0011 |
  | plot_mean_dbh | PIG | | | 0.9461 | 6.5613 | 0.6849 | 0.0587 | 0.0015 | | 0.0291 | γ 1.63 |
  | plot_mean_dbh | EPN | | | 1.4324 | 4.4337 | 0.9183 | 7.6017 | 2.1394 | | 0.0226 | γ 1.56 |
  | dominant | PIG | | | 0.8950 | −0.2009 | 0.9632 | 0.1944 | | −0.0031 | 0.3329 | |
  | dominant | EPN | | | 1.5999 | −0.3031 | 1.1438 | 0.5571 | | 1.6414 | 0.1935 | |

- **Spot checks (image vs CSV):** every cell of Tableaux 7–9 was compared
  (31 values). Row/species alignment is confirmed: PIG is the left column and
  EPN the right in all three tables.
- **Typography:** Tableau 9 prints the SE of PIG α11 as "(0.0085)" with a
  decimal point, while the rest of the table uses decimal commas. This is
  harmless: SEs are not transcribed.
- **⚠ Flagged / uncertain values:** none. All cells are legible.
- **Precision:**
  - The Korf α1 values are printed as integers.
  - α2, α3 and the generalized coefficients have 4 decimals.
  - Rounding the Korf α3 by ±0.00005 changes H by ≈0.01 m at DBH 10 cm, which
    is negligible.

## Sanity computations (from the candidate CSV, Python, not R)

| Check | PIG | EPN |
|---|---|---|
| Korf H at DBH 0.1 / 1 / 5 / 10 / 20 / 30 cm | 1.31 / 1.67 / 4.21 / 7.77 / 15.03 / 22.17 | 1.32 / 1.82 / 4.34 / 7.30 / 12.62 / 17.41 |
| H at the Tableau 1 means: Korf / DMOY / dominant (observed mean H) | 7.26 / 7.04 / 7.31 (7.4) | 5.49 / 5.56 / 5.53 (5.4) |
| Dominant variant at DBH = DDOM mean (vs HDOM mean) | 7.51 (8.0) | 5.94 (6.3) |
| DMOY variant at DBH 10 cm, DMOY 5 → 15 | 6.33 → 10.96 | 6.68 → 8.68 |

Shapes are monotone in DBH, and the values match the Tableau 1 means. Height
increases with DMOY at a fixed DBH, as in Fig. 7. All are plausible.

## Review decisions (maintainer)

The Stage 1 extraction (equations, coefficients, Tableau 1 ranges) was
approved. Resolutions of the open points:

1. **Argument names** in English, consistent with other functions:
   `mean_DBH` (source DMOY) and `dominant_DBH` (source DDOM); `HDOM` stays.

   ```r
   hd_auger2025(DBH, species,
                variant = c("dbh", "plot_mean_dbh", "dominant"),
                mean_DBH = NULL, HDOM = NULL, dominant_DBH = NULL,
                keep_model_id = FALSE)
   ```

   `variant` defaults to `"dbh"` via `rlang::arg_match()`.
2. **`mean_DBH`** is documented as the arithmetic mean DBH of all trees in
   the plot, with the verbatim quote; inclusion of natural ingress and any
   minimum DBH are not stated by the source.
3. **Inputs not used by the selected variant** are ignored with a warning.
   A missing required input aborts; `NA` gives an `NA` height.
4. **Validation bounds:** `DBH > 0`, `mean_DBH >= 0`, `HDOM > 0`,
   `dominant_DBH >= 0`. Values outside the Tableau 1 (calibration) ranges
   are computed and reported in one warning per call.
5. **Plantation scope** is stated in the documentation and the registry
   (`stand_origin = "plantation"`, `province_scope = "QC"`) only; no runtime
   message.
6. **Registry:** three rows `auger2025_dbh`, `auger2025_plot_mean_dbh`,
   `auger2025_dominant` with `fixed_args = list(variant = ...)`,
   `equation_family` `"korf"` / `"chapman_richards; hossfeld_ii"`,
   `calibration_published = FALSE`, `params_key = "parameters_hd_Auger2025"`.

In the CSV the range columns are `mean_DBH_*` and `dominant_DBH_*`; the
source symbols DMOY and DDOM are used in the equations above.

## Benchmark plan

- **Fidelity (Tier 1):** **none available.**
  - The report has no worked example and no table of predicted heights.
  - Figs 4, 7 and 10–11 show curves, but without a numeric grid.
  - A GitHub code search for the coefficients found no external
    implementation.
  - ⚠ This model will ship with **no source benchmark**. Note this in this
    spec and in NEWS (one sentence).
- **Plausibility (Tier 2), asserted in tests:**
  - Monotone increasing in DBH over the Tableau 1 range, for each species ×
    variant.
  - H → 1.3 as DBH → 0 (for example, |H(0.01) − 1.3| < 0.05).
  - Height increases with DMOY at a fixed DBH, as in Fig. 7.
  - Height increases with HDOM at fixed DBH and DDOM, as in Fig. 10.
  - Predictions at the Tableau 1 mean inputs are within ±15 % of the observed
    mean H. Computed: PIG 7.0–7.3 vs 7.4; EPN 5.5–5.6 vs 5.4.
  - Hand-computed regression values (the table above) are locked to 1e-3 as
    an implementation-regression check. These are not fidelity evidence.
  - A dominant tree (DBH = DDOM) gets a height near HDOM (within about
    ±15 %).
- **Cross-model (reported, not asserted), per §3.7:**
  - `hd_auger2016` for EPN/PIG (natural forests, DBH ≥ 9 cm, with typical
    BA/QMD/class inputs).
  - `hd_huang2016` (AB) and `hd_sharmaparton2007` (ON) for PICE.MAR and
    PINU.BAN.
  - Expectation from the source: for PIG, the Korf curve exceeds Prégent
    et al. (2001) by up to 2 m at large DBH (p.26). Prégent is not in the
    package, so this is information only.
