# Model spec: hd_staudhammerlemay2000

## Source

- **Citation:** Staudhammer, C. and LeMay, V. 2000. Height prediction equations
  using diameter and stand density measures. The Forestry Chronicle 76(2):
  303–309. doi:10.5558/tfc76303-2
- **BibTeX key:** @StaudhammerLeMay2000
- **Document in `sources/`:**
  `sources/height-diameter/staudhammer-lemay-2011-height-prediction-equations-using-diameter-and-stand-density-measures.pdf`
  (WARN: the filename says 2011, but the rendered footer reads "March/April 2000,
  Vol. 76, No. 2, The Forestry Chronicle".)
- **Model family:** hd
- **Target function:** `hd_staudhammerlemay2000`

## Scope / domain of applicability

- **Jurisdiction / region:** BC coast. UBC Malcolm Knapp Research Forest
  (~5000 ha, Coastal Western Hemlock zone), 1995 inventory, 327 plots in 82
  clusters (p. 304).
- **Species covered (NFI codes):** THUJ.PLI (CW), ALNU.RUB (D), PSEU.MEN (F),
  TSUG.HET (HW).
- **Valid input ranges:** see Applicability ranges below (Table 1, p. 304).
- **Caveats / assumptions from the source:**
  - The source label "D" is *Alnus rubra* + *A. tenuifolia* fitted together.
    By user decision, the model is mapped to ALNU.RUB only.
  - Alder has only 28 trees on 16 plots (p. 306).
  - Height was measured on a subjectively chosen subset of trees (p. 304).
  - Plot design: fixed-radius plots where BA < 15 m²/ha, otherwise
    variable-radius (p. 304).
  - The paper fitted 2a and 2b for all four species (Table 2) but published
    coefficients only for eq 2 (all species), eq 2a (alder) and eq 2b
    (CW, F, HW) (Table 5). Eqs 1, 1a–1f and 2c–2f have no coefficients and are
    not implemented.

## Variables and units

| Symbol (source) | Package arg | Meaning | Units |
|-----------------|-------------|---------|-------|
| dbh | `DBH` | diameter at breast height | cm |
| height | (output) | total tree height | m |
| TSTEMSHA | `TPH` | stems per hectare, all species combined | stems/ha |
| TBAHA | `BA` | basal area per hectare, all species combined | m²/ha |
| REL_dbh | computed: `DBH / DMAX` | tree DBH ÷ maximum DBH on the plot (all species) | – (Table 1 mislabels it "cm") |
| – | `DMAX` | maximum DBH on the plot, all species | cm |

## Model form(s)

```
[2]  height = 1.3 + E1 * (1 - exp(E2 * dbh^E3))             (Yang et al. 1978)

[2a] (parameter prediction, alder)
     Ek = ck0 + ck1 * TSTEMSHA + ck2 * TBAHA                k = 1, 2, 3

[2b] (parameter prediction, CW / F / HW)
     Ek = ck0 + ck1 * REL_dbh + ck2 * REL_dbh^2              k = 1, 2, 3
```

- **Direction(s):** height from DBH only. No inversion.
- **Notes on form:**
  - The 1.3 m intercept is constrained (p. 305: "y-intercept was restricted
    … to predict a height of 1.3 m when dbh is zero").
  - The sign of E2 is carried by the coefficient, inside `exp(E2 * dbh^E3)`
    (E2 < 0 for a valid curve).
  - Eqs 2a and 2b are described in prose (p. 305: "the two stand density
    variables were incorporated as linear models to predict each of the
    parameters"). The linear form above follows the Table 5 column headings
    "Constant / Slope with TSTEMSHA / Slope with TBAHA" and "Constant / Slope
    with REL_dbh / Slope with REL_dbh2".
- **Unified CSV form:**
  `Ek = ek_0 + ek_TPH*TPH + ek_BA*BA + ek_rel*REL + ek_rel2*REL^2`, with empty
  cells treated as 0. For variant "dbh" only `ek_0` is filled.

## DBH–height (hd) models only

- **Height convention:** total height; 1.3 m offset as an additive constant.
- **Fit type:** fixed-effects NLS. Marquardt method in SAS PROC NLIN, fitted
  by species; global minimum checked with Gauss–Newton and alternative
  starting values (p. 305).
- **Coefficients used for population prediction:** Table 5 (p. 308), which is
  the only table of coefficients.
- **Random-effects structure:** none.
- **Variance components transcribed:** none published. Table 2 R², bias and
  RMSE are stored as `r2`, `bias`, `rmse`.
- **Back-transformation:** none.
- **Variants and how they are selected:** explicit `variant` argument
  (user decision):
  - `"dbh"`: eq 2, all 4 species, DBH only.
  - `"density"`: eq 2a for ALNU.RUB (requires `TPH`, `BA`); eq 2b for
    THUJ.PLI, PSEU.MEN and TSUG.HET (requires `DMAX`). The required inputs
    differ by species, as in hd_auger2016, where requirements vary by species.
- **Source species mappings:** D (*A. rubra* + *A. tenuifolia*) → ALNU.RUB
  (user decision; ALNU.INC / ALNU.SPP not supported).
- **Registry:** `staudhammerlemay2000_dbh` and
  `staudhammerlemay2000_density`, scope regional (BC), stand_origin "both".
  The inventory sampled the whole research forest, and the source does not
  restrict stand origin.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | dbh | tree | cm | TRUE | "For all trees above 2 cm dbh, the species, dbh (cm) … were obtained" | p. 304 |
| TPH | TSTEMSHA | plot | stems/ha | FALSE (density, ALNU.RUB) | "stems per hectare for all species combined (TSTEMSHA)" | p. 304 |
| BA | TBAHA | plot | m²/ha | FALSE (density, ALNU.RUB) | "basal area per hectare for all species combined (TBAHA)" | p. 304 |
| DMAX | (max dbh for REL_dbh) | plot | cm | FALSE (density, THUJ.PLI/PSEU.MEN/TSUG.HET) | "relative dbh (i.e., dbh divided by the maximum dbh for all species; REL_dbh)"; Table 1 caption: "dbh divided by the maximum dbh for the plot" | p. 304 |

WARN on predictor definitions:

- The tree threshold is implied as DBH > 2 cm (all measured trees), but it is
  not stated explicitly for TSTEMSHA and TBAHA.
- It is not stated whether dead standing trees are included, although live
  versus dead status was recorded (p. 304). The docs will say "all trees
  > 2 cm DBH, all species" and flag the live/dead ambiguity.
- `DMAX` must be ≥ `DBH`. The function will error if `DBH > DMAX`.

### Applicability ranges

Source: Table 1, p. 304 (PDF p2); same ranges for both variants.

| Species | n trees | n plots | DBH min–max (mean) | Height min–max (mean) | TPH min–max | BA min–max | REL_dbh min–max |
|---------|---------|---------|--------------------|-----------------------|-------------|------------|-----------------|
| THUJ.PLI (CW) | 210 | 124 | 2.7–141.0 (42.4) | 2.8–49.0 (23.4) | 85–19094 | 3.77–126.00 | 0.05–1.00 |
| ALNU.RUB (D) | 28 | 16 | 2.3–62.3 (13.5) | 2.1–30.7 (12.3) | 85–6854 | 3.77–72.00 | 0.12–1.00 |
| PSEU.MEN (F) | 230 | 138 | 2.0–180.5 (42.3) | 2.0–66.2 (26.4) | 55–23899 | 2.40–120.00 | 0.12–1.00 |
| TSUG.HET (HW) | 261 | 161 | 2.0–90.8 (32.2) | 2.5–51.5 (24.2) | 55–35042 | 2.55–140.00 | 0.05–1.00 |

## Parameters

Values are in `data-raw/StaudhammerLeMay2000_hd_parameters.csv`.

- **Source table(s):** Table 5, p. 308 (PDF p6) for the coefficients; Table 2,
  p. 306 (PDF p4) for R², bias and RMSE; Table 1, p. 304 (PDF p2) for ranges.
- **Transcription:** every table was transcribed from 300 dpi renders. The
  text layer garbled several values (e.g. "4.01365", "-3.3873 1", "-0.01 18",
  "-4.1 1239"). Evidence crops are in `tmp/staudhammerlemay2000_*.png`.
- **Spot checks:** Table 5 was re-read in full against the render. Row
  alignment was confirmed: CW, Alder, Douglas-fir, Hemlock for eq 2;
  Alder E1–E3; CW, F, HW E1–E3 for eq 2b.

### Eq 2 (variant "dbh")

| Species | E1 | E2 | E3 |
|---------|----|----|----|
| CW | 39.0002 | -0.02164 | 1.01568 |
| Alder | 26.5495 | -0.03079 | 1.20438 |
| F | 68.6382 | -0.01296 | 0.98848 |
| HW | 41.4831 | -0.01365 | 1.21692 |

### Eq 2a, alder (variant "density")

| Parameter | Constant | Slope TSTEMSHA | Slope TBAHA |
|-----------|----------|----------------|-------------|
| E1 | 68.0529 | 0.0192872 | -0.77315 |
| E2 | -0.00877 | 0.00000044 | -0.00061 |
| E3 | 0.64991 | -0.00008194 | 0.00554 |

### Eq 2b, CW / F / HW (variant "density")

| Species | Parameter | Constant | Slope REL_dbh | Slope REL_dbh² |
|---------|-----------|----------|---------------|----------------|
| CW | E1 | 59.5789 | 12.5373 | -36.98 |
| CW | E2 | 0.00071 | -0.03818 | 0.1551 printed → **0.01551** used ⚠ |
| CW | E3 | 1.6577 | -2.02199 | 1.41599 |
| F | E1 | 15.759 | 767.419 | -699.761 |
| F | E2 | 0.00025 | -0.00332 | -0.0118 |
| F | E3 | 2.07538 | -3.38731 | 2.17848 |
| HW | E1 | -1.25839 | 163.146 | -126.716 |
| HW | E2 | 0.00021 | -0.06091 | 0.05552 |
| HW | E3 | 2.34135 | -4.11239 | 3.31119 |

### ⚠ Flagged / uncertain values

1. **CW eq 2b, E2 slope on REL_dbh²: printed 0.1551, implemented as 0.01551
   (user decision, 2026-10-30).**
   - With the printed value, E2 > 0 for REL_dbh > ~0.23, and the model
     predicts **negative heights**: REL 0.5 / DBH 30 gives −46.6 m, and
     REL 1.0 / DBH 60 gives about −2×10⁵ m. This is incompatible with the
     Table 2 fit (RMSE 4.308 m, R² 0.804).
   - With 0.01551 (a dropped zero): REL 0.5 / DBH 30 → 21.3 m; REL 1.0 /
     DBH 60 → 29.5 m. E2 at REL = 1 (−0.0220) matches eq 2 E2 (−0.02164).
   - The CSV holds 0.01551. The correction is documented in @details and
     tested. It is an inference, not a published erratum.
2. **Eq 2b E1 for F and HW** varies strongly with REL_dbh (HW E1 = −1.26 at
   REL = 0; F E1 peaks at ~226 near REL 0.55). Predictions are plausible
   within the published REL range (0.05–1.00), since E2 and E3 compensate.
   Parameters become invalid (E1 ≤ 0, E2 ≥ 0 or E3 ≤ 0) only outside the
   data: F REL < 0.062, CW REL < 0.019, HW REL < 0.008. Alder eq 2a is
   invalid for TPH > ~7900, or large BA with low TPH. Such rows abort,
   following the hd_temesgen2004 / hd_auger2025 precedent. Out-of-range
   inputs warn.
3. Eq 2b property (not a transcription issue): within a plot (fixed DMAX),
   heights increase with DBH up to REL ≈ 0.85–0.95 and then decline for the
   largest trees, for all of CW, F and HW. This is documented in @details.
4. Eq 2 hemlock E2 = −0.01365. The text layer reads "4.01365"; the render is
   unambiguous.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No. There is no worked
  example or predicted-height table. ⚠ The model will ship with **no source
  benchmark** (user accepted Tier 2); note this in NEWS and @details.
- **Plausibility (Tier 2):**
  - Compare at the source mean predictors with Table 1 mean heights. Example:
    eq 2 at the mean DBH gives CW 25.6 m at 42.4 cm versus an observed mean
    of 23.4 m (Jensen's inequality, so not an exact check). The paper reports bias in Table 2; expect agreement within
    ±1 RMSE.
  - Same-family comparison with `hd_temesgen2004` for overlapping BC species
    (THUJ.PLI, PSEU.MEN, TSUG.HET) over DBH 5–80 cm. Expect the same
    magnitude, ±~25%.
  - Structural checks:
    - height(DBH → 0) → 1.3;
    - monotone increasing in DBH for eq 2;
    - `density` heights in range across Table 1 TPH, BA and REL ranges;
    - out-of-range warnings via `.hd_warn_out_of_range()`.
  - Hand-computed regression values for one tree per species × variant,
    computed independently from the CSV coefficients.
