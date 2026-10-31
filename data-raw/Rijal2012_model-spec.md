# Model spec: hd_rijal2012

Status: **Stage 2 — implemented** (parameters approved by maintainer).

## Review decisions (maintainer, triage consult)

| Gap | Decision |
|-----|----------|
| CSI and CCF not computable from the source | Required, user-supplied predictors; document definitions/references in `@param` |
| CSI argument name | `CSI` (climate-derived SI, distinct from package `si`) |
| Coefficient sets | Both: `variant = c("gnls", "nlme")`, default `"gnls"` (source recommendation without local calibration) |
| BAL units not stated | Assume m²/ha; document as an assumption |
| No fidelity benchmark | Tier-2 plausibility only |
| Naming | `hd_rijal2012`, bib key `Rijal2012`, params `parameters_hd_Rijal2012` |

## Source

- **Citation:** Rijal, B., Weiskittel, A.R. & Kershaw, J.A. Jr. (2012).
  Development of regional height to diameter equations for 15 tree species in
  the North American Acadian Region. *Forestry* 85(3):379–389.
  doi:10.1093/forestry/cps036
- **BibTeX key:** @Rijal2012
- **Document in `sources/`:** `sources/height-diameter/Rijal et al. - 2012 - Development of regional height to diameter equations for 15 tree species in the North American Acadi.pdf`
- **Model family:** hd
- **Target function:** `hd_rijal2012`
- Equation: Eq. 4, p.381 (PDF p3). Params: Table 5 (GNLS) and Table 6 (NLME),
  p.387 (PDF p9); variance components Table 7, p.388 (PDF p10). Ranges:
  Table 1, p.381 (PDF p3).
- All values transcribed from 300–400 dpi renders (`tmp/rijal2012/eq4_p381.png`,
  `table1_p381.png`, `table5_p387.png`, `table6_p387.png`, `table7_p388.png`),
  cross-checked against the text layer (which agreed for every cell).

## Feasibility triage — verdict: GO WITH GAPS

| Check | Result |
|-------|--------|
| Model form | Pass — Eq. 4 explicit (verified on render) |
| Parameters present | Pass — Tables 5, 6, 7 in this document |
| Coverage | Pass — all 15 species in both tables |
| Units | Partial — DBH cm, HT m stated; **CSI and BAL units not stated** |
| Direction | Pass — height from DBH |
| Height convention | Pass — total height; 1.37 m intercept (p.381) |
| Extra predictors defined | **Partial** — CSI from external climate model; CCF from external crown-width eqs; BAL size threshold/species basis not stated |
| Fit type | Pass — GNLS and NLME; GNLS recommended without calibration (p.386) |
| Applicability ranges | Partial — DBH and height only (Table 1) |
| Validation | **No fidelity benchmark** → Tier 2 |
| Legibility | Pass — text-based PDF, tables clean |

## Scope / domain of applicability

- **Region:** North American Acadian Region — Maine (USA), New Brunswick,
  Nova Scotia, Prince Edward Island, southern Québec; NL data also referenced
  (p.380). Registry: provinces NB, NS, PE, QC; scope regional (Acadian Region).
  ⚠ Confirm whether to list NL (not part of the Acadian Region proper).
- **Data:** PSPs/research installations from 33 sources (USFS FIA, UMaine CFRU,
  provincial DNRs); mixed-species, uneven-aged stands predominate (p.380–381).
- **Species (NFI):** FAGU.GRA, BETU.POP, BETU.PAP, POPU.TRE, ACER.RUB,
  QUER.RUB, ACER.SAH, BETU.ALL, ABIE.BAL, PICE.MAR, TSUG.CAN, PICE.RUB,
  THUJ.OCC, PINU.STR, PICE.GLA.
- **Caveats:** DBH measured at 1.3 m in Canadian data but 1.37 m intercept used
  throughout; authors judged the difference negligible (p.381). NLME fixed
  effects without local calibration were more biased than GNLS (p.386, Tables
  3–4).

## Variables and units

| Source | Package arg | Unit | Level | Definition (source) |
|--------|-------------|------|-------|---------------------|
| d.b.h. | `DBH` | cm | tree | Diameter at breast height (Table 1 header "d.b.h. (cm)") |
| HT | (output `height`) | m | tree | Total tree height (Table 1 header "Height (m)") |
| CSI | `CSI` | m ⚠ | plot/site | "a climate-derived estimate of site index (CSI). The CSI was based on a non-parametric regression model that relates observed site index to local climate variables (Weiskittel et al., 2011b)." p.381 |
| CCF | `CCF` | % (dimensionless) | plot | Stand-level "crown competition factor (CCF – Krajicek et al., 1961)"; "CCF was estimated using maximum crown width equations developed for the region (Russell and Weiskittel, 2011)." p.380–381 |
| BAL | `BAL` | m²/ha ⚠ | tree | "basal area larger than subject tree (BAL)" p.381 |

⚠ **CSI units not stated.** Plausibility strongly favours metres: with
CSI = 15, CCF = 150, BAL = 10, GNLS predictions at Table-1 mean DBH match
observed mean heights within −0.8 to +1.4 m for all 15 species; CSI = 50
(a feet-scale value) over-predicts by 3–12 m. Base age of CSI not stated.

⚠ **BAL units not stated** (assumed m²/ha per maintainer decision); species
basis (all species assumed) and minimum DBH threshold not stated.

⚠ **CCF:** tree-size threshold and species basis not stated; the Russell &
Weiskittel (2011) MCW equations are not reproduced in the paper.

## Model form(s)

Eq. 4 (p.381; verified on render `eq4_p381.png`):

```
HT = 1.37 + (c0 + CSI^c3) * (1 - exp(-c1 * DBH))^(c2 + c4 * log(CCF + 1) + c5 * BAL)
```

- `log` = natural logarithm ("The natural logarithm of CCF was used", p.381).
- CSI enters the asymptote only (as `CSI^c3`, added to c0); CCF and BAL enter
  the exponent (c2) only (p.381).
- Base model Eq. 3 (`HT = 1.37 + c0(1 − exp(−c1·DBH))^c2`) — **no coefficients
  published**; not implementable.
- **Direction:** height from DBH only.

## DBH–height (hd) models only

- **Height convention:** total height; breast-height offset **1.37 m** as the
  additive intercept (HT → 1.37 as DBH → 0).
- **Fit type:** GNLS (Table 5) with power variance function
  `Var(ε) = σ²·DBH^(2δ)` ("Var. power"); NLME (Table 6) with random effects on
  the asymptote only: `c0 = β0 + B_i + B_ij + B_ijk` (source / stand / year,
  Eq. 5, p.382).
- **Coefficients for population prediction:** `variant = "gnls"` (default) →
  Table 5. `variant = "nlme"` → Table 6 fixed effects with random effects = 0.
  Source recommendation: "GNLS parameters should be used when no observations
  are available for local calibration." (p.386)
- **Variance components:** Table 7 (p.388) → CSV columns `var_source`,
  `var_stand`, `var_year` (NLME rows only). ⚠ Table titled "variance
  components"; whether values are variances or SDs is not stated. Stored as
  printed; not used in v1.
- **Back-transformation:** none.
- **Variants and how selected:** explicit `variant` argument (same predictors
  for both).
- **Source species mappings:** none.

### Predictor table

Same for both variants (`rijal2012_gnls`, `rijal2012_nlme`):

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | d.b.h. | tree | cm | TRUE | d.b.h. (measured at 1.3 m in Canada, 1.37 m in US) | p.381, Table 1 |
| CSI | CSI | plot | m | TRUE | "climate-derived estimate of site index (CSI) ... non-parametric regression model that relates observed site index to local climate variables (Weiskittel et al., 2011b)" | p.381 |
| CCF | CCF | plot | % | TRUE | "crown competition factor (CCF – Krajicek et al., 1961) ... estimated using maximum crown width equations developed for the region (Russell and Weiskittel, 2011)" | p.380–381 |
| BAL | BAL | tree | m²/ha (assumed) | TRUE | "basal area larger than subject tree (BAL)" | p.381 |

### Applicability ranges (Table 1, p.381)

Per-species DBH and height min/mean/max/SD in the CSV (`DBH_*`, `height_*`).
**No ranges published for CSI, CCF, BAL** (no `_min`/`_max` columns).

## Parameters

`data-raw/Rijal2012_hd_parameters.csv` — 30 rows (15 species × 2 variants).
Columns: `species_src, species_name, nfi_species, variant, c0–c5, var_power,
res_std, r2, var_source, var_stand, var_year, n_sources, n_stands, n_years,
n_obs, DBH_min/mean/max/sd, height_min/mean/max/sd, source_params,
source_ranges`. (`n_sources`, `n_stands`, `n_years`, `n_obs` = Table 1
columns "Sources", "Stands", "Year", "Total observation".)

**Species → NFI mapping** (Table 1 common/scientific names):
AB American beech *Fagus grandifolia* → FAGU.GRA; GB Grey birch *Betula
populifolia* → BETU.POP; PB Paper birch → BETU.PAP; QA Quaking aspen →
POPU.TRE; RM Red maple → ACER.RUB; RO Northern red oak → QUER.RUB; SM Sugar
maple → ACER.SAH; YB Yellow birch → BETU.ALL; BF Balsam fir → ABIE.BAL; BS
Black spruce → PICE.MAR; EH Eastern hemlock → TSUG.CAN; RS Red spruce →
PICE.RUB; WC White cedar *Thuja occidentalis* → THUJ.OCC; WP White pine →
PINU.STR; WS White spruce → PICE.GLA. All codes exist in `species_dictionary`.

Table 5 — GNLS (default):

| Sp | c0 | c1 | c2 | c3 (CSI) | c4 (CCF) | c5 (BAL) | Var. power | Res. Std. | R² |
|---|---|---|---|---|---|---|---|---|---|
| AB | 11.8567 | 0.0717 | 2.8805 | 0.6286 | −0.2668 | −0.0058 | 0.5668 | 0.5170 | 0.44 |
| GB | 13.0688 | 0.0254 | 1.1725 | 0.8638 | −0.0815 | −0.0010 | 0.6444 | 0.4689 | 0.47 |
| PB | 13.0163 | 0.0504 | 1.1434 | 0.7727 | −0.0070 | −0.0034 | 0.5679 | 0.5739 | 0.70 |
| QA | 16.6487 | 0.0528 | 1.1460 | 0.5241 | −0.0038 | −0.0014 | 0.5709 | 0.5567 | 0.61 |
| RM | 15.0375 | 0.0349 | 1.8921 | 0.8042 | −0.1562 | −0.0029 | 0.5193 | 0.6373 | 0.48 |
| RO | 12.3769 | 0.0645 | 2.3885 | 0.6235 | −0.1868 | −0.0031 | 0.5069 | 0.6328 | 0.46 |
| SM | 9.0164 | 0.0488 | 2.2266 | 0.9057 | −0.2037 | −0.0021 | 0.4224 | 0.7642 | 0.51 |
| YB | 11.8042 | 0.0533 | 2.1054 | 0.7612 | −0.1635 | −0.0043 | 0.3379 | 1.0097 | 0.51 |
| BF | 10.7493 | 0.0722 | 1.3652 | 0.7706 | 0.0076 | −0.0046 | 0.7301 | 0.3115 | 0.79 |
| BS | 17.6022 | 0.0457 | 1.2197 | 0.6272 | 0.0099 | −0.0082 | 0.7901 | 0.2218 | 0.86 |
| EH | 14.3151 | 0.0489 | 1.7724 | 0.6029 | −0.1141 | −0.0020 | 0.3791 | 0.9803 | 0.45 |
| RS | 12.7969 | 0.0495 | 2.0286 | 0.8269 | −0.1281 | −0.0057 | 0.3919 | 0.7746 | 0.61 |
| WC | 9.7757 | 0.0442 | 1.6765 | 0.7875 | −0.1398 | 0.0019 | 0.5174 | 0.5614 | 0.40 |
| WP | 14.5706 | 0.0530 | 2.1061 | 0.6265 | −0.1454 | −0.0027 | 0.4543 | 0.7646 | 0.54 |
| WS | 20.5678 | 0.0275 | 1.4422 | 0.7615 | −0.0643 | −0.0041 | 0.4590 | 0.6032 | 0.69 |

Table 6 — NLME fixed effects; Table 7 — variance components (Source/Stand/Year):

| Sp | c0 | c1 | c2 | c3 | c4 | c5 | Var. power | Res. Std. | R² | Source | Stand | Year |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| AB | 10.6325 | 0.0966 | 2.4101 | 0.6709 | −0.1742 | −0.0014 | 0.5772 | 0.3175 | 0.40 | 2.7971 | 2.2604 | 0.0618 |
| GB | 11.2772 | 0.0883 | 2.0154 | 0.5999 | −0.1530 | – | 0.6069 | 0.3414 | 0.32 | 5.4736 | 0.0021 | 0.001 |
| PB | 10.1648 | 0.0787 | 1.3415 | 0.7386 | −0.0437 | −0.0005 | 0.4155 | 0.5448 | 0.68 | 2.8836 | 2.4133 | 0.9041 |
| QA | 9.6980 | 0.0851 | 1.5131 | 0.7644 | −0.0526 | 0.0020 | 0.5058 | 0.4739 | 0.58 | 2.1098 | 2.5932 | 0.8298 |
| RM | 11.8852 | 0.0869 | 2.4003 | 0.6885 | −0.1818 | −0.0013 | 0.4479 | 0.5019 | 0.36 | 2.5698 | 2.3183 | 0.8940 |
| RO | 9.5568 | 0.1128 | 3.4246 | 0.7446 | −0.2805 | – | 0.4174 | 0.6011 | 0.33 | 2.5295 | 2.4600 | 0.6425 |
| SM | 11.2675 | 0.0848 | 2.6286 | 0.6838 | −0.2451 | 0.0016 | 0.3771 | 0.6076 | 0.41 | 2.9882 | 2.3968 | 0.8260 |
| YB | 11.7827 | 0.0955 | 2.6872 | 0.6405 | −0.2170 | −0.0013 | 0.2581 | 0.8453 | 0.41 | 3.2271 | 0.0534 | 0.0345 |
| BF | 9.6643 | 0.0892 | 1.4746 | 0.6992 | −0.0044 | −0.0035 | 0.5869 | 0.3377 | 0.78 | 2.0959 | 2.3222 | 1.0019 |
| BS | 10.2200 | 0.0654 | 1.2496 | 0.7612 | 0.0105 | −0.0067 | 0.6054 | 0.2552 | 0.84 | 2.1442 | 3.0738 | 1.0193 |
| EH | 11.1074 | 0.0805 | 2.0797 | 0.6592 | −0.1320 | 0.0016 | 0.2158 | 1.1112 | 0.43 | 3.7597 | 0.1241 | 0.0423 |
| RS | 12.3532 | 0.0715 | 1.9533 | 0.6443 | −0.1206 | −0.0016 | 0.2857 | 0.7378 | 0.58 | 2.1811 | 2.5822 | 0.9148 |
| WC | 8.9351 | 0.0918 | 2.2097 | 0.6864 | −0.1124 | −0.0025 | 0.5163 | 0.3454 | 0.38 | 2.6117 | 2.5210 | 0.0003 |
| WP | 10.8532 | 0.0774 | 2.3743 | 0.7202 | −0.1687 | −0.0011 | 0.2651 | 0.9155 | 0.52 | 4.4990 | 0.0023 | 0.0012 |
| WS | 10.9777 | 0.0768 | 1.9216 | 0.6445 | −0.1105 | −0.0007 | 0.4401 | 0.4923 | 0.65 | 2.0231 | 2.4903 | 0.9811 |

### ⚠ Flagged / uncertain values

1. **NLME c5 for GB and RO printed "–"** ("Only significant parameters at
   α = 0.05 are displayed"). Stored as empty (NA) → treat as 0 in code
   (BAL has no effect for these two species under `nlme`).
2. **NLME c5 for RS (−0.0016 0.0001) and WC (−0.0025 0.0004):** SEs printed
   without parentheses (typesetting only). Coefficients unambiguous.
3. **GNLS PB c5 SE printed as (0.0000)** — metadata only.
4. **Table 7 GB Year = 0.001** and similar small values printed with fewer
   decimals — stored as printed.
5. **Sign differences between fits:** c4 for BF is +0.0076 (GNLS) but −0.0044
   (NLME); c5 for QA, SM, EH is negative under GNLS but positive under NLME.
   Verified on render — published values, not extraction errors.
6. **Positive CCF coefficient** for BF, BS (GNLS) and BS (NLME): c4 > 0 raises
   the exponent → *lower* height with more CCF. All other species have c4 < 0
   (height increases with CCF). Published behaviour.
7. **Exponent can become ≤ 0** only for extreme inputs (e.g. AB GNLS:
   2.8805 − 0.2668·ln(CCF+1) − 0.0058·BAL stays > 0 up to BAL ≈ 260 m²/ha at
   CCF = 300). Proposal: abort (as in `hd_temesgen2004`) when the exponent ≤ 0.

## Benchmark plan (Tier 2 — plausibility; **no source benchmark**)

1. **Mean-height check** (computed during extraction): GNLS at Table-1 mean
   DBH with CSI = 15 m, CCF = 150, BAL = 10 reproduces observed mean height
   within −0.8 to +1.4 m for all 15 species (NLME: +0.9 to +2.1 m; NLME without
   random effects is expected to be less accurate, per source). Test asserts
   |diff| < 2 m for GNLS.
2. **Structural checks:** HT = 1.37 at DBH 0; monotone increasing in DBH;
   asymptote = 1.37 + c0 + CSI^c3; height increases with CSI; direction of
   CCF/BAL effects follows coefficient signs; `variant = "nlme"` GB/RO
   invariant to BAL.
3. **Hand-computed regression values** for a small grid (both variants).
4. **Cross-model comparison** (report, loose): `hd_auger2016` / `hd_auger2025`
   (Québec), `hd_eslamdoust2026` (Ontario; PINU.STR, THUJ.OCC, ACER.SAH,
   BETU.ALL), `hd_sharmaparton2007` (Ontario boreal; ABIE.BAL, PICE.MAR,
   PICE.GLA, BETU.PAP, POPU.TRE).

## Implementation notes for Stage 2 (proposals)

- `R/hd_rijal2012.R`:
  `hd_rijal2012(DBH, species, CSI, CCF, BAL, variant = c("gnls", "nlme"), keep_model_id = FALSE)`
  (flags/return shape mirroring existing hd_ functions and `hd_utils.R`).
- Registry: two `.hd_registry_row()`s, `rijal2012_gnls` / `rijal2012_nlme`,
  provinces NB/NS/PE/QC, scope regional (Acadian Region), with predictor rows
  as above (CSI, CCF, BAL required).
- `data-raw/preprocess_data.R`: build `parameters_hd_Rijal2012` (rename
  `nfi_species` → `Species`), add to `internal_objs`.
- `REFERENCES.bib` entry `Rijal2012`; `NEWS.md`; HD vignette predictor table
  (new predictors `CSI`, `CCF`).

## Implementation outcome (Stage 2)

- Validation tier: **plausibility only; no source benchmark.**
- Review defaults accepted: CSI in metres; NLME GB/RO c5 = 0; provinces
  NB, NS, PE, QC; error when the exponent is non-positive.
- Independent check: the GNLS table matches the earlier 0.8.4
  `hd_rijal2012` transcription (commit 08e80ff) digit-for-digit (that version
  mapped sugar maple to `ACER.SAC`; now `ACER.SAH`).
- GNLS at Table-1 mean DBH (CSI 15, CCF 150, BAL 10) is within 2 m of the
  observed mean height for all species (test).
- Cross-check (CSI 15, CCF 150, BAL 10; DBH 10/20/35 cm) vs
  `hd_eslamdoust2026` M4 (Ontario): THUJ.OCC within ~1.5 m; TSUG.CAN 1–3 m
  lower; PINU.STR, ACER.SAH, BETU.ALL, FAGU.GRA 2.5–6.6 m lower. Vs
  `hd_sharmaparton2007` (HDOM 18, BA 25, TPH 1200): ABIE.BAL within ~1 m;
  PICE.MAR, PICE.GLA 1.6–2.6 m lower; BETU.PAP, POPU.TRE 0.7–4.8 m lower.
  Consistent with the shorter Acadian fitting data (Table 1 mean heights
  8–12.7 m); depends on the assumed CSI.
- Coverage of `R/hd_rijal2012.R`: 100%.
