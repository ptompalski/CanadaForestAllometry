# Model spec: hd_fortin2009

> Human-reviewed extraction artifact (Stage 1). Page numbers are **printed**
> page numbers of MRF 153 (PDF page = printed page + 16). Every coefficient was
> transcribed from 200-dpi renders of the table pages and cross-checked against
> the PDF text layer and the CFSForestTools Java parameter files (see Benchmark
> plan). Items marked ⚠ need a reviewer decision.

## Source

- **Citation:** Fortin, M., Bernier, S., Saucier, J.-P., Labbé, F. 2009. Une
  relation hauteur-diamètre tenant compte de l'influence de la station et du
  climat pour 20 espèces commerciales du Québec. Gouvernement du Québec,
  ministère des Ressources naturelles et de la Faune, Direction de la recherche
  forestière. Mémoire de recherche forestière n° 153. 22 p.
  ISBN (PDF) 978-2-550-55438-7.
- **BibTeX key:** @Fortin2009 (proposed; not yet in `REFERENCES.bib`)
- **Document in `sources/`:** `sources/height-diameter/fortin2009_memoire153_qc_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_fortin2009`
- **Plan reference:** `sources/height-diameter/models.csv` id `fortin2009_eq6`.
  ⚠ `implementation_plan.md` (l.75) lists Fortin 2009 as *superseded by
  Auger 2016* (already implemented as `hd_auger2016`). The maintainer chose to
  implement it anyway; the roxygen `@details` should point users to
  `hd_auger2016` as the newer Québec relation.

## Feasibility triage — verdict: GO WITH GAPS (resolved at triage)

- Equation, all coefficients (Tableaux 4–5), variance components and ranges
  (Tableaux 1–2) are in the document and legible.
- Gaps and decisions (maintainer, triage):
  1. No numeric worked example → search for an external implementation first
     (done: CFSForestTools, see Benchmark plan); Figure 5 used as a
     semi-quantitative check.
  2. Ecological subdivision is defined only by a map (Fig. 2, p.8) →
     user supplies the subdivision directly (4 classes). *Revised at
     review:* the user supplies the ecological region code (`ecoregion`),
     mapped to the subdivision as in CFSForestTools (decision 5).
  3. Drainage (4 classes) and light disturbance (3 classes) → user supplies
     the class directly.
  4. Drainage and disturbance are **required** (no defaults).

## Scope / domain of applicability

- **Jurisdiction / region:** Québec (QC), public commercial forest; provincial
  network of permanent sample plots (400 m², 11 215 plots), 1970–2006
  measurements (p.3).
- **Species covered:** 20 (mapping below).
- **Fitting data:** trees with DBH ≥ 9.1 cm (only numbered trees, p.3);
  144 262 height observations (p.4); species with ≥ 500 observations retained.
- **Valid input ranges:** Tableau 2 (p.4) per species DBH and height; Tableau 1
  (p.3) plot BA 0.2–66.9 m²/ha (all periods) and MAT −4.7 to 6.8 °C.
- **Caveats:** none stated explicitly beyond the data domain.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| h_ijk | total height of tree j, plot i, measurement k | m |
| dhp_ijk | DBH at 1.3 m | cm |
| ST_ik | basal area of all trees with DBH ≥ 9.1 cm in plot i at measurement k | m²/ha |
| T_i | mean annual temperature of plot i, 1971–2000 normals (BioSIM) | °C |
| d | drainage class: xérique (1), mésique (2), subhydrique (3), hydrique (4) | class |
| e | ecological subdivision: Ouest (1), Centre (2), Nord-Est (3), Sud-Est (4) | class |
| f | light disturbance: anthropique (1), naturelle (2), aucune (3) | class |
| ISS_ijk | social status index = (dhp_ijk − dmq_ik)² | cm² |
| dmq_ik | quadratic mean diameter of the plot's stems (DBH ≥ 9.1 cm, Tab. 3 footnote) | cm |

## Model form(s)

Eq. (6), p.7 (verified from image):

```
h_ijk = 1.3 + (β1,s + β2,s·ST_ik + β3·T_i + β4,d + β5,e + β6,f + β7·ISS_ijk) · ln(dhp_ijk + 1)
            + (β8,s + β9,s·ST_ik + β10·ISS_ijk + b_is) · (ln(dhp_ijk + 1))²
            + ε_ijk
ISS_ijk = (dhp_ijk − dmq_ik)²                                     eq. (7), p.7
```

- Base form is eq. (1), p.5: `h = 1.3 + A·ln(dhp+1) + B·(ln(dhp+1))² + ε`.
  The +1 ensures h → 1.3 m as DBH → 0 (p.5).
- **Direction:** predict h from DBH and covariates only.
- All 20 species share the same predictor set. β3–β7 and β10 are common to all
  species; β1, β2, β8 and β9 are species-specific.

## DBH–height (hd) models only

- **Height convention:** total height; 1.3 m offset added explicitly.
- **Fit type:** linear mixed model (SAS PROC MIXED, p.7).
- **Coefficients used for population prediction:** the published fixed
  effects with `b_is = 0`, `ε = 0`. The source's own cross-validation uses
  this (p.8, eq. 9: « obtenue en fixant l'effet aléatoire et l'erreur
  résiduelle à 0 »). There is only one fit.
- **Random-effects structure:** plot random effect b_is ~ N(0, G_s) on the
  (ln(dhp+1))² term only, variance by species (p.7). Residual: "linear"
  covariance structure within tree across remeasurements, eq. (8), p.7:
  `Cov(ε_ijk, ε_ijk') = γ_h (1 − δ_h |t_ijk − t_ijk'|)` if
  `δ_h |Δt| < 1`, else 0; h = 1 deciduous, h = 2 conifers.
- **Variance components transcribed:** G_s (Tableau 4, p.11) → column `G_s`
  in `Fortin2009_hd_parameters.csv`; γ_h and δ_h (Tableau 5, p.12) →
  `component = covariance` rows in `Fortin2009_hd_effects.csv`. Not used in v1.
- **Species group (h) assignment:** not tabulated in the source. Taken from
  CFSForestTools `0_SpeciesHD.txt` (FEU/RES): MEL (larch) = conifer.
  Only affects the unused residual covariance.
- **Back-transformation:** none (response is untransformed h).
- **Variants:** none.
- **Source species mappings:** none.

### Species mapping (Tableau 2 p.4 / Tableau 4 p.11 index s)

QC codes → NFI verified with
`translate_species_code(x, from = "jurisdiction", jurisdiction = "QC", to = "nfi")`:

| s | Source name | QC | NFI | group |
|---|---|---|---|---|
| 1 | Bouleau jaune | BOJ | BETU.ALL | deciduous |
| 2 | Bouleau à papier | BOP | BETU.PAP | deciduous |
| 3 | Chêne rouge | CHR | QUER.RUB | deciduous |
| 4 | Épinette blanche | EPB | PICE.GLA | conifer |
| 5 | Épinette noire | EPN | PICE.MAR | conifer |
| 6 | Épinette rouge | EPR | PICE.RUB | conifer |
| 7 | Érable rouge | ERR | ACER.RUB | deciduous |
| 8 | Érable à sucre | ERS | ACER.SAH | deciduous |
| 9 | Frêne noir | FRN | FRAX.NIG | deciduous |
| 10 | Hêtre à grandes feuilles | HEG | FAGU.GRA | deciduous |
| 11 | Mélèze laricin | MEL | LARI.LAR | conifer |
| 12 | Ostryer de Virginie | OSV | OSTR.VIR | deciduous |
| 13 | Peuplier à grandes dents | PEG | POPU.GRA | deciduous |
| 14 | Peuplier faux-tremble | PET | POPU.TRE | deciduous |
| 15 | Pin blanc | PIB | PINU.STR | conifer |
| 16 | Pin gris | PIG | PINU.BAN | conifer |
| 17 | Pruche du Canada | PRU | TSUG.CAN | conifer |
| 18 | Sapin baumier | SAB | ABIE.BAL | conifer |
| 19 | Thuya occidental | THO | THUJ.OCC | conifer |
| 20 | Tilleul d'Amérique | TIL | TILI.AME | deciduous |

### Predictor table

Signature (decision 1: argument order of `hd_auger2016`):

```r
hd_fortin2009(DBH, species, BA, QMD, ecoregion, drainage, MAT,
              disturbance, keep_model_id = FALSE)
```

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | dhp | tree | cm | TRUE | « diamètre à hauteur de poitrine de ce même arbre (cm) », 1,3 m | p.5 |
| ecoregion | e (subdivision écologique), via région écologique | plot | code ('1a'…'7c') | TRUE | « regroupement de régions écologiques … l'Ouest (e = 1), le Centre (e = 2), le Nord-Est (e = 3) et le Sud-Est (e = 4) (Figure 2) »; region → subdivision lookup from CFSForestTools (`data-raw/Fortin2009_hd_ecoregions.csv`) | p.7, Fig. 2 p.8 |
| BA | ST | plot (measurement) | m²/ha | TRUE | « surface terrière de tous les arbres dont le dhp est égal ou supérieur à 9,1 cm dans la placette » (all species) | p.7 |
| QMD | dmq | plot (measurement) | cm | TRUE | « diamètre moyen quadratique des tiges de la placette » (stems ≥ 9.1 cm per Tab. 3 footnote); used in ISS = (DBH − QMD)² | p.7, Tab. 3 p.6 |
| MAT | T | plot | °C | TRUE | « température annuelle moyenne de la placette i pour la période 1971–2000 » (BioSIM) | p.3, p.7 |
| drainage | d | plot | class | TRUE | « xérique (d = 1), mésique (d = 2), subhydrique (d = 3) et hydrique (d = 4) » | p.7 |
| disturbance | f | plot (measurement) | class | TRUE | « perturbations anthropiques (f = 1), … naturelles comme les chablis partiels et les épidémies légères (f = 2), et l'absence de perturbation (f = 3) »; light disturbance / partial cut removing 25–75 % of BA since last measurement | p.7 |

Class values: accept French or English, case-insensitive, as in
`hd_auger2016`:
- `ecoregion`: Québec ecological region code (48 codes, case-insensitive),
  mapped to west/centre/northeast/southeast.
- `drainage`: xerique/xérique/xeric, mesique/mésique/mesic,
  subhydrique/subhydric, hydrique/hydric.
- `disturbance`: anthropique/anthropogenic, naturelle/natural, aucune/none.

### Applicability ranges

- Per species (Tableau 2, p.4): n, mean and range of height and DBH → CSV
  `height_*`, `DBH_*`. DBH_min = 9.1 for all species except TIL (9.2).
- Plot level, all species (Tableau 1, p.3): BA 0.2–66.9 m²/ha (min/max over
  the four periods) and MAT −4.7 to 6.8 °C → `BA_min/max`, `MAT_min/max`,
  repeated on every species row. QMD range is not published.

## Parameters

- **Species-specific (Tableau 4, p.11):** `data-raw/Fortin2009_hd_parameters.csv`
  → columns `b1`, `b2`, `b8`, `b9`, `G_s`; plus `species_group`, ranges,
  provenance.
- **Species-independent (Tableau 5, p.12):** `data-raw/Fortin2009_hd_effects.csv`
  (long: component, parameter, variable, class, class_src, estimate, source):
  β3 (MAT), β4,d (drainage), β5,e (subdivision), β6,f (disturbance), β7, β10
  (ISS), and covariance γ_h, δ_h. The reference levels (hydrique, Sud-Est,
  aucune) are printed as 0 and stored as 0.
- **Proposed objects:** `parameters_hd_Fortin2009` (species table, registry
  `params_key`) and `parameters_hd_Fortin2009_effects`.
- **Spot checks against images:** Tableau 4 all 20 rows re-read against the
  image (row alignment confirmed: s index = row), Tableau 5 all rows; Tableau 2
  all rows. The text layer matches the images exactly.
- **Cross-check against CFSForestTools** `0_HDRelationBeta.csv` /
  `0_HDRelationCovParms.csv` (full precision): b3 0.08828289, drainage
  −0.11172690 / 0.03236540 / −0.00276952 / 0, subdivision Centre 0.32664699 /
  Nord-Est 0.33190136 / Ouest 0.60075113 / Sud-Est 0, disturbance 0.03562712 /
  0.02190542 / 0, β7 −0.00234810, β10 0.00048283, SAB b1 −2.65842725,
  SAB b8 1.87262074, G_s and γ/δ — all agree with the published values to the
  printed precision.
- **⚠ Flagged / uncertain values:** none illegible. See decision 2 (Figure 5e).

## Decisions (resolved at review)

1. **Signature:** argument order of `hd_auger2016` (`DBH, species, BA, QMD,
   ecoregion, drainage, MAT, disturbance, keep_model_id`); all predictors
   required.
2. **Figure 5e vs Tableau 5 (ISS):** implement as CFSForestTools, i.e. the
   tabulated β7/β10 with ISS = (DBH − QMD)² (the interface javadoc defines it
   as the squared difference). Panel (e) gives 13.16 m vs ≈12.7 m in the
   figure; documented in `@details`. (CFSForestTools' *test harness* passes
   the unsquared DBH − QMD; this affects only its reference values, see
   Benchmark.)
3. **Precision:** full-precision coefficients from CFSForestTools
   `0_HDRelationBeta.csv` (column `estimate` / `b*`); printed values kept as
   `*_published` columns. Acknowledged in `@details`.
4. **DBH < 9.1 cm:** computed and reported in the out-of-range warning.
5. **Ecological region:** as CFSForestTools — `ecoregion` takes the Québec
   ecological region code, mapped to the four subdivisions
   (`DUMMY_ECO_REGION`, Fortin2009HeightPredictor.java):
   - Centre: 2b 2c 3c 3d 4d 4e 4f 5e 5f
   - Nord-Est: 6h–6l 6n–6s 7a–7c
   - Ouest: 1a 2a 3a 3b 4a–4c 5a–5d 6a–6g
   - Sud-Est: 4g 4h 5g–5j 6m

   Unknown codes abort. Drainage is supplied as the class; the
   CFSForestTools fallback from the physical environment (0 xeric, 1–3
   mesic, 4–6 subhydric, 7–9 hydric; `QuebecGeneralSettings.ENVIRONMENT_TYPE`)
   is documented in `@param drainage` as guidance only.

## Benchmark plan

- **Fidelity (Tier 1) reference values in source?** No numeric worked
  example. **Semi-quantitative:** Figure 5 (p.13) plots predictions for a
  reference tree: SAB, DBH 20 cm, MAT 0 °C, BA 20 m²/ha, Centre, mésique,
  ISS 0, no disturbance. Values computed from the candidate CSVs, against
  values read from the figure:

  | Panel | Computed (m) | Figure (≈ m) |
  |---|---|---|
  | reference | 13.43 | 13.45 |
  | MAT −4 / −2 / 2 / 4 | 12.36 / 12.89 / 13.97 / 14.51 | 12.4 / 12.9 / 14.0 / 14.5 |
  | BA 10 / 30 / 40 | 12.54 / 14.32 / 15.21 | 12.6 / 14.3 / 15.2 |
  | Ouest / Nord-Est / Sud-Est | 14.27 / 13.45 / 12.44 | 14.3 / 13.45 / 12.45 |
  | xérique / subhydrique / hydrique | 12.99 / 13.32 / 13.33 | 13.0 / 13.35 / 13.35 |
  | anthropique / naturelle | 13.54 / 13.50 | 13.55 / 13.5 |
  | ISS = 100 (±10 cm) | **13.16** | **12.7** ⚠ decision 2 |

  Figure values are read by eye from a 150-dpi render (±0.05 m).

  Proposal: test the reference tree and the MAT, BA, subdivision, drainage
  and disturbance panels at `tolerance = 0.1` m (absolute), labelled as
  *figure-read* values.
- **External implementation (Tier 1, exact):** CWFC-CCFB/CFSForestTools
  commit `fa86f80c`, `src/allometry/hdrelationship/fortin2009generalhdrelationship/`.
  - `aDeadTreeOnly` unit test: BOJ, DBH 25, BA 0, region 3a, mesic, MAT 2,
    no disturbance, harness SSI = 25 → **15.129429409429527 m**; reproduced
    with QMD = 20 (ISS = 25) to 4e-15.
  - `refPredictions.xml` / `refBlup.xml` (present on `main`; earlier note of
    absence was wrong): BLUP-calibrated predictions for the first 10 plots of
    the 2014 test-tree file. Population prediction recovered as
    prediction − BLUP_s · ln(DBH+1)²; harness uses mesic, no disturbance,
    region 1a, SSI = DBH − QMD (unsquared) → reproduced with
    QMD_equiv = DBH − √(DBH − QMD) for trees with DBH ≥ QMD. 58 trees,
    11 species, max |diff| 7e-15 m.
  - Generator: `tmp/generate_hd_fortin2009_comparison_values.R` →
    `tmp/hd_fortin2009_comparison_values.csv`; one tree per species inlined
    in `tests/testthat/test-hd_fortin2009.R` (tolerance 1e-8).
- **Plausibility (Tier 2), result:** vs `hd_auger2016` (BA 20, QMD 20,
  Centre/4est, mesic/MP 2, MAT 2–3 °C, DBH 15/25/40): mean |Δ| 0.96 m,
  max 3.3 m (OSTR.VIR at 40 cm). Not asserted in tests.
- **Plausibility (Tier 2), plan:** compare with `hd_auger2016` (same equation
  family and data source, 1981–2010 normals) for all 20 shared species at
  matched inputs. Report differences; do not assert them.
- No source numeric benchmark → state "figure-read and external-coefficient
  checks only" in the spec and NEWS as required.
