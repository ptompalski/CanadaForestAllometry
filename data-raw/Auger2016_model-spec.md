# Model spec: hd_auger2016

> **Argument rename (0.8.7):** the user-facing arguments are now `bioclim_subdomain` (SDOM), `physical_env` (MP), `potential_veg` (VP), `MAT` (TMOY), `MAP` (PTOT) and `elevation` (altitude). The parameter tables keep the source names.

> Human-reviewed extraction artifact (Stage 1). Page numbers are the printed
> page numbers of NRF 146, which match the PDF page numbers. Items marked ⚠
> need a reviewer decision. Every coefficient was transcribed from 300-dpi
> renders of the annex pages and checked against the PDF text layer.

## Review decisions (maintainer, Stage 1 review)

- Coefficients: **the published NRF 146 values** (4 decimals). Do not use the
  CFSForestTools parameters.
- ⚠1 BOG: **implement as published**, linear in LDHP, with no LDHP² term.
  Document this in `@details`.
- ⚠3 Uncovered class levels: return **`NA` height with one cli warning per
  call**, listing the affected rows, species and levels.
- ⚠4 Signature **approved**:

  ```r
  hd_auger2016(DBH, species, BA, QMD = NULL, SDOM = NULL, MP = NULL,
               VP = NULL, TMOY = NULL, PTOT = NULL, altitude = NULL,
               disturbance = NULL)
  ```

  - A predictor is required only for the species that use it.
  - `disturbance` accepts French or English values, case-insensitive:
    anthropique/anthropogenic, naturelle/natural, aucune/none.
  - SDOM codes are matched case-insensitively.
- ⚠5 DBH < 9 cm: **compute the height** and include these rows in the
  out-of-range warning (8.1 #8).
- Benchmark: the EPB worked example, plus a CFSForestTools fixture for the 10
  matching species at a tolerance of 0.02 m.
- Plan §3.7 / §2.1: corrected (no back-transformation; the CFSForestTools
  benchmark is limited to a subset of species).

## Source

- **Citation:** Auger, I. 2016. Une nouvelle relation hauteur-diamètre tenant
  compte de l'influence de la station et du climat pour 27 essences
  commerciales du Québec. Gouvernement du Québec, ministère des Forêts, de la
  Faune et des Parcs, Direction de la recherche forestière. Note de recherche
  forestière n° 146. 31 p.
- **BibTeX key:** @Auger2016 (proposed; not yet in `REFERENCES.bib`)
- **Document in `sources/`:** `sources/height-diameter/auger2016_nrf146_qc_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_auger2016`
- **Plan reference:** `implementation_plan.md` §6 **PR 4**, models.csv id
  `auger2016_eq1`. The request called this "PR 1", but §6 lists PR 1 as
  `hd_huang2016`.
- **Supersedes:** Fortin et al. 2009 (`fortin2009_eq6`), as the plan notes in §2.2.

## Feasibility triage — verdict: GO WITH GAPS (input coding only)

- The source gives the equation, every coefficient (Annexes 1–4), the variance
  components, applicability ranges (Tableaux 1–2) and a worked example
  (Annexe 1). Nothing in the model content is missing.
- **Gap:** users must supply Québec classification codes (SDOM, MP, VP,
  disturbance) and 1981–2010 climate normals (TMOY, PTOT). Plan decision 8.1
  #17 says these are user-supplied, with no spatial lookups.
- ⚠ **Precision:** coefficients are printed to 4 decimals. The full-precision
  values exist only "sur demande auprès de l'auteur … sous forme de
  chiffrier" (p.9). See ⚠2.

## Scope / domain of applicability

- **Jurisdiction / region:** Québec (QC). The fitting data are permanent and
  temporary inventory plots from the MFFP network, covering the managed
  forest.
- **Species covered:** 27 (see the mapping below).
- **Fitting data:** merchantable trees only, DBH ≥ 9.1 cm; 167,424 trees on
  80,976 plots measured 1–6 times (p.3).
- **Caveat (p.19, verbatim):** « ces relations ne devraient pas être utilisées
  pour estimer la hauteur des arbres dont le DHP est inférieur à 9 cm, puisque
  de tels arbres ne sont pas présents dans la base de données ayant servi à
  l'étalonnage des modèles ». See ⚠5.
- **Caveat (p.19):** because each species has its own fit, a model can easily
  be pushed into extrapolation. The plan handles this through the
  out-of-range warning (8.1 #8).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| h | total tree height | m |
| DHP | DBH at 1.3 m | cm |
| LDHP | ln(DHP + 1) | – |
| ST | merchantable basal area of the plot (surface terrière marchande) | m²/ha |
| RDHP | DHP / quadratic mean DBH of the plot's trees | – |
| ALT | altitude | m |
| TMOY | mean annual temperature, 1981–2010 normal | °C |
| PTOT | total annual precipitation, 1981–2010 normal | mm |
| SDOM | bioclimatic subdomain (Saucier et al. 1998), 11 classes | code |
| MP | physical environment (milieu physique, MRN 2013), 10 classes 0–9 | code |
| VP | potential vegetation, 3-character code (e.g. FE3, RS2) | code |
| PERT | light disturbance (perturbation légère): anthropique / naturelle / aucune | code |

## Model form(s)

Eq. [1], p.4 (verified from the image):

```
h_ijk = 1.3 + A_ijk * ln(DHP_ijk + 1) + (B_ijk + b_is) * ln(DHP_ijk + 1)^2 + ε_ijk

A = a_LDHP + a_ST*ST + a_RDHP*RDHP + a_ALT*ALT + a_PTOT*PTOT + a_TMOY*TMOY
    + a_SDOM[sdom] + a_MP[mp] + a_VP[vp] + a_PERT[pert]
B = b_LDHP2          (species constant)
```

- Every covariate effect interacts with LDHP: the annex tables label them
  `LDHP×ST`, `LDHP×VP` and so on. Only `B` multiplies LDHP².
- Each term applies only to the species that use it (Table 5). An absent
  term contributes 0.
- **Direction:** predict h from DBH and covariates. Inversion is out of scope.
- ⚠1 **BOG has no LDHP² term.** Annexe 1 shows "–" for BOG LDHP2, so its
  model is `1.3 + (a_LDHP + a_ST*ST) * LDHP`, linear in LDHP. This is not a
  transcription gap: the row is consistent in both the image and the text
  layer.
- **Worked example (Annexe 1 box, p.22):** EPB, DHP 20 cm, ST 20 m²/ha, QMD
  15 cm, SDOM 1, VP FC1, TMOY 2.3 °C → **12.0 m**. Reproduced from the
  candidate CSVs: A = −2.9051 + 0.0252·20 − 0.2230·(20/15) + 0.0789·2.3 −
  0.3581 − 0.4203 = −3.2954, giving h = 1.3 − 3.2954·ln 21 + 2.2411·(ln 21)²
  = **12.0402** ✓.

## DBH–height (hd) models only

- **Height convention:** total height. The 1.3 m offset is added explicitly,
  so h → 1.3 as DBH → 0. The model is not valid there (DBH < 9 cm).
- **Fit type:** linear mixed model. The plot random effect `b_is` sits on the
  LDHP² term, and the residuals have an SP(POW) serial correlation across
  remeasurements.
- **Coefficients used for population prediction:** the published fixed
  effects with `b_is = 0` and `ε = 0`. The source's own evaluation uses this
  marginal prediction (p.7: « prévisions marginales du modèle (obtenues en
  fixant l'effet aléatoire à 0) »). There is only one fit, so 8.1 #10 needs no
  choice.
- **Random-effects structure:** plot × species random effect on LDHP², plus a
  residual SP(POW) correlation (per measurement interval).
- **Variance components transcribed:** Annexe 1 pp.22–23 → CSV columns
  `sd_plot`, `rho_plot`, `sd_resid`. These are not used in v1 (8.1 #6).
- **Back-transformation:** **none**. The response h is untransformed, and
  ln(DHP+1) is a predictor transform only. ⚠ Plan §3.7 refers to an "Auger
  log-scale back-transformation"; that statement is wrong and should be
  removed from the plan.
- **Variants:** none. Each species has one fixed predictor set (Table 5).
- **Source species mappings:** none. There are no surrogate species.
- **Class grouping:** classes were grouped per species when data were sparse
  (fewer than 60 observations, p.6). Annexes 2–4 repeat the group value for
  every member class: « La valeur du paramètre associée à un groupe de classes
  est répétée pour chacune des classes qui le constituent ». A blank cell means
  the class was **not represented** in that species' data (see ⚠3).

### Species → predictor sets (Tableau 5, pp.10–11; consistent with Annexes 1–4)

| Code | NFI | ST | RDHP | ALT | TMOY | PTOT | SDOM | MP | VP | PERT |
|---|---|---|---|---|---|---|---|---|---|---|
| BOG | BETU.POP | ✓ | | | | | | | | |
| BOJ | BETU.ALL | ✓ | ✓ | | ✓ | ✓ | | | ✓ | |
| BOP | BETU.PAP | ✓ | ✓ | ✓ | | ✓ | ✓ | ✓ | ✓ | ✓ |
| CET | PRUN.SER | ✓ | | | | | | | ✓ | |
| CHR | QUER.RUB | ✓ | | | | | | ✓ | | |
| EPB | PICE.GLA | ✓ | ✓ | | ✓ | | ✓ | | ✓ | |
| EPN | PICE.MAR | ✓ | ✓ | | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| EPR | PICE.RUB | ✓ | ✓ | | | ✓ | | | ✓ | |
| ERA | ACER.SAC | ✓ | | | | | | | | |
| ERR | ACER.RUB | ✓ | ✓ | | ✓ | ✓ | | ✓ | ✓ | ✓ |
| ERS | ACER.SAH | ✓ | ✓ | | ✓ | ✓ | ✓ | | ✓ | |
| FRA | FRAX.AME | ✓ | | | | | | ✓ | | |
| FRN | FRAX.NIG | ✓ | ✓ | ✓ | | | ✓ | | ✓ | |
| HEG | FAGU.GRA | ✓ | ✓ | ✓ | | | | | | |
| MEL | LARI.LAR | ✓ | ✓ | | | | ✓ | | | |
| ORA | ULMU.AME | ✓ | | | | | | | | |
| OSV | OSTR.VIR | ✓ | | | | | | ✓ | | |
| PEB | POPU.BAL | ✓ | ✓ | | | | | | ✓ | |
| PEG | POPU.GRA | ✓ | ✓ | | | | | ✓ | ✓ | |
| PET | POPU.TRE | ✓ | ✓ | | ✓ | ✓ | ✓ | ✓ | ✓ | |
| PIB | PINU.STR | ✓ | ✓ | | | | ✓ | | | ✓ |
| PIG | PINU.BAN | ✓ | ✓ | | | | ✓ | ✓ | | ✓ |
| PIR | PINU.RES | ✓ | ✓ | | | | | | | |
| PRU | TSUG.CAN | ✓ | ✓ | | | ✓ | | | ✓ | |
| SAB | ABIE.BAL | ✓ | ✓ | ✓ | | ✓ | ✓ | ✓ | ✓ | |
| THO | THUJ.OCC | ✓ | ✓ | | ✓ | ✓ | ✓ | | ✓ | |
| TIL | TILI.AME | ✓ | ✓ | | | | | | | |

Totals: ST 27; RDHP 21; VP 15; PTOT 10; SDOM 11; MP 10; TMOY 7; ALT 4;
PERT 5. These match the text on pp.9–11.

The NFI mapping was verified with
`translate_species_code(x, from = "jurisdiction", jurisdiction = "QC", to = "nfi")`.
Note that ERA is *Acer saccharinum* (silver maple) → ACER.SAC, and ERS is
*A. saccharum* (sugar maple) → ACER.SAH.

### Predictor table

Proposed argument names follow §3.2 and 8.1 #2/#3. A predictor is required
only for the species that use it (⚠4).

| Arg | Source name | Level | Units | Required | Definition (verbatim / paraphrase) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | DHP | tree | cm | TRUE | « DHP … de ce même arbre (cm) », measured at 1.3 m | p.4 |
| BA | ST | plot (measurement) | m²/ha | TRUE (all 27) | « surface terrière marchande dans la placette » (merchantable trees, DBH > 9 cm) | p.4, Tab. 3 p.6 |
| QMD | (RDHP denominator) | plot (measurement) | cm | species-specific (21) | « DHP moyen quadratique des arbres de la placette (cm) »; the function computes RDHP = DBH / QMD | Tab. 3 p.6 |
| altitude | ALT | plot | m | species-specific (4) | « Altitude (m) » | Tab. 3 p.6 |
| TMOY | TMOY | plot | °C | species-specific (7) | « Température annuelle moyenne sur la période 1981–2010 (°C) » | Tab. 3 p.6 |
| PTOT | PTOT | plot | mm | species-specific (10) | « Précipitations totales annuelles sur la période 1981–2010 (mm) » | Tab. 3 p.6 |
| SDOM | SDOM | plot | code | species-specific (11) | « Sous-domaine bioclimatique (11 classes) »; codes 1, 2est, 2ouest, 3est, 3ouest, 4est, 4ouest, 5est, 5ouest, 6est, 6ouest | Tab. 3 p.6; Annexe 3 |
| MP | MP | plot | code | species-specific (10) | « Milieu physique (10 classes) »; codes 0–9 (MRN 2013) | Tab. 3 p.6; Annexe 2 |
| VP | VP | plot | code | species-specific (15) | « Végétation potentielle (32 classes) »; Annexe 4 lists 34 codes (⚠6) | Tab. 3 p.6; Annexe 4 |
| disturbance | PERT | plot (measurement) | code | species-specific (5) | « Occurrence d'une perturbation légère (3 classes) »: anthropique / naturelle / aucune | Tab. 3 p.6; Annexe 2 |

Not used, although listed in Tableau 3: stem density, drainage class, the
BAL-ratio and Fortin social-rank indices, and the ecological type.

### Applicability ranges

Tableau 1 (p.3) gives per species the number of trees and the mean and range
of height, DBH and RDHP. Tableau 2 (p.4) gives per species the number of
measurements and plots, and the mean and range of ST, altitude, TMOY and PTOT.
All values are in `Auger2016_hd_parameters.csv` (`*_mean/_min/_max`). DBH_min
is 9.1 cm for every species except PIR (9.2).

## Parameters

- **Continuous effects and variance components:** Annexe 1, pp.22–23 →
  `data-raw/Auger2016_hd_parameters.csv`. This file has one row per species,
  with columns `b_ldhp`, `b_ldhp2`, `b_st`, `b_rdhp`, `b_alt`, `b_ptot` and
  `b_tmoy` (an empty cell means the term is absent), the `uses_sdom/mp/vp/pert`
  flags, `sd_plot`, `rho_plot`, `sd_resid`, and the ranges. SEs were not
  transcribed.
- **Class effects:** Annexes 2–4 → `data-raw/Auger2016_hd_class_effects.csv`.
  This is a long table (species, variable, class, estimate, source) with 585
  rows: SDOM pp.26–27, MP and PERT pp.24–25, VP pp.28–31. Only the cells
  printed in the source are included. A printed 0 is the reference level, and
  a blank cell becomes an absent row, meaning the class is uncovered.
- **Coverage notes:**
  - ERS, FRN and PIB SDOM have no 6est/6ouest values; THO has no 6est.
  - CHR MP is missing 4, 7, 8 and 9. FRA MP is missing 4, 7 and 9. OSV and PEG
    MP are missing 7 and 9.
  - VP has wide per-species gaps; for example, no species has MS7 except SAB.
- **Reference levels vary by species.** For example, EPB SDOM 6est = 0 but
  6ouest = 0.0182, and SAB MP 0 = 0. The values are stored exactly as printed.
- **Spot checks against the images:**
  - Annexe 1: EPB, SAB and BOG rows.
  - Annexe 2: CHR and FRA MP.
  - Annexe 3: PIB and THO.
  - Annexe 4: EPB FC1/FE1/RS5, EPN RE4, SAB MS4/MS7/RB2, PET RS1 and THO RS1.
- **⚠ Flagged / uncertain values:** none that are illegible. The points
  needing decisions are listed below.

## ⚠ Decisions needed

1. **BOG has no LDHP² term** (linear in LDHP). Implement it as published?
   (Recommended: yes, and document it.)
2. **4-decimal precision.** ALT and PTOT slopes are printed as, for example,
   −0.0005, so they carry at most one significant digit. With PTOT ≈ 1000 mm,
   the rounding (±0.00005) shifts A by ±0.05 and h by ±0.15 m. CFSForestTools
   has full-precision parameter files, but they come from a **different fit**
   (see the benchmark section). Options:
   - (a) ship the published 4-decimal values. This is faithful to the
     citable source. (Recommended)
   - (b) ask the author for the chiffrier and replace the values later.
   - (c) use the CFSForestTools parameters. This gives a different model
     from NRF 146.
3. **Uncovered class levels** (blank cells), for example CHR with MP = 7.
   Options:
   - abort with a cli error naming the species and level;
   - return `NA` with one warning per call. This is consistent with 8.1 #14
     and #8. (Recommended)
4. **Signature and requiredness.** The proposed signature is:

   ```r
   hd_auger2016(DBH, species, BA, QMD = NULL, SDOM = NULL, MP = NULL,
                VP = NULL, TMOY = NULL, PTOT = NULL, altitude = NULL,
                disturbance = NULL)
   ```

   - Each argument is validated per row: a row aborts if its species uses a
     predictor that was not supplied (NULL). An `NA` value gives an `NA`
     height (8.1 #14).
   - Registry: `required_predictors = c("DBH", "species", "BA")`; all other
     predictors are optional and species-dependent. The plan shows `QMD`,
     `SDOM`, `MP`, `TMOY`, `PTOT` and `altitude` as required with no default.
     They cannot be strictly required, because ERA, ORA and BOG need only BA.
   - Alternative: `disturbance` levels could be accepted in English
     (`"anthropogenic"`, `"natural"`, `"none"`), French, or both,
     case-insensitive.
   - SDOM codes should be normalized case-insensitively (CFSForestTools
     uses `2EST`).
5. **DBH < 9 cm** (outside the fitting data; the source says not to use the
   model). Options:
   - compute and include these rows in the standard out-of-range warning
     (8.1 #8) (recommended);
   - return `NA`.
6. **VP count.** Tableau 3 says "32 classes", but Annexe 4 lists 34 codes:
   FC1, FE1–FE6, FO1, ME1, MF1, MJ1–2, MS1, MS2, MS4, MS6, MS7, RB1, RB2,
   RB5, RC3, RE1–RE4, RE7, RP1, RS1–RS5, RS7 and RT1. Accept exactly the
   Annexe 4 codes. (Information only; no action needed.)
7. **Plan corrections:**
   - §3.7: remove the "log-scale back-transformation" wording.
   - §3.7 / §6: CFSForestTools is a fidelity benchmark for only a subset of
     species (see below).

## Benchmark plan

- **Fidelity (Tier 1), source:** the Annexe 1 EPB worked example. The source
  prints 12.0 m and the candidate CSVs reproduce 12.0402 m. Test with
  `expect_equal(..., 12.0, tolerance = 0.05)` on the absolute scale; the
  unrounded value is 12.04.
- **External implementation:** CFSForestTools
  `generalhdrelation2014`, commit fa86f80, LGPL-3, file
  `fichier_test_unitaire_smaller.csv`. It has 5,400 rows of `hautm_pred`,
  27 species × 200 trees, all with PERT = NON and SDOM 1/2est/2ouest. I
  computed the published model from the candidate CSVs and compared:

  | Agreement | Species | max \|Δh\| |
  |---|---|---|
  | Matches within rounding | CET, ERA, FRA, FRN, MEL, ORA, OSV, PIB, PIR, TIL | ≤ 0.016 m |
  | Close | EPB | 0.062 m |
  | Differs (different variable sets/estimates in CFSForestTools) | BOG 1.73, BOJ 1.24, BOP 0.39, CHR 2.57, EPN 0.16, EPR 0.99, ERR 0.14, ERS 0.47, HEG 1.06, PEB 0.85, PEG 1.18, PET 0.20, PIG 0.30, PRU 0.22, SAB 0.19, THO 0.25 | m |

  The CFSForestTools per-species `parametersXXX.csv` files come from a
  different fit from the one published in NRF 146. For example, its BOG model
  includes TMOY and VP, its BOJ model adds SDOM and MP, and its EPB model has
  no FC1 VP level and adds an `ldhp×pert` term.
  - **Proposal:** for the 10 matching species, extract the CFSForestTools rows
    into `tmp/` and copy them as a comparison CSV under
    `tests/testthat/fixtures/`, then test at `tolerance = 0.02` m (absolute).
    This is a Tier-1 external benchmark that also covers SDOM, MP, VP and
    the continuous terms for those species.
  - The other 17 species get Tier-2 checks plus the EPB worked example.
    Report the differences in the roxygen `@details` and in NEWS, without
    asserting them in tests.
- **Plausibility (Tier 2), from the source text (p.11, Fig. 3):** reference
  trees EPN, ERS and SAB with DHP 20, RDHP 1.3 (QMD 15.4), ST 20, PERT
  aucune, ALT 300, MP 2 and PTOT 1000. SAB and EPN are in 6est, RS2, with
  TMOY 1; ERS is in 3ouest, FE3, with TMOY 3.5. Reported differences to check
  approximately (±0.1 m):
  - EPN in 5ouest is 1.0 m taller than in 6est, and SAB is 1.8 m taller.
  - TMOY +3 °C gives EPN +0.9 m; TMOY +2 °C gives ERS +1.2 m.
  - EPN on RE2 is 0.5 m taller than on RS2. SAB on RE2 is 0.59 m smaller than
    on RS2. ERS on MJ1 is 1.0 m smaller than on FE3.
  - Over ST 5 → 40, the height response is monotone and positive.
  - These were checked by hand against the candidate CSVs at DBH 20 cm
    (ln 21 = 3.0445). Every reported difference reproduces:

    | Comparison | Computed | Reported |
    |---|---|---|
    | EPN 5ouest − 6est | 1.04 | 1.0 |
    | SAB 5ouest − 6est | 1.77 | 1.8 |
    | EPN TMOY +3 °C | 0.91 | 0.9 |
    | ERS TMOY +2 °C | 1.23 | 1.2 |
    | EPN RE2 − RS2 | 0.49 | 0.5 |
    | SAB RE2 − RS2 | −0.59 | −0.59 |
    | ERS MJ1 − FE3 | −1.00 | −1.0 |

    This independently checks the SDOM, VP and TMOY transcriptions for these
    species.
- **Cross-model:** compare EPN and PIG against `hd_auger2025` once it exists
  (§3.7). Report the differences; do not assert them.
