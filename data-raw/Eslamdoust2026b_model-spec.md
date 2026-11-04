# Model spec: hd_eslamdoust2026b

> Extraction approved by the maintainer in this session.
> Source gaps accepted for extraction in this session. Printed page p corresponds
> to PDF page p + 8 for the numbered report body.

## Source

- **Citation:** Eslamdoust, J. 2026. Height-diameter model for white pine
  plantations in Ontario. Ontario Ministry of Natural Resources, Science and
  Research Branch, Peterborough, ON. Science and Research Technical Report
  TR-79. 12 p.
- **BibTeX key:** proposed `Eslamdoust2026b`.
- **Document:** `sources/height-diameter/TR-79.pdf`.
- **Model family:** hd.
- **Target function:** `hd_eslamdoust2026b()`; the suffix distinguishes this
  single-author report from the existing Eslamdoust et al. (2026) implementation.
- **Proposed registry model_id:** `eslamdoust2026b`; scope provincial,
  jurisdiction ON, stand_origin plantation.

## Feasibility and accepted gaps

- **Verdict:** GO WITH GAPS, accepted for extraction.
- Equation 1, Table 2 coefficients and Table 1 ranges are complete and legible.
- BA's minimum tree-size threshold and species basis are not stated.
  HDOM and TPH species bases are not explicit either; do not invent them.
- No reproducible source benchmark: Figure 4 does not state the stand
  predictors used. Use explicitly labelled plausibility comparisons.
- IMPORTANT: the rendered p. 4 defines TPH using DBH >= 2.5 cm. The text layer
  drops the equality bar, giving an apparent strict inequality. The rendered
  definition takes precedence over that extraction and the earlier discussion.

## Scope / domain of applicability

- **Region:** Ontario white pine plantations.
- **Species:** white pine, Pinus strobus, NFI `PINU.STR`; no species substitutions.
- **Data:** 2265 trees in 41 plots (19 PSPs, 22 PGPs), p. 2.
- **Plot selection:** plots with BAH <= 5 m2/ha removed; at least three white
  pine trees with measured heights required; white pine comprised 60-100% of
  stand composition (p. 2).
- **Caveats:** bias increases for large DBH (> 50 cm) and highest relative
  heights; plot calibration may improve predictions (pp. 8-10).
- Age basis and site-index base age: not applicable.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H_ij | Total height of tree j in plot i | m |
| DBH_ij | Diameter at breast height, measured at 1.3 m | cm |
| SHT_i | Mean height of dominant and codominant trees | m |
| BAH_i | Stand basal area | m2/ha |
| TPH_i | Trees per hectare with DBH >= 2.5 cm | stems/ha |
| a0-a4 | Fixed-effect coefficients of nonlinear mixed model | model-specific |
| u_i | Plot-level random effect on a0 | model-specific |
| e_ij | Residual height error | m |

## Model form

Equation 1, p. 4 (PDF p. 12), verified from the rendered page:

```text
H_ij = 1.3 + (a0 + u_i) * SHT_i^a1 *
       (1 - exp(-a2 * (TPH_i / BAH_i)^a3 * DBH_ij))^a4 + e_ij

Population-level prediction (u_i = 0, e_ij = 0):
H = 1.3 + a0 * HDOM^a1 *
    (1 - exp(-a2 * (TPH / BA)^a3 * DBH))^a4
```

- **Direction:** total height from DBH and stand predictors only.
- **Height convention:** total height, additive breast-height offset 1.3 m.
- **Fit:** nonlinear mixed effects (`nlme`), with `nlsLM` starting values.
- **Population coefficients:** Table 2 mixed-model fixed effects, random effect
  set to zero. Figure 4 explicitly illustrates the fixed-effects component.
  No separate final fixed-only fit coefficients are published.
- **Random effects:** one normally distributed plot effect on a0;
  var(u_i) = 0.0072; residual variance = 1.4349 (Table 2).
  No residual correlation structure is specified.
- **Back-transformation:** none; model is on the original height scale.
- **Variants:** one plantation parameter set; no variant selector needed.
- **API proposal:** `DBH`, `species = "PINU.STR"`, required `HDOM`, `BA`, `TPH`,
  then `keep_model_id = FALSE`. Preserve the existing natural-stand API.
- **Mathematical domain:** DBH >= 0; HDOM, BA and TPH > 0. Inputs outside
  fitting ranges should be computed with the existing range-warning mechanism.

### Predictor table

| Arg | Source name | Level | Units | Required | Source definition | Locator |
|-----|-------------|-------|-------|----------|-------------------|---------|
| DBH | DBH | tree | cm | TRUE | "DBH (cm; measured at 1.3 m)" | p. 2 |
| HDOM | SHT | plot | m | TRUE | "stand height (mean height of dominant and co-dominant trees, SHT - m)" | p. 2 |
| BA | BAH | plot | m2/ha | TRUE | "basal area (BAH - m2 ha-1)" | p. 2 |
| TPH | TPH | plot | stems/ha | TRUE | "number of trees per hectare" with DBH >= 2.5 cm | rendered p. 4 |

The TPH threshold above is rendered as a greater-than-or-equal symbol.
BA size threshold and species bases remain undocumented by the source.

### Applicability ranges

Table 1, p. 3 (PDF p. 11), read from its 300-dpi rendering:

| Variable | Mean | Minimum | Maximum |
|----------|------|---------|---------|
| DBH | 24.13 | 2.9 | 58.50 |
| height | 17.07 | 2.81 | 36.30 |
| HDOM | 18.65 | 4.57 | 33.56 |
| BA | 37.66 | 7.55 | 53.84 |
| TPH | 1426.87 | 433.0 | 4700.0 |

## Parameters

- **CSV:** `data-raw/Eslamdoust2026b_hd_parameters.csv`.
- Table 2, p. 7 (PDF p. 15), Estimate column:
  a0 = 1.8711; a1 = 0.8053; a2 = 0.0927; a3 = -0.1568; a4 = 0.6247.
- Standard errors, same rows: 0.0836, 0.0134, 0.0081, 0.0225, 0.0267.
- Variance estimates, same table: sigma2_e = 1.4349; sigma2_u = 0.0072.
- Source locators for coefficient, SE, variance and range columns are in the CSV.
- All coefficients apply to plantation white pine; no ns/dropped terms.
- **Numeric uncertainty:** none in the rendered tables.
- **Self-check:** a0, a3 and both variance rows rechecked against the rendering;
  Table 1 DBH and BA limits also rechecked, with row alignment confirmed.
- **Evidence:** `tmp/eslamdoust2026b_tr79_table2.png`; citation, equation,
  Table 1 and Table 2 checked visually, no OCR required.

## Benchmark plan

- **Tier 1:** no worked examples or predicted-value tables with complete inputs.
  Figure 4 cannot serve as a fidelity benchmark because HDOM, BA and TPH
  values are omitted; Tables 3-4 report bias, not reproducible predictions.
- **Tier 2:** compare with `hd_eslamdoust2026()` M5 for `PINU.STR`, at identical
  DBH/HDOM/BA/TPH inputs inside the overlapping fitting ranges. Natural versus
  plantation differences are expected; report magnitude and direction, do not
  assert tight agreement.
- Check monotonic increase with DBH and HDOM, 1.3 m limit at DBH zero, finite
  positive predictions, and plausible asymptotes.
- Since a3 is negative, height decreases with TPH at fixed BA and increases
  with BA at fixed TPH; retain the published signs.
- Generate independent equation evaluations over a grid as regression values;
  label these as equation-derived, not published source benchmarks.
- **Validation status:** no source benchmark.

### Implemented comparison

- Independent equation-derived grid: 189 combinations of DBH
  (2.9, 5, 10, 20, 30, 50, 58.5 cm), HDOM (8, 18, 30 m), BA
  (10, 30, 50 m2/ha), and TPH (500, 1500, 4000 stems/ha), all within
  the overlapping fitting ranges. Generator and CSV are in `tmp/`.
- TR-79 minus natural-stand M5 ranges from approximately -7.59 to +6.50 m;
  mean difference +0.205 m. These differences are model comparisons, not errors.
- At HDOM = 18 m, BA = 30 m2/ha, TPH = 1500 stems/ha:
  DBH 10 cm gives 12.03 versus 9.41 m (+2.62 m);
  DBH 20 cm gives 15.73 versus 15.61 m (+0.12 m);
  DBH 50 cm gives 19.50 versus 23.70 m (-4.20 m).
- Existing natural-stand selection and engine behavior are preserved;
  `hd()` now selects TR-79 for eligible Ontario plantation white pine.

## Human review

- Accepted: documented predictor-definition gaps and plausibility-only validation.
- Approved: equation, coefficients, standard errors, variances, fitting ranges,
  rendered TPH threshold, proposed name and population-level API.
- Implementation authorized after Stage 1 approval.
