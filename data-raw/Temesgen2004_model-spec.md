# Model spec: hd_temesgen2004

Status: **Stage 2 — implemented** (parameters approved by maintainer).

## Review decisions (maintainer, triage consult)

| Gap | Decision |
|-----|----------|
| Exponent sign typo (`1 − e^{bD^c}` printed) | Implement `1 − exp(−b·D^c)`; document the typo |
| No fidelity benchmark | Accept Tier-2 plausibility only |
| Variant API | **Infer** variant from inputs: Model 7 if `BAL`, `TPH`, `BA` supplied, else Model 2 |
| "Spruce" (Engelmann + white complex) | Map `PICE.ENG`, `PICE.GLA`, `PICE.ENG.GLA` to the Spruce row |
| Blank cells in Table 4 (dropped terms) | Store `NA` in CSV; treat as 0 in code |
| Naming | `hd_temesgen2004`, bib key `Temesgen2004`, params `parameters_hd_Temesgen2004` |

## Source

- Temesgen, H. & von Gadow, K. (2004). Generalized height–diameter models — an
  application for major tree species in complex stands of interior British
  Columbia. *European Journal of Forest Research* 123:45–51.
  doi:10.1007/s10342-004-0020-z
- File: `sources/height-diameter/s10342-004-0020-z.pdf`
- Equations: pp.46–47 (PDF p2–3). Params: Table 4 p.50 (PDF p6, Model 7),
  Table 5 p.51 (PDF p7, Model 2). Ranges: Table 1 p.50 (PDF p6).
- Verified against 300-dpi renders (`tmp/temesgen2004/t4-6.png`, `t5-7.png`,
  `eq1_5-2.png`, `eq7_10-3.png`); not from the text layer.

## Feasibility triage — verdict: GO WITH GAPS

| Check | Result |
|-------|--------|
| Model form fully specified | Yes (with sign typo — see below) |
| All coefficients tabulated | Yes — 8 species × 2 recommended models (2, 7) |
| Predictors defined | Partially — BAL/BA species set and DBH threshold not stated |
| Units stated | Yes (cm, m, m²/ha, trees/ha) |
| Fidelity benchmark | **None** (no worked example; Fig. 1 = residuals only) → Tier 2 |
| Species mappable to NFI | Yes; "Spruce" = complex (decision above) |

Models 1, 3–6, 8–10 were evaluated but **no coefficients published**; only
Models 2 and 7 are implementable.

## Scope / domain of applicability

- Southern interior BC, Interior Douglas-fir (IDF) BEC zone, 49°–52°30′N.
- BC MoF PSPs (400 m²), complex (mixed-species, uneven-aged) stands; small-tree
  data 1999–2001 added; trees DBH < 2.0 cm not recorded.
- ⚠ Source excludes disturbed plots and warns against use in **thinned, spaced
  or selectively logged stands**.
- Province: BC; registry scope: regional (IDF zone, BC interior).

## Variables and units

| Source | Package arg | Unit | Level | Definition (source) |
|--------|-------------|------|-------|---------------------|
| D / DBH | `DBH` | cm | tree | Diameter outside bark at 1.3 m |
| Ĥ | (output `height`) | m | tree | Total tree height |
| BAL | `BAL` | m²/ha | tree | "Basal area in larger trees" |
| SPH | `TPH` | stems/ha | plot | "Number of trees per hectare" |
| BA | `BA` | m²/ha | plot | "Basal area per hectare" |

⚠ BAL/BA/SPH: not stated whether all species are included, nor the DBH
threshold (PSPs recorded DBH ≥ 2.0 cm). Document as "all live trees, as
measured in BC PSPs (DBH ≥ 2 cm)" assumption in `@param`.

## Model form(s)

**Model 2** (Yang et al. 1978, Weibull), DBH only:

    H = 1.3 + a · (1 − exp(−b · DBH^c))

**Model 7** (generalized):

    H = 1.3 + a · (1 − exp(−b · DBH^c))
    a = a1 + a2·BAL + a3·SPH + a4·BA
    c = a5 + a6·BAL

⚠ **Sign typo**: both equations are printed `1 − e^{b D^c}` (no minus in the
exponent; confirmed on render). All fitted b > 0, so the printed form would give
negative heights; the standard Yang/Weibull form `exp(−b·D^c)` is used.
Plausibility check (Model 2 at Table-1 mean DBH) reproduces observed mean
heights within ~1 m for all species (see Benchmark plan).

Storage: Model 2 is stored in the same columns as Model 7 (a → `a1`, b → `b`,
c → `a5`; `a2`,`a3`,`a4`,`a6` = NA), so one code path evaluates both.

## DBH–height (hd) models only

### Predictor table

| model_id | variant | Required predictors |
|----------|---------|---------------------|
| temesgen2004_m2 | m2 | DBH, species |
| temesgen2004_m7 | m7 | DBH, species, BAL, TPH, BA |

Variant inference: Model 7 when all of `BAL`, `TPH`, `BA` are non-NULL; Model 2
when all are NULL; error if only some are supplied (avoids silently mixing).
For species where Table 4 dropped a term (e.g. Larch has no BAL/SPH/BA in `a`),
the predictor is still required for m7 for API consistency but contributes 0.

### Applicability ranges (Table 1)

DBH_min = 2.0 cm for all species. Per-species DBH and height min/mean/max/SD are
in the CSV. **No ranges published for BAL, BA, SPH** (leave NA).

## Parameters

`data-raw/Temesgen2004_hd_parameters.csv` — 20 rows: (7 species + 3 spruce
codes) × 2 variants. Columns: `species_src, nfi_species, variant,
source_equation, a1, a2, a3, a4, b, a5, a6, rmse, n_plots, n_trees, DBH_*,
height_*, source_params, source_ranges`.

Table 4 (Model 7) — blank = term dropped (stored NA → 0):

| Species | a1 | a2 | a3 | a4 | b | a5 | a6 | RMSE |
|---|---|---|---|---|---|---|---|---|
| Aspen | 20.6548 | | | 0.08724 | 0.01509 | 1.39936 | | 1.44 |
| Cedar | 17.9468 | | −0.0009 | 0.14087 | 0.03497 | 1.30435 | | 0.62 |
| Paper birch | 20.4464 | | −0.0007 | 0.13355 | 0.03576 | 1.26223 | | 1.93 |
| Douglas fir | 32.037 | −0.3504 | −0.0007 | 0.18308 | 0.01797 | 1.09263 | 0.00802 | 2.23 |
| Larch | 41.7918 | | | | 0.01709 | 1.1185 | 0.00404 | 2.04 |
| Lodgepole pine | 20.852 | 0.3168 | −0.0004 | 0.23962 | 0.03184 | 1.0875 | −0.0014 | 2.29 |
| Ponderosa pine | 32.2082 | | | | 0.01738 | 1.10721 | | 2.33 |
| Spruce | 17.0803 | 0.0932 | | 0.34276 | 0.01073 | 1.46187 | | 2.35 |

Table 5 (Model 2):

| Species | a | b | c | RMSE |
|---|---|---|---|---|
| Aspen | 20.9169 | 0.0281 | 1.3791 | 0.6523 |
| Cedar | 22.0648 | 0.0094 | 1.6327 | 1.5972 |
| Paper birch | 23.4556 | 0.0247 | 1.4218 | 2.0267 |
| Douglas-fir | 34.1073 | 0.0181 | 1.1583 | 2.5445 |
| Larch | 36.8541 | 0.0244 | 1.1126 | 2.3924 |
| Lodgepole pine | 27.7486 | 0.0216 | 1.2828 | 2.8976 |
| Ponderosa pine | 32.2084 | 0.0174 | 1.1072 | 2.3287 |
| Spruce | 28.3578 | 0.0074 | 1.5965 | 2.9366 |

⚠ RMSE inconsistencies (metadata only, coefficients unaffected): Table 5 Aspen
0.6523 vs Table 2 model-2 Aspen 2.59; Table 4 Cedar 0.62 vs Table 2 model-7
Cedar 1.44 (Table 4 Aspen 1.44 equals that value — column may be shifted).
RMSE stored as printed in Tables 4/5.

⚠ Ponderosa pine Model 7 (a1 = 32.2082, a5 = 1.10721, b = 0.01738) is almost
identical to its Model 2 (32.2084, 1.1072, 0.0174) — consistent with all stand
terms being dropped; not an extraction error.

## Benchmark plan (Tier 2 — plausibility)

1. Model 2 at Table-1 mean DBH vs observed mean height (computed):

   | Species | H̃(mean DBH) | obs mean H | H̃(max DBH) | obs max H |
   |---|---|---|---|---|
   | Aspen | 14.1 | 13.0 | 21.2 | 26.9 |
   | Cedar | 16.5 | 15.1 | 23.3 | 24.7 |
   | Paper birch | 17.5 | 16.1 | 24.5 | 28.5 |
   | Douglas fir | 16.0 | 14.7 | 34.7 | 39.4 |
   | Larch | 19.8 | 19.0 | 32.7 | 36.9 |
   | Lodgepole pine | 16.8 | 15.7 | 28.6 | 37.0 |
   | Ponderosa pine | 14.6 | 14.0 | 26.2 | 33.1 |
   | Spruce | 12.3 | 11.3 | 29.7 | 34.4 |

   Slight over-prediction at the mean is expected (concave curve, Jensen).
2. Tests: H → 1.3 as DBH → 0; monotone increasing in DBH; asymptote 1.3 + a;
   Model 7 with zero-effect species (Ponderosa) ≈ Model 2.
3. Hand-computed regression values (fixed in tests) for both variants.
4. Optional cross-check vs `hd_huang2016` (AB) for overlapping species — loose.

## Implementation notes for Stage 2 (proposals)

- `R/hd_temesgen2004.R`:
  `hd_temesgen2004(DBH, species, BAL = NULL, TPH = NULL, BA = NULL, keep_model_id = FALSE)`
  (flags/return shape to mirror existing hd_ functions and `hd_utils.R`).
- Registry: two `.hd_registry_row()`s, `temesgen2004_m2` / `temesgen2004_m7`,
  province BC, scope regional, with predictor rows as above.
- `data-raw/preprocess_data.R`: build `parameters_hd_Temesgen2004`, add to
  `internal_objs`.
- `REFERENCES.bib` entry `Temesgen2004`; `NEWS.md`; HD vignette predictor table;
  update `sources/height-diameter/sources.csv` / `missing_literature.md`
  (now available).

## Implementation outcome (Stage 2)

- Validation tier: **plausibility only; no source benchmark.**
- Model 2 at Table-1 mean DBH over-predicts observed mean height by 0.6–1.4 m
  for all species (test asserts 0 < diff < 1.5 m).
- Structural checks: H = 1.3 m at DBH 0; monotone in DBH; asymptote 1.3 + a;
  Ponderosa pine Model 7 ≈ Model 2 (tol 1e-3); Douglas-fir height decreases
  with TPH and increases with BA. BAL acts on both a (a2 < 0) and c (a6 > 0):
  for Douglas-fir at DBH 20 cm, raising BAL 5 → 20 m²/ha *increases* height
  (15.9 → 17.3 m) — published behaviour, not asserted in tests.
- Model 7 aborts if a or c ≤ 0 (possible only for extreme TPH/BAL, e.g. cedar
  at TPH 30,000); no source ranges exist for BAL/TPH/BA.
- Cross-check vs `hd_huang2016` (Alberta, provincial, DBH 10/20/35 cm):
  PINU.CON, PICE.GLA within ~1.4 m; POPU.TRE 1.6–3.6 m lower; BETU.PAP
  0.9–2.9 m higher; PSEU.MEN close at 10–20 cm, 4.7 m higher at 35 cm
  (Alberta Douglas-fir is at its range edge). Regional differences, not defects.
- Coverage of `R/hd_temesgen2004.R`: 100%.
