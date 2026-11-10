# Model spec: si_huang2026 climate variant

> The user verified and approved the equations, coefficients, climate
> definitions and benchmark values before implementation. The climate
> point-prediction/inversion variant is implemented; annual trajectories
> remain deferred.

## Source and feasibility

- **Citation:** Huang, S., Stadt, K., Hossain, K., and Odell, R. 2026.
  Improved Height Growth Models for Major Alberta Tree Species with and without
  Climate Variables. Technical Report Pub. No. T/2026-RIBS01. Alberta Ministry
  of Forestry and Parks, Government of Alberta, Edmonton, Alberta. April 2026.
  ISBN 978-1-4601-6473-0.
- **BibTeX key:** @HuangEtAl2026 (already present).
- **Document:** `sources/fp-improved-height-growth-models-major-alberta-tree-species-2026 (1).pdf`
- **Family / target:** extend `si_huang2026()` with `variant = "climate"`,
  leaving `"non_climate"` as the default.
- **Verdict: GO.** Forward equations and implicit inversion are published;
  Table 3.1 contains complete, legible coefficients for all eight species;
  units, total-age basis, reference age and temporal climate definitions are
  stated; Section 4 supplies numerical benchmarks.
- **Page convention:** printed Arabic page + 6 = PDF page.
- **Current increment:** point prediction and inversion using already
  aggregated climate inputs. A separate annual trajectory projection function,
  climate aggregation from annual records, downloads and spatial climate-data
  retrieval are deferred.

## Scope and applicability

- **Region:** Alberta; provincial fits, not natural-subregion-specific fits.
- **Height:** dominant/codominant tree height from stem analysis, in metres;
  not a prescribed plot-average height of the 100 largest-DBH trees/ha.
- **Age:** total tree age, with reference age 50 years. No breast-height
  conversion, 1.3 m offset or forced zero-height condition is part of the models.
- **Data:** Section 2 describes 1,647 fitting trees and 3,006 validation trees.
  The independent validation trees lack the required location/climate data and
  validate the non-climate fits, not an independent climate-model validation.
  The `n` values in Table 3.1 count height-age observations, not trees.
- Trees older than 180 total years were excluded. The trajectories include
  younger sections: final-tree ages of at least 50 do not impose a prediction
  minimum of 50. HI inversion before about 15-20 years can be inaccurate.
- **Species / pooling:** AW = `POPU.TRE`, SW = `PICE.GLA`, PL = `PINU.CON`,
  SB = `PICE.MAR`, PB = `POPU.BAL`, FB = `ABIE.BAL`, FD = `PSEU.MEN`,
  PJ = `PINU.BAN`. PB uses PB + AW data, FB uses FB + SW, FD uses FD + PL,
  and PJ uses PJ + PL. Each species has its own coefficients.
- No automatic larch proxy or white-birch model is included.
- Table 3.3 (printed p.20 / PDF p.26) supplies observed ranges for the climate
  averages and normals. These are stored in the candidate CSV. The minor-species
  summaries exclude their pooled main-species data and are not hard mathematical
  domain limits. They must not be used to clip inputs or predictions.
- Approved extrapolation policy: warn, rather than reject, for ages above 180
  and climate inputs outside these observed ranges; reject invalid mathematical
  inputs, impossible inversions and multiple inverse solutions explicitly.
  Some published examples deliberately exceed the observed climate ranges.

## Variables, units and temporal meaning

| Source symbol | Argument / meaning | Units |
|---------------|-----------------------------|-------|
| H | `height`: dominant/codominant tree height | m |
| totage | `age`: total tree age | years |
| HI | `si`: inherent HI parameter used in the climate equations | m |
| MAPPY20, MAPPY10, MAPPY | `MAP`: arithmetic mean of annual MAP over the species-specific previous-years window | mm |
| MWMTPY20, MWMTPY10, MWMTPY | `MWMT`: arithmetic mean of annual mean-warmest-month temperatures over that same window | degrees C |
| MAPn | `MAP_normal`: location-specific 1961-1990 mean annual precipitation | mm |
| MWMTn | `MWMT_normal`: location-specific 1961-1990 mean warmest month temperature | degrees C |
| C | Multiplicative climate modifier | dimensionless |
| b0-b4, c1-c2 | Fitted coefficients, in source units | as fitted |

MWMT is NOT mean annual temperature (MAT). Its ratio uses the published Celsius
values, without conversion to kelvin or an added temperature offset.

### Required climate windows

| Species | Source suffix | Window before the corresponding year |
|---------|---------------|--------------------------------------|
| AW, SW, PB, FB | PY20 | Previous 20 years |
| SB | PY10 | Previous 10 years |
| PL, FD, PJ | PY | All previous years since tree initiation |

The current year is excluded in every case. For a young tree, the source uses
the available previous years since initiation rather than years before the tree
existed (Section 2.4, printed pp.10-11). This is not permission to silently
discard missing observations from a required climate history.

The Table 3.1 models are the recommended previous-years formulations, not the
alternative current-year-inclusive fits in Supplement S5. Do not mix their
coefficients or temporal definitions. Table 3.2's alternative aspen windows are
also outside this increment.

The point-prediction function accepts precomputed averages. It cannot verify
their temporal coverage from aggregate values alone; this responsibility must
be explicit in its documentation. An annual-data interface will need explicit
calendar-year alignment and missing-history rules in the later increment.

The 1961-1990 normals are fixed for the location, including in future
projections. They are required inputs, not provincial defaults. The approximate
normals used in the worked examples below are benchmark inputs only.

## Literal forward equations

Here A = totage, S = HI, P = the required MAP average, T = the required MWMT
average, Pn = MAPn and Tn = MWMTn. `ln` is the natural logarithm.
All coefficients below come from Table 3.1, NOT Table 3.4.

### AW and PB: equation 3.1, printed p.15 / PDF p.21

```text
C = (P/Pn)^c1 * (T/Tn)^c2
H = C * b0 * S^b1 /
    (1 + exp(ln(b0*S^b1/S - 1) + b2*ln((A+1)/51)))
```

### SW and FB: equation 3.3, printed p.15 / PDF p.21

```text
C = (P/Pn)^c1 * (T/Tn)^c2
H = C * S *
    (1 + exp(b1 + b2*ln(50+b4) + b3*(sqrt(ln(S)))^3)) /
    (1 + exp(b1 + b2*ln(A+b4) + b3*(sqrt(ln(S)))^3))
```

The cube applies to the square root of ln(S), not to ln(S) itself.

### PL, FD and PJ: equation 3.5, printed p.16 / PDF p.22

```text
C = (P/Pn)^(1/b1) * (T/Tn)^c2
H = C * S *
    (1 + exp(b1 + b2*ln(50+b4) + b3*ln(S))) /
    (1 + exp(b1 + b2*ln(A+b4) + b3*ln(S)))
```

There is no independently estimated c1 for these species: the precipitation
exponent is exactly `1/b1`. The CSV records `map_power = 1/b1` and leaves c1
empty rather than storing a rounded derived coefficient.

### SB: equation 3.7, printed p.16 / PDF p.22

```text
C = (P/Pn)^(1/b0) * (T/Tn)^c2
H = C * b0 * S^b1 /
    (1 + exp(ln(b0*S^b1/S - 1) + b2*ln((A+1)/51)))
```

SB's precipitation exponent is exactly `1/b0`, NOT `1/b1` or an estimated c1.

## Inversion, conditioning and numerical domains

For fixed supplied climate values, C is independent of HI. Write the forward
expression as `H = C*F(A,S)` using the CLIMATE coefficients. The inverse solves
`F(A,S) = H/C` within the appropriate mathematical domain.

For the aspen form, the source's implicit inverse (equation 3.12, p.22;
equation 4.4, p.31) is:

```text
K(S) = b0*S^b1
S = K(S) /
    (1 + exp(ln(C*K(S)/H - 1) - b2*ln((A+1)/51)))
```

S occurs on both sides: this is NOT a closed-form inverse. For every form,
solve the original forward equation numerically; do not assume uniqueness
without checking the climate parameter set's monotonicity.

- AW/PB/SB require `S > 0`, `A > -1`, and `b0*S^(b1-1) > 1`.
- SW/FB require `S >= 1`, `A+b4 > 0` and `50+b4 > 0`.
- PL/FD/PJ require `S > 0`, `A+b4 > 0` and `50+b4 > 0`.
- Public inputs require finite positive age and height/HI, and finite
  positive P, T, Pn and Tn. Zero precipitation produces a zero multiplier and
  destroys inversion; negative or zero temperature ratios are not supported
  by the published fractional-power formulation.
- Stable evaluation can use `ln(C) = p*ln(P/Pn) + c2*ln(T/Tn)`, with p selected
  from c1, 1/b1 or 1/b0. Evaluate differences of logarithms rather than first
  forming potentially overflowing ratios. Never clamp invalid domains.
- Reassess turning points and inverse uniqueness with the climate coefficients;
  conclusions proved for the non-climate fits do not automatically transfer.
  This reassessment confirms that the climate SW/FB fits remain increasing:
  the negative elasticity contribution for `u = ln(HI)` is bounded by
  `1.5*abs(b3)*sqrt(u)*plogis(a+b3*u^1.5)`, where
  `a = b1+b2*ln(50+b4)`. Its maxima are approximately 0.88452 (SW) and
  0.85349 (FB), both below 1. This is a mathematical/numerical check of the
  supplied coefficients, not an additional published parameter.
  The PL/FD/PJ fits still require explicit turning-point/root-count checks
  under old-age extrapolation. For example, climate PL at age 1000 and C = 1
  has turning heights about 32.1662 and 24.5235 m; H = 25 m is ambiguous.
- At reference age, `F(50,S) = S`, hence **`H(50) = C*S`**.
  Inverting at age 50 returns inherent `S = H/C`, subject to its source domain.
- The source distinguishes inherent HI from realized HI (= height at age 50)
  explicitly in Section 4.3, printed pp.33-35. The climate variant's `si` input
  and returned `si` must be documented as inherent HI, not automatically H50.
- Setting C = 1 gives the climate fit at baseline. It does NOT give the
  separately estimated non-climate model.

## Parameter provenance and self-check

- **Approved CSV:** `data-raw/Huang2026_climate_parameters.csv`.
- **Fitted coefficients:** Table 3.1, printed p.18 / PDF p.24, Estimate column.
  One table on one page covers all eight species and 41 coefficient values.
- **Per-value coefficient locator:** source_table + page + pdf_page +
  source_label + the coefficient column name.
- **Climate ranges:** Table 3.3, printed p.20 / PDF p.26, the Min and Max columns
  for the climate averages and the 1961-1990 normals. Range locators are
  range_source_table + range_page + range_pdf_page + source_label +
  the corresponding variable and Min/Max column.
- Four decimal places are retained for coefficients, including trailing zeros.
  Range precision follows Table 3.3. Empty coefficient cells mean not applicable,
  not zero. An empty climate_years with window PY means full previous lifetime,
  not missing metadata or a zero-year window.
- **Extraction:** prose from `pdftotext -layout`; coefficients, ranges and
  benchmark numbers transcribed from 300-dpi rendered pages. No OCR was needed.
- **Images:** `tmp/Huang2026_climate_table3_1.png`,
  `tmp/Huang2026_climate_table3_3.png`,
  `tmp/Huang2026_climate_equations_p15.png` and
  `tmp/Huang2026_climate_equations_p16.png` (gitignored).
- All eight species/row associations were checked. Additional image spot-checks:
  AW c2 = -0.6043 and PJ b4 = 1.6453 in Table 3.1;
  SW MAP maximum = 885.4 and FD MWMT-normal maximum = 14.7 in Table 3.3.
- **Flagged coefficient digits:** none; extraction verified and approved.

## Numerical source benchmark plan (Tier 1)

These are published reference values, not outputs generated by package code.
Benchmark images are `tmp/Huang2026_climate_worked_p29.png`,
`tmp/Huang2026_climate_table4_2.png` and
`tmp/Huang2026_climate_table4_4.png`.

### Section 4.1 worked example, printed p.29 / PDF p.35

AW, inherent HI = 16, MAP_normal = 500, MWMT_normal = 15.5.

| Total age | Baseline MAP | Baseline MWMT | Baseline height (m) | Changed MAP | Changed MWMT | Changed height (m) |
|-----------|--------------|---------------|---------------------|-------------|--------------|--------------------|
| 20 | 500 | 15.5 | 6.97 | 400 | 18.0 | 5.91 |
| 50 | 500 | 15.5 | 16.00 | 400 | 18.0 | 13.56 |
| 80 | 500 | 15.5 | 21.85 | 400 | 18.0 | 18.52 |

Published relative change: -15.2% at every age, from `100*(C-1)`.
This example checks direct height calculation, the multiplier and the fact
that a climate fit at age 50 need not return the supplied inherent HI.

### Table 4.2, printed p.32 / PDF p.38

AW, initial observed height = 8.30 m at total age 25; MAP_normal = 500,
MWMT_normal = 15.5. Infer HI from that anchor under each scenario, retain its
full solved precision, then predict the listed ages at the same climate.

| Scenario | MAP | MWMT | Published inherent HI (m) |
|----------|-----|------|---------------------------|
| 1 | 200 | 21.5 | 22.80 |
| 2 | 400 | 18.5 | 17.79 |
| 3 | 800 | 12.0 | 11.86 |

| Total age | Scenario 1 height (m) | Scenario 2 height (m) | Scenario 3 height (m) |
|-----------|------------------------|------------------------|------------------------|
| 25 | 8.30 | 8.30 | 8.30 |
| 30 | 9.64 | 9.82 | 10.02 |
| 50 | 13.75 | 14.83 | 16.21 |
| 60 | 15.22 | 16.78 | 18.86 |
| 80 | 17.42 | 19.83 | 23.34 |
| 100 | 18.94 | 22.07 | 26.91 |
| 120 | 20.03 | 23.76 | 29.77 |
| 150 | 21.19 | 25.61 | 33.08 |
| 180 | 21.99 | 26.92 | 35.56 |

The sentence introducing Table 4.2 on p.31 says "total age 50" but immediately
specifies totage = 25. The worked example, table and figure consistently use
25 as the initial age; use 25 and record this prose typo rather than treating
it as an alternative benchmark.

### Table 4.4 selected rows, printed p.34 / PDF p.40

AW, observed height = 20 m, MAP_normal = 500 and MWMT_normal = 15.5.
Recover the inherent HI and predict realized HI at total age 50 under the
same supplied climate.

| Observed age | MAP | MWMT | Published inherent HI (m) | Published realized HI (m) |
|--------------|-----|------|---------------------------|---------------------------|
| 30 | 500 | 15.5 | 27.14 | 27.14 |
| 30 | 300 | 18.0 | 32.95 | 25.36 |
| 50 | 300 | 15.5 | 23.75 | 20.00 |
| 50 | 500 | 15.5 | 20.00 | 20.00 |
| 50 | 800 | 15.5 | 17.08 | 20.00 |
| 80 | 500 | 15.5 | 14.35 | 14.35 |

The three age-50 rows are particularly useful for catching an erroneous
`height = si` shortcut in the climate variant.

### Precision and coverage of the source benchmarks

- Initial absolute tolerance target: 0.01 m for values printed to two decimals,
  and 0.1 percentage point for percent changes printed to one decimal.
  Use the original anchor and the solver's full precision, not a published
  rounded HI as an exact input for later predictions. Investigate discrepancies
  beyond these tolerances; do not silently widen them.
- Tables A33-A40 (printed pp.79-86 / PDF pp.85-92) provide percent-change
  references for every species, using the approximate normals on p.78.
  Selected cells transcribed from rendered images are recorded below.
  They validate species-specific multipliers, not the complete height equation
  independently. The full numerical height/HI references above cover AW;
  do not claim independent exact full-height benchmarks for all species.
- Later projection benchmarks: Table 4.3 (p.33) for piecewise re-anchoring;
  Tables 4.5 (pp.36-37) and A41 (pp.87-88) for annual projection. These do not
  require a trajectory function in the current increment.

### All-species multiplier references from Tables A33-A40

At MAP = 400 mm and MWMT = 10 degrees C, the following percent height changes
are printed in the first temperature row. They are independent source readings,
not rounded outputs of the package implementation. The approximate normals
are the source's benchmark values (p.78), not public API defaults.

| Species | MAP_normal | MWMT_normal | Percent change | Table | Printed page / PDF page |
|---------|------------|-------------|----------------|-------|-------------------------|
| AW | 500 | 15.5 | 20.9 | A33 | 79 / 85 |
| SW | 500 | 15.0 | 12.6 | A34 | 80 / 86 |
| PL | 625 | 14.0 | 4.8 | A35 | 81 / 87 |
| SB | 550 | 15.0 | 32.6 | A36 | 82 / 88 |
| PB | 550 | 15.0 | 13.8 | A37 | 83 / 89 |
| FB | 625 | 14.0 | 3.6 | A38 | 84 / 90 |
| FD | 625 | 14.5 | 4.3 | A39 | 85 / 91 |
| PJ | 450 | 16.5 | 14.0 | A40 | 86 / 92 |

Saved crops: `tmp/Huang2026_climate_percent_pdf_85.png` through `_92.png`.
The Table A38 detail crop confirms that 3.6 belongs to MAP = 400; 6.2 is the
adjacent MAP = 450 value, not the selected benchmark.

## Cross-model checks and approved implementation contract

- Compare the climate fit at C = 1 against `si_huang2026()`'s non-climate fit
  for all eight species on a common age/inherent-index grid. Report absolute
  and relative differences, their direction and the largest divergences;
  do not force equality between separately estimated fits.
- Also compare the four overlapping species with
  `si_huang2009(index_age = "total")`. These comparisons assess plausibility,
  not proof of climate-model fidelity or a causal climate effect.
- Confirm positive/increasing height with fixed HI and fixed climate,
  forward/inverse round trips, baseline C = 1, and H(50) = C*HI for all species.
  Increasing MAP and increasing MWMT must have the directions implied by the
  published exponents when holding the other inputs and inherent HI fixed.
- Preserve existing positional arguments and outputs; append climate predictors
  and a scalar `variant = c("non_climate", "climate")` selector. Follow
  `rlang::arg_match` and compatible vector recycling.
- Require all four climate inputs for the climate variant. Reject accidentally
  supplied climate inputs for the non-climate variant rather than silently
  ignoring them. Return the existing one-column `height` or `si` tibble.
- Store climate coefficients as a separate internal table, assembled from the
  approved CSV through `preprocess_data.R` and listed in `internal_objs`.
  Do not overwrite or refit the approved non-climate coefficients.
- Registry/article integration must make required climate predictors explicit.
  Existing consumers must retain non-climate behaviour unless a climate scenario
  is explicitly supplied; never fabricate a baseline scenario for convenience.
- Add numerical fidelity tests and all validation branches; target 100% line
  coverage of new/changed model logic, regenerate docs, run the full tests and
  appropriate package checks, and add one brief NEWS sentence.
- **Review gate completed:** the user verified and approved Table 3.1
  coefficients, Table 3.3 ranges, the source equations and precipitation
  exponents, averaging windows, inherent vs realized HI, benchmark values and
  the point-prediction/inversion scope before climate R code was written.

## Recorded verification and comparisons

- The new climate tests failed before implementation, then passed against the
  Section 4.1 example, all three Table 4.2 scenarios, selected Table 4.4 cases,
  and every species' multiplier reference above, at the predeclared tolerances.
- Coverage of `R/si_huang2026.R` and all its helpers is **100%**, including
  default non-climate behaviour, input validation, climate extrapolation,
  inverse ambiguity and stable scaling when intermediate ratios would overflow.
  No coverage exclusions are used.
- Every pre-existing object in rebuilt `R/sysdata.rda` was checked identical;
  only `parameters_Huang2026_climate` was added. The unrelated public
  `merchcrit` object's serialization was restored after verifying object equality.
- `devtools::document()` regenerated the function's help page; the export
  remains unchanged. The full test suite passes with one pre-existing skip
  concerning a volume-parameter Province column. `devtools::check()` with
  `--no-manual` and `--no-build-vignettes` reports 0 errors, 0 warnings and the
  same 4 existing notes (development files, nonstandard top-level files,
  existing static-analysis bindings and wide `get_volume_params.Rd` examples).
  The changed model functions have no unbound-variable diagnostics.
- `tmp/generate_si_huang2026_climate_comparison_values.R` generates the two
  comparison CSVs below. They are plausibility outputs, NOT independent source
  benchmarks or estimates of a causal climate effect.

### Climate fit at C = 1 versus separately fitted non-climate model

Common total ages 20, 40, 80 and 120, and indices 10, 15 and 20:

| Species | Climate minus non-climate height range (m) | Relative difference range (%) |
|---------|--------------------------------------------|-------------------------------|
| AW | -0.0501 to +1.0896 | -0.2925 to +3.9207 |
| SW | -0.1936 to +0.2437 | -3.4318 to +1.4082 |
| PL | +0.0004 to +0.3332 | +0.0030 to +1.7929 |
| SB | -0.0896 to +0.2295 | -2.0804 to +1.1507 |
| PB | -0.0319 to +0.8923 | -0.1863 to +3.8141 |
| FB | -0.1701 to +0.2710 | -3.3698 to +1.5715 |
| FD | +0.0020 to +0.3172 | +0.0159 to +1.7022 |
| PJ | +0.0018 to +0.3264 | +0.0145 to +1.7598 |

The largest relative difference is AW at total age 20, index 10: climate
baseline height 3.8976 m versus non-climate height 3.7505 m (+0.1470 m, +3.9207%).
CSV: `tmp/si_huang2026_climate_vs_non_climate.csv`.

### Climate fit at C = 1 versus total-age GYPSY

Same common age/index grid, `si_huang2009(index_age = "total")`:

| Species | Climate minus GYPSY height range (m) | Relative difference range (%) |
|---------|-------------------------------------|-------------------------------|
| AW | -1.5687 to +2.2216 | -14.3128 to +7.9970 |
| SW | -1.9420 to +2.3877 | -30.8899 to +12.6459 |
| PL | -0.9229 to +1.6466 | -12.0801 to +9.5348 |
| SB | -0.5328 to +0.8756 | -9.6163 to +4.5243 |

The largest relative difference is SW at total age 20, index 10: climate
baseline height 2.2117 m versus GYPSY height 3.2002 m (-0.9885 m, -30.8899%).
CSV: `tmp/si_huang2026_climate_vs_huang2009.csv`.
