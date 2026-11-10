# Model spec: si_huang2026

> Equations, species mappings and coefficients verified and approved by the
> user before implementation. Non-climate implementation follows this extraction.

## Source

- **Citation:** Huang, S., Stadt, K., Hossain, K., and Odell, R. 2026.
  Improved Height Growth Models for Major Alberta Tree Species with and without
  Climate Variables. Technical Report Pub. No. T/2026-RIBS01. Alberta Ministry
  of Forestry and Parks, Government of Alberta, Edmonton, Alberta. April 2026.
  ISBN 978-1-4601-6473-0.
- **BibTeX key:** @HuangEtAl2026
- **Document:** `sources/fp-improved-height-growth-models-major-alberta-tree-species-2026 (1).pdf`
- **Model family:** si
- **Target function:** `si_huang2026`
- **Extraction scope:** the separately fitted models WITHOUT explicit climate
  variables, equations [3.2], [3.4], [3.6] and [3.8], with Table 3.4 coefficients.
  Climate models and trajectory projection are deferred, not silently represented
  by these coefficients.
- **Page convention:** printed Arabic page numbers are six less than PDF page
  numbers (e.g. printed p. 20 = PDF p. 26).

## Scope / domain of applicability

- **Jurisdiction:** Alberta; provincial fits, not natural-subregion-specific
  parameter sets.
- **Height convention:** tree height from stem analysis of dominant and
  codominant trees (Sections 2.1 and 3.1, pp. 4-5, 15). The report does not
  define H as a plot average of the 100 largest-DBH trees/ha; do not import that
  definition from the 2009 GYPSY model.
- **Age basis:** total tree age, years; NOT breast-height age.
- **Base age:** 50 years total age, fixed in these equations.
- **Height index:** observed HI is linearly interpolated from two stem sections
  encompassing age 50 (p. 16). For the non-climate models, HI maps to the
  package's `si` argument and H(50, HI) = HI.
- **Data:** 1,647 fitting trees and 3,006 independent validation trees (p. 5).
  Trees older than 180 total years were excluded. The trajectories include
  younger sections; a minimum final-tree age of 50 is NOT a minimum prediction
  age.
- **Young ages:** inversion from young trees can be inaccurate; the report
  discusses improved behaviour after approximately 15-20 total years, with
  species-dependent exceptions (pp. 25-26).
- **Extrapolation:** predictions beyond 180 years exceed the study's age
  coverage. Numerical domain restrictions below are separate from empirical
  applicability; do not present either as the other.

Table 2.1 (p. 7) describes final sectioned-tree measurements, not the ranges of
all height-age pairs or an explicit set of permitted prediction inputs:

| Label | Fitting trees | Final total age range (yr) | Final height range (m) |
|-------|---------------|----------------------------|------------------------|
| AW | 150 | 50-95 | 9.20-29.38 |
| SW | 482 | 51-178 | 10.20-33.50 |
| PL | 668 | 50-180 | 9.00-30.12 |
| SB | 139 | 50-180 | 5.50-24.26 |
| PB | 32 | 50-129 | 13.75-29.34 |
| FB | 66 | 52-179 | 11.10-28.10 |
| FD | 27 | 51-128 | 11.20-22.33 |
| PJ | 83 | 50-156 | 13.75-21.73 |

### Species mapping and pooling

Species names follow Table 1.1 (p. 3). Pooling follows Section 3.1 (p. 15).
Pooled fits have their OWN Table 3.4 coefficients, not copied coefficients from
the other species.

| Source label | Source species | NFI code | Data used to fit this species' model |
|--------------|----------------|----------|--------------------------------------|
| AW | Aspen (Populus tremuloides) | POPU.TRE | AW |
| SW | White spruce (Picea glauca) | PICE.GLA | SW |
| PL | Lodgepole pine (Pinus contorta var. latifolia) | PINU.CON | PL |
| SB | Black spruce (Picea mariana) | PICE.MAR | SB |
| PB | Balsam poplar (Populus balsamifera) | POPU.BAL | PB + AW |
| FB | Balsam fir (Abies balsamea) | ABIE.BAL | FB + SW |
| FD | Douglas-fir (Pseudotsuga menziesii) | PSEU.MEN | FD + PL |
| PJ | Jack pine (Pinus banksiana) | PINU.BAN | PJ + PL |

The report suggests PL (generally) or SB as possible proxies for larch depending
on location and management objectives, but does not publish larch coefficients.
No automatic larch mapping is applied. White birch was not represented.

## Variables and units

| Source symbol | Package argument / meaning | Units |
|---------------|----------------------------|-------|
| H | `height`: dominant/codominant tree height | m |
| totage | `age`: total tree age | years |
| HI | `si`: height index at 50 years TOTAL age | m |
| b0, b1, b2, b3, b4 | Fitted coefficients, using source units | As fitted |
| exp | Exponential function | - |
| ln | Natural logarithm | - |

No DBH, climate, breast-height offset or years-to-breast-height conversion enters
these non-climate equations.

## Model forms

The following are literal mathematical transcriptions, with `^` denoting a power
and `*` multiplication. All occurrences of HI denote the same unknown/input.

### AW and PB: equation [3.2], p. 15 (PDF p. 21)

```text
H = b0 * HI^b1 /
    (1 + exp(ln(b0 * HI^b1 / HI - 1) +
             b2 * ln((totage + 1) / (50 + 1))))
```

### SW and FB: equation [3.4], p. 15 (PDF p. 21)

```text
H = HI * (1 + exp(b1 + b2 * ln(50 + b4) +
                  b3 * (sqrt(ln(HI)))^3)) /
         (1 + exp(b1 + b2 * ln(totage + b4) +
                  b3 * (sqrt(ln(HI)))^3))
```

The cube applies to the square root of ln(HI), not to ln(HI) alone. There is NO
square root applied to the age logarithm.

### PL, PJ and FD: equation [3.6], p. 16 (PDF p. 22)

```text
H = HI * (1 + exp(b1 + b2 * ln(50 + b4) + b3 * ln(HI))) /
         (1 + exp(b1 + b2 * ln(totage + b4) + b3 * ln(HI)))
```

### SB: equation [3.8], p. 16 (PDF p. 22)

```text
H = b0 * HI^b1 /
    (1 + exp(ln(b0 * HI^b1 / HI - 1) +
             b2 * ln((totage + 1) / (50 + 1))))
```

This has the same algebraic form as [3.2], but SB has its own coefficients.

### Prediction directions and inversion

- **Height from age + HI:** directly evaluate the appropriate source equation.
- **HI from age + height:** solve `F(totage, HI) - H = 0` numerically using the
  SAME forward equation. No closed-form HI inverse is published for these fits.
  Section 3.2 illustrates implicit numerical inversion for the corresponding
  climate model [3.12]; setting C = 1 gives the following rearrangement for the
  [3.2]/[3.8] form (use Table 3.4, not climate coefficients):

```text
K(HI) = b0 * HI^b1
HI = K(HI) /
     (1 + exp(ln(K(HI) / H - 1) -
              b2 * ln((totage + 1) / (50 + 1))))
```

This is an implicit identity, not a closed-form solution; HI occurs on both
sides. The root equation above also defines inversion for [3.4]/[3.6].

### Mathematical domains and implementation safeguards

These are deductions from the published expressions, not extra fitted parameters:

- [3.2]/[3.8]: HI > 0, totage > -1, and
  `b0 * HI^(b1 - 1) > 1` for the displayed logarithm to exist.
- [3.4]: HI >= 1, totage + b4 > 0, and 50 + b4 > 0.
  HI = 1 is mathematically allowed; HI < 1 is not.
- [3.6]: HI > 0, totage + b4 > 0, and 50 + b4 > 0.
- Biological inputs must also be finite and meaningful. Do not force H to zero
  at age zero or add 1.3 m: neither operation is part of the fitted equations.
- A numerical inverse must respect its species' domain and report impossible
  inputs, missing brackets or non-convergence explicitly, never clamp an invalid
  input or return an unverified root.
- Numerical stabilizations must preserve the source equation on its domain;
  cancelling an expression does not automatically authorize extrapolation beyond
  the domain of the published form.
- **Approved extrapolation policy:** warn for total ages above 180 years rather
  than rejecting them; reject impossible and ambiguous inversions explicitly.
  PL/PJ/FD curves can cease to be monotone in HI at large extrapolated ages
  (e.g. PL at age 1000 has three HI solutions for H = 25 m).
- Inversion is performed on log(HI), partitioning PL/PJ/FD curves at all
  stationary points in the representable HI domain. For
  `a = b1 + b2*ln(50+b4)`, `b = b1 + b2*ln(age+b4)`, `v = ln(HI)`,
  the log magnitude of the negative elasticity contribution is
  `ln(-b3) + ln(1-exp(b-a)) + ln(plogis(a+b3*v)) + ln(plogis(-b-b3*v))`.
  For age > 50 this is concave in v. Its zeros delimit monotone branches,
  allowing root counting without relying on a coarse grid.
- SW/FB remain increasing in HI for every positive age: for `u = ln(HI)`,
  the negative elasticity contribution is bounded above by
  `1.5*abs(b3)*sqrt(u)*plogis(a+b3*u^1.5)`. Maximizing this bound for the
  published coefficients gives approximately 0.91569 (SW) and 0.87508 (FB),
  both strictly below 1. AW/PB/SB are also increasing in HI on their domains.

## Parameters and extraction provenance

- **Approved CSV:** `data-raw/Huang2026_parameters.csv`
- **Source:** Table 3.4 part 1, printed p. 20 / PDF p. 26 (AW, SW, PL, SB, PB);
  part 2, printed p. 21 / PDF p. 27 (FB, FD, PJ).
- **Per-value locator:** combine a row's `source_table`, `page`, `pdf_page` and
  `source_label` with the coefficient column name. Values come from the
  table's Estimate column; NOT standard errors or test statistics.
- **Precision:** four decimal places retained exactly, including trailing zeros.
  Blank coefficient cells mean not applicable, not zero.
- **Method:** coefficients transcribed from 300-dpi rendered table images;
  prose read using `pdftotext -layout`. OCR was not needed.
- **Local evidence:** `tmp/Huang2026_table3_4_part1.png` and
  `tmp/Huang2026_table3_4_part2.png` (gitignored; regenerate from source as needed).
- **Self-check:** row/species alignment checked for all eight rows. Additional
  spot-checks against the images: part 1 AW b0 = 13.5656, PL b2 = -1.7700;
  part 2 FB b4 = 4.3494, PJ b3 = -1.3563.
- **Flagged coefficient digits:** none; extraction approved by the user.

## Validation evidence and results

### Source fidelity evidence

**No exact tabulated source benchmark identified for the Table 3.4 fits.**
The numerical worked examples in Section 4, including Tables 4.2-4.5 and A41,
use the Table 3.1 CLIMATE fits; they must not be used as reference values for
this non-climate implementation, even when C is set to 1.

Graphical source references DO exist:

- Figure 3.3, p. 25 / PDF p. 31, shows AW curves at HI = 6, 9, 12, 16, 20,
  24 and 28 m. Panels 1 and 2 extend to total age 120 and 180, respectively.
- Figures A14-A20, pp. 63-69 / PDF pp. 69-75, show the other seven species'
  non-climate fits. Their HI grids are listed on p. 55: SW/PL = 3, 7, 11, 15,
  19, 22, 25; SB = 2, 5, 8, 11, 14, 17; PB = 6, 9, 12, 16, 20, 24, 28;
  FB/FD = 2, 6, 10, 14, 18, 22; PJ = 5, 10, 15, 20, 25.
- During implementation, read selected well-separated curve points independently
  from the rendered figures, record their figure/panel/HI/age locators and
  graphical reading uncertainty, then compare predictions at that precision.
  This is graphical fidelity evidence, not a tight numerical benchmark. Do not
  manufacture exact expected values by evaluating the implementation itself.
- Aggregate fit/validation statistics cannot be reproduced without the original
  observations; they are not per-input expected predictions.

Independent graphical readings used in the tests (absolute tolerance 0.6 m):

| Species | Figure (panel) | Printed page | Total age | HI (m) | Height read from figure (m) |
|---------|----------------|--------------|-----------|--------|-----------------------------|
| AW | 3.3 (2) | 25 | 180 | 28 | 38.5 |
| SW | A14 (1) | 63 | 180 | 25 | 38.0 |
| PL | A15 (1) | 64 | 180 | 25 | 34.0 |
| SB | A16 (1) | 65 | 180 | 17 | 28.5 |
| PB | A17 (1) | 66 | 150 | 28 | 37.0 |
| FB | A18 (1) | 67 | 180 | 22 | 36.0 |
| FD | A19 (1) | 68 | 180 | 22 | 31.5 |
| PJ | A20 (1) | 69 | 180 | 25 | 34.0 |

These are the highest plotted curves, read before evaluating the new engine;
the readings are not rounded model outputs. Local crops for A14-A20 are
`tmp/Huang2026_figure_page_69.png` through `_75.png`.

### Plausibility and cross-model comparison

- Primary comparator: `si_huang2009(index_age = "total")` for AW, SW, PL and
  SB, using a common total-age/total-age-index grid. Report absolute and relative
  differences, their direction and the largest divergences. Similar curve
  magnitudes are expected, not numerical identity or a universal percentage
  cutoff.
- Secondary comparator: `si_huang1994()` covers all eight species but uses
  breast-height age and a breast-height-age index. Do not pass the same numeric
  age and SI values and call them equivalent inputs. Any comparison needs
  explicit, justified age/index alignment; do not invent conversion constants.
- Confirm positive, increasing height curves on appropriate domains; H(50, HI)
  = HI for every species; and forward/inverse round trips. These are structural
  checks, NOT independent proof of fidelity.
- Unless an exact external reference is obtained, document **no exact source
  benchmark**, with graphical fidelity checks plus plausibility evidence.

### Recorded cross-model results

The comparison generator `tmp/generate_si_huang2026_comparison_values.R`
produces the model grid and `tmp/si_huang2026_vs_huang2009.csv`. Both are
generated outputs, NOT independent fidelity benchmarks. On the common grid
of total ages 20, 40, 80 and 120 and total-age-50 indices 10, 15 and 20:

| Species | New minus GYPSY height range (m) | Relative difference range (%) |
|---------|--------------------------------|-------------------------------|
| SW | -1.7483 to +2.1924 | -28.4339 to +11.0817 |
| SB | -0.4451 to +0.7453 | -7.9973 to +3.3352 |
| PL | -1.1191 to +1.3134 | -13.3722 to +7.6056 |
| AW | -1.6966 to +1.1320 | -16.5787 to +4.7866 |

The largest relative difference is SW at total age 20, HI = 10 m: new height
2.2903 m versus GYPSY 3.2002 m (difference -0.9099 m, -28.4339%).
No adjustment was made to force the distinct models to agree.

## Approved implementation contract

- `si_huang2026(age, height = NULL, si = NULL, species)`.
- Exactly one of `height`/`si`; vectorized inputs, compatible recycling, NFI
  normalization, and single-column `height`/`si` tibble outputs.
- Fixed total-age-50 index; no unrequested breast-height conversion, proxy
  species, natural-subregion selector or climate behaviour.
- Coefficient data flow, registry, reference, roxygen documentation, tests and
  NEWS are included in the non-climate implementation. Coverage target: 100%
  of new code.
- The user approved the extraction and the weaker non-climate benchmark
  evidence before R implementation, and selected warnings rather than rejection
  for ages above 180 years.

## Implementation verification

- **Validation tier:** graphical source fidelity checks at 0.6 m absolute
  reading uncertainty, plus same-family plausibility comparison; no exact
  source benchmark.
- All eight graphical comparisons pass. Conditioning at total age 50,
  forward/inverse round trips, vectorization/recycling, species normalization,
  source domains, extrapolation warnings and ambiguous-inverse errors pass.
- The new R file and its internal helpers reach **100% line coverage** under
  `covr::file_coverage()`, without coverage exclusions.
- The full test suite passes, with one unrelated existing skip for a volume
  parameter table without a Province column.
- `devtools::document()` regenerated the export and help page.
- `devtools::check()` with `--no-manual` and `--no-build-vignettes` completed
  with **0 errors, 0 warnings and 4 notes**: hidden development files,
  nonstandard top-level files, existing static-analysis bindings and wide
  example lines in `get_volume_params.Rd`. The new helper's join column uses a
  quoted `join_by()` expression to avoid adding an unbound-variable diagnostic.
- Every pre-existing object in rebuilt `R/sysdata.rda` was checked identical;
  `parameters_Huang2026` is the only added internal object.
