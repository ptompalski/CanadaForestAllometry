# Model spec: hd_sharmazhang2004

> Human-reviewed extraction artifact (Stage 1). Page numbers are given as
> printed journal page and PDF page (printed = PDF + 440). All tables and
> equations were verified on pages rendered at 200–220 dpi (`tmp/sharma2004/`);
> the text layer renders minus signs as "/", φ as "8" and γ as "g".

## Review decisions

- Equations and coefficients: **approved**.
- Function name: **`hd_sharmazhang2004`**.
- ⚠1 Variants: **Eq 2 (`"eq2"`, default) and Eq 5 (`"eq5"`)**, selected via
  `variant`. Eqs 3/4 are not implemented; their rows are dropped at build time.
- Registry `scope`: **`"provincial"`** (ON).
- Validation: **Tier 2 only** (plausibility); no source benchmark.

## Source

- **Citation:** Sharma, M., and Zhang, S.Y. 2004. Height–diameter models using
  stand characteristics for *Pinus banksiana* and *Picea mariana*.
  *Scandinavian Journal of Forest Research* 19(5): 442–451.
  doi:10.1080/02827580410030163 (confirmed from the PDF cover page and the
  running heads).
- **BibTeX key:** @SharmaZhang2004 (proposed; matches the `SharmaParton2007` style)
- **Document in `sources/`:** `sources/height-diameter/sharma2004_sjfr_jackpine_blackspruce_stand_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_sharmazhang2004`. It mirrors `hd_sharmaparton2007`,
  and there is no clash with `hd_sharma2016`. Internal helpers use the prefix
  `.sharmazhang2004_*`. Data-raw files use `SharmaZhang2004_hd_*`.

## Feasibility triage — verdict: GO WITH GAPS

| Check | Result |
|---|---|
| Direction | Height from DBH (+ BA, TPH, SI). ✔ |
| Model form | Eqs. (2)–(5), p.444 (PDF p4). ✔ |
| Parameters present | Table 3 (jack pine) and Table 4 (black spruce), p.445 (PDF p5). All four equations, with SE, MSE and R². ✔ |
| Parameter coverage | Two species, both tabulated. ✔ |
| Units | H m, D cm, BA m²/ha, SD trees/ha, SI m (p.444). ✔ |
| Height convention | Total height; additive 1.3 m offset ("total height equals 1.3 m when DBH = 0", p.444). ✔ |
| Extra predictors defined | BA = total plot BA (all species); TPH counts trees with DBH ≥ 2.5 cm; SI comes from the working-group species (p.443). ⚠ The SI base age and source curves are not stated (affects Eq 4 only). |
| Fit type | Fixed-effects OLS NLS (SAS NLIN), fit on a random half of the data (1200 JP / 1440 BS trees), p.443–444. ✔ |
| Applicability ranges | Table 1 (plot SI/BA/TPH) and Table 2 (DBH/height), covering model + validation data. ✔ |
| Validation | ✘ No worked example or prediction table. Fig. 3 (Eq 2) uses an unspecified "adjusted" density, so the figure cannot serve as a benchmark. → Tier 2 only. |
| Legibility | Text-based PDF; tables legible when rendered. ✔ |

**Gaps:** (G1) no fidelity benchmark; (G2) Eqs 3/4 behave degenerately as
printed (see ⚠1); (G3) SI base age unstated (Eq 4 only).

## Scope / domain of applicability

- **Jurisdiction / region:** Ontario (permanent growth plots across much of
  northern Ontario), p.443.
- **Species covered:** jack pine → `PINU.BAN`; black spruce → `PICE.MAR`.
- **Valid input ranges:** see Applicability ranges below.
- **Caveats:**
  - Trees were included regardless of origin (natural or planted).
  - Broken and dead trees, and trees with no measured height, were excluded (p.443).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | total tree height | m |
| D | DBH (breast height = 1.3 m) | cm |
| BA | stand basal area | m²/ha |
| SD | stand density | trees/ha |
| SI | site index | m |

## Model form(s)

```
(2) H = 1.3 + α·BA^δ·(1 − exp(−β·SD^φ·D))
(3) H = 1.3 + α·BA^δ·(1 − exp(−β·SD^φ·D))^γ
(4) H = 1.3 + α·BA^δ·(1 − exp(−β·SD^φ·D))^(γ·SI^η)
(5) H = 1.3 + α·(1 − exp(−β·D))^γ          (Peng et al. 2001 form, refit here)
```

- **Direction:** predict H from D (+ stand variables). The models are not inverted.
- **Paper's recommendation:**
  - Eq 2 is the preferred model (p.445, p.449–450). Eqs 3/4 "did not perform
    any better than" Eq 2, and Eq 2 "provides more accurate predictions than
    eq. (5) for both species".
  - Eq 5 is the DBH-only baseline.

## DBH–height (hd) models only

- **Height convention:** total height; the 1.3 m offset is additive in every equation.
- **Fit type:** ordinary nonlinear least squares, unweighted (p.444).
- **Coefficients used for prediction:** Tables 3/4 (the only fit).
- **Random effects / variance components:** none. The MSE is transcribed as `mse`.
- **Back-transformation:** none.
- **Variants and how they are selected:** use an explicit `variant` argument
  (`"eq2"` default, …), since the variants overlap in their inputs. The variant
  set is pending review (see ⚠1).
- **Source species mappings:** none.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|-----|-------------|-------|-------|----------|-----------------------|---------|
| DBH | D | tree | cm | TRUE | "DBH (breast height = 1.3 m)" | p.443, p.444 |
| BA | BA | plot | m²/ha | eq2/3/4 | "total tree basal area" (plot-level, all species) | p.443 |
| TPH | SD | plot | trees/ha | eq2/3/4 | "stand density of the trees with DBH ≥ 2.5 cm" | p.443 |
| si | SI | plot | m | eq4 | "site index calculated from working group species (species with the greatest basal area in the plot)"; ⚠ base age and SI curves not stated | p.443 |

## Applicability ranges

The ranges cover all data (model + validation). Columns: `*_min` / `*_max`.

| Species | DBH (cm) | Height (m) | BA (m²/ha) | TPH | SI (m) |
|---|---|---|---|---|---|
| PINU.BAN | 2.50–44.70 | 2.42–26.50 | 0.25–53.50 | 50–17625 | 5.40–31.60 |
| PICE.MAR | 2.50–40.80 | 2.06–29.84 | 1.76–53.61 | 300–17625 | 5.40–30.00 |

Sources: Table 2 p.444 (DBH, height) and Table 1 p.443 (plots).

## Parameters

CSV: `data-raw/SharmaZhang2004_hd_parameters.csv`, one row per species × variant.
It also holds the SEs, MSE, R², n_trees_fit and the ranges.

| Species | Eq | α | β | γ | δ | φ | η | MSE | R² |
|---|---|---|---|---|---|---|---|---|---|
| PINU.BAN | 2 | 5.2067 | 2.4280 | – | 0.4177 | −0.3292 | – | 2.8374 | 0.8986 |
| PINU.BAN | 3 | 5.3201 | 3.1282 | −0.4123 | 0.4496 | 0.6611 | – | 2.6640 | 0.9048 |
| PINU.BAN | 4 | 5.3296 | 3.2467 | −0.4166 | 0.4489 | 0.5570 | 0.0555 | 2.6609 | 0.9050 |
| PINU.BAN | 5 | 30.7546 | 0.0370 | 1.1342 | – | – | – | 6.1435 | 0.7803 |
| PICE.MAR | 2 | 14.6726 | 0.1175 | – | 0.2176 | −0.1146 | – | 2.770 | 0.8968 |
| PICE.MAR | 3 | 12.2395 | 0.1867 | −0.1176 | 0.2147 | 1.1919 | – | 2.7383 | 0.8980 |
| PICE.MAR | 4 | 12.1831 | 0.1903 | −0.1205 | 0.2175 | 1.0531 | 0.0368 | 2.7307 | 0.8984 |
| PICE.MAR | 5 | 28.9282 | 0.0431 | 1.2191 | – | – | – | 3.6374 | 0.8644 |

- **Source tables:** Table 3 (jack pine) and Table 4 (black spruce), p.445 (PDF p5).
- **Spot-checks against the rendered image:**
  - Every cell was checked, including signs.
  - Row/species alignment is confirmed: Table 3 is jack pine and Table 4 is black spruce.
  - BS Eq 2 MSE is printed as "2.770" (3 dp); it is kept as printed.

### ⚠ Flagged items

- **⚠1 Eqs 3/4 are degenerate as printed.**
  - Signs are verified on the image: γ < 0 and φ > 0.
  - With φ > 0, β·SD^φ·D is so large that (1 − e^…) ≈ 1 for all realistic inputs.
  - Example: JP Eq 3, BA = 25, TPH = 1500. Here β·SD^φ ≈ 393, so H ≈ 23.9 m
    whether DBH is 2.5, 10, 20 or 40 cm. BS Eq 3 gives 25.7 m at every DBH.
  - The paper's own claims, however, are only consistent with Eq 2-like behaviour:
    a mean MSE of ~2.7, and the statement that the parameters "can be
    biologically interpreted".
  - This is likely a typesetting error in Tables 3/4 (a sign or a column swap), but it cannot be resolved from the paper.
  - **Recommendation:** do not implement Eqs 3/4. They remain recorded in the CSV
    for provenance.
- **⚠2** The SI base age and site curves are unstated (Eq 4 only, which is moot if ⚠1 is accepted).
- **⚠3** Ranges are taken from the combined data (model + validation), since the paper gives no separate ranges for the model data set.
- **⚠4** Eq 5 is DBH-only. It depends on no stand variables, and the paper rates it the least accurate.

## Benchmark plan

- **Fidelity (Tier 1):** none available. The paper has no worked example or
  prediction table. Fig. 3 (Eq 2 at BA = 12.5/25/37.5/50) uses density
  "adjusted based on DBH" by an unstated rule, so it cannot be reproduced.
  ⚠ **No source benchmark.**
- **Plausibility (Tier 2):**
  - Expected Eq 2 values: at BA = 25, TPH = 1500, D = 15, JP ≈ 20.5 m and BS ≈ 17.1 m.
  - Eq 2 should be monotone increasing in DBH, with H → 1.3 as DBH → 0.
  - Qualitative Fig. 3 shape: jack pine flattens beyond DBH ≈ 14 cm; black
    spruce keeps rising to 40 cm (p.449).
  - Compare against `hd_sharmaparton2007` and `hd_sharma2016` for PINU.BAN and
    PICE.MAR (both Ontario, with the same BA/TPH predictors). Report
    discrepancies; do not assert agreement.
