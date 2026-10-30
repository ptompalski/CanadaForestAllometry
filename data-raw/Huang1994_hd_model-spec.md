# Model spec: hd_huang1994

> Human-reviewed extraction artifact (Stage 1). Page numbers are given as
> printed page and PDF page. The PDF is a scan with **no text layer**; every
> value was transcribed from 200-dpi page renders (`tmp/huang1994hd/p-NN.png`).

## Review decisions (maintainer, 2026-10-30)

- Equations and coefficients: **approved** (flags F1–F4 kept as printed).
- Region argument: **`natural_subregion`**, reusing the 2006 subregion codes
  of `hd_huang2016()`. 1994 regions 1–20 share the numeric codes; 2006 code 21
  (Upper Boreal Highlands) uses the 1994 region-6 (Boreal Highlands) rows.
- Grassland regions 17–20: **provincial model** with a one-time message.
- Softwood/Hardwood group rows: **exposed as `species = "CONIF"` / `"DECID"`**
  (regional only). Without a usable region (`NULL`, `NA`, 17–20) they return
  `NA` height with a warning (implementation choice; no provincial group fit).
- Registry: **single row** `huang1994` (`scope = "provincial"`, AB).
- Benchmark: Tier 2 plausibility only (`tmp/generate_hd_huang1994_comparison_values.R`).

## Source

- **Citation:** Huang, S., Titus, S.J., Lakusta, T.W., and Held, R.J. 1994.
  Ecologically based individual tree height-diameter models for major Alberta
  tree species. Report #2 of *Ecologically Based Individual Tree Volume
  Estimation for Major Alberta Tree Species*. Alberta Environmental
  Protection, Land and Forest Services, Edmonton, AB. Pub. No. T/291.
  ISBN 0-7732-1274-4.
- **BibTeX key:** `@Huang1994hd` (proposed). The existing `@Huang1994` entry
  is the series title (volume reports) and `@Huang1994si` is the SI paper.
- **Document in `sources/`:** `sources/height-diameter/huang1994_t291_ecological_hd.pdf`
- **Model family:** hd
- **Target function:** `hd_huang1994`. There is no clash: `si_huang1994` has a
  different prefix. Data-raw files use `Huang1994_hd_*` to avoid
  `Huang1994_parameters.csv` (the SI model).

## Feasibility triage: verdict GO WITH GAPS

| Check | Result |
|---|---|
| Direction | Height from DBH. ✔ |
| Model form | H = 1.3 + a(1 − e^(−bD))^c, p.2 (PDF p10), repeated under Tables 1, 2, 5 and 6. ✔ |
| Parameters present | Table 1 / 5 (natural-region based) and Table 2 / 6 (provincial). The duplicate tables are identical. ✔ |
| Coverage | 11 species provincial. 6 species plus softwood and hardwood groups by natural-region group. Every regional set partitions regions 1–16 exactly. ✔ |
| Units | H total height (m); D = DBH outside bark (cm) (p.2). ✔ |
| Height convention | Total height; additive 1.3 m offset ("when D = 0, H = 1.3"). ✔ |
| Extra predictors | Natural region only (Appendix 2, Fig. 1). ✔ |
| Fit type | Weighted NLS, w = 1/D, SAS PROC NLIN, DUD method (p.14, PDF p22). Fixed effects only. ✔ |
| Applicability ranges | Table 3 p.9 (PDF p17): provincial DBH and height ranges by species. No ranges by region group or for the softwood/hardwood groups. ⚠ |
| Validation | **No worked example or reference predictions** → Tier 2 plausibility only. ⚠ |
| Legibility | Scanned, but clean. Tables 1/5 and 2/6 were cross-checked against each other. ✔ |

**Gaps:**
- (G1) No fidelity benchmark.
- (G2) The source does not say how the "Softwood group" and "Hardwood group" rows are meant to be used.
- (G3) Natural regions 17–20 (grassland) are not covered by any regional row.

## Scope / domain of applicability

- **Jurisdiction:** Alberta. Data come from 13,144 felled trees (Phase 3 inventory tree sectioning and PSP data). 165 dead trees and 415 forked or broken-top trees were removed (p.9, PDF p17).
- **Stand origin:** natural (inventory and PSP data from the 1980s–90s).
- **Species:** see the species map. Fb and Fa are combined ("Balsam/alpine fir").
- **Valid input ranges:** Table 3 (provincial, by species). See the CSV columns `DBH_min/max` and `height_min/max`.
- **Caveat (p.1, p.16):** the natural-region models "will generally provide more accurate height estimates"; the provincial models are for "making height predictions on a provincial basis".

## Variables and units

| Symbol | Meaning | Units |
|---|---|---|
| H | total tree height | m |
| D | DBH outside bark (1.3 m) | cm |
| a, b, c | coefficients (Tables 1/5, 2/6) | – |
| natural region | Alberta natural region code 1–20 (Appendix 2 p.25; Fig. 1 p.3) | code |

## Model form

```
H = 1.3 + a * (1 - exp(-b * D))^c          p.2 (PDF p10); Tables 1,2,5,6
```

- **Direction:** height from DBH only.
- H = 1.3 at D = 0. All a, b and c values are positive, so H increases monotonically towards 1.3 + a.

## DBH–height (hd) section

- **Height convention:** total height; 1.3 m additive offset.
- **Fit type:** weighted NLS (fixed effects only).
- **Population prediction:** Table 2/6 (provincial) or Table 1/5 (natural-region group).
- **Random effects / variance components:** none. MSE is the weighted MSE (Σw(H − Ĥ)²/(n − 3)) and is not used for prediction.
- **Back-transformation:** none.
- **Variants (proposed, mirrors `hd_huang2016`):**
  - `natural_subregion = NULL` → provincial model (Table 2).
  - Otherwise, the species' region-group model (Table 1).
  - Species with only a provincial model (BW, FD, LT, PJ, SE) always use the provincial model, as the source states (p.16).
- **Source species mappings:** Fb and Fa share one set ("Balsam/alpine fir"; only 16 Fa trees). No other mappings.

### Natural-region coding (key finding)

The 1994 natural-region numbers 1–20 (Appendix 2, Fig. 1) use the same numbering as `hd_huang2016_natural_subregions` (`code_numeric`, Natural Regions Committee 2006). The only difference is that in 2006, "Boreal Highlands" (6) was split into Lower (6, LBH) and Upper (21, UBH). Several renames are already recorded as old names in that lookup:

| 1994 | 2006 subregion |
|---|---|
| 3 Wetland Mixedwood | 3 Northern Mixedwood (NM) |
| 4 Sub-Arctic | 4 Boreal Subarctic (BSA) |
| 5 Peace River Lowlands | 5 Peace-Athabasca Delta (PAD) |
| 6 Boreal Highlands | 6 LBH **and** 21 UBH |
| 8 Sub-Alpine | 8 Subalpine (SA) |

**Proposal:** reuse the `natural_subregion` argument and the existing lookup, which accepts numeric 1–21 or the character codes. Code 21 (UBH) would map to 1994 region 6.

### Predictor table

| Arg | Source name | Level | Units | Required | Definition (verbatim) | Locator |
|---|---|---|---|---|---|---|
| DBH | D | tree | cm | TRUE | "diameter at breast height outside bark (cm)" | p.2 (PDF p10) |
| natural_subregion | Natural regions | plot | code 1–21 or character | FALSE | "See Appendix 2 for list of natural regions and their designation numbers. Figure 1 shows the locations of natural regions." | Table 1 footnote p.5 (PDF p13); App. 2 p.25 (PDF p33) |

### Applicability ranges

- Table 3 p.9 (PDF p17), provincial ranges by species, DBH (cm) and H (m).
- For regional rows, the CSV repeats the species' provincial range (flagged; no per-group ranges are published).
- The softwood and hardwood groups have no published ranges (left empty).

## Parameters

- **Files:**
  - `data-raw/Huang1994_hd_parameters.csv`: 36 rows (11 provincial, 25 regional).
  - `data-raw/Huang1994_hd_species_map.csv`: 12 species (Appendix 3).
- **Source tables:**
  - Provincial: Table 2 p.6 (PDF p14) = Table 6 p.17 (PDF p25).
  - Natural region: Table 1 p.5 (PDF p13) = Table 5 p.15 (PDF p23).
  - Ranges: Table 3 p.9 (PDF p17).
- **Species → NFI mapping:**
  - Sw → PICE.GLA, Lt → LARI.LAR, Se → PICE.ENG, Pl → PINU.CON, Pj → PINU.BAN
  - Aw → POPU.TRE, Bw → BETU.PAP, Pb → POPU.BAL, Sb → PICE.MAR
  - Fb → ABIE.BAL, Fa → ABIE.LAS, Fd → PSEU.MEN
- **⚠ Flagged:**
  - (F1) In Table 1/5, the Hardwood group regions "2, 14, 15, 16" have R² = 0.8155 and MSE = 0.3722. These are identical to the Softwood "1, 3, 4, 5, 6, 12, 13" row and are probably a copy error in the source. Only fit statistics are affected, not coefficients. Transcribed as printed.
  - (F2) The regional n values do not always sum to the provincial n (e.g. Aspen 3622 vs 3604; fir 413 vs 497). These are source data; transcribed as printed.
  - (F3) Regional DBH/height ranges are the species' provincial ranges (not published per group).
  - (F4) Printed p.4 is the blank back of the fold-out map (Fig. 1); no content is missing.

## Benchmark plan

- **Fidelity (Tier 1):** none. The source has no worked example or predicted-height table. Fig. 2–4 curves are graphical only.
- **Tests:** hand-computed formula values for each coefficient row, plus structural, validation and recycling tests.
- **Plausibility (Tier 2):**
  - Compare with `hd_huang2016` (same lead author, same province, the same Chapman-Richards form for deciduous species, and the same subregion numbering) over DBH 5–50 cm for shared species, both provincial and by subregion group.
  - Compare with `hd_huang2009` m9 for PICE.MAR in Lower Foothills (11).
  - Expect the same order of magnitude, with differences within about ±15%.
- ⚠ Ships with **no source benchmark**.
