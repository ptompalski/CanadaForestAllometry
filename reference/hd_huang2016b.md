# Huang and Yang (2016) expanded DBH–height models for Alberta tree species

Vectorized implementation of the population-level expanded
individual-tree height–diameter models in Huang and Yang (2016; Alberta
Tech. Rep. T/610). Predicts total tree height (m) from DBH (cm), the
basal area (m²/ha) and the mean height of the dominant and co-dominant
trees (m) of the tree's species.

## Usage

``` r
hd_huang2016b(DBH, species, HDOM, BA, keep_model_id = FALSE)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.GLA"`;
  compact `"PICEGLA"` is accepted), or one of `"DECID"`, `"CONIF"`,
  `"ALL"` for the source's combined models.

- HDOM:

  Numeric vector. Plot level. Mean height (m) of all the dominant and
  co-dominant trees with measured heights in the plot, computed for the
  tree's species (source group, Table 1), or for all deciduous, all
  conifer or all trees with `"DECID"`, `"CONIF"` or `"ALL"` (source
  symbol \\H\_{DC}\\; Section 6.2). Must be `> 0`.

- BA:

  Numeric vector. Plot level. Basal area (m²/ha) of the trees taller
  than 1.3 m of the tree's species (source group, Table 1), or of all
  deciduous, all conifer or all trees with `"DECID"`, `"CONIF"` or
  `"ALL"` (Section 6.1). Must be `>= 0`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"huang2016b"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope (species coverage):** aspen (`POPU.TRE`), balsam poplar
(`POPU.BAL`), white birch (`BETU.PAP`), lodgepole pine (`PINU.CON`),
white spruce (`PICE.GLA`), black spruce (`PICE.MAR`), Douglas-fir
(`PSEU.MEN`), tamarack (`LARI.LAR`) and jack pine (`PINU.BAN`).
Following the source species grouping (Table 1), whitebark pine
(`PINU.ALB`) and limber pine (`PINU.FLE`) use the lodgepole pine
equation, Engelmann spruce (`PICE.ENG`) uses white spruce, and western
(`LARI.OCC`) and alpine larch (`LARI.LYA`) use tamarack; a one-time
message reports the mapping. The source's combined models are selected
with `species = "DECID"` (all deciduous species), `"CONIF"` (all
conifers) or `"ALL"` (all species combined).

**Balsam fir is not available:** in the source, the balsam fir base
coefficients (Table A1, part 2) are an exact copy of the black spruce
column, while the fit statistics in Table 5 show a different balsam fir
model. Balsam fir (`ABIE.BAL`) and subalpine fir (`ABIE.LAS`, grouped
with balsam fir) therefore raise an error.

**Model forms:** deciduous species and `"DECID"` use eq. \[1\], \$\$H =
1.3 + (b_1 + b_2 BA + b_3 HDOM) \[1 - \exp(-b_4 DBH)\]^{b_5};\$\$
conifers, `"CONIF"` and `"ALL"` use eq. \[2\], \$\$H = 1.3 + (b_1 + b_2
BA + b_3 HDOM) / (1 + \exp\[b_4 + b_5 \ln(DBH + 0.01)\]).\$\$

**Height convention:** total tree height; DBH measured at 1.3 m, so
predicted height tends to 1.3 m as DBH tends to 0.

**Population-level prediction:** coefficients are those of the base
(fixed-effects) models in Table A1, which the source recommends for
trees in plots without height measurements (Section 5). Plot-specific
calibration with the mixed models is not implemented.

Rows with `NA` in any input return `NA` height. DBH, `BA` or `HDOM`
values outside the range of the fitting data of the species (Table 2)
are computed and reported in a single warning.

## References

Huang, S., and Yang, Y. (2016). Expanded individual tree height
prediction models from tree diameter and other variables. Report \#5,
Tech. Rep. Pub. No. T/610. Forestry Division, Alberta Agriculture and
Forestry, Edmonton, Alberta.

## See also

[`hd_huang2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_huang2016.md)
for the DBH-only models of the same report series;
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Aspen worked example (T/610 Table 6, plot 1)
hd_huang2016b(DBH = c(29.7, 1.6), species = "POPU.TRE",
              HDOM = 29.9333, BA = 36.8225)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1  27.2 
#> 2   3.14

trees <- data.frame(
  DBH = c(12, 25, 40),
  species = c("PICE.MAR", "PICE.GLA", "PINU.CON"),
  HDOM = c(15, 24, 20),
  BA = c(20, 25, 34)
)
dplyr::mutate(trees, hd_huang2016b(DBH, species, HDOM, BA))
#>   DBH  species HDOM BA   height
#> 1  12 PICE.MAR   15 20 12.18090
#> 2  25 PICE.GLA   24 25 21.93206
#> 3  40 PINU.CON   20 34 22.51643

# Combined conifer model (HDOM and BA of all conifers)
hd_huang2016b(DBH = 30, species = "CONIF", HDOM = 21, BA = 37)
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   23.2
```
