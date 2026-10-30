# Huang, Yang and Aitkin (2016) DBH–height models for Alberta tree species

Vectorized implementation of the population-level individual-tree
height–diameter models in Huang, Yang and Aitkin (2016; Alberta Tech.
Rep. T/609). Predicts total tree height (m) from DBH (cm), optionally by
natural-subregion group.

## Usage

``` r
hd_huang2016(DBH, species, natural_subregion = NULL, keep_model_id = FALSE)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.GLA"`;
  compact `"PICEGLA"` is accepted), or one of `"DECID"`, `"CONIF"`,
  `"ALL"` for the source's combined models.

- natural_subregion:

  Optional. Plot level. Alberta natural subregion (Natural Regions
  Committee 2006), given either as the numeric code `1`–`21` or the
  character code (e.g. `"CM"` = Central Mixedwood, `"LF"` = Lower
  Foothills, `"UF"` = Upper Foothills), as listed in Table 2 of the
  source. Length 1 or `length(DBH)`. `NULL` (default) uses the
  provincial models.

- keep_model_id:

  Logical. If `TRUE`, add columns `model_id` (`"huang2016"`) and
  `subregion_group` (the natural-subregion group of the model used, e.g.
  `"7-9"`, or `"provincial"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id` and `subregion_group`.

## Details

**Model scope (species coverage):** aspen (`POPU.TRE`), balsam poplar
(`POPU.BAL`), white birch (`BETU.PAP`), lodgepole pine (`PINU.CON`),
white spruce (`PICE.GLA`), black spruce (`PICE.MAR`), balsam fir
(`ABIE.BAL`), Douglas-fir (`PSEU.MEN`), tamarack (`LARI.LAR`) and jack
pine (`PINU.BAN`). Following the source species grouping (Table 1),
whitebark pine (`PINU.ALB`) and limber pine (`PINU.FLE`) use the
lodgepole pine equations, Engelmann spruce (`PICE.ENG`) uses white
spruce, subalpine fir (`ABIE.LAS`) uses balsam fir, and western
(`LARI.OCC`) and alpine larch (`LARI.LYA`) use tamarack; a one-time
message reports the mapping. The source's combined models are selected
with `species = "DECID"` (all deciduous species), `"CONIF"` (all
conifers) or `"ALL"` (all species combined).

**Model forms:** deciduous species and `"DECID"` use eq. \[1\], \$\$H =
1.3 + b_1 \[1 - \exp(-b_2 DBH)\]^{b_3};\$\$ conifers, `"CONIF"` and
`"ALL"` use eq. \[2\], \$\$H = 1.3 + b_1 / (1 + \exp\[b_2 + b_3
\ln(DBH + 0.01)\]).\$\$

**Height convention:** total tree height; DBH measured at 1.3 m, so
predicted height tends to 1.3 m as DBH tends to 0.

**Population-level prediction:** coefficients are those of the base
(fixed-effects) models in Table 4, which the source recommends for trees
in plots without height measurements (Section 6). Plot-specific
calibration with the mixed models (Table 5) is not implemented.

**Variants (natural-subregion groups):** if `natural_subregion` is
`NULL` (default) the provincial models are used. Otherwise the model for
the species' natural-subregion group is used (e.g. white spruce:
subregions 7–9, 10, 11, and 1–6 and 12–21). Species fitted only at the
provincial level (`BETU.PAP`, `PSEU.MEN`, `LARI.LAR`, `PINU.BAN` and
their mapped species, and the combined models), and rows with `NA`
`natural_subregion`, use the provincial model. One-time messages report
which variant is used.

Rows with `NA` `DBH` or `species` return `NA` height. DBH values outside
the range of the fitting data of the selected model (Table 3) are
computed and reported in a single warning.

## References

Huang, S., Yang, Y., and Aitkin, D. (2016). Updated population and
plot-specific individual tree height-diameter models for major Alberta
tree species. Report \#4, Tech. Rep. Pub. No. T/609. Forestry Division,
Alberta Agriculture and Forestry, Edmonton, Alberta.

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Provincial models
hd_huang2016(DBH = c(10, 25, 40), species = c("PICE.GLA", "POPU.TRE", "PINU.CON"))
#> hd_huang2016(): `natural_subregion` not supplied; using the provincial models.
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   9.94
#> 2  22.7 
#> 3  26.7 

# Natural-subregion-group models (Lower Foothills), numeric or character code
hd_huang2016(DBH = 25, species = "PICE.MAR", natural_subregion = "LF")
#> hd_huang2016(): using natural-subregion-group models where available.
#> This message is displayed once per session.
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   19.5
hd_huang2016(DBH = 25, species = "PICE.MAR", natural_subregion = 11,
             keep_model_id = TRUE)
#> # A tibble: 1 × 3
#>   height model_id  subregion_group
#>    <dbl> <chr>     <chr>          
#> 1   19.5 huang2016 11             

# Combined conifer model
hd_huang2016(DBH = 30, species = "CONIF")
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   24.1
```
