# Huang, Titus, Lakusta and Held (1994) DBH–height models for Alberta tree species

Vectorized implementation of the ecologically based individual-tree
height–diameter models in Huang, Titus, Lakusta and Held (1994; Alberta
Pub. T/291). Predicts total tree height (m) from DBH (cm), optionally by
natural-region group.

## Usage

``` r
hd_huang1994(DBH, species, natural_subregion = NULL, keep_model_id = FALSE)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), outside bark, in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (e.g. `"PICE.GLA"`;
  compact `"PICEGLA"` is accepted), or `"CONIF"` / `"DECID"` for the
  source's softwood / hardwood group models.

- natural_subregion:

  Optional. Plot level. Alberta natural region (source Appendix 2),
  given as the 2006 natural subregion numeric code `1`–`21` or character
  code (e.g. `"CM"` = Central Mixedwood, `"LF"` = Lower Foothills), as
  in
  [`hd_huang2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_huang2016.md).
  Length 1 or `length(DBH)`. `NULL` (default) uses the provincial
  models.

- keep_model_id:

  Logical. If `TRUE`, add columns `model_id` (`"huang1994"`) and
  `subregion_group` (the natural-region group of the model used, e.g.
  `"7,8,10"`, or `"provincial"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id` and `subregion_group`.

## Details

**Model scope (species coverage):** aspen (`POPU.TRE`), balsam poplar
(`POPU.BAL`), white birch (`BETU.PAP`), lodgepole pine (`PINU.CON`),
jack pine (`PINU.BAN`), white spruce (`PICE.GLA`), Engelmann spruce
(`PICE.ENG`), black spruce (`PICE.MAR`), balsam and subalpine fir
(`ABIE.BAL`, `ABIE.LAS`; fitted together in the source), Douglas-fir
(`PSEU.MEN`) and tamarack (`LARI.LAR`). The source's natural-region
"Softwood group" and "Hardwood group" models are selected with
`species = "CONIF"` and `"DECID"`; they exist only by natural-region
group and therefore require `natural_subregion`.

**Model form** (p. 2): \$\$H = 1.3 + a \[1 - \exp(-b\\ DBH)\]^{c},\$\$
fitted by weighted nonlinear least squares (weights \\1/DBH\\).

**Height convention:** total tree height; DBH outside bark at 1.3 m, so
predicted height tends to 1.3 m as DBH tends to 0.

**Variants (natural-region groups):** if `natural_subregion` is `NULL`
(default) the provincial models (Table 2) are used. Otherwise the model
for the species' natural-region group (Table 1) is used; the source
recommends these where the natural region is known (p. 16). The 1994
natural regions 1–20 share their numeric codes with the 2006 natural
subregions; 2006 subregion 21 (Upper Boreal Highlands), split from 1994
region 6 (Boreal Highlands), uses the region-6 models. The grassland
regions 17–20 and rows with `NA` `natural_subregion` have no regional
models and use the provincial model, as do species fitted only at the
provincial level (`BETU.PAP`, `PINU.BAN`, `PICE.ENG`, `PSEU.MEN`,
`LARI.LAR`). For `"CONIF"` and `"DECID"`, which have no provincial
model, such rows return `NA` height with a warning. One-time messages
report which variant is used.

Rows with `NA` `DBH` or `species` return `NA` height. DBH values outside
the provincial range of the fitting data for the species (Table 3) are
computed and reported in a single warning; no ranges are published for
the softwood/hardwood groups.

## References

Huang, S., Titus, S.J., Lakusta, T.W., and Held, R.J. (1994).
Ecologically based individual tree height-diameter models for major
Alberta tree species. Report \#2, Pub. No. T/291. Alberta Environmental
Protection, Land and Forest Services, Edmonton, Alberta.

## See also

[`hd_huang2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_huang2016.md),
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Provincial models
hd_huang1994(DBH = c(10, 25, 40), species = c("PICE.GLA", "POPU.TRE", "PINU.CON"))
#> hd_huang1994(): `natural_subregion` not supplied; using the provincial models.
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   10.1
#> 2   21.7
#> 3   25.5

# Natural-region-group models (Lower Foothills), numeric or character code
hd_huang1994(DBH = 25, species = "PICE.MAR", natural_subregion = "LF")
#> hd_huang1994(): using natural-region-group models where available.
#> This message is displayed once per session.
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   18.2
hd_huang1994(DBH = 25, species = "PICE.MAR", natural_subregion = 11,
             keep_model_id = TRUE)
#> # A tibble: 1 × 3
#>   height model_id  subregion_group
#>    <dbl> <chr>     <chr>          
#> 1   18.2 huang1994 7,8,9,10,11    

# Softwood group model (natural-region groups only)
hd_huang1994(DBH = 30, species = "CONIF", natural_subregion = "CM")
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   23.0
```
