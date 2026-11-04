# Eslamdoust (2026; TR-79) DBH-height model for Ontario white pine plantations

Predicts total tree height (m) for plantation-origin white pine
(`PINU.STR`) in Ontario from DBH and stand height, basal area and
density. This is the TR-79 model, not the natural-stand models in
[`hd_eslamdoust2026()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_eslamdoust2026.md).

## Usage

``` r
hd_eslamdoust2026b(
  DBH,
  species = "PINU.STR",
  HDOM,
  BA,
  TPH,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m), in cm.
  Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species code; only `"PINU.STR"`
  (white pine, Pinus strobus) is supported. Compact `"PINUSTR"` is
  accepted. Defaults to `"PINU.STR"`.

- HDOM:

  Numeric vector. Plot level. Stand height (m), mean height of dominant
  and codominant trees (source SHT, p. 2). The source does not specify
  the species basis. Must be `> 0`.

- BA:

  Numeric vector. Plot level. Stand basal area (m2/ha; source BAH, p.
  2). The source does not specify the minimum tree-size threshold or
  species basis. Must be `> 0`.

- TPH:

  Numeric vector. Plot level. Trees per hectare with DBH \\\ge\\ 2.5 cm
  (source TPH, Equation 1, p. 4). The source does not specify the
  species basis. Must be `> 0`.

- keep_model_id:

  Logical scalar. If `TRUE`, add `model_id`.

## Value

A tibble with `height` (m) and, if `keep_model_id = TRUE`, `model_id`
(`"eslamdoust2026b"`).

## Details

The modified Chapman-Richards equation (Equation 1, p. 4) is \$\$H =
1.3 + \alpha_0 HDOM^{\alpha_1} \[1 - \exp(-\alpha_2 (TPH /
BA)^{\alpha_3} DBH)\]^{\alpha_4}.\$\$ Population predictions use the
mixed-model fixed effects (Table 2, p. 7), with the plot random effect
on \\\alpha_0\\ set to zero. Plot-specific calibration is not
implemented.

Fitted to 2,265 trees in 41 Ontario plots. Fitting ranges (Table 1, p.
3) are DBH 2.9-58.50 cm, `HDOM` 4.57-33.56 m, `BA` 7.55-53.84 m2/ha and
`TPH` 433-4,700 stems/ha. Inputs outside these ranges are computed with
a single extrapolation warning. The report notes increased bias above
DBH 50 cm and in the highest relative-height classes.

Inputs recycle to a common length; incompatible lengths raise an error.
Rows with `NA` in any input return `NA` height.

## References

Eslamdoust, J. 2026. Height-diameter model for white pine plantations in
Ontario. Ontario Ministry of Natural Resources, Science and Research
Branch, Peterborough, ON. Science and Research Technical Report TR-79.
12 p.

## See also

[`hd_eslamdoust2026()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_eslamdoust2026.md),
[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
hd_eslamdoust2026b(DBH = c(10, 20, 30), HDOM = 18, BA = 30, TPH = 1500)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   12.0
#> 2   15.7
#> 3   17.7
hd_eslamdoust2026b(
  DBH = 25, species = "PINU.STR", HDOM = 20, BA = 35, TPH = 1200,
  keep_model_id = TRUE
)
#> # A tibble: 1 × 2
#>   height model_id       
#>    <dbl> <chr>          
#> 1   18.5 eslamdoust2026b
```
