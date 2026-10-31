# Zhang, Peng, Huang and Zhou (2002) ecoregion-based jack pine DBH–height models for Ontario

Vectorized implementation of the Chapman–Richards height–diameter models
of Zhang et al. (2002) for jack pine (`PINU.BAN`) in Ontario. Predicts
total tree height (m) from DBH (cm), by ELC ecoregion, by forest region,
or with the province-wide (Overall) fit.

## Usage

``` r
hd_zhang2002(
  DBH,
  species,
  ecoregion = NULL,
  forest_region = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground) outside bark, in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species code; only jack pine,
  `"PINU.BAN"` (compact `"PINUBAN"` is accepted), is covered.

- ecoregion:

  Optional character vector. Plot level. Ontario ecoregion code (ELC
  Working Group 2000): one of `"3E"`, `"3S"`, `"3W"`, `"4E"`, `"4S"`,
  `"4W"`, `"5E"` (case and spaces ignored, so `"3 e"` is accepted).
  Length 1 or `length(DBH)`. `NULL` (default) or `NA` uses the regional
  or Overall fit.

- forest_region:

  Optional character vector. Plot level. Ontario forest region:
  `"boreal"` or `"central"` (Great Lakes–St. Lawrence; `"glsl"` and
  `"great lakes-st. lawrence"` are also accepted; case-insensitive).
  Length 1 or `length(DBH)`. Used when `ecoregion` is `NULL`/`NA`;
  otherwise it must agree with the ecoregion. `NULL` (default) or `NA`
  uses the Overall fit when `ecoregion` is not supplied.

- keep_model_id:

  Logical. If `TRUE`, add columns `model_id` (`"zhang2002_ecoregion"`,
  `"zhang2002_regional"` or `"zhang2002_overall"`), `variant`
  (`"ecoregion"`, `"regional"` or `"overall"`) and `parameter_set` (the
  ecoregion code, `"boreal"`, `"central"` or `"overall"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`, `variant` and `parameter_set`.

## Details

**Model form** (eq. \[1\]): \$\$H = 1.3 + a \[1 - \exp(-b \cdot
DBH)\]^{c},\$\$ with \\a\\, \\b\\, \\c\\ from Table 2 of the source.
Predicted height tends to 1.3 m as DBH tends to 0.

**Fit:** ordinary nonlinear least squares (SAS PROC NLIN), fitted to 10
727 jack pine trees from Ontario permanent sample plots (trees with
forks or damaged tops excluded). The source does not state stand origin.

**Parameter sets** (selected per row):

- `ecoregion` supplied: the ecoregion fit. Ecoregions 3E, 3S, 3W, 4S and
  4W (Boreal forest region) and 4E and 5E (Central forest region) are
  covered (ELC Working Group 2000). Other ecoregion codes are rejected.

- `ecoregion` `NULL`/`NA` and `forest_region` supplied: the Boreal or
  Central regional fit.

- neither supplied: the Overall fit (all data combined).

If both are supplied, the ecoregion fit is used and `forest_region` must
match the forest region of the ecoregion. One-time messages report the
parameter sets in use. The authors found significant differences among
ecoregions and recommend the ecoregion models; the regional and Overall
fits produced significant prediction bias in individual ecoregions
(Tables 4–6 of the source).

**Ecoregion boundaries** follow the ELC Working Group (2000) map of
Ontario ecoregions used by the source; later revisions of the Ontario
ecoregion map (e.g. Crins et al. 2009) may differ.

**Ecoregion 3S caveat:** the authors note an "unusually large" asymptote
for ecoregion 3S (\\a = 79.0082\\), attributed to a lack of large trees
in the data; predictions beyond its DBH range (2.5–35.3 cm) are
unreliable.

Rows with `NA` `DBH` or `species` return `NA` height. DBH values outside
the range of the fitting data of the selected parameter set (Table 1)
are computed and reported in a single warning.

No worked example or prediction table is published, so this
implementation is validated against the equation and coefficients only
(no source benchmark).

## References

Zhang, L., Peng, C., Huang, S., and Zhou, X. (2002). Development and
evaluation of ecoregion-based jack pine height-diameter models for
Ontario. The Forestry Chronicle, 78(4), 530–538.
[doi:10.5558/tfc78530-4](https://doi.org/10.5558/tfc78530-4)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
# Ecoregion models
hd_zhang2002(DBH = c(10, 20, 30), species = "PINU.BAN", ecoregion = c("3E", "4W", "5E"))
#> hd_zhang2002(): using the ecoregion models (Table 2).
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   10.1
#> 2   17.4
#> 3   23.0

# Forest-region model when the ecoregion is unknown
hd_zhang2002(DBH = 20, species = "PINU.BAN", forest_region = "central")
#> hd_zhang2002(): `ecoregion` not supplied; using the forest-region
#> (Boreal/Central) models.
#> This message is displayed once per session.
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   19.0

# Overall (province-wide) model
hd_zhang2002(DBH = c(10, 20, 30), species = "PINU.BAN", keep_model_id = TRUE)
#> # A tibble: 3 × 4
#>   height model_id          variant parameter_set
#>    <dbl> <chr>             <chr>   <chr>        
#> 1   10.1 zhang2002_overall overall overall      
#> 2   17.8 zhang2002_overall overall overall      
#> 3   21.8 zhang2002_overall overall overall      

# Inside dplyr::mutate()
trees <- data.frame(DBH = c(12, 25, 18), ecoregion = c("3W", "3W", "4S"))
dplyr::mutate(trees, hd_zhang2002(DBH, "PINU.BAN", ecoregion = ecoregion))
#>   DBH ecoregion   height
#> 1  12        3W 12.47433
#> 2  25        3W 20.16552
#> 3  18        4S 15.48519
```
