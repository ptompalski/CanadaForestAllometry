# Sharma and Zhang (2004) DBH–height models for Ontario jack pine and black spruce

Vectorized implementation of the individual-tree height–diameter models
of Sharma and Zhang (2004) for jack pine and black spruce in northern
Ontario. Predicts total tree height (m) from DBH (cm), optionally with
stand basal area and stand density.

## Usage

``` r
hd_sharmazhang2004(
  DBH,
  species,
  variant = c("eq2", "eq5"),
  BA = NULL,
  TPH = NULL,
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Numeric vector. Tree level. Diameter at breast height (1.3 m above
  ground), in cm. Must be `>= 0`.

- species:

  Character vector. Tree level. NFI species codes (`"PINU.BAN"` or
  `"PICE.MAR"`; compact `"PINUBAN"` is accepted).

- variant:

  Model variant: `"eq2"` (default; DBH, `BA` and `TPH`) or `"eq5"` (DBH
  only).

- BA:

  Numeric vector. Plot level. Total stand basal area (m²/ha) of all
  trees in the plot, all species (source "BA", "total tree basal
  area", p. 443). Must be `> 0`. Required by `variant = "eq2"`.

- TPH:

  Numeric vector. Plot level. Stand density (stems/ha) "of the trees
  with DBH \\\ge\\ 2.5 cm" (source "SD", p. 443). Must be `> 0`.
  Required by `variant = "eq2"`.

- keep_model_id:

  Logical. If `TRUE`, add column `model_id` (`"sharmazhang2004_eq2"` or
  `"sharmazhang2004_eq5"`).

## Value

A tibble with column `height` (m) and, if `keep_model_id = TRUE`,
`model_id`.

## Details

**Model scope (species coverage):** jack pine (`PINU.BAN`) and black
spruce (`PICE.MAR`). Fitted to permanent growth study plots (400 m²)
across much of northern Ontario; trees of natural and planted origin
were both included (p. 443).

**Variants** (chosen with `variant`; Tables 3 and 4, p. 445):

- `"eq2"` (default): eq. (2), p. 444, the model the source recommends
  \$\$H = 1.3 + \alpha \\ BA^{\delta} \[1 - \exp(-\beta \\ TPH^{\phi} \\
  DBH)\].\$\$

- `"eq5"`: eq. (5), p. 444, the DBH-only Chapman–Richards model (form of
  Peng et al. 2001, refitted by the source) \$\$H = 1.3 + \alpha \[1 -
  \exp(-\beta \\ DBH)\]^{\gamma}.\$\$

Eqs. (3) and (4) are not implemented: with the published coefficients
they predict a height that does not change with DBH.

**Height convention:** total tree height; the 1.3 m breast height is
added so that predicted height equals 1.3 m when DBH is 0 (p. 444).

**Fit:** ordinary (unweighted) nonlinear least squares; there are no
random effects, so the predictions are the published fixed curves.

Rows with `NA` in any input used by the variant return `NA` height.
Inputs outside the per-species range of the data (DBH, Table 2; plot BA
and stems/ha, Table 1) are computed and reported in a single warning.

## References

Sharma, M., and Zhang, S.Y. (2004). Height–diameter models using stand
characteristics for *Pinus banksiana* and *Picea mariana*. Scandinavian
Journal of Forest Research, 19(5), 442–451.
[doi:10.1080/02827580410030163](https://doi.org/10.1080/02827580410030163)

## See also

[`hd_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/hd_model_registry.md),
[`get_hd_model_info()`](https://ptompalski.github.io/CanadaForestAllometry/reference/get_hd_model_info.md)

## Examples

``` r
hd_sharmazhang2004(
  DBH = c(10, 20, 30),
  species = c("PINU.BAN", "PICE.MAR", "PICE.MAR"),
  BA = 25, TPH = 1500
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   19.0
#> 2   20.2
#> 3   24.4

# DBH only
hd_sharmazhang2004(DBH = c(10, 20, 30), species = "PINU.BAN", variant = "eq5")
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   9.43
#> 2  16.0 
#> 3  20.8 

# Inside dplyr::mutate() with tree and plot data
trees <- data.frame(
  DBH = c(12, 25, 18),
  species = c("PINU.BAN", "PICE.MAR", "PICE.MAR"),
  BA = c(30, 30, 18), TPH = c(3000, 3000, 1200)
)
dplyr::mutate(trees, hd_sharmazhang2004(DBH, species, BA = BA, TPH = TPH))
#>   DBH  species BA  TPH   height
#> 1  12 PINU.BAN 30 3000 20.18434
#> 2  25 PICE.MAR 30 3000 22.54408
#> 3  18 PICE.MAR 18 1200 18.05406
```
