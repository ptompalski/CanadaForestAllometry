# Calculate tree total and merchantable volume using Nigh (2016) equations (BC)

Implementation of the British Columbia volume equations developed by
Nigh (2016). The model calculates total stem volume and merchantable
stem volume as a function of diameter at breast height (DBH) and total
tree height.

## Usage

``` r
vol_nigh2016(DBH, height, species, subregion)
```

## Source

Nigh, G.D. (2016). *Total and merchantable volume equations for common
tree species in British Columbia: by region and biogeoclimatic zone*.
Province of British Columbia, Technical Report 106.

## Arguments

- DBH:

  Numeric vector of diameter at breast height (cm).

- height:

  Numeric vector of total tree height (m).

- species:

  Character vector of species codes

- subregion:

  Character scalar or vector defining the spatial stratum for the
  coefficients. Can be `"Coast"` / `"Interior"` or a BEC zone code
  (e.g., `"CWH"`, `"ICH"`, `"BWBS"`). Must have length 1 or the same
  length as `DBH`.

## Value

A tibble with volumes (m\\^3\\):

- vol_total:

  Numeric. Total stem volume (m\\^3\\), under bark.

- vol_merchantable:

  Numeric. Merchantable stem volume (m\\^3\\), under bark.

## Details

Parameters are stratified either by region (`"Coast"`, `"Interior"`) or
by BEC zone (e.g., `"CWH"`, `"ICH"`, `"BWBS"`), depending on which
parameter row is requested via `subregion`.

Model form (Nigh 2016): \$\$V = \exp(b_0) \cdot DBH^{b_1} \cdot
H^{b_2}\$\$ where \\V\\ is volume (m\\^3\\), \\DBH\\ is in cm, and \\H\\
is in m.

Merchantable volume in Nigh (2016) is defined as total volume minus
stump (0.3 m) and top volume above a 4 cm inside-bark diameter threshold
(fixed definition in the original report).

Nigh (2016) function parameters are available for the following species
and `subregion` combinations:

|            |                                                        |
|------------|--------------------------------------------------------|
| Species    | Supported `subregion` values                           |
| `ABIE.LAS` | AT, BWBS, ESSF, ICH, IDF, Interior, MS, SBS, SWB       |
| `ABIE.SPP` | Coast, CWH, MH                                         |
| `ACER.MAC` | Coast, CWH                                             |
| `ALNU.RUB` | Coast, CWH                                             |
| `BETU.PAP` | BWBS, ICH, IDF, Interior, SBS                          |
| `CHAM.NOO` | Coast                                                  |
| `LARI.SPP` | BWBS, ESSF, ICH, IDF, Interior, MS                     |
| `PICE.SIT` | Coast                                                  |
| `PICE.SPP` | Interior                                               |
| `PINU.ALB` | ESSF, Interior                                         |
| `PINU.CON` | Coast, Interior                                        |
| `PINU.MON` | Coast, Interior                                        |
| `PINU.PON` | Interior                                               |
| `POPU.TRE` | BWBS, ICH, IDF, Interior, SBS                          |
| `POPU.TRI` | BWBS, Coast, CWH, ICH, IDF, Interior, SBS              |
| `PSEU.MEN` | CDF, Coast, CWH, ESSF, ICH, IDF, Interior, MS, PP, SBS |
| `THUJ.PLI` | Coast, CWH, ESSF, ICH, IDF, Interior, MS               |
| `TSUG.HET` | Coast, CWH, ESSF, ICH, IDF, Interior, MH, SBS          |

## Examples

``` r
# Single tree, region coefficients
vol_nigh2016(
  DBH = 35,
  height = 25,
  species = "POPU.TRI",
  subregion = "Interior"
)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.821            0.788

# Multiple trees, vectorized (mixed subregions allowed)
vol_nigh2016(
  DBH = c(22, 30, 45),
  height = c(18, 24, 33),
  species = c("PSEU.MEN", "THUJ.PLI", "POPU.TRI"),
  subregion = c("Coast", "ICH", "Interior")
)
#> # A tibble: 3 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.258            0.244
#> 2     0.661            0.626
#> 3     1.81             1.77 
```
