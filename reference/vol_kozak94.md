# Estimate tree volume using Kozak (1994) taper model (BC, BEC-zone specific)

Implements the Kozak (1994) taper/volume formulation for British
Columbia, using BEC-zone–specific parameter sets. Total volume is
computed independent of merchantability; merchantable volume is set to 0
when DBH is below the jurisdictional minimum DBH criterion.

## Usage

``` r
vol_kozak94(DBH, height, species, BEC_zone)
```

## Arguments

- DBH:

  Numeric vector of diameter at breast height (cm).

- height:

  Numeric vector of total tree height (m).

- species:

  Character vector of species codes (e.g. "TSUG.HET").

- BEC_zone:

  Character vector of BEC zone codes (e.g., "CWH", "ICH", "IDF").

## Value

A tibble with volumes (m^3): total, merchantable.

## Details

Kozak (1994) function parameters are available for the following species
and BEC-zone combinations:

|            |                                               |
|------------|-----------------------------------------------|
| Species    | BEC zones                                     |
| `ABIE.BAL` | AT, BWBS, CWH, ESSF, ICH, MH, MS, SBS, SWB    |
| `ACER.MAC` | CWH                                           |
| `ALNU.RUB` | CWH                                           |
| `BETU.PAP` | BWBS, ICH, IDF, SBS                           |
| `CHAM.NOO` | CWH                                           |
| `LARI.SPP` | BWBS, ESSF, ICH, IDF, MS                      |
| `PICE.SPP` | BWBS, CWH, ESSF, ICH, IDF, MS, SBPS, SBS, SWB |
| `PINU.ALB` | ESSF                                          |
| `PINU.CON` | BWBS, CWH, ESSF, ICH, IDF, MS, SBPS, SBS, SWB |
| `PINU.MON` | CWH, ICH, IDF                                 |
| `PINU.PON` | ICH, IDF, PP                                  |
| `POPU.BAL` | BWBS, CWH, ICH, IDF, SBS                      |
| `POPU.TRE` | BWBS, ICH, IDF, SBS                           |
| `PSEU.MEN` | CWH, ESSF, ICH, IDF, MS, PP, SBS              |
| `THUJ.PLI` | CWH, ESSF, ICH, IDF, MS                       |
| `TSUG.SPP` | CWH, ESSF, ICH, IDF, MH, SBS                  |

## Examples

``` r
# Single tree
vol_kozak94(
  DBH = 35,
  height = 25,
  species = "PSEU.MEN",
  BEC_zone = "CWH"
)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.834            0.755

# Multiple trees, vectorized
vol_kozak94(
  DBH = c(22, 30, 45),
  height = c(18, 24, 33),
  species = c("PSEU.MEN", "THUJ.PLI", "POPU.BAL"),
  BEC_zone = c("CWH", "ICH", "SBS")
)
#> # A tibble: 3 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.257            0.156
#> 2     0.704            0.604
#> 3     1.79             1.69 
```
