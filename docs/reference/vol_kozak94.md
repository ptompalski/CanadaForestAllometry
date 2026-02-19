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
