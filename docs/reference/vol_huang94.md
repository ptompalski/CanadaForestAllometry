# Estimate tree volume in Alberta using Kozak (1988) model form with Huang (1994) parameters

Estimate tree volume in Alberta using Kozak (1988) model form with Huang
(1994) parameters

## Usage

``` r
vol_huang94(DBH, height, species, subregion = "Province")
```

## Arguments

- DBH:

  Numeric vector. Diameter at breast height (cm).

- height:

  Numeric vector. Total height (m).

- species:

  Character vector. Species code (e.g., "PICE.MAR").

- subregion:

  Character vector. Alberta subregion name; defaults to "Province".

## Value

A tibble with volumes (m^3): total and merchantable.

## References

Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation
for Major Alberta Tree Species. Report 1 - Individual tree volume
estimation procedures for Alberta: Methods of Formulation and
Statistical Foundations. Alberta Environmental Protection, Land and
Forest Service, Forest Management Division, Edmonton, AB.

Kozak, A. (1988). A variable-exponent taper equation. *Canadian Journal
of Forest Research*, 18, 1363–1368.
