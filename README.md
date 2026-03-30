
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/ptompalski/CanadaForestAllometry/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/ptompalski/CanadaForestAllometry/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ptompalski/CanadaForestAllometry/graph/badge.svg)](https://app.codecov.io/gh/ptompalski/CanadaForestAllometry)
<!-- badges: end -->

# CanadaForestAllometry

`CanadaForestAllometry` is an R package that brings together a
collection of published allometric models developed for Canadian
forests. The package provides a unified interface to estimate a range of
tree- and stand-level attributes, based on models published in the
Canadian forestry literature. The focus of `CanadaForestAllometry` is on
standardization: models are implemented as faithfully as possible to
their original formulations, with transparent parameter tables,
consistent inputs/outputs, and jurisdiction-appropriate assumptions
(e.g. merchantability rules).

Part of the package standardization is to use NFI species codes in all
implemented models (e.g. `PICE.MAR` for black spruce). The package also
includes `translate_species_code()`, which can translate among
jurisdiction-specific codes, NFI and CANFI codes, common English and
French names, and scientific names.

## Development status

`CanadaForestAllometry` is under active development. Package functions
are tested, but not every published source provides reference values or
worked examples that allow the implementation to be checked against
expected numerical results. In those cases, models are implemented from
the published equations and checked for internal consistency, input
validation, and reproducibility, but external validation against
benchmark results may still be limited by the available documentation.

If a particular model is not yet implemented and you think it could be,
please let me know!

## Online documentation

Full documentation is available at
<https://ptompalski.github.io/CanadaForestAllometry/>

## Included models

`CanadaForestAllometry` currently includes allometric models to:

- Estimate tree-level volume (total and merchantable): [Tree volume
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Tree-Volume-Models.html)
- Estimate aboveground biomass (AGB): [Aboveground biomass
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Tree-Aboveground-Biomass-Models.html)
- Convert volume to biomass: [National volume-to-biomass conversions
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Volume-to-biomass.html)
- Estimate site index and site productivity: [Site index
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Site-Index-Models.html)
- Convert total volume to merchantable volume
- Apply simple growth models to estimate changes in attributes over time

## Installation

You can install the most recent version of the package by executing the
code below:

``` r
devtools::install_github("ptompalski/CanadaForestAllometry")
library(CanadaForestAllometry)
```

## License

This package is licensed under the GNU Lesser General Public License
(LGPL-3.0).

© His Majesty the King in Right of Canada, as represented by the
Minister of Natural Resources, 2026.

See the `LICENSE` file for full license terms.
