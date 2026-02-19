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

## Online documentation

Full documentation is available at
<https://ptompalski.github.io/CanadaForestAllometry/>

## Included models

`CanadaForestAllometry` currently includes allometric models to:

- Estimate tree-level volume (total and merchantable). Read more: [Tree
  volume
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Tree-Volume-Models.html)
- Estimate aboveground biomass (AGB). Read more: [Aboveground biomass
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Tree-Aboveground-Biomass-Models.html)
- Convert volume to biomass. Read more: [National volume-to-biomass
  conversions
  models](https://ptompalski.github.io/CanadaForestAllometry/articles/Volume-to-biomass.html)
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
