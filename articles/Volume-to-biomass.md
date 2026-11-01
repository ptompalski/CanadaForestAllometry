# National volume-to-biomass conversions models

The `CanadaForestAllometry` package implements the national
volume-to-biomass conversion models developed by Boudewyn et al.
([2007](#ref-Boudewyn2007)), which allow to translate between
merchantable stem volume and aboveground biomass components. These
models were designed to enable consistent large-scale biomass estimation
in situations where inventory systems report volume rather than directly
measured biomass. The Boudewyn et al. ([2007](#ref-Boudewyn2007))
approach combines species- and ecozone-specific conversion factors with
empirically derived component allocation equations to estimate stemwood,
bark, branches, and foliage biomass from merchantable volume. The
functions use the updated model parameters available at
https://nfi.nfis.org/en/biomass_models.

- [`v2b()`](https://ptompalski.github.io/CanadaForestAllometry/reference/v2b.md) -
  converts merchantable volume to biomass components
- [`vol_total_to_merchantable()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_total_to_merchantable.md) -
  converts total volume to merchantable volume
- [`agb_component_proportions()`](https://ptompalski.github.io/CanadaForestAllometry/reference/agb_component_proportions.md) -
  calculates component proportions (stemwood, bark, branches, foliage)
  using either merchantable volume or total aboveground biomass as input

## References

Boudewyn, P., X Song, Magnussen, S, Gillis, M.D., 2007. Model Based
Volume-to-biomass Conversion for Forested and Vegetated Land in Canada,
Forestry. Natural Resources Canada, Canadian Forest Service, Pacific
Forestry Centre, Information Report BC-X-411, Victoria, Canada.
