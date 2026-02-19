# Convert volume to aboveground biomass components (Boudewyn et al. 2007)

High-level wrapper for the Boudewyn et al. (2007) model-based
volume-to-biomass conversion for live trees. Given gross merchantable
volume per hectare, the function:

1.  converts volume to stem wood biomass components (Tables 3–5; Eqs.
    1–3),

2.  computes biomass proportions using the volume-based
    multinomial-logit models (Table 6; Eqs. 4–7),

3.  applies the Table 7 proportion bounds (caps) to prevent unrealistic
    proportions when extrapolating,

4.  returns total aboveground biomass and components (bark, branches,
    foliage).

## Usage

``` r
v2b(
  vol_merchantable,
  species,
  jurisdiction,
  ecozone,
  include_props = FALSE,
  include_intermediates = FALSE,
  renormalize = TRUE,
  clamp_x = FALSE
)
```

## Arguments

- vol_merchantable:

  Numeric vector. Gross merchantable volume per hectare.

- species:

  Character vector. NFI species codes (e.g., `"PICE.MAR"`). Genus-level
  codes (`"PICE.SPP"`) and variety-level codes (`"PICE.MAR.AAA"`) are
  supported.

- jurisdiction:

  Character vector. Jurisdiction code (e.g., `"AB"`).

- ecozone:

  Ecozone identifier. Either: numeric ecozone code (1–15) or official
  ecozone name (English or French; case-insensitive).

- include_props:

  Logical. If TRUE, include biomass proportions (`p_sw`, `p_sb`, `p_br`,
  `p_fl`) in the output.

- include_intermediates:

  Logical. If TRUE, include intermediate stem-biomass quantities (`b_m`,
  `b_n`, `b_nm`, `b_s`) and factors (`f_nm`, `f_s`, `has_sapling`,
  `b_stem_total`).

- renormalize:

  Logical. If TRUE (default), renormalize capped proportions to sum
  to 1. This can be helpful when multiple components hit bounds.

- clamp_x:

  Logical. If TRUE, clamp `vol_merchantable` to the calibration range
  `[x_min, x_max]` from Table 7 before evaluating the proportion
  equations. Disabled by default.

## Value

A tibble with one row per input observation. By default includes:

- b_total:

  Total aboveground tree biomass (t/ha).

- b_bark:

  Bark biomass (t/ha).

- b_branches:

  Branch biomass (t/ha).

- b_foliage:

  Foliage biomass (t/ha).

Additional columns are included when `include_props` and/or
`include_intermediates` are TRUE.

## Details

The function is vectorized over all inputs and is designed to work
naturally inside
[`dplyr::mutate()`](https://rdrr.io/pkg/dplyr/man/mutate.html)
(typically followed by
[`tidyr::unnest()`](https://rdrr.io/pkg/tidyr/man/unnest.html)).

Proportion caps (Table 7). The Table 6 proportion models can yield
extreme values when extrapolated beyond the calibration range. This
implementation applies Table 7 bounds (low/high caps) to each proportion
to keep outputs within plausible limits.

Extrapolation vs clamping. By default (`clamp_x = FALSE`), the input
`vol_merchantable` is used directly. If `vol_merchantable` falls outside
the calibration range (`x_min/x_max`) reported in Table 7, results are
extrapolations, although Table 7 proportion caps are still applied.
Setting `clamp_x = TRUE` replaces `vol_merchantable` with
`min(max(vol_merchantable, x_min), x_max)` before computing proportions;
this can materially change results for high-volume stands and may lead
to systematic underestimation where `vol_merchantable > x_max`. For this
reason, clamping is disabled by default.

## Examples

``` r
# Single case (one stand)
v2b(
  vol_merchantable = 350,
  species = "PSEU.MEN",
  jurisdiction = "BC",
  ecozone = 13
)
#> # A tibble: 1 × 4
#>   b_total b_bark b_branches b_foliage
#>     <dbl>  <dbl>      <dbl>     <dbl>
#> 1    241.   33.2       24.0      6.03

# Include proportions and intermediate quantities used in the calculation
v2b(
  vol_merchantable = 350,
  species = "PSEU.MEN",
  jurisdiction = "BC",
  ecozone = 13,
  include_props = TRUE,
  include_intermediates = TRUE
)
#> # A tibble: 1 × 16
#>   b_total b_bark b_branches b_foliage  p_sw  p_sb   p_br   p_fl   b_m   b_n
#>     <dbl>  <dbl>      <dbl>     <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1    241.   33.2       24.0      6.03 0.738 0.138 0.0995 0.0250  153.  24.7
#> # ℹ 6 more variables: b_nm <dbl>, b_s <dbl>, f_nm <dbl>, f_s <dbl>,
#> #   has_sapling <lgl>, b_stem_total <dbl>

# Tidyverse workflow: compute biomass for multiple stands
# (one row per stand) and bind results as new columns.
library(dplyr)
library(tidyr)

stands <- tibble::tibble(
  stand_id = 1:3,
  vol_merchantable = c(120, 350, 300),
  species = c("PICE.MAR", "PSEU.MEN", "PINU.BAN"),
  jurisdiction = c("AB", "BC", "ON"),
  ecozone = c(5, 13, 6)
)

stands |>
  mutate(
    v2b = v2b(
      vol_merchantable = vol_merchantable,
      species = species,
      jurisdiction = jurisdiction,
      ecozone = ecozone,
      include_props = TRUE
    )
  ) |>
  unnest(v2b)
#> # A tibble: 3 × 13
#>   stand_id vol_merchantable species  jurisdiction ecozone b_total b_bark
#>      <int>            <dbl> <chr>    <chr>          <dbl>   <dbl>  <dbl>
#> 1        1              120 PICE.MAR AB                 5    136.   13.6
#> 2        2              350 PSEU.MEN BC                13    241.   33.2
#> 3        3              300 PINU.BAN ON                 6    176.   13.5
#> # ℹ 6 more variables: b_branches <dbl>, b_foliage <dbl>, p_sw <dbl>,
#> #   p_sb <dbl>, p_br <dbl>, p_fl <dbl>
```
