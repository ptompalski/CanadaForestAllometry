# Equation-derived regression values, not a published source benchmark.
# TR-79 Equation 1 and Table 2; random effect and residual set to zero.
devtools::load_all(quiet = TRUE)

grid <- tidyr::expand_grid(
  species = "PINU.STR",
  DBH = c(2.9, 5, 10, 20, 30, 50, 58.5),
  HDOM = c(8, 18, 30),
  BA = c(10, 30, 50),
  TPH = c(500, 1500, 4000)
)
grid$height_equation <- with(
  grid,
  1.3 + 1.8711 * HDOM^0.8053 *
    (-expm1(-0.0927 * (TPH / BA)^(-0.1568) * DBH))^0.6247
)
grid$source <- "TR-79 equation-derived; no source benchmark"
write.csv(grid, "tmp/hd_eslamdoust2026b_comparison_values.csv", row.names = FALSE)

comparison <- grid |>
  dplyr::mutate(
    height_plantation = hd_eslamdoust2026b(DBH, species, HDOM, BA, TPH)$height,
    height_natural = hd_eslamdoust2026(DBH, species, HDOM, BA, TPH)$height,
    difference = height_plantation - height_natural
  )
print(comparison |>
  dplyr::summarise(
    min_difference = min(difference), max_difference = max(difference),
    mean_difference = mean(difference)
  ))
print(comparison |>
  dplyr::filter(HDOM == 18, BA == 30, TPH == 1500) |>
  dplyr::select(DBH, height_plantation, height_natural, difference))
