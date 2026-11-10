huang2026_species <- c(
  "POPU.TRE", "PICE.GLA", "PINU.CON", "PICE.MAR",
  "POPU.BAL", "ABIE.BAL", "PSEU.MEN", "PINU.BAN"
)

test_that("si_huang2026 reproduces independently read source curves", {
  # Highest curves: Fig. 3.3(2), p.25 and Figs. A14-A20(1), pp.63-69.
  # Graphical readings only: 0.6 m absolute uncertainty, not exact benchmarks.
  si <- c(28, 25, 25, 17, 28, 22, 22, 25)
  age <- c(180, 180, 180, 180, 150, 180, 180, 180)
  expected <- c(38.5, 38.0, 34.0, 28.5, 37.0, 36.0, 31.5, 34.0)
  out <- si_huang2026(age, si = si, species = huang2026_species)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_true(all(abs(out$height - expected) < 0.6))
})

test_that("si_huang2026 conditions and inverts every species at total age 50", {
  si <- c(20, 15, 15, 10, 20, 10, 10, 15)
  expect_equal(
    si_huang2026(50, si = si, species = huang2026_species)$height,
    si
  )
  out <- si_huang2026(50, height = si, species = huang2026_species)
  expect_named(out, "si")
  expect_equal(out$si, si, tolerance = 1e-7)
  expect_no_warning(
    near_base <- si_huang2026(50 + 1e-14, height = 15, species = "PINU.BAN")
  )
  expect_equal(near_base$si, 15, tolerance = 1e-7)
})

test_that("si_huang2026 round-trips over its fitted age range", {
  grid <- expand.grid(
    species = huang2026_species, age = c(1, 15, 40, 80, 180),
    si = c(2, 8, 16, 25), stringsAsFactors = FALSE
  )
  h <- si_huang2026(grid$age, si = grid$si, species = grid$species)$height
  back <- si_huang2026(grid$age, height = h, species = grid$species)$si
  expect_equal(back, grid$si, tolerance = 1e-7)
  for (sp in huang2026_species) {
    h <- si_huang2026(c(1, 20, 50, 100, 180), si = 15, species = sp)$height
    expect_true(all(diff(h) > 0))
  }
})

test_that("si_huang2026 recycles and normalizes species in mutate", {
  out <- si_huang2026(c(25, 50, 80), si = 15, species = " picegla ")
  expect_equal(nrow(out), 3L)
  trees <- tibble::tibble(age = c(25, 50, 80), si = 15)
  predicted <- dplyr::mutate(
    trees,
    height = si_huang2026(age, si = si, species = "PICE.GLA")$height
  )
  expect_equal(predicted$height, out$height)
  expect_equal(
    si_huang2026(50, si = 15, species = huang2026_species)$height,
    rep(15, 8)
  )
  expect_error(
    si_huang2026(c(20, 30), si = c(10, 15, 20), species = "PICE.GLA"),
    "Incompatible input lengths"
  )
})

test_that("si_huang2026 validates presence, length and numeric inputs", {
  expect_error(si_huang2026(50, species = "PICE.GLA"), "exactly one")
  expect_error(
    si_huang2026(50, height = 20, si = 20, species = "PICE.GLA"),
    "exactly one"
  )
  expect_error(
    si_huang2026(numeric(), si = numeric(), species = character()),
    "length"
  )
  expect_error(si_huang2026(numeric(), si = 15, species = "PICE.GLA"), "length")
  for (bad in list(NA_real_, Inf, -1, 0, "50")) {
    expect_error(si_huang2026(bad, si = 15, species = "PICE.GLA"))
    expect_error(si_huang2026(50, si = bad, species = "PICE.GLA"))
    expect_error(si_huang2026(50, height = bad, species = "PICE.GLA"))
  }
  for (bad in c("LARI.LAR", "BETU.PAP", "PICE.SPP", NA_character_)) {
    expect_error(
      si_huang2026(50, si = 15, species = bad),
      "No Huang2026 parameters"
    )
  }
})

test_that("si_huang2026 enforces source-specific logarithm domains", {
  for (sp in c("POPU.TRE", "POPU.BAL", "PICE.MAR")) {
    expect_error(si_huang2026(50, si = 100, species = sp), "model domain")
    expect_error(si_huang2026(50, height = 100, species = sp), "No site-index")
  }
  for (sp in c("PICE.GLA", "ABIE.BAL")) {
    expect_error(si_huang2026(50, si = 0.9, species = sp), "at least 1")
    expect_error(si_huang2026(50, height = 0.9, species = sp), "No site-index")
    expect_equal(si_huang2026(50, si = 1, species = sp)$height, 1)
    expect_equal(si_huang2026(50, height = 1, species = sp)$si, 1)
  }
  expect_equal(si_huang2026(50, si = 0.5, species = "PINU.CON")$height, 0.5)
  expect_equal(
    si_huang2026(50, height = 0.5, species = "PINU.CON")$si, 0.5
  )
})

test_that("si_huang2026 warns on extrapolation and rejects ambiguous inverses", {
  expect_warning(
    h <- si_huang2026(200, si = 15, species = "POPU.TRE")$height,
    "180", class = "cfa_huang2026_extrapolation"
  )
  expect_warning(
    back <- si_huang2026(200, height = h, species = "POPU.TRE")$si,
    "180"
  )
  expect_equal(back, 15, tolerance = 1e-7)
  expect_warning(
    expect_error(
      si_huang2026(1000, height = 25, species = "PINU.CON"),
      "Multiple site-index solutions"
    ),
    "180"
  )
  expect_warning(
    back <- si_huang2026(1000, height = 40, species = "PINU.CON")$si,
    "180"
  )
  expect_warning(
    expect_equal(
      si_huang2026(1000, si = back, species = "PINU.CON")$height,
      40, tolerance = 1e-7
    ),
    "180"
  )
  expect_warning(
    expect_error(
      si_huang2026(1e300, height = 25, species = "PINU.CON"),
      "Multiple site-index solutions"
    ),
    "180"
  )
})

test_that("si_huang2026 reports predictions below representable precision", {
  expect_error(
    si_huang2026(
      1, si = .Machine$double.xmin * .Machine$double.eps, species = "POPU.TRE"
    ),
    "Non-finite or non-positive prediction"
  )
})

test_that("si_huang2026 has plausible magnitudes against total-age GYPSY", {
  # Sanity check, not source fidelity: both inputs use TOTAL-age-50 indices.
  grid <- expand.grid(
    species = huang2026_species[1:4], age = c(20, 40, 80, 120),
    si = c(10, 15, 20), stringsAsFactors = FALSE
  )
  h26 <- si_huang2026(grid$age, si = grid$si, species = grid$species)$height
  h09 <- si_huang2009(
    grid$age, si = grid$si, species = grid$species, index_age = "total"
  )$height
  expect_true(all(is.finite(h09) & h09 > 0))
  expect_true(all(abs(h26 - h09) / h09 < 0.5))
})

test_that("si_huang2026 is discoverable with its compiled parameter table", {
  reg <- si_model_registry_species()
  row <- reg[reg$model_id == "huang2026", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$age_basis, "total_age")
  expect_equal(row$age_domain_max, 180)
  expect_equal(row$params_key, "parameters_Huang2026")
  expect_setequal(row$species[[1]], huang2026_species)
  expect_equal(
    nrow(.get_internal_data("parameters_Huang2026")), 8L
  )
})
