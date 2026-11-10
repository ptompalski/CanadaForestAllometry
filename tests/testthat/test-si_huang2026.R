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

huang2026_climate <- function(age, height = NULL, si = NULL, species = "POPU.TRE",
                             MAP = 500, MWMT = 15.5,
                             MAP_normal = 500, MWMT_normal = 15.5) {
  si_huang2026(
    age, height = height, si = si, species = species,
    MAP = MAP, MWMT = MWMT, MAP_normal = MAP_normal, MWMT_normal = MWMT_normal,
    variant = "climate"
  )
}

test_that("Huang2026 climate fit reproduces the section 4.1 numerical example", {
  # Printed p.29: exact published predictions to two decimals.
  age <- c(20, 50, 80)
  baseline <- huang2026_climate(age, si = 16)$height
  changed <- huang2026_climate(age, si = 16, MAP = 400, MWMT = 18)$height
  expect_lt(max(abs(baseline - c(6.97, 16, 21.85))), 0.01)
  expect_lt(max(abs(changed - c(5.91, 13.56, 18.52))), 0.01)
  expect_lt(max(abs(100 * (changed / baseline - 1) - (-15.2))), 0.1)
  expect_false(isTRUE(all.equal(
    baseline, si_huang2026(age, si = 16, species = "POPU.TRE")$height
  )))
})

test_that("Huang2026 climate inversion and forward predictions match Table 4.2", {
  age <- c(25, 30, 50, 60, 80, 100, 120, 150, 180)
  expected <- list(
    c(8.30, 9.64, 13.75, 15.22, 17.42, 18.94, 20.03, 21.19, 21.99),
    c(8.30, 9.82, 14.83, 16.78, 19.83, 22.07, 23.76, 25.61, 26.92),
    c(8.30, 10.02, 16.21, 18.86, 23.34, 26.91, 29.77, 33.08, 35.56)
  )
  for (i in seq_len(3)) {
    MAP <- c(200, 400, 800)[[i]]
    MWMT <- c(21.5, 18.5, 12)[[i]]
    # Scenarios 1 and 3 intentionally extrapolate the published climate ranges.
    si <- suppressWarnings(huang2026_climate(
      25, height = 8.3, MAP = MAP, MWMT = MWMT
    ))$si
    h <- suppressWarnings(huang2026_climate(
      age, si = si, MAP = MAP, MWMT = MWMT
    ))$height
    expect_lt(abs(si - c(22.80, 17.79, 11.86)[[i]]), 0.01)
    expect_lt(max(abs(h - expected[[i]])), 0.01)
  }
})

test_that("Huang2026 distinguishes inherent and realized HI as in Table 4.4", {
  age <- c(30, 30, 50, 50, 50, 80)
  MAP <- c(500, 300, 300, 500, 800, 500)
  MWMT <- c(15.5, 18, 15.5, 15.5, 15.5, 15.5)
  si <- suppressWarnings(huang2026_climate(
    age, height = 20, MAP = MAP, MWMT = MWMT
  ))$si
  realized <- suppressWarnings(huang2026_climate(
    50, si = si, MAP = MAP, MWMT = MWMT
  ))$height
  expect_lt(max(abs(si - c(27.14, 32.95, 23.75, 20, 17.08, 14.35))), 0.01)
  expect_lt(max(abs(realized - c(27.14, 25.36, 20, 20, 20, 14.35))), 0.01)
})

test_that("Huang2026 climate multipliers match Tables A33-A40 for every species", {
  # First row (MWMT=10), MAP=400; printed pp.79-86. Normals: p.78.
  expect_warning(
    h <- huang2026_climate(
      50, si = 15, species = huang2026_species, MAP = 400, MWMT = 10,
      MAP_normal = c(500, 500, 625, 550, 550, 625, 625, 450),
      MWMT_normal = c(15.5, 15, 14, 15, 15, 14, 14.5, 16.5)
    )$height,
    class = "cfa_huang2026_climate_extrapolation"
  )
  expect_lt(max(abs(
    100 * (h / 15 - 1) - c(20.9, 12.6, 4.8, 32.6, 13.8, 3.6, 4.3, 14)
  )), 0.1)
})

test_that("Huang2026 climate inputs recycle and round-trip for every species", {
  MAP_normal <- c(500, 500, 625, 550, 550, 625, 625, 450)
  MWMT_normal <- c(15.5, 15, 14, 15, 15, 14, 14.5, 16.5)
  for (age in c(1, 25, 50, 100, 180)) {
    baseline <- huang2026_climate(
      age, si = 15, species = huang2026_species,
      MAP = MAP_normal, MWMT = MWMT_normal,
      MAP_normal = MAP_normal, MWMT_normal = MWMT_normal
    )$height
    changed <- huang2026_climate(
      age, si = 15, species = huang2026_species,
      MAP = 0.9 * MAP_normal, MWMT = 1.01 * MWMT_normal,
      MAP_normal = MAP_normal, MWMT_normal = MWMT_normal
    )$height
    back <- huang2026_climate(
      age, height = changed, species = huang2026_species,
      MAP = 0.9 * MAP_normal, MWMT = 1.01 * MWMT_normal,
      MAP_normal = MAP_normal, MWMT_normal = MWMT_normal
    )$si
    expect_true(all(changed < baseline & changed > 0))
    expect_equal(back, rep(15, 8), tolerance = 1e-7)
    if (age == 50) expect_equal(baseline, rep(15, 8))
  }
  trees <- tibble::tibble(age = c(25, 50, 80), MAP = c(400, 500, 600))
  out <- dplyr::mutate(
    trees,
    height = huang2026_climate(age, si = 16, MAP = MAP)$height
  )
  expect_equal(
    out$height, huang2026_climate(trees$age, si = 16, MAP = trees$MAP)$height
  )
  expect_error(huang2026_climate(c(25, 50), si = 16, MAP = 1:3), "Incompatible")
})

test_that("Huang2026 requires explicit valid climate inputs only for climate fits", {
  climate <- list(MAP = 500, MWMT = 15.5, MAP_normal = 500, MWMT_normal = 15.5)
  for (arg in names(climate)) {
    args <- c(list(age = 50, si = 16, species = "POPU.TRE"), climate)
    args[[arg]] <- NULL
    expect_error(
      do.call(si_huang2026, c(args, list(variant = "climate"))),
      "requires.*climate inputs"
    )
    for (bad in list(NA_real_, Inf, -1, 0, "500", numeric())) {
      invalid <- climate
      invalid[[arg]] <- bad
      expect_error(do.call(
        si_huang2026,
        c(list(age = 50, si = 16, species = "POPU.TRE", variant = "climate"), invalid)
      ))
    }
    expect_error(
      do.call(si_huang2026, c(
        list(age = 50, si = 16, species = "POPU.TRE"), climate[arg]
      )),
      "only.*climate"
    )
  }
  expect_error(
    si_huang2026(50, si = 16, species = "POPU.TRE", variant = "unknown"),
    "variant"
  )
  expect_error(huang2026_climate(50, si = 100), "model domain")
  expect_error(huang2026_climate(50, height = 100), "No site-index")
  expect_error(
    huang2026_climate(50, si = 0.5, species = "PICE.GLA"), "at least 1"
  )
})

test_that("Huang2026 warns without clipping climate extrapolation", {
  expect_warning(
    h <- huang2026_climate(50, si = 16, MAP = 1000)$height,
    "MAP", class = "cfa_huang2026_climate_extrapolation"
  )
  expect_gt(h, huang2026_climate(50, si = 16, MAP = 766.4)$height)
  expect_warning(
    huang2026_climate(50, si = 16, MAP_normal = 1000),
    "MAP_normal", class = "cfa_huang2026_climate_extrapolation"
  )
})

test_that("Huang2026 climate scaling avoids overflowing intermediate ratios", {
  expect_warning(
    h <- huang2026_climate(
      50, si = 16, MAP = 1e300, MAP_normal = 1e-300,
      MWMT = 1e300, MWMT_normal = 1e-300
    )$height,
    class = "cfa_huang2026_climate_extrapolation"
  )
  expect_true(is.finite(h) && h > 0)
  expect_warning(
    back <- huang2026_climate(
      50, height = h, MAP = 1e300, MAP_normal = 1e-300,
      MWMT = 1e300, MWMT_normal = 1e-300
    )$si,
    class = "cfa_huang2026_climate_extrapolation"
  )
  expect_equal(back, 16, tolerance = 1e-7)
})

test_that("Huang2026 climate inversion rejects ambiguous extrapolated pairs", {
  expect_warning(
    expect_error(
      huang2026_climate(
        1000, height = 25, species = "PINU.CON",
        MAP = 625, MWMT = 14, MAP_normal = 625, MWMT_normal = 14
      ),
      "Multiple site-index solutions"
    ),
    class = "cfa_huang2026_extrapolation"
  )
})

test_that("Huang2026 climate variant is discoverable without implicit scenarios", {
  reg <- si_model_registry_species()
  row <- reg[reg$model_id == "huang2026_climate", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$fixed_args[[1]], list(variant = "climate"))
  expect_equal(row$params_key, "parameters_Huang2026_climate")
  expect_setequal(row$species[[1]], huang2026_species)
  expect_match(row$description, "1961-1990")
  expect_error(
    do.call(get(row$engine), c(
      list(age = 50, si = 16, species = "POPU.TRE"), row$fixed_args[[1]]
    )),
    "requires.*climate inputs"
  )
})
