# tests/testthat/test-si_cieszewski1993.R
# testthat 3e.

test_that("si_cieszewski1993 returns a well-formed tibble when predicting height", {
  out <- si_cieszewski1993(
    age = c(25, 50),
    si = c(12, 16),
    species = "PINU.BAN"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$height)))
})

test_that("si_cieszewski1993 returns a well-formed tibble when predicting si", {
  out <- si_cieszewski1993(
    age = c(25, 50),
    height = c(9, 16),
    species = "PINU.BAN"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$si)))
})

test_that("si_cieszewski1993 recycles inputs to a common length", {
  out <- si_cieszewski1993(age = c(20, 40, 60), si = 16, species = "PINU.BAN")
  expect_equal(nrow(out), 3L)
})

test_that("si_cieszewski1993 errors on incompatible input lengths", {
  expect_error(
    si_cieszewski1993(age = c(20, 40), si = c(12, 16, 20), species = "PINU.BAN")
  )
})

test_that("si_cieszewski1993 validates inputs", {
  # both height and si supplied
  expect_error(
    si_cieszewski1993(age = 50, height = 16, si = 16, species = "PINU.BAN")
  )
  # neither supplied
  expect_error(si_cieszewski1993(age = 50, species = "PINU.BAN"))
  # non-positive age
  expect_error(si_cieszewski1993(age = 0, si = 16, species = "PINU.BAN"))
  # si/height must exceed breast height (1.3 m)
  expect_error(si_cieszewski1993(age = 50, si = 1.0, species = "PINU.BAN"))
  expect_error(si_cieszewski1993(age = 50, height = 1.2, species = "PINU.BAN"))
  # unknown species (not in the 11-species set)
  expect_error(si_cieszewski1993(age = 50, si = 16, species = "PSEU.MEN"))
  # zero-length inputs
  expect_error(
    si_cieszewski1993(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    )
  )
})

test_that("si_cieszewski1993 aborts on non-finite predictions", {
  # Valid-but-extreme inputs (finite, age > 0, si/height > 1.3) can drive the
  # closed-form curve to a non-finite value via age^a underflow; each direction
  # must abort with an informative message rather than return NaN/Inf.
  expect_error(
    si_cieszewski1993(age = 1e-300, si = 1e300, species = "PINU.BAN"),
    regexp = "Non-finite height"
  )
  expect_error(
    si_cieszewski1993(age = 1e-300, height = 1e300, species = "PINU.BAN"),
    regexp = "Non-finite site index"
  )
})

test_that("si_cieszewski1993 covers all eleven species", {
  species <- c(
    "ABIE.BAL",
    "POPU.BAL",
    "PICE.MAR",
    "PINU.BAN",
    "PINU.CON",
    "ACER.NEG",
    "POPU.TRE",
    "LARI.LAR",
    "BETU.PAP",
    "ULMU.AME",
    "PICE.GLA"
  )
  out <- si_cieszewski1993(age = 50, si = 16, species = species)
  expect_equal(nrow(out), length(species))
  expect_true(all(is.finite(out$height)))
})

# --- Validation tier 1 (fidelity vs. external SAS reference implementation) ---
# The report publishes no per-species prediction table, but the companion NRCan
# SAS macros (%HT_Ciesz_1993, %SI_Ciesz_1993) implement the exact model with the
# same coefficients. The committed comparison grid
# (tmp/generate_si_cieszewski1993_comparison_values.R) stores those SAS outputs,
# reproduced independently. A faithful implementation must match them.

test_that("si_cieszewski1993 matches the SAS macro reference (HT_Ciesz_1993)", {
  ref <- readr::read_csv(
    testthat::test_path(
      "..",
      "..",
      "tmp",
      "si_cieszewski1993_comparison_values.csv"
    ),
    show_col_types = FALSE
  )

  height <- mapply(
    function(a, s, sp) si_cieszewski1993(age = a, si = s, species = sp)$height,
    ref$age,
    ref$si,
    ref$species
  )
  expect_equal(height, ref$height_sas, tolerance = 1e-8)
})

test_that("si_cieszewski1993 matches the SAS macro reference (SI_Ciesz_1993)", {
  ref <- readr::read_csv(
    testthat::test_path(
      "..",
      "..",
      "tmp",
      "si_cieszewski1993_comparison_values.csv"
    ),
    show_col_types = FALSE
  )

  si_est <- mapply(
    function(a, h, sp) {
      si_cieszewski1993(age = a, height = h, species = sp)$si
    },
    ref$age,
    ref$height_sas,
    ref$species
  )
  expect_equal(si_est, ref$si_recovered_sas, tolerance = 1e-8)
})

# --- Exact model self-consistency identities ---

test_that("height at the base age (50 yr) equals the input site index", {
  si_in <- c(8, 12, 16, 20, 24)
  h50 <- si_cieszewski1993(age = 50, si = si_in, species = "PINU.BAN")$height
  expect_equal(h50, si_in, tolerance = 1e-9)
})

test_that("si_cieszewski1993 round-trips si -> height -> si", {
  ref <- expand.grid(
    species = c("PINU.BAN", "PICE.GLA", "POPU.TRE"),
    age = c(20, 35, 50, 90),
    si = c(10, 16, 22),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  height <- mapply(
    function(a, s, sp) si_cieszewski1993(age = a, si = s, species = sp)$height,
    ref$age,
    ref$si,
    ref$species
  )
  si_rec <- mapply(
    function(a, h, sp) si_cieszewski1993(age = a, height = h, species = sp)$si,
    ref$age,
    height,
    ref$species
  )
  expect_equal(si_rec, ref$si, tolerance = 1e-6)
})

test_that("predicted height increases with age and with site index", {
  h_by_age <- si_cieszewski1993(
    age = c(10, 20, 40, 80, 120),
    si = 16,
    species = "PINU.BAN"
  )$height
  expect_true(all(diff(h_by_age) > 0))

  h_by_si <- si_cieszewski1993(
    age = 40,
    si = c(8, 12, 16, 20, 24),
    species = "PINU.BAN"
  )$height
  expect_true(all(diff(h_by_si) > 0))
})

# --- Registry integration ---

test_that("cieszewski1993 is registered and species-mapped", {
  reg <- si_model_registry_species()
  row <- reg[reg$model_id == "cieszewski1993", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$engine, "si_cieszewski1993")
  expect_equal(row$n_species, 11L)
})
