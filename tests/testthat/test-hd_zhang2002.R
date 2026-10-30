# hd_zhang2002(): Zhang, Peng, Huang & Zhou (2002), For. Chron. 78:530-538.

# Source eq. [1], for independent checks.
eq1 <- function(DBH, a, b, c) 1.3 + a * (1 - exp(-b * DBH))^c

# Table 2, p. 533 (and Table 1 DBH ranges, p. 532).
tab2 <- tibble::tribble(
  ~set, ~ecoregion, ~forest_region, ~a, ~b, ~c, ~DBH_min, ~DBH_max,
  "3E", "3E", NA, 25.3621, 0.0735, 1.6178, 1.40, 51.30,
  "3S", "3S", NA, 79.0082, 0.0082, 0.8275, 2.50, 35.30,
  "3W", "3W", NA, 21.8002, 0.1085, 2.1053, 1.30, 42.50,
  "4E", "4E", NA, 25.5031, 0.0545, 0.9629, 3.10, 39.50,
  "4S", "4S", NA, 22.8653, 0.0807, 1.7913, 2.50, 37.00,
  "4W", "4W", NA, 19.0772, 0.1245, 1.9955, 2.50, 33.50,
  "5E", "5E", NA, 23.1614, 0.1065, 1.5542, 3.40, 35.20,
  "boreal", NA, "boreal", 23.9313, 0.0812, 1.7149, 1.30, 51.30,
  "central", NA, "central", 25.1702, 0.0650, 1.1038, 3.10, 39.50,
  "overall", NA, NA, 24.0677, 0.0808, 1.7062, 1.30, 51.30
)

hd <- function(...) suppressMessages(suppressWarnings(hd_zhang2002(...)))

test_that("hd_zhang2002 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PINU.BAN")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)

  ids <- hd(
    DBH = c(10, 10, 10, NA, 10),
    species = c("PINU.BAN", "PINU.BAN", "PINU.BAN", "PINU.BAN", NA),
    ecoregion = c("4W", NA, NA, "3E", "3E"),
    forest_region = c(NA, "glsl", NA, NA, NA),
    keep_model_id = TRUE
  )
  expect_identical(names(ids), c("height", "model_id", "variant", "parameter_set"))
  expect_identical(
    ids$model_id,
    c("zhang2002_ecoregion", "zhang2002_regional", "zhang2002_overall", "zhang2002_ecoregion", NA)
  )
  expect_identical(ids$variant, c("ecoregion", "regional", "overall", "ecoregion", NA))
  expect_identical(ids$parameter_set, c("4W", "central", "overall", "3E", NA))
  expect_identical(is.na(ids$height), c(FALSE, FALSE, FALSE, TRUE, TRUE))

  expect_identical(
    hd_zhang2002(numeric(), "PINU.BAN"),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_zhang2002(numeric(), "PINU.BAN", keep_model_id = TRUE),
    tibble::tibble(
      height = numeric(), model_id = character(),
      variant = character(), parameter_set = character()
    )
  )
})

# --- coefficient wiring (Table 2) ----------------------------------------------

test_that("predictions use the Table 2 coefficients for every parameter set", {
  DBH <- c(0, 2.5, 10, 25, 50)
  for (i in seq_len(nrow(tab2))) {
    r <- tab2[i, ]
    expect_equal(
      hd(DBH, "PINU.BAN", ecoregion = r$ecoregion, forest_region = r$forest_region)$height,
      eq1(DBH, r$a, r$b, r$c),
      info = r$set
    )
  }
  expect_equal(hd(20, "PINUBAN"), hd(20, "PINU.BAN"))
  expect_equal(hd(20, "PINU.BAN", ecoregion = " 3 e "), hd(20, "PINU.BAN", ecoregion = "3E"))
  expect_equal(hd(20, "PINU.BAN", ecoregion = factor("3W")), hd(20, "PINU.BAN", ecoregion = "3W"))
})

test_that("parameter-set selection follows ecoregion > forest_region > overall", {
  b <- eq1(20, 23.9313, 0.0812, 1.7149)
  cn <- eq1(20, 25.1702, 0.0650, 1.1038)
  ov <- eq1(20, 24.0677, 0.0808, 1.7062)
  e3 <- eq1(20, 25.3621, 0.0735, 1.6178)

  # forest_region aliases, case-insensitive
  for (fr in c("central", "Central", "GLSL", "glsl", "Great Lakes-St. Lawrence", "great  lakes-st. lawrence")) {
    expect_equal(hd(20, "PINU.BAN", forest_region = fr)$height, cn, info = fr)
  }
  expect_equal(hd(20, "PINU.BAN", forest_region = "BOREAL")$height, b)
  expect_equal(hd(20, "PINU.BAN", forest_region = factor("boreal"))$height, b)
  # NA ecoregion falls back per row
  expect_equal(
    hd(20, "PINU.BAN", ecoregion = c("3E", NA, NA), forest_region = c("boreal", "boreal", NA))$height,
    c(e3, b, ov)
  )
  # all-NA (logical) region inputs are treated as not supplied
  expect_equal(hd(20, "PINU.BAN", ecoregion = NA, forest_region = NA)$height, ov)
  # consistent ecoregion + forest_region uses the ecoregion fit
  expect_equal(hd(20, "PINU.BAN", ecoregion = "5E", forest_region = "glsl"), hd(20, "PINU.BAN", ecoregion = "5E"))
})

test_that("parameter sets in use are reported once per session", {
  for (id in c("ecoregion", "regional", "overall")) {
    rlang::reset_message_verbosity(paste0("hd_zhang2002_", id))
  }
  expect_message(hd_zhang2002(20, "PINU.BAN"), "Overall")
  expect_no_message(hd_zhang2002(20, "PINU.BAN"))
  expect_message(hd_zhang2002(20, "PINU.BAN", ecoregion = "3E"), "ecoregion models")
  expect_message(hd_zhang2002(20, "PINU.BAN", forest_region = "boreal"), "forest-region")
  expect_no_message(hd_zhang2002(20, NA_character_))
})

test_that("hd_zhang2002 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18),
    ecoregion = c("3W", "3W", "4S")
  )
  res <- suppressMessages(
    dplyr::mutate(trees, hd_zhang2002(DBH, "PINU.BAN", ecoregion = ecoregion))
  )
  expect_equal(res$height, hd(trees$DBH, "PINU.BAN", ecoregion = trees$ecoregion)$height)
  expect_equal(nrow(hd(c(10, 20, 30), "PINU.BAN", ecoregion = "4E")), 3L)
  expect_error(hd(1:3, c("PINU.BAN", "PINU.BAN")), "Incompatible")
  expect_error(hd(1:3, "PINU.BAN", ecoregion = c("3E", "3S")), "Incompatible")
  expect_error(hd(1:3, "PINU.BAN", forest_region = c("boreal", "central")), "Incompatible")
})

test_that("hd_zhang2002 validates inputs", {
  expect_error(hd(-1, "PINU.BAN"), ">= 0")
  expect_error(hd("10", "PINU.BAN"), "numeric")
  expect_error(hd(Inf, "PINU.BAN"), "finite")
  expect_error(hd(10, 1), "character")
  expect_error(hd(10, "PINU.BAN", keep_model_id = NA), "keep_model_id")
  expect_error(hd(10, "PICE.MAR"), "not covered")
  expect_error(hd(10, "PINU.SPP"), "not covered")
  expect_error(hd(10, "PINU.BAN", ecoregion = "6E"), "Invalid `ecoregion`")
  expect_error(hd(10, "PINU.BAN", ecoregion = 3), "character")
  expect_error(hd(10, "PINU.BAN", forest_region = "taiga"), "Invalid `forest_region`")
  expect_error(hd(10, "PINU.BAN", forest_region = 1), "character")
  expect_error(hd(10, "PINU.BAN", ecoregion = "3E", forest_region = "central"), "disagree")
  expect_error(hd(10, "PINU.BAN", ecoregion = "4E", forest_region = "boreal"), "disagree")
})

test_that("DBH outside the Table 1 range of the selected set warns once", {
  expect_warning(
    suppressMessages(hd_zhang2002(c(2, 36, 20), "PINU.BAN", ecoregion = "3S")),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  # 2 cm is inside the Overall range (1.3-51.3) but outside 4E (3.1-39.5)
  expect_no_warning(suppressMessages(hd_zhang2002(2, "PINU.BAN")))
  expect_warning(
    suppressMessages(hd_zhang2002(2, "PINU.BAN", ecoregion = "4E")),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
})

# --- TIER: plausibility (no source benchmark) ----------------------------------
# The source publishes no worked example or prediction table (Tables 4-6 are
# aggregate errors over unpublished data). These are sanity checks, not
# fidelity tests. See tmp/generate_hd_zhang2002_comparison_values.R.

test_that("curves start at 1.3 m and increase monotonically with DBH", {
  DBH <- seq(0, 50, by = 0.5)
  for (i in seq_len(nrow(tab2))) {
    r <- tab2[i, ]
    h <- hd(DBH, "PINU.BAN", ecoregion = r$ecoregion, forest_region = r$forest_region)$height
    expect_equal(h[1], 1.3, info = r$set)
    expect_true(all(diff(h) > 0), info = r$set)
  }
})

test_that("relative ordering matches the source's findings (plausibility)", {
  at <- function(DBH, ...) hd(DBH, "PINU.BAN", ...)$height
  # Central fits above Boreal at mid DBH (Overall/Boreal under-predict in 4E/5E,
  # Tables 4-5); Overall over-predicts large trees in 4S and 4W (Fig. 2).
  expect_gt(at(20, forest_region = "central"), at(20, forest_region = "boreal"))
  expect_gt(at(20, ecoregion = "5E"), at(20))
  expect_lt(at(35, ecoregion = "4S"), at(35))
  expect_lt(at(35, ecoregion = "4W"), at(35))
  # Ecoregion curves within 35 % of the Overall curve over 10-35 cm (Central
  # 4E/5E are ~25-30 % higher at 10 cm; all others within 10 %).
  for (eco in tab2$ecoregion[!is.na(tab2$ecoregion)]) {
    r <- at(c(10, 20, 35), ecoregion = eco) / at(c(10, 20, 35))
    expect_true(all(abs(r - 1) < 0.35), info = eco)
  }
})

test_that("Overall curve is close to other jack pine models (plausibility)", {
  DBH <- c(10, 20, 30)
  z <- hd(DBH, "PINU.BAN")$height
  # Alberta provincial jack pine (Huang et al. 2016): same form, other province.
  h16 <- suppressMessages(suppressWarnings(hd_huang2016(DBH, "PINU.BAN")$height))
  expect_true(all(abs(z / h16 - 1) < 0.15))
  # Ontario (Sharma & Parton 2007), HDOM 20 m stand: close at 20-30 cm; the
  # stand-level model is taller for small trees (~-34 % at 10 cm).
  sp20 <- suppressMessages(suppressWarnings(
    hd_sharmaparton2007(c(20, 30), "PINU.BAN", 20, 25, 1500)$height
  ))
  expect_true(all(abs(z[2:3] / sp20 - 1) < 0.15))
})
