# hd_eslamdoust2026(): Eslamdoust et al. (2026), Forest Sci. Technol. 22:217-230.

# Source eqs. 4 and 5 (u = 0), for independent checks.
eq4 <- function(DBH, a0, a1, a2) 1.3 + a0 * (1 - exp(-a1 * DBH))^a2
eq5 <- function(DBH, SHT, BAH, TPH, a0, a1, a2, a3, a4) {
  1.3 + a0 * SHT^a1 * (1 - exp(-a2 * (TPH / BAH)^a3 * DBH))^a4
}

# Table 4 (M4) and Table 5 (M5), p. 225. LARI.LAR M5 a2: corrected 0.09793
# (printed 0.9793; see data-raw/Eslamdoust2026_model-spec.md).
m4 <- tibble::tribble(
  ~species, ~a0, ~a1, ~a2,
  "PINU.STR", 26.6379, 0.05122, 1.2205,
  "THUJ.OCC", 15.4903, 0.0528, 1.0548,
  "ACER.SAH", 23.6882, 0.0655, 0.9913,
  "BETU.ALL", 19.6081, 0.0808, 0.9464,
  "FAGU.GRA", 25.4007, 0.0597, 1.0772,
  "LARI.LAR", 17.2602, 0.0855, 1.1668,
  "TILI.AME", 22.7806, 0.07795, 1.2494,
  "TSUG.CAN", 22.9118, 0.0502, 1.1992
)
m5 <- tibble::tribble(
  ~species, ~a0, ~a1, ~a2, ~a3, ~a4,
  "PINU.STR", 7.7934, 0.4012, 0.0908, -0.1540, 1.1949,
  "THUJ.OCC", 8.4291, 0.1876, 0.0865, -0.1683, 0.8909,
  "ACER.SAH", 4.5633, 0.5334, 0.0507, 0.1319, 1.1211,
  "BETU.ALL", 8.9718, 0.3004, 0.0711, 0.0356, 0.9508,
  "FAGU.GRA", 5.8311, 0.4571, 0.0604, 0.0485, 1.2074,
  "LARI.LAR", 4.0095, 0.5567, 0.09793, 0, 1.7817,
  "TILI.AME", 5.3007, 0.4614, 0.0883, 0, 1.2497,
  "TSUG.CAN", 7.1026, 0.3904, 0.0602, -0.0686, 1.1816
)

hd <- function(...) suppressMessages(suppressWarnings(hd_eslamdoust2026(...)))

test_that("hd_eslamdoust2026 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "ACER.SAH")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd(DBH = 10, species = "ACER.SAH", HDOM = 20, BA = 25, TPH = 1500, keep_model_id = TRUE)$model_id,
    "eslamdoust2026_m5"
  )
  expect_identical(
    hd(c(10, NA), c("ACER.SAH", NA), keep_model_id = TRUE)$model_id,
    c("eslamdoust2026_m4", NA)
  )
  expect_identical(
    hd_eslamdoust2026(numeric(), "ACER.SAH"),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_eslamdoust2026(numeric(), "ACER.SAH", keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("variant is inferred from the stand predictors and reported once", {
  rlang::reset_message_verbosity("hd_eslamdoust2026_m4")
  rlang::reset_message_verbosity("hd_eslamdoust2026_m5")
  expect_message(hd_eslamdoust2026(20, "ACER.SAH"), "M4")
  expect_no_message(hd_eslamdoust2026(20, "ACER.SAH"))
  expect_message(hd_eslamdoust2026(20, "ACER.SAH", 20, 25, 1500), "M5")

  expect_error(hd(20, "ACER.SAH", HDOM = 20), "together")
  expect_error(hd(20, "ACER.SAH", HDOM = 20, BA = 25), "TPH")
  expect_error(hd(20, "ACER.SAH", BA = 25, TPH = 1500), "HDOM")
})

test_that("hd_eslamdoust2026 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18),
    species = c("FAGU.GRA", "BETU.ALL", "THUJ.OCC"),
    HDOM = c(20, 20, 15), BA = c(28, 28, 40), TPH = c(1500, 1500, 3000)
  )
  res <- suppressMessages(
    dplyr::mutate(trees, hd_eslamdoust2026(DBH, species, HDOM, BA, TPH))
  )
  expect_equal(
    res$height,
    hd(trees$DBH, trees$species, trees$HDOM, trees$BA, trees$TPH)$height
  )
  expect_equal(nrow(hd(c(10, 20, 30), "ACER.SAH", 20, 25, 1500)), 3L)
  expect_error(hd(1:3, c("ACER.SAH", "PINU.STR")), "Incompatible")
  expect_error(hd(1:3, "ACER.SAH", c(20, 22), 25, 1500), "Incompatible")
})

test_that("hd_eslamdoust2026 validates inputs", {
  expect_error(hd(-1, "ACER.SAH"), ">= 0")
  expect_error(hd("10", "ACER.SAH"), "numeric")
  expect_error(hd(Inf, "ACER.SAH"), "finite")
  expect_error(hd(10, 1), "character")
  expect_error(hd(10, "ACER.SAH", 0, 25, 1500), "HDOM")
  expect_error(hd(10, "ACER.SAH", 20, 0, 1500), "BA")
  expect_error(hd(10, "ACER.SAH", 20, 25, -5), "TPH")
  expect_error(hd(10, "PICE.MAR"), "not covered")
  expect_error(hd(10, "ACER.SAH", keep_model_id = NA), "keep_model_id")
})

test_that("NA in any input returns NA height (incl. a3 = 0 species)", {
  out <- hd(c(NA, 20), c("ACER.SAH", NA))
  expect_true(all(is.na(out$height)))
  for (sp in c("ACER.SAH", "LARI.LAR", "TILI.AME")) {
    out <- hd(
      DBH = c(NA, 20, 20, 20, 20),
      species = sp,
      HDOM = c(18, NA, 18, 18, 18),
      BA = c(25, 25, NA, 25, 25),
      TPH = c(2000, 2000, 2000, NA, 2000)
    )
    expect_identical(is.na(out$height), c(rep(TRUE, 4), FALSE), info = sp)
  }
})

# --- coefficient wiring (Tables 4 and 5) ---------------------------------------

test_that("predictions use the Table 4 (M4) and Table 5 (M5) coefficients", {
  DBH <- c(2.5, 10, 25, 50)
  for (i in seq_len(nrow(m4))) {
    r <- m4[i, ]
    expect_equal(hd(DBH, r$species)$height, eq4(DBH, r$a0, r$a1, r$a2), info = r$species)
  }
  for (i in seq_len(nrow(m5))) {
    r <- m5[i, ]
    expect_equal(
      hd(DBH, r$species, 18, 30, 1800)$height,
      eq5(DBH, 18, 30, 1800, r$a0, r$a1, r$a2, r$a3, r$a4),
      info = r$species
    )
  }
  expect_equal(hd(20, "ACERSAH"), hd(20, "ACER.SAH"))
})

# --- TIER: figure-read (no exact source benchmark) -----------------------------

test_that("M4 reproduces Fig. 3a curves (p. 226) within reading precision", {
  # Digitized from a 300 dpi render
  # (tmp/generate_hd_eslamdoust2026_comparison_values.R). By and Be excluded
  # (overlapping curves). He curve is drawn ~0.5 m below Table 4 throughout,
  # hence 0.75 m.
  fig3a <- tibble::tribble(
    ~species, ~DBH, ~expected,
    "PINU.STR", 31.69, 21.61, "PINU.STR", 60.53, 26.36, "PINU.STR", 70, 26.80,
    "THUJ.OCC", 10, 7.40, "THUJ.OCC", 23.11, 12.06, "THUJ.OCC", 43.83, 15.15,
    "ACER.SAH", 32.57, 22.22, "ACER.SAH", 63.25, 24.33, "ACER.SAH", 75, 24.46,
    "LARI.LAR", 10, 10.47, "LARI.LAR", 16.77, 13.86, "LARI.LAR", 31.04, 16.75,
    "TILI.AME", 10, 12.04, "TILI.AME", 23.25, 19.88, "TILI.AME", 43.80, 23.35,
    "TSUG.CAN", 20, 14.00, "TSUG.CAN", 31.55, 18.08, "TSUG.CAN", 60.79, 22.44
  )
  h <- hd(fig3a$DBH, fig3a$species)$height
  expect_true(all(abs(h - fig3a$expected) <= 0.75))
})

test_that("corrected tamarack M5 curve is consistent with Fig. 3b", {
  # Fig. 3b La line (stand inputs not stated); best-fitting HDOM within the
  # Table 1 range. a3 = 0, so BA and TPH are irrelevant.
  la <- tibble::tribble(
    ~DBH, ~expected,
    8, 9.18, 10, 10.65, 12, 11.86, 16.81, 14.17, 22, 15.87,
    26, 16.72, 31.03, 17.39, 35, 17.94, 37, 18.03
  )
  h <- hd(la$DBH, "LARI.LAR", 15.2, 20, 3000)$height
  expect_lt(sqrt(mean((h - la$expected)^2)), 1)
  # Near-flat curve of the printed value (0.9793) is not reproduced
  expect_lt(hd(2.5, "LARI.LAR", 14.02, 21.85, 3175)$height, 5)
})

# --- behaviour / plausibility -------------------------------------------------

test_that("height starts at 1.3 m, increases with DBH and responds to HDOM", {
  d <- c(0, 0.5, seq(1, 80, by = 1))
  for (sp in m4$species) {
    h4 <- hd(d, sp)$height
    h5 <- hd(d, sp, 20, 25, 1500)$height
    expect_equal(h4[1], 1.3, info = sp)
    expect_equal(h5[1], 1.3, info = sp)
    expect_true(all(diff(h4) > 0) && all(diff(h5) > 0), info = sp)
    expect_true(all(h4 < 35) && all(h5 < 35), info = sp)
    # a1 > 0: taller with dominant height
    expect_true(all(diff(hd(20, sp, c(6, 12, 20, 26), 25, 1500)$height) > 0), info = sp)
  }
})

test_that("BA/TPH response follows the sign of a3; none for tamarack and basswood", {
  dens <- function(sp) hd(20, sp, 20, c(30, 30), c(500, 5000))$height
  for (sp in c("ACER.SAH", "BETU.ALL", "FAGU.GRA")) expect_gt(diff(dens(sp)), 0)
  for (sp in c("PINU.STR", "THUJ.OCC", "TSUG.CAN")) expect_lt(diff(dens(sp)), 0)
  for (sp in c("LARI.LAR", "TILI.AME")) {
    expect_identical(diff(dens(sp)), 0)
    expect_identical(diff(hd(20, sp, 20, c(10, 40), 1500)$height), 0)
  }
})

test_that("M4 and M5 agree broadly at Table 1 mean stand conditions", {
  # Table 1 (p. 219) mean SHT, BAH, TPH per species
  means <- tibble::tribble(
    ~species, ~HDOM, ~BA, ~TPH,
    "PINU.STR", 23.33, 32.39, 1877,
    "THUJ.OCC", 17.01, 41.66, 3035,
    "ACER.SAH", 22.81, 25.71, 1178,
    "BETU.ALL", 20.92, 24.17, 1536,
    "FAGU.GRA", 25.00, 28.23, 1191,
    "LARI.LAR", 14.02, 21.85, 3175,
    "TILI.AME", 22.92, 25.49, 1630,
    "TSUG.CAN", 22.52, 36.88, 1232
  )
  d <- seq(2.5, 50, by = 2.5)
  for (i in seq_len(nrow(means))) {
    r <- means[i, ]
    h4 <- hd(d, r$species)$height
    h5 <- hd(d, r$species, r$HDOM, r$BA, r$TPH)$height
    expect_lt(max(abs(h4 - h5)), 3, label = r$species)
  }
})

# --- warnings --------------------------------------------------------------------

test_that("inputs outside the fitting-data ranges are computed with one warning", {
  # Sugar maple (Table 1): DBH 2.50-78.20; SHT 4.57-39.62; BAH 5.14-55.96;
  # TPH 173-6333.
  expect_no_warning(suppressMessages(hd_eslamdoust2026(c(3, 40, 75), "ACER.SAH")))
  expect_no_warning(suppressMessages(hd_eslamdoust2026(20, "ACER.SAH", 20, 25, 1500)))
  cases <- list(
    list(DBH = 1, HDOM = NULL, BA = NULL, TPH = NULL),
    list(DBH = 80, HDOM = NULL, BA = NULL, TPH = NULL),
    list(DBH = 20, HDOM = 4, BA = 25, TPH = 1500),
    list(DBH = 20, HDOM = 41, BA = 25, TPH = 1500),
    list(DBH = 20, HDOM = 20, BA = 5, TPH = 1500),
    list(DBH = 20, HDOM = 20, BA = 57, TPH = 1500),
    list(DBH = 20, HDOM = 20, BA = 25, TPH = 150),
    list(DBH = 20, HDOM = 20, BA = 25, TPH = 6400)
  )
  for (a in cases) {
    expect_warning(
      out <- suppressMessages(hd_eslamdoust2026(a$DBH, "ACER.SAH", a$HDOM, a$BA, a$TPH)),
      class = "CanadaForestAllometry_hd_out_of_range"
    )
    expect_true(is.finite(out$height))
  }
})

test_that("hd_eslamdoust2026 is registered consistently", {
  m4_info <- get_hd_model_info("eslamdoust2026_m4")
  m5_info <- get_hd_model_info("eslamdoust2026_m5")
  expect_identical(m4_info$engine, "hd_eslamdoust2026")
  expect_identical(m5_info$engine, "hd_eslamdoust2026")
  expect_identical(m4_info$predictors$arg, "DBH")
  expect_identical(m5_info$predictors$arg, c("DBH", "HDOM", "BA", "TPH"))
  expect_true(all(m5_info$predictors$required))
  expect_setequal(m4_info$species, m4$species)
  expect_setequal(m5_info$species, m5$species)
  found <- find_hd_models(jurisdiction = "ON", species = "TSUG.CAN", available = "DBH")
  expect_identical(found$model_id[found$usable], "eslamdoust2026_m4")
  expect_true("eslamdoust2026_m5" %in% found$model_id)
})
