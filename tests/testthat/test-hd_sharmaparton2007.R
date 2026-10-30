# hd_sharmaparton2007(): Sharma & Parton (2007), For. Ecol. Manage. 249:187-198.

# Source eqs. 6/8 (u = 0), for independent checks.
eq8 <- function(DBH, HDOM, BA, TPH, theta, delta, beta, phi, gamma) {
  1.3 + theta * HDOM^delta * (1 - exp(-beta * (TPH / BA)^phi * DBH))^gamma
}

# Table 6 (p. 195), Method 2 estimates.
m2 <- tibble::tribble(
  ~species, ~theta, ~delta, ~beta, ~phi, ~gamma,
  "ABIE.BAL", 4.7942, 0.4675, 0.0607, 0.0977, 1.5166,
  "POPU.BAL", 3.1718, 0.6508, 0.0397, 0.1482, 0.8763,
  "PICE.MAR", 3.3952, 0.6390, 0.0424, 0.1380, 1.2948,
  "PINU.BAN", 1.1583, 0.9721, 0.0401, 0.2289, 0.9399,
  "PINU.RES", 1.0323, 0.9962, 0.0494, 0.2612, 1.9307,
  "POPU.TRE", 2.3161, 0.7456, 0.0543, 0.1738, 1.0967,
  "BETU.PAP", 2.3273, 0.6889, 0.0452, 0.2263, 1.1041,
  "PICE.GLA", 7.4622, 0.4368, 0.0314, 0.0664, 1.1101
)

hd <- function(...) suppressWarnings(hd_sharmaparton2007(...))

test_that("hd_sharmaparton2007 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PICE.MAR", HDOM = 18, BA = 25, TPH = 2000)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd_sharmaparton2007(numeric(), "PICE.MAR", 18, 25, 2000),
    tibble::tibble(height = numeric())
  )
})

test_that("hd_sharmaparton2007 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18),
    species = c("ABIE.BAL", "PICE.GLA", "BETU.PAP"),
    HDOM = c(16, 16, 22), BA = c(30, 30, 18), TPH = c(3000, 3000, 1200)
  )
  res <- dplyr::mutate(trees, hd_sharmaparton2007(DBH, species, HDOM, BA, TPH))
  expect_equal(
    res$height,
    hd_sharmaparton2007(trees$DBH, trees$species, trees$HDOM, trees$BA, trees$TPH)$height
  )
  expect_equal(nrow(hd(c(10, 20, 30), "PICE.MAR", 18, 25, 2000)), 3L)
  expect_error(hd(1:3, c("PICE.MAR", "PICE.GLA"), 18, 25, 2000), "Incompatible")
  expect_error(hd(1:3, "PICE.MAR", c(18, 20), 25, 2000), "Incompatible")
})

test_that("hd_sharmaparton2007 validates inputs", {
  expect_error(hd_sharmaparton2007(-1, "PICE.MAR", 18, 25, 2000), ">= 0")
  expect_error(hd_sharmaparton2007("10", "PICE.MAR", 18, 25, 2000), "numeric")
  expect_error(hd_sharmaparton2007(Inf, "PICE.MAR", 18, 25, 2000), "finite")
  expect_error(hd_sharmaparton2007(10, 1, 18, 25, 2000), "character")
  expect_error(hd_sharmaparton2007(10, "PICE.MAR", 0, 25, 2000), "HDOM")
  expect_error(hd_sharmaparton2007(10, "PICE.MAR", 18, 0, 2000), "BA")
  expect_error(hd_sharmaparton2007(10, "PICE.MAR", 18, 25, -5), "TPH")
  expect_error(hd_sharmaparton2007(10, "PICE.MAR", 18, 25), "TPH")
  expect_error(hd_sharmaparton2007(10, "ACER.RUB", 18, 25, 2000), "not covered")
})

test_that("NA in any input returns NA height", {
  out <- hd(
    DBH = c(NA, 20, 20, 20, 20, 20),
    species = c("PICE.MAR", NA, "PICE.MAR", "PICE.MAR", "PICE.MAR", "PICE.MAR"),
    HDOM = c(18, 18, NA, 18, 18, 18),
    BA = c(25, 25, 25, NA, 25, 25),
    TPH = c(2000, 2000, 2000, 2000, NA, 2000)
  )
  expect_identical(is.na(out$height), c(rep(TRUE, 5), FALSE))
})

# --- coefficient wiring (Table 6, Method 2) ------------------------------------

test_that("predictions use the Table 6 Method 2 coefficients for all species", {
  DBH <- c(3, 10, 25, 40)
  for (i in seq_len(nrow(m2))) {
    r <- m2[i, ]
    expect_equal(
      hd(DBH, r$species, 17, 30, 1800)$height,
      eq8(DBH, 17, 30, 1800, r$theta, r$delta, r$beta, r$phi, r$gamma),
      info = r$species
    )
  }
  # not the Method 1 (fixed-effects-only) estimates, e.g. black spruce
  expect_false(isTRUE(all.equal(
    hd(20, "PICE.MAR", 17, 30, 1800)$height,
    eq8(20, 17, 30, 1800, 3.7096, 0.6206, 0.0451, 0.1239, 1.3222)
  )))
  expect_equal(hd(20, "PICEMAR", 17, 30, 1800), hd(20, "PICE.MAR", 17, 30, 1800))
})

# --- TIER: figure-read (no exact source benchmark) -----------------------------

test_that("hd_sharmaparton2007 reproduces Fig. 2 curves (p. 196) within reading precision", {
  # Fig. 2: BA = 25 m2/ha, N/ha = 2500, SHT = 20 m. Digitized from a 300 dpi
  # render (tmp/generate_hd_sharmaparton2007_comparison_values.R); +-0.5 m.
  fig2 <- tibble::tribble(
    ~species, ~DBH, ~expected,
    "ABIE.BAL", 40, 20.13,
    "BETU.PAP", 40, 19.55,
    "PICE.GLA", 40, 23.45,
    "PINU.BAN", 2, 6.10
  )
  h <- hd(fig2$DBH, fig2$species, 20, 25, 2500)$height
  expect_true(all(abs(h - fig2$expected) <= 0.5))
})

test_that("species ranking matches the source text (p. 195)", {
  # At 10 cm: white spruce < balsam fir < black spruce; at 40 cm white spruce
  # is tallest, followed by black spruce (Fig. 2 conditions).
  h10 <- hd(10, c("PICE.GLA", "ABIE.BAL", "PICE.MAR"), 20, 25, 2500)$height
  expect_true(h10[1] < h10[2] && h10[2] < h10[3])
  h40 <- stats::setNames(hd(40, m2$species, 20, 25, 2500)$height, m2$species)
  expect_identical(names(sort(h40, decreasing = TRUE))[1:2], c("PICE.GLA", "PICE.MAR"))
})

# --- behaviour / plausibility -------------------------------------------------

test_that("height starts at 1.3 m, increases with DBH and responds to stand predictors", {
  d <- c(0, 0.5, seq(1, 60, by = 1))
  for (sp in m2$species) {
    h <- hd(d, sp, 20, 25, 2500)$height
    expect_equal(h[1], 1.3, info = sp)
    expect_true(all(diff(h) > 0), info = sp)
    expect_true(all(h < 40), info = sp)

    # delta > 0: taller with dominant height; phi > 0: taller with TPH / BA
    expect_true(all(diff(hd(20, sp, c(8, 15, 25, 33), 25, 2500)$height) > 0), info = sp)
    expect_true(all(diff(hd(20, sp, 20, 25, c(200, 1000, 5000))$height) > 0), info = sp)
    expect_true(all(diff(hd(20, sp, 20, c(10, 25, 60), 2500)$height) < 0), info = sp)
  }
})

# --- warnings --------------------------------------------------------------------

test_that("inputs outside the fitting-data ranges are computed with one warning", {
  # black spruce DBH (Table 1): 2.5-42.9 cm; plots: BA 5.03-85.32, N/ha 50-17625,
  # SHT 3.80-33.10 (p. 188).
  expect_no_warning(hd_sharmaparton2007(c(3, 20, 35), "PICE.MAR", 18, 25, 2000))
  cases <- list(
    list(DBH = 1, HDOM = 18, BA = 25, TPH = 2000),
    list(DBH = 50, HDOM = 18, BA = 25, TPH = 2000),
    list(DBH = 20, HDOM = 3, BA = 25, TPH = 2000),
    list(DBH = 20, HDOM = 40, BA = 25, TPH = 2000),
    list(DBH = 20, HDOM = 18, BA = 4, TPH = 2000),
    list(DBH = 20, HDOM = 18, BA = 90, TPH = 2000),
    list(DBH = 20, HDOM = 18, BA = 25, TPH = 40),
    list(DBH = 20, HDOM = 18, BA = 25, TPH = 20000)
  )
  for (a in cases) {
    expect_warning(
      out <- hd_sharmaparton2007(a$DBH, "PICE.MAR", a$HDOM, a$BA, a$TPH),
      class = "CanadaForestAllometry_hd_out_of_range"
    )
    expect_true(is.finite(out$height))
  }
})

test_that("hd_sharmaparton2007 is registered consistently", {
  info <- get_hd_model_info("sharmaparton2007")
  expect_identical(info$engine, "hd_sharmaparton2007")
  expect_identical(info$predictors$arg, c("DBH", "HDOM", "BA", "TPH"))
  expect_true(all(info$predictors$required))
  expect_setequal(info$species, m2$species)
  found <- find_hd_models(jurisdiction = "ON", species = "PINU.RES", available = "DBH")
  expect_true("sharmaparton2007" %in% found$model_id)
})
