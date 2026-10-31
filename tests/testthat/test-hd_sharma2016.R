# hd_sharma2016(): Sharma (2016), For. Sci. 62:70-77.

# Source eq. 4 (u = 0), for independent checks.
eq4 <- function(DBH, HDOM, BA, TPH, theta, delta, beta, phi, gamma) {
  1.3 + theta * HDOM^delta * (1 - exp(-beta * (TPH / BA)^phi * DBH))^gamma
}

# Table 3 (p. 74). White spruce plantation phi is blank in the source; set to 0.
t3 <- tibble::tribble(
  ~species, ~variant, ~theta, ~delta, ~beta, ~phi, ~gamma,
  "PINU.BAN", "plantation", 1.0187, 1.0031, 0.0437, 0.2550, 1.0286,
  "PINU.BAN", "natural", 1.2685, 0.9380, 0.04731, 0.1927, 0.9678,
  "PINU.RES", "plantation", 1.2014, 0.9477, 0.0767, 0.1713, 1.5011,
  "PINU.RES", "natural", 3.3440, 0.6179, 0.1428, -0.0905, 1.8619,
  "PICE.MAR", "plantation", 2.8147, 0.6195, 0.0801, 0.0439, 1.1066,
  "PICE.MAR", "natural", 28.9566, -0.0451, 0.0603, -0.0223, 1.2830,
  "PICE.GLA", "plantation", 3.7437, 0.5702, 0.0819, 0, 1.2657,
  "PICE.GLA", "natural", 7.3968, 0.3848, 0.0445, 0.0771, 1.3741
)

hd <- function(...) suppressWarnings(hd_sharma2016(...))

test_that("hd_sharma2016 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PICE.MAR", variant = "natural",
            HDOM = 16, BA = 25, TPH = 2200)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd_sharma2016(numeric(), "PICE.MAR", "natural", 16, 25, 2200),
    tibble::tibble(height = numeric())
  )
})

test_that("variant is per tree, recycled, and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18, 18),
    species = c("PICE.MAR", "PICE.GLA", "PINU.BAN", "PINU.BAN"),
    origin = c("plantation", "natural", "natural", "plantation"),
    HDOM = c(12, 18, 18, 18), BA = c(22, 30, 30, 30), TPH = c(3000, 2000, 2000, 2000)
  )
  res <- dplyr::mutate(trees, hd_sharma2016(DBH, species, origin, HDOM, BA, TPH))
  each <- vapply(seq_len(nrow(trees)), function(i) {
    with(trees[i, ], hd(DBH, species, origin, HDOM, BA, TPH)$height)
  }, numeric(1))
  expect_equal(res$height, each)
  expect_false(res$height[3] == res$height[4])
  expect_equal(
    hd(20, "PINU.RES", factor("natural"), 16, 25, 2200),
    hd(20, "PINU.RES", "natural", 16, 25, 2200)
  )
  expect_error(hd(1:3, "PICE.MAR", c("natural", "plantation"), 16, 25, 2200), "Incompatible")
})

test_that("hd_sharma2016 validates inputs", {
  expect_error(hd_sharma2016(10, "PICE.MAR", HDOM = 16, BA = 25, TPH = 2200), "variant")
  expect_error(hd_sharma2016(10, "PICE.MAR", "planted", 16, 25, 2200), "variant")
  expect_error(hd_sharma2016(10, "PICE.MAR", 1, 16, 25, 2200), "variant")
  expect_error(hd_sharma2016(-1, "PICE.MAR", "natural", 16, 25, 2200), ">= 0")
  expect_error(hd_sharma2016("10", "PICE.MAR", "natural", 16, 25, 2200), "numeric")
  expect_error(hd_sharma2016(10, 1, "natural", 16, 25, 2200), "character")
  expect_error(hd_sharma2016(10, "PICE.MAR", "natural", 0, 25, 2200), "HDOM")
  expect_error(hd_sharma2016(10, "PICE.MAR", "natural", 16, 0, 2200), "BA")
  expect_error(hd_sharma2016(10, "PICE.MAR", "natural", 16, 25, -5), "TPH")
  expect_error(hd_sharma2016(10, "ABIE.BAL", "natural", 16, 25, 2200), "not covered")
})

test_that("NA in any input returns NA height", {
  out <- hd(
    DBH = c(NA, 20, 20, 20, 20, 20, 20),
    species = c("PICE.MAR", NA, rep("PICE.MAR", 5)),
    variant = c("natural", "natural", NA, rep("natural", 4)),
    HDOM = c(16, 16, 16, NA, 16, 16, 16),
    BA = c(25, 25, 25, 25, NA, 25, 25),
    TPH = c(2200, 2200, 2200, 2200, 2200, NA, 2200)
  )
  expect_identical(is.na(out$height), c(rep(TRUE, 6), FALSE))
  expect_true(is.na(hd(20, "PICE.MAR", NA, 16, 25, 2200)$height))
})

# --- coefficient wiring (Table 3) ----------------------------------------------

test_that("predictions use the Table 3 coefficients for every species x origin", {
  DBH <- c(3, 10, 25, 40)
  for (i in seq_len(nrow(t3))) {
    r <- t3[i, ]
    expect_equal(
      hd(DBH, r$species, r$variant, 17, 30, 1800)$height,
      eq4(DBH, 17, 30, 1800, r$theta, r$delta, r$beta, r$phi, r$gamma),
      info = paste(r$species, r$variant)
    )
  }
  expect_equal(
    hd(20, "PICEMAR", "natural", 17, 30, 1800),
    hd(20, "PICE.MAR", "natural", 17, 30, 1800)
  )
})

test_that("white spruce plantation ignores BA and TPH (phi = 0)", {
  h <- hd(20, "PICE.GLA", "plantation", 16, c(10, 25, 50), c(500, 2200, 8000))$height
  expect_equal(h, rep(h[1], 3))
})

# --- TIER: figure-read (no exact source benchmark) -----------------------------

test_that("hd_sharma2016 reproduces separable Fig. 4 points (p. 76) within reading precision", {
  # Fig. 4: BA = 25 m2/ha, TPH = 2200, SHT = 16 m. Marker centroids digitized
  # from a 300 dpi render (tmp/generate_hd_sharma2016_comparison_values.R);
  # +-0.3 m. Fig. 4 departs from Table 3 at large DBH for the plantation
  # spruce curves, so only points agreeing within reading precision are used.
  fig4 <- tibble::tribble(
    ~species, ~variant, ~DBH, ~expected,
    "PINU.RES", "natural", 2, 1.98,
    "PINU.RES", "natural", 10, 8.73,
    "PINU.RES", "natural", 20, 14.83,
    "PINU.RES", "plantation", 10, 13.47,
    "PINU.RES", "plantation", 20, 17.15,
    "PINU.RES", "plantation", 30, 17.89,
    "PICE.MAR", "natural", 2, 2.65,
    "PICE.MAR", "natural", 10, 9.71,
    "PICE.MAR", "natural", 20, 16.40,
    "PICE.MAR", "natural", 30, 20.67,
    "PICE.MAR", "plantation", 2, 3.74,
    "PICE.MAR", "plantation", 10, 10.72,
    "PICE.GLA", "natural", 10, 8.81,
    "PICE.GLA", "plantation", 10, 10.16
  )
  h <- hd(fig4$DBH, fig4$species, fig4$variant, 16, 25, 2200)$height
  expect_true(all(abs(h - fig4$expected) <= 0.3))
})

test_that("plantation vs natural pattern matches the source text (p. 76)", {
  # Small trees are shorter in natural stands; beyond a threshold DBH, natural
  # stand trees are taller (Fig. 4 conditions).
  for (sp in unique(t3$species)) {
    nat <- hd(c(5, 40), sp, "natural", 16, 25, 2200)$height
    pla <- hd(c(5, 40), sp, "plantation", 16, 25, 2200)$height
    expect_true(nat[1] < pla[1], info = sp)
    expect_true(nat[2] > pla[2], info = sp)
  }
})

# --- behaviour / plausibility -------------------------------------------------

test_that("height starts at 1.3 m and increases with DBH for every variant", {
  d <- c(0, 0.5, seq(1, 50, by = 1))
  for (i in seq_len(nrow(t3))) {
    r <- t3[i, ]
    h <- hd(d, r$species, r$variant, 16, 25, 2200)$height
    expect_equal(h[1], 1.3, info = paste(r$species, r$variant))
    expect_true(all(diff(h) > 0), info = paste(r$species, r$variant))
    expect_true(all(h < 35), info = paste(r$species, r$variant))
  }
})

test_that("stand predictor effects follow the signs of delta and phi", {
  # delta > 0 except black spruce natural; phi < 0 for red pine and black
  # spruce natural (p. 75).
  hdom <- c(6, 12, 18, 24)
  dens <- c(500, 2000, 8000)
  for (i in seq_len(nrow(t3))) {
    r <- t3[i, ]
    lab <- paste(r$species, r$variant)
    dh <- diff(hd(20, r$species, r$variant, hdom, 25, 2200)$height)
    expect_true(all(sign(dh) == sign(r$delta)), info = lab)
    if (r$phi != 0) {
      dt <- diff(hd(20, r$species, r$variant, 16, 25, dens)$height)
      expect_true(all(sign(dt) == sign(r$phi)), info = lab)
    }
  }
})

# --- warnings --------------------------------------------------------------------

test_that("inputs outside the fitting-data ranges are computed with one warning", {
  # black spruce plantation (Tables 1-2): DBH 2.5-25.6, BA 5.03-49.31,
  # TPH 575-10300, SHT 3.81-22.99.
  expect_no_warning(hd_sharma2016(c(3, 10, 25), "PICE.MAR", "plantation", 12, 25, 3000))
  cases <- list(
    list(DBH = 1, HDOM = 12, BA = 25, TPH = 3000),
    list(DBH = 30, HDOM = 12, BA = 25, TPH = 3000),
    list(DBH = 10, HDOM = 3, BA = 25, TPH = 3000),
    list(DBH = 10, HDOM = 24, BA = 25, TPH = 3000),
    list(DBH = 10, HDOM = 12, BA = 4, TPH = 3000),
    list(DBH = 10, HDOM = 12, BA = 55, TPH = 3000),
    list(DBH = 10, HDOM = 12, BA = 25, TPH = 500),
    list(DBH = 10, HDOM = 12, BA = 25, TPH = 11000)
  )
  for (a in cases) {
    expect_warning(
      out <- hd_sharma2016(a$DBH, "PICE.MAR", "plantation", a$HDOM, a$BA, a$TPH),
      class = "CanadaForestAllometry_hd_out_of_range"
    )
    expect_true(is.finite(out$height))
  }
  # ranges are origin-specific: DBH 30 is inside the natural-stand range
  expect_no_warning(hd_sharma2016(30, "PICE.MAR", "natural", 12, 25, 3000))
})

test_that("hd_sharma2016 is registered consistently", {
  for (v in c("natural", "plantation")) {
    id <- paste0("sharma2016_", v)
    info <- get_hd_model_info(id)
    expect_identical(info$engine, "hd_sharma2016")
    expect_identical(info$fixed_args, list(variant = v))
    expect_identical(info$stand_origin, v)
    expect_identical(info$predictors$arg, c("DBH", "HDOM", "BA", "TPH"))
    expect_setequal(info$species, unique(t3$species))
  }
  found <- find_hd_models(jurisdiction = "ON", species = "PINU.RES", available = "DBH")
  expect_true(all(c("sharma2016_natural", "sharma2016_plantation") %in% found$model_id))
})

test_that("keep_model_id adds the variant-specific model_id", {
  out <- hd_sharma2016(
    c(20, 20, NA), c("PICE.MAR", "PICE.MAR", NA), c("natural", "plantation", "natural"),
    HDOM = 16, BA = 25, TPH = 2200, keep_model_id = TRUE
  )
  expect_identical(out$model_id, c("sharma2016_natural", "sharma2016_plantation", NA))
  expect_identical(
    hd_sharma2016(numeric(), "PICE.MAR", "natural", 16, 25, 2200, keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
  expect_error(
    hd_sharma2016(20, "PICE.MAR", "natural", 16, 25, 2200, keep_model_id = NA),
    "keep_model_id"
  )
})
