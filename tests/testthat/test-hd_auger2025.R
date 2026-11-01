# hd_auger2025(): Auger, DeBlois & Lambert (2025), Avis technique SSS-10.

# Source eqs. 10-14, for independent checks.
eq10 <- function(DBH, a1, a2, a3) 1.3 + a1 * exp(-a2 * DBH^(-a3))
eq_cr <- function(DBH, asym, rate, a3) 1.3 + asym * (1 - exp(-rate * DBH))^a3
eq_h2 <- function(DBH, asym, rate, a3) 1.3 + asym * DBH^a3 / (rate + DBH^a3)

# Tableaux 7 (p. 11), 8 (p. 14) and 9 (p. 17).
korf <- tibble::tribble(
  ~species, ~a1, ~a2, ~a3,
  "PINU.BAN", 1630094, 15.3007, 0.0900,
  "PICE.MAR", 101489, 12.1844, 0.0974
)

hd <- function(...) suppressWarnings(hd_auger2025(...))

test_that("hd_auger2025 returns a well-formed tibble", {
  out <- hd(DBH = c(5, 10), species = "PICE.MAR")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd(c(5, NA), c("PICE.MAR", NA), keep_model_id = TRUE)$model_id,
    c("auger2025_dbh", NA)
  )
  expect_identical(
    hd(5, "PINU.BAN", "plot_mean_dbh", DMEAN = 5, keep_model_id = TRUE)$model_id,
    "auger2025_plot_mean_dbh"
  )
  expect_identical(
    hd(5, "PINU.BAN", "dominant", HDOM = 8, DDOM = 10, keep_model_id = TRUE)$model_id,
    "auger2025_dominant"
  )
  expect_identical(
    hd_auger2025(numeric(), "PICE.MAR"),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_auger2025(numeric(), "PICE.MAR", keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("variant inputs are required, and unused ones are ignored with a warning", {
  expect_error(hd(5, "PICE.MAR", "plot_mean_dbh"), "DMEAN")
  expect_error(hd(5, "PICE.MAR", "dominant", HDOM = 6), "DDOM")
  expect_error(hd(5, "PICE.MAR", "dominant", DDOM = 6), "HDOM")
  expect_error(hd(5, "PICE.MAR", "other"), "variant")

  expect_warning(
    out <- hd_auger2025(5, "PICE.MAR", HDOM = 6),
    class = "CanadaForestAllometry_hd_unused_predictor"
  )
  expect_equal(out, hd_auger2025(5, "PICE.MAR"))
  expect_warning(
    out <- hd_auger2025(5, "PICE.MAR", "dominant", DMEAN = -1, HDOM = 6, DDOM = 8),
    "DMEAN"
  )
  expect_equal(out, hd_auger2025(5, "PICE.MAR", "dominant", HDOM = 6, DDOM = 8))
  expect_warning(
    hd_auger2025(5, "PICE.MAR", "plot_mean_dbh", DMEAN = 5, HDOM = 6, DDOM = 8),
    "HDOM.*DDOM"
  )
})

test_that("hd_auger2025 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(6, 9, 12),
    species = c("PICE.MAR", "PINU.BAN", "PICE.MAR"),
    HDOM = c(9, 10, 9), DDOM = c(11, 12, 11)
  )
  res <- dplyr::mutate(
    trees,
    hd_auger2025(DBH, species, "dominant", HDOM = HDOM, DDOM = DDOM)
  )
  expect_equal(
    res$height,
    hd(trees$DBH, trees$species, "dominant", HDOM = trees$HDOM, DDOM = trees$DDOM)$height
  )
  expect_equal(nrow(hd(c(5, 10, 15), "PINU.BAN", "plot_mean_dbh", DMEAN = 8)), 3L)
  expect_error(hd(1:3, c("PICE.MAR", "PINU.BAN")), "Incompatible")
  expect_error(hd(1:3, "PICE.MAR", "plot_mean_dbh", DMEAN = c(5, 6)), "Incompatible")
})

test_that("hd_auger2025 validates inputs", {
  expect_error(hd(0, "PICE.MAR"), "> 0")
  expect_error(hd("10", "PICE.MAR"), "numeric")
  expect_error(hd(Inf, "PICE.MAR"), "finite")
  expect_error(hd(10, 1), "character")
  expect_error(hd(10, "PICE.MAR", "plot_mean_dbh", DMEAN = -1), "DMEAN")
  expect_error(hd(10, "PICE.MAR", "dominant", HDOM = 0, DDOM = 8), "HDOM")
  expect_error(hd(10, "PICE.MAR", "dominant", HDOM = 6, DDOM = -1), "DDOM")
  expect_error(hd(10, "PICE.GLA"), "not covered")
  expect_error(hd(10, "PICE.MAR", keep_model_id = NA), "keep_model_id")
  # Zero mean or dominant DBH is valid (Tableau 1 minimum 0.0)
  expect_true(is.finite(hd(1, "PICE.MAR", "plot_mean_dbh", DMEAN = 0)$height))
  expect_true(is.finite(hd(1, "PICE.MAR", "dominant", HDOM = 2, DDOM = 0)$height))
})

test_that("inputs that make the jack pine dominant model undefined abort", {
  # Eq. 13 rate a20 + a22 * DDOM = 0.1944 - 0.0031 * DDOM <= 0 for DDOM >= 62.7
  expect_error(
    hd(c(10, 10), "PINU.BAN", "dominant", HDOM = 20, DDOM = c(20, 70)),
    "undefined heights for 1 row"
  )
  expect_true(is.finite(hd(10, "PICE.MAR", "dominant", HDOM = 20, DDOM = 70)$height))
})

test_that("NA in any used input returns NA height", {
  expect_true(all(is.na(hd(c(NA, 5), c("PICE.MAR", NA))$height)))
  for (sp in c("PICE.MAR", "PINU.BAN")) {
    out <- hd(c(NA, 5, 5), sp, "plot_mean_dbh", DMEAN = c(5, NA, 5))
    expect_identical(is.na(out$height), c(TRUE, TRUE, FALSE), info = sp)
    out <- hd(c(NA, 5, 5, 5), sp, "dominant", HDOM = c(6, NA, 6, 6), DDOM = c(8, 8, NA, 8))
    expect_identical(is.na(out$height), c(TRUE, TRUE, TRUE, FALSE), info = sp)
  }
})

# --- coefficient wiring (Tableaux 7-9) -------------------------------------------

test_that("predictions use the Tableau 7-9 coefficients and eqs. 10-14", {
  DBH <- c(1, 5, 10, 20)
  for (i in seq_len(nrow(korf))) {
    r <- korf[i, ]
    expect_equal(hd(DBH, r$species)$height, eq10(DBH, r$a1, r$a2, r$a3), info = r$species)
  }
  MD <- 8
  expect_equal(
    hd(DBH, "PINU.BAN", "plot_mean_dbh", DMEAN = MD)$height,
    eq_cr(DBH, 6.5613 + 0.6849 * MD, 0.0587 + 0.0015 * MD, 0.9461)
  )
  expect_equal(
    hd(DBH, "PICE.MAR", "plot_mean_dbh", DMEAN = MD)$height,
    eq_h2(DBH, 4.4337 + 0.9183 * MD, 7.6017 + 2.1394 * MD, 1.4324)
  )
  HDOM <- 10
  DD <- 12
  expect_equal(
    hd(DBH, "PINU.BAN", "dominant", HDOM = HDOM, DDOM = DD)$height,
    eq_cr(DBH, -0.2009 + 0.9632 * HDOM, 0.1944 - 0.0031 * DD, 0.8950)
  )
  expect_equal(
    hd(DBH, "PICE.MAR", "dominant", HDOM = HDOM, DDOM = DD)$height,
    eq_h2(DBH, -0.3031 + 1.1438 * HDOM, 0.5571 + 1.6414 * DD, 1.5999)
  )
  # Mixed species in one call use the species' own form
  expect_equal(
    hd(c(10, 10), c("PINU.BAN", "PICE.MAR"), "dominant", HDOM = HDOM, DDOM = DD)$height,
    c(
      hd(10, "PINU.BAN", "dominant", HDOM = HDOM, DDOM = DD)$height,
      hd(10, "PICE.MAR", "dominant", HDOM = HDOM, DDOM = DD)$height
    )
  )
  expect_equal(hd(5, "PICEMAR"), hd(5, "PICE.MAR"))
})

# --- regression values (self-generated; NOT fidelity evidence) --------------------

test_that("predictions match stored regression values", {
  # Computed independently (Python) from the Stage 1 CSV; the source publishes
  # no predicted heights. See tmp/hd_auger2025_comparison_values.csv.
  d <- c(0.1, 1, 5, 10, 20, 30)
  expect_equal(round(hd(d, "PINU.BAN")$height, 2), c(1.31, 1.67, 4.21, 7.77, 15.03, 22.17))
  expect_equal(round(hd(d, "PICE.MAR")$height, 2), c(1.32, 1.82, 4.34, 7.30, 12.62, 17.41))
  expect_equal(
    round(hd(c(1, 5, 10, 20), "PINU.BAN", "plot_mean_dbh", DMEAN = 15)$height, 2),
    c(2.81, 7.26, 10.96, 14.98)
  )
  expect_equal(
    round(hd(c(1, 5, 10, 20), "PICE.MAR", "plot_mean_dbh", DMEAN = 15)$height, 2),
    c(1.75, 4.97, 8.68, 13.10)
  )
})

# --- TIER: plausibility (no source benchmark) ------------------------------------

test_that("predictions at the Tableau 1 means are close to the observed mean height", {
  # Tableau 1 (p. 4): means of DHP, DHP moyen, HDOM, DDOM and H.
  means <- tibble::tribble(
    ~species, ~DBH, ~MD, ~HDOM, ~DD, ~H,
    "PINU.BAN", 9.3, 7.3, 8.0, 10.2, 7.4,
    "PICE.MAR", 6.9, 5.4, 6.3, 8.1, 5.4
  )
  for (i in seq_len(nrow(means))) {
    r <- means[i, ]
    h <- c(
      hd(r$DBH, r$species)$height,
      hd(r$DBH, r$species, "plot_mean_dbh", DMEAN = r$MD)$height,
      hd(r$DBH, r$species, "dominant", HDOM = r$HDOM, DDOM = r$DD)$height
    )
    expect_true(all(abs(h / r$H - 1) < 0.15), info = r$species)
    # A tree of dominant DBH is predicted near the dominant height
    expect_lt(
      abs(hd(r$DD, r$species, "dominant", HDOM = r$HDOM, DDOM = r$DD)$height - r$HDOM),
      1
    )
  }
})

test_that("height starts near 1.3 m, increases with DBH, mean DBH and HDOM", {
  d <- c(0.01, seq(0.5, 30, by = 0.5))
  for (sp in c("PICE.MAR", "PINU.BAN")) {
    h <- list(
      hd(d, sp)$height,
      hd(d, sp, "plot_mean_dbh", DMEAN = 8)$height,
      hd(d, sp, "dominant", HDOM = 10, DDOM = 12)$height
    )
    for (hh in h) {
      expect_lt(abs(hh[1] - 1.3), 0.05)
      expect_true(all(diff(hh) > 0), info = sp)
    }
    expect_true(all(diff(hd(10, sp, "plot_mean_dbh", DMEAN = c(2, 6, 10, 15))$height) > 0), info = sp)
    expect_true(
      all(diff(hd(10, sp, "dominant", HDOM = c(4, 8, 12, 16), DDOM = 12)$height) > 0),
      info = sp
    )
  }
})

# --- warnings ----------------------------------------------------------------------

test_that("inputs outside the Tableau 1 ranges are computed with one warning", {
  # Black spruce (Tableau 1): DBH 0.2-27.0; mean DBH 0.0-18.5; DDOM 0.1-23.4;
  # HDOM 1.2-17.8.
  expect_no_warning(hd_auger2025(c(1, 26), "PICE.MAR"))
  expect_no_warning(hd_auger2025(10, "PICE.MAR", "plot_mean_dbh", DMEAN = 10))
  expect_no_warning(hd_auger2025(10, "PICE.MAR", "dominant", HDOM = 10, DDOM = 12))
  cases <- list(
    list(DBH = 0.1, variant = "dbh"),
    list(DBH = 28, variant = "dbh"),
    list(DBH = 10, variant = "plot_mean_dbh", DMEAN = 19),
    list(DBH = 10, variant = "dominant", HDOM = 1, DDOM = 12),
    list(DBH = 10, variant = "dominant", HDOM = 18, DDOM = 12),
    list(DBH = 10, variant = "dominant", HDOM = 10, DDOM = 0.05),
    list(DBH = 10, variant = "dominant", HDOM = 10, DDOM = 24)
  )
  for (a in cases) {
    expect_warning(
      out <- do.call(hd_auger2025, c(list(species = "PICE.MAR"), a)),
      class = "CanadaForestAllometry_hd_out_of_range"
    )
    expect_true(is.finite(out$height))
  }
})

test_that("hd_auger2025 is registered consistently", {
  ids <- c("auger2025_dbh", "auger2025_plot_mean_dbh", "auger2025_dominant")
  preds <- list(
    "DBH", c("DBH", "DMEAN"), c("DBH", "HDOM", "DDOM")
  )
  for (i in seq_along(ids)) {
    info <- get_hd_model_info(ids[[i]])
    expect_identical(info$engine, "hd_auger2025")
    expect_identical(info$stand_origin, "plantation")
    expect_identical(info$fixed_args$variant, sub("auger2025_", "", ids[[i]]))
    expect_identical(info$predictors$arg, preds[[i]])
    expect_true(all(info$predictors$required))
    expect_setequal(info$species, c("PICE.MAR", "PINU.BAN"))
  }
  found <- find_hd_models(jurisdiction = "QC", species = "PINU.BAN", available = c("DBH", "HDOM"))
  expect_true("auger2025_dbh" %in% found$model_id[found$usable])
  expect_false("auger2025_dominant" %in% found$model_id[found$usable])
})
