# hd_staudhammerlemay2000(): Staudhammer & LeMay (2000), For. Chron. 76:303-309.

# Eq. 2 (p. 305), for independent checks.
eq <- function(DBH, E1, E2, E3) 1.3 + E1 * (1 - exp(E2 * DBH^E3))

# Table 5 (p. 308), eq. 2 (DBH only).
eq2 <- tibble::tribble(
  ~species, ~E1, ~E2, ~E3,
  "THUJ.PLI", 39.0002, -0.02164, 1.01568,
  "ALNU.RUB", 26.5495, -0.03079, 1.20438,
  "PSEU.MEN", 68.6382, -0.01296, 0.98848,
  "TSUG.HET", 41.4831, -0.01365, 1.21692
)
# Table 5, eq. 2b: E_k = c0 + c1 * REL + c2 * REL^2 (rows E1, E2, E3).
# Cedar E2 c2 uses the corrected 0.01551 (printed 0.1551).
eq2b <- list(
  THUJ.PLI = rbind(
    c(59.5789, 12.5373, -36.98),
    c(0.00071, -0.03818, 0.01551),
    c(1.6577, -2.02199, 1.41599)
  ),
  PSEU.MEN = rbind(
    c(15.759, 767.419, -699.761),
    c(0.00025, -0.00332, -0.0118),
    c(2.07538, -3.38731, 2.17848)
  ),
  TSUG.HET = rbind(
    c(-1.25839, 163.146, -126.716),
    c(0.00021, -0.06091, 0.05552),
    c(2.34135, -4.11239, 3.31119)
  )
)
# Table 5, eq. 2a (alder): E_k = c0 + c1 * TSTEMSHA + c2 * TBAHA.
eq2a <- rbind(
  c(68.0529, 0.0192872, -0.77315),
  c(-0.00877, 0.00000044, -0.00061),
  c(0.64991, -0.00008194, 0.00554)
)

hd <- function(...) suppressWarnings(hd_staudhammerlemay2000(...))
dens <- function(...) hd(..., variant = "density")

test_that("hd_staudhammerlemay2000 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PSEU.MEN")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    dens(10, "ALNU.RUB", TPH = 1000, BA = 30, keep_model_id = TRUE)$model_id,
    "staudhammerlemay2000_density"
  )
  expect_identical(
    hd(c(10, NA), c("PSEU.MEN", NA), keep_model_id = TRUE)$model_id,
    c("staudhammerlemay2000_dbh", NA)
  )
  expect_identical(
    hd_staudhammerlemay2000(numeric(), "PSEU.MEN"),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_staudhammerlemay2000(numeric(), "PSEU.MEN", keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("variant 'density' requires the predictors of the supplied species", {
  expect_error(dens(20, "ALNU.RUB", TPH = 1000), "BA")
  expect_error(dens(20, "PSEU.MEN", TPH = 1000, BA = 30), "DMAX")
  expect_error(
    dens(c(20, 20), c("PSEU.MEN", "ALNU.RUB"), DMAX = 50),
    "TPH"
  )
  expect_no_error(dens(c(20, 20), c("PSEU.MEN", "ALNU.RUB"), TPH = 1000, BA = 30, DMAX = 50))
  expect_error(hd(20, "PSEU.MEN", variant = "stand"), "variant")
})

test_that("predictors not used by the variant or species warn and are ignored", {
  expect_warning(
    out <- hd_staudhammerlemay2000(20, "PSEU.MEN", TPH = 1000, DMAX = 50),
    class = "CanadaForestAllometry_hd_unused_predictor"
  )
  expect_equal(out, hd(20, "PSEU.MEN"))
  expect_warning(
    out <- hd_staudhammerlemay2000(
      20, "ALNU.RUB", variant = "density", TPH = 1000, BA = 30, DMAX = 50
    ),
    "DMAX"
  )
  expect_equal(out, dens(20, "ALNU.RUB", TPH = 1000, BA = 30))
})

test_that("hd_staudhammerlemay2000 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 45, 80, 15),
    species = c("TSUG.HET", "PSEU.MEN", "PSEU.MEN", "ALNU.RUB"),
    TPH = 1200, BA = 45, DMAX = 80
  )
  res <- dplyr::mutate(
    trees,
    hd_staudhammerlemay2000(DBH, species, variant = "density", TPH = TPH, BA = BA, DMAX = DMAX)
  )
  expect_equal(
    res$height,
    dens(trees$DBH, trees$species, TPH = 1200, BA = 45, DMAX = 80)$height
  )
  expect_error(hd(1:3, c("PSEU.MEN", "TSUG.HET")), "Incompatible")
  expect_error(dens(1:3, "PSEU.MEN", DMAX = c(50, 60)), "Incompatible")
})

test_that("hd_staudhammerlemay2000 validates inputs", {
  expect_error(hd(-1, "PSEU.MEN"), ">= 0")
  expect_error(hd("10", "PSEU.MEN"), "numeric")
  expect_error(hd(Inf, "PSEU.MEN"), "finite")
  expect_error(hd(10, 1), "character")
  expect_error(dens(10, "ALNU.RUB", TPH = 0, BA = 30), "TPH")
  expect_error(dens(10, "ALNU.RUB", TPH = 1000, BA = 0), "BA")
  expect_error(dens(10, "PSEU.MEN", DMAX = 0), "DMAX")
  expect_error(dens(60, "PSEU.MEN", DMAX = 50), "exceeds")
  expect_error(hd(10, "PICE.MAR"), "not covered")
  expect_error(hd(10, "ALNU.INC"), "not covered")
  expect_error(hd(10, "PSEU.MEN", keep_model_id = NA), "keep_model_id")
})

test_that("invalid 'density' parameters far outside the data raise an error", {
  # Douglas-fir: E2 >= 0 for REL < ~0.062.
  expect_error(dens(5, "PSEU.MEN", DMAX = 100), "invalid")
  expect_no_error(dens(7, "PSEU.MEN", DMAX = 100))
  # Alder: E3 <= 0 for TPH above ~7900 (BA = 0 limit); E1 <= 0 for large BA, low TPH.
  expect_error(dens(10, "ALNU.RUB", TPH = 12000, BA = 30), "invalid")
  expect_error(dens(10, "ALNU.RUB", TPH = 100, BA = 100), "invalid")
  # DBH = 0 returns 1.3 m whatever the parameters.
  expect_equal(dens(0, "PSEU.MEN", DMAX = 100)$height, 1.3)
})

test_that("NA in any used input returns NA height", {
  expect_true(all(is.na(hd(c(NA, 20), c("PSEU.MEN", NA))$height)))
  out <- dens(c(NA, 20, 20), "PSEU.MEN", DMAX = c(50, NA, 50))
  expect_identical(is.na(out$height), c(TRUE, TRUE, FALSE))
  out <- dens(20, "ALNU.RUB", TPH = c(NA, 1000, 1000), BA = c(30, NA, 30))
  expect_identical(is.na(out$height), c(TRUE, TRUE, FALSE))
  # Alder rows do not need DMAX, conifer rows do not need TPH/BA.
  out <- dens(
    c(20, 20), c("ALNU.RUB", "PSEU.MEN"),
    TPH = c(1000, NA), BA = c(30, NA), DMAX = c(NA, 50)
  )
  expect_false(anyNA(out$height))
})

test_that("inputs outside the Table 1 ranges warn once and are still computed", {
  expect_warning(
    out <- hd_staudhammerlemay2000(c(1, 20, 100), "ALNU.RUB"),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_false(anyNA(out$height))
  expect_warning(
    hd_staudhammerlemay2000(20, "ALNU.RUB", variant = "density", TPH = 8000, BA = 30),
    "TPH"
  )
  expect_warning(
    hd_staudhammerlemay2000(20, "ALNU.RUB", variant = "density", TPH = 1000, BA = 80),
    "BA"
  )
  expect_warning(
    hd_staudhammerlemay2000(8, "PSEU.MEN", variant = "density", DMAX = 100),
    "DBH / DMAX"
  )
  expect_no_warning(
    hd_staudhammerlemay2000(20, "PSEU.MEN", variant = "density", DMAX = 40)
  )
})

# --- coefficient wiring (Table 5) ---------------------------------------------

test_that("predictions use the Table 5 coefficients", {
  DBH <- c(2, 10, 25, 50)
  for (i in seq_len(nrow(eq2))) {
    r <- eq2[i, ]
    expect_equal(hd(DBH, r$species)$height, eq(DBH, r$E1, r$E2, r$E3), info = r$species)
  }
  DMAX <- 55
  DBH <- c(5, 10, 25, 50)
  for (sp in names(eq2b)) {
    R <- DBH / DMAX
    E <- lapply(1:3, function(k) eq2b[[sp]][k, 1] + eq2b[[sp]][k, 2] * R + eq2b[[sp]][k, 3] * R^2)
    expect_equal(
      dens(DBH, sp, DMAX = DMAX)$height,
      eq(DBH, E[[1]], E[[2]], E[[3]]),
      info = sp
    )
  }
  TPH <- 2500
  BA <- 35
  E <- eq2a[, 1] + eq2a[, 2] * TPH + eq2a[, 3] * BA
  expect_equal(
    dens(DBH, "ALNU.RUB", TPH = TPH, BA = BA)$height,
    eq(DBH, E[1], E[2], E[3])
  )
  expect_equal(hd(20, "PSEUMEN"), hd(20, "PSEU.MEN"))
})

test_that("cedar eq. 2b uses the corrected E2 coefficient (0.01551, printed 0.1551)", {
  # The printed value gives negative heights at mid-range relative DBH.
  R <- 0.5
  E <- eq2b$THUJ.PLI[, 1] + eq2b$THUJ.PLI[, 2] * R
  printed <- eq(30, E[1] - 36.98 * R^2, E[2] + 0.1551 * R^2, E[3] + 1.41599 * R^2)
  expect_lt(printed, 0)
  h <- dens(30, "THUJ.PLI", DMAX = 60)$height
  expect_equal(h, 21.31, tolerance = 1e-3)
})

# --- TIER 2: plausibility (no exact source benchmark) -------------------------

test_that("eq. 2 at the mean DBH is close to the observed mean height (Table 1)", {
  # Table 1 (p. 304): mean DBH and mean height of the fitting data. A concave
  # curve evaluated at the mean DBH over-predicts the mean height.
  tab1 <- tibble::tribble(
    ~species, ~DBH, ~height,
    "THUJ.PLI", 42.4, 23.4,
    "ALNU.RUB", 13.5, 12.3,
    "PSEU.MEN", 42.3, 26.4,
    "TSUG.HET", 32.2, 24.2
  )
  pred <- hd(tab1$DBH, tab1$species)$height
  expect_true(all(pred - tab1$height > -1 & pred - tab1$height < 3))
})

test_that("curves start at 1.3 m and increase with DBH", {
  DBH <- c(0, 5, 10, 20, 40, 80)
  for (sp in eq2$species) {
    h <- hd(DBH, sp)$height
    expect_equal(h[1], 1.3, info = sp)
    expect_true(all(diff(h) > 0), info = sp)
  }
  # Within a plot (fixed DMAX) eq. 2b heights increase with DBH up to
  # relative DBH ~0.85, then decline for the plot's largest trees (property of
  # the published quadratic in REL_dbh, for all three species).
  for (sp in names(eq2b)) {
    h <- dens(seq(12, 78, by = 6), sp, DMAX = 90)$height
    expect_true(all(diff(h) > 0), info = sp)
    h <- dens(c(84, 90), sp, DMAX = 90)$height
    expect_lt(h[2], h[1])
  }
  h <- dens(c(0, 5, 15, 30, 60), "ALNU.RUB", TPH = 3371, BA = 30.73)$height
  expect_equal(h[1], 1.3)
  expect_true(all(diff(h) > 0))
})

test_that("density heights stay plausible across the Table 1 ranges", {
  grid <- expand.grid(DBH = c(12, 20, 40, 80), DMAX = c(80, 100, 140))
  for (sp in names(eq2b)) {
    h <- dens(grid$DBH, sp, DMAX = grid$DMAX)$height
    expect_true(all(h > 1.3 & h < 70), info = sp)
  }
  grid <- expand.grid(TPH = c(85, 1000, 3371, 6854), BA = c(3.77, 30, 72))
  h <- dens(15, "ALNU.RUB", TPH = grid$TPH, BA = grid$BA)$height
  expect_true(all(h > 1.3 & h < 40))
})

test_that("coastal BC curves are comparable to Temesgen and von Gadow (2004)", {
  # Cross-model sanity check (different region: interior IDF vs coastal CWH).
  # Expect similar small-tree heights; coastal trees are taller at large DBH.
  DBH <- c(10, 20)
  for (sp in c("THUJ.PLI", "PSEU.MEN")) {
    sl <- hd(DBH, sp)$height
    tg <- suppressMessages(suppressWarnings(hd_temesgen2004(DBH, sp)$height))
    expect_true(all(abs(sl / tg - 1) < 0.25), info = sp)
  }
})
