# hd_huang2016b(): Huang & Yang (2016), Alberta T/610.

quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# Source equations with Table A1 (base model) coefficients, for independent checks.
eq1 <- function(DBH, BA, HDOM, b) {
  1.3 + (b[1] + b[2] * BA + b[3] * HDOM) * (1 - exp(-b[4] * DBH))^b[5]
}
eq2 <- function(DBH, BA, HDOM, b) {
  1.3 + (b[1] + b[2] * BA + b[3] * HDOM) / (1 + exp(b[4] + b[5] * log(DBH + 0.01)))
}

test_that("hd_huang2016b returns a well-formed tibble", {
  out <- quiet(hd_huang2016b(DBH = c(10, 20), species = "PICE.GLA", HDOM = 20, BA = 25))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)

  out <- quiet(hd_huang2016b(20, "PICE.GLA", 20, 25, keep_model_id = TRUE))
  expect_identical(names(out), c("height", "model_id"))
  expect_identical(out$model_id, "huang2016b")
})

test_that("hd_huang2016b handles zero-length input", {
  expect_identical(
    hd_huang2016b(numeric(), "PICE.GLA", 20, 25),
    tibble::tibble(height = numeric())
  )
  out <- hd_huang2016b(numeric(), "PICE.GLA", 20, 25, keep_model_id = TRUE)
  expect_identical(names(out), c("height", "model_id"))
  expect_equal(nrow(out), 0L)
})

test_that("hd_huang2016b recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 40),
    species = c("POPU.TRE", "PICE.GLA", "PINU.CON"),
    HDOM = c(18, 24, 20),
    BA = c(20, 25, 34)
  )
  res <- quiet(dplyr::mutate(trees, hd_huang2016b(DBH, species, HDOM, BA)))
  expect_equal(
    res$height,
    quiet(hd_huang2016b(trees$DBH, trees$species, trees$HDOM, trees$BA))$height
  )
  expect_error(hd_huang2016b(1:3, c("PICE.GLA", "PICE.MAR"), 20, 25), "Incompatible")
  expect_error(hd_huang2016b(1:3, "PICE.GLA", c(20, 21), 25), "Incompatible")
})

test_that("hd_huang2016b validates inputs", {
  expect_error(hd_huang2016b(-1, "PICE.GLA", 20, 25), ">= 0")
  expect_error(hd_huang2016b("10", "PICE.GLA", 20, 25), "numeric")
  expect_error(hd_huang2016b(10, 1, 20, 25), "character")
  expect_error(hd_huang2016b(10, "PICE.GLA", 0, 25), "> 0")
  expect_error(hd_huang2016b(10, "PICE.GLA", 20, -1), ">= 0")
  expect_error(hd_huang2016b(10, "PICE.GLA", 20, Inf), "finite")
  expect_error(hd_huang2016b(10, "PICE.GLA"), "HDOM")
  expect_error(hd_huang2016b(10, "PICE.GLA", 20), "BA")
  expect_error(hd_huang2016b(10, "ACER.RUB", 20, 25), "not covered")
  expect_error(hd_huang2016b(10, "PICE.GLA", 20, 25, keep_model_id = NA), "keep_model_id")
})

test_that("balsam and subalpine fir are rejected (duplicated FB column in Table A1)", {
  expect_error(hd_huang2016b(10, "ABIE.BAL", 20, 25), "duplicate")
  expect_error(hd_huang2016b(10, c("PICE.GLA", "ABIE.LAS"), 20, 25), "ABIE.LAS")
  expect_error(hd_huang2016b(10, "ABIEBAL", 20, 25), "ABIE.BAL")
})

test_that("NA in any input returns NA height", {
  out <- quiet(hd_huang2016b(
    DBH = c(NA, 20, 20, 20, 20),
    species = c("PICE.GLA", NA, "PICE.GLA", "PICE.GLA", "PICE.GLA"),
    HDOM = c(20, 20, NA, 20, 20),
    BA = c(25, 25, 25, NA, 25),
    keep_model_id = TRUE
  ))
  expect_identical(is.na(out$height), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_true(is.na(out$model_id[2]))
})

# --- TIER 1 (fidelity): aspen, base model [1] ---------------------------------

test_that("hd_huang2016b reproduces the aspen worked example (T/610 p. 9)", {
  # Plot 1 tree 1: DBH 29.7, BA 36.8225, HDC 29.9333 -> 27.2046.
  h <- hd_huang2016b(29.7, "POPU.TRE", HDOM = 29.9333, BA = 36.8225)$height
  expect_equal(h, 27.2046, tolerance = 5e-5 / 27.2046)
})

test_that("hd_huang2016b reproduces Table 6 population predictions (T/610 p. 9)", {
  # Table 6: DBH and unadjusted population prediction (2 dp); BA/HDC per plot.
  h <- c(
    hd_huang2016b(c(29.7, 26.9, 37.5, 1.6, 4.0), "POPU.TRE", 29.9333, 36.8225)$height,
    hd_huang2016b(c(13.9, 15.3, 15.3), "POPU.TRE", 16.0000, 47.0862)$height
  )
  expect_equal(
    round(h, 2),
    c(27.20, 26.58, 28.26, 3.14, 7.09, 16.44, 17.37, 17.37)
  )
})

# --- Table A1 wiring for the other species groups ----------------------------

test_that("each species group uses its Table A1 base coefficients", {
  d <- c(0, 5, 20, 45)
  h <- function(sp) quiet(hd_huang2016b(d, sp, HDOM = 18, BA = 22))$height
  expect_equal(h("POPU.BAL"), eq1(d, 22, 18, c(20.2034, -0.0158, 0.2968, 0.0602, 1.1481)))
  expect_equal(h("BETU.PAP"), eq1(d, 22, 18, c(13.9495, 0.087, 0.2685, 0.0914, 1.0039)))
  expect_equal(h("DECID"), eq1(d, 22, 18, c(16.5495, 0.0545, 0.3163, 0.0857, 1.3305)))
  expect_equal(h("PINU.CON"), eq2(d, 22, 18, c(4.4929, 0.0557, 0.8195, 3.7328, -1.7193)))
  expect_equal(h("PICE.GLA"), eq2(d, 22, 18, c(16.3681, 0.1255, 0.5657, 4.5717, -1.5770)))
  expect_equal(h("PICE.MAR"), eq2(d, 22, 18, c(11.1383, 0.0992, 0.6271, 3.7403, -1.4773)))
  expect_equal(h("PSEU.MEN"), eq2(d, 22, 18, c(17.0503, 0.0722, 0.2153, 3.8079, -1.3787)))
  expect_equal(h("LARI.LAR"), eq2(d, 22, 18, c(2.2861, 0.0605, 1.073, 2.8852, -1.4050)))
  expect_equal(h("PINU.BAN"), eq2(d, 22, 18, c(5.5979, 0.0696, 0.7115, 2.9795, -1.5749)))
  expect_equal(h("CONIF"), eq2(d, 22, 18, c(14.8675, 0.1044, 0.5389, 4.2967, -1.5561)))
  expect_equal(h("ALL"), eq2(d, 22, 18, c(15.9499, 0.1053, 0.4652, 4.1794, -1.5228)))
  expect_equal(h("all"), h("ALL"))
})

test_that("population predictions use base, not mixed fixed effects", {
  # Table 9 col. 4 / Table 18 (AW mixed model [3], u = 0) -> 28.8727; base -> 27.2046.
  h <- hd_huang2016b(29.7, "POPU.TRE", 29.9333, 36.8225)$height
  mixed <- eq1(29.7, 36.8225, 29.9333, c(5.7308, 0.03232, 0.7515, 0.1082, 1.5764))
  expect_equal(round(mixed, 4), 28.8727)
  expect_false(isTRUE(all.equal(h, mixed)))
})

test_that("Table 1 species mapping uses the group equation and informs once", {
  ref <- quiet(hd_huang2016b(25, "PINU.CON", 20, 30))$height
  rlang::local_options(rlib_message_verbosity = "verbose")
  expect_message(
    out <- hd_huang2016b(25, "PINU.FLE", 20, 30),
    "PINU.FLE is predicted with the PL \\(PINU.CON\\)"
  )
  expect_equal(out$height, ref)
  expect_equal(
    quiet(hd_huang2016b(25, "PICE.ENG", 20, 30))$height,
    quiet(hd_huang2016b(25, "PICE.GLA", 20, 30))$height
  )
  expect_equal(
    quiet(hd_huang2016b(25, c("LARI.OCC", "LARI.LYA"), 10, 15))$height,
    rep(quiet(hd_huang2016b(25, "LARI.LAR", 10, 15))$height, 2)
  )
})

test_that("out-of-range DBH, BA and HDOM warn once and still predict", {
  # FD ranges (Table 2): DBH 1.30-60.90, BA 1.46-71.79, HDC 11.30-22.20.
  w <- expect_warning(
    out <- hd_huang2016b(c(0.5, 20, 20, 20), "PSEU.MEN", c(15, 25, 15, 15), c(30, 30, 80, 30)),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_match(conditionMessage(w), "3 rows")
  expect_false(anyNA(out$height))
  expect_no_warning(hd_huang2016b(20, "PSEU.MEN", 15, 30))
})

# --- TIER 2 (plausibility): shape ---------------------------------------------

test_that("predictions behave plausibly for all groups", {
  sp <- c("POPU.TRE", "POPU.BAL", "BETU.PAP", "PINU.CON", "PICE.GLA", "PICE.MAR",
          "PSEU.MEN", "LARI.LAR", "PINU.BAN", "DECID", "CONIF", "ALL")
  for (s in sp) {
    d <- c(0, 1, 5, 10, 20, 30, 40)
    h <- quiet(hd_huang2016b(d, s, HDOM = 18, BA = 20))$height
    expect_equal(h[1], 1.3, tolerance = 0.05, info = s)
    expect_true(all(diff(h) > 0), info = s)
    expect_true(all(h < 45), info = s)
    # taller dominant height -> taller trees (all b3 > 0)
    expect_true(
      quiet(hd_huang2016b(20, s, HDOM = 25, BA = 20))$height >
        quiet(hd_huang2016b(20, s, HDOM = 15, BA = 20))$height,
      info = s
    )
  }
})

test_that("registry is wired", {
  info <- get_hd_model_info("huang2016b")
  expect_identical(info$engine, "hd_huang2016b")
  expect_setequal(info$required_predictors, c("DBH", "HDOM", "BA"))
  expect_false(any(c("ABIE.BAL", "ABIE.LAS") %in% info$species))
  expect_true(all(c("POPU.TRE", "PICE.GLA", "PINU.FLE") %in% info$species))
})
