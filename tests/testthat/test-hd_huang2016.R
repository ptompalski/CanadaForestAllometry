# hd_huang2016(): Huang, Yang & Aitkin (2016), Alberta T/609.

quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# Source equations with Table 4 (base model) coefficients, for independent checks.
eq1 <- function(DBH, b1, b2, b3) 1.3 + b1 * (1 - exp(-b2 * DBH))^b3
eq2 <- function(DBH, b1, b2, b3) 1.3 + b1 / (1 + exp(b2 + b3 * log(DBH + 0.01)))

test_that("hd_huang2016 returns a well-formed tibble", {
  out <- quiet(hd_huang2016(DBH = c(10, 20), species = "PICE.GLA"))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)

  out <- quiet(hd_huang2016(DBH = 20, species = "PICE.GLA", keep_model_id = TRUE))
  expect_identical(names(out), c("height", "model_id", "subregion_group"))
  expect_identical(out$model_id, "huang2016")
  expect_identical(out$subregion_group, "provincial")
})

test_that("hd_huang2016 handles zero-length input", {
  expect_identical(hd_huang2016(numeric(), "PICE.GLA"), tibble::tibble(height = numeric()))
  out <- hd_huang2016(numeric(), "PICE.GLA", keep_model_id = TRUE)
  expect_identical(names(out), c("height", "model_id", "subregion_group"))
  expect_equal(nrow(out), 0L)
})

test_that("hd_huang2016 recycles inputs and works inside mutate()", {
  out <- quiet(hd_huang2016(DBH = c(10, 20, 30), species = "PICE.MAR", natural_subregion = "LF"))
  expect_equal(nrow(out), 3L)

  trees <- tibble::tibble(
    DBH = c(12, 25, 40),
    species = c("POPU.TRE", "PICE.GLA", "PINU.CON"),
    nsr = c(1L, 10L, 8L)
  )
  res <- quiet(dplyr::mutate(trees, hd_huang2016(DBH, species, natural_subregion = nsr)))
  expect_equal(
    res$height,
    quiet(hd_huang2016(trees$DBH, trees$species, trees$nsr))$height
  )

  expect_error(hd_huang2016(DBH = 1:3, species = c("PICE.GLA", "PICE.MAR")), "Incompatible")
  expect_error(
    hd_huang2016(DBH = 1:3, species = "PICE.GLA", natural_subregion = c(1, 2)),
    "Incompatible"
  )
})

test_that("hd_huang2016 validates inputs", {
  expect_error(hd_huang2016(-1, "PICE.GLA"), ">= 0")
  expect_error(hd_huang2016("10", "PICE.GLA"), "numeric")
  expect_error(hd_huang2016(Inf, "PICE.GLA"), "finite")
  expect_error(hd_huang2016(10, 1), "character")
  expect_error(hd_huang2016(10, "ACER.RUB"), "not covered")
  expect_error(hd_huang2016(10, "PICE.GLA", keep_model_id = NA), "keep_model_id")
  expect_error(hd_huang2016(10, "PICE.GLA", keep_model_id = c(TRUE, FALSE)), "keep_model_id")
  expect_error(hd_huang2016(10, "PICE.GLA", natural_subregion = 22), "Invalid")
  expect_error(hd_huang2016(10, "PICE.GLA", natural_subregion = 1.5), "Invalid")
  expect_error(hd_huang2016(10, "PICE.GLA", natural_subregion = "XX"), "Invalid")
  expect_error(hd_huang2016(10, "PICE.GLA", natural_subregion = "0"), "Invalid")
  expect_error(hd_huang2016(10, "PICE.GLA", natural_subregion = TRUE), "numeric or character")
})

test_that("NA DBH or species returns NA height", {
  out <- quiet(hd_huang2016(
    DBH = c(NA, 20, 20),
    species = c("PICE.GLA", NA, "PICE.GLA"),
    keep_model_id = TRUE
  ))
  expect_true(is.na(out$height[1]))
  expect_true(is.na(out$height[2]))
  expect_false(is.na(out$height[3]))
  expect_true(is.na(out$model_id[2]))
  expect_true(is.na(out$subregion_group[2]))
})

# --- TIER 1 (fidelity): white spruce, provincial base model [2] ---------------

test_that("hd_huang2016 reproduces the Section 5.1 worked example (T/609 p. 25)", {
  # Huang et al. (2016) Section 5.1: SW, DBH = 32.5 cm -> H = 25.4846 m.
  h <- quiet(hd_huang2016(32.5, "PICE.GLA"))$height
  expect_equal(h, 25.4846, tolerance = 5e-5 / 25.4846)
})

test_that("hd_huang2016 reproduces Table 9 population predictions (T/609 p. 24)", {
  # Table 9, column 3 (DBH, cm) and column 5 (unadjusted prediction, m; 2 dp).
  DBH <- c(
    32.5, 11.9, 36.6, 30.0, 22.0, 29.8, 34.6, 38.7, 25.7, 11.7, 27.4, 13.5,
    19.0, 26.2, 20.1, 18.5, 27.9, 30.4, 19.2, 20.8, 26.5, 26.2, 20.8, 31.2,
    22.1, 15.5, 24.4, 20.3
  )
  expected <- c(
    25.48, 11.86, 26.96, 24.44, 20.12, 24.35, 26.27, 27.62, 22.33, 11.66,
    23.21, 13.40, 18.03, 22.59, 18.83, 17.65, 23.46, 24.61, 18.18, 19.32,
    22.75, 22.59, 19.32, 24.96, 20.19, 15.20, 21.60, 18.97
  )
  h <- quiet(hd_huang2016(DBH, "PICE.GLA"))$height
  expect_equal(round(h, 2), expected)
  # Grand mean of predictions reported in the text: 20.9276.
  expect_equal(round(mean(h), 4), 20.9276)
})

# --- Table 4 wiring for subregion groups, mappings and combined models --------

test_that("natural-subregion groups select the Table 4 coefficients", {
  d <- c(5, 20, 45)
  sw <- function(nsr) quiet(hd_huang2016(d, "PICE.GLA", natural_subregion = nsr))$height
  expect_equal(sw(8), eq2(d, 26.3534, 4.1663, -1.4355))  # SW 7 to 9
  expect_equal(sw(10), eq2(d, 32.0178, 4.4626, -1.4887)) # SW 10
  expect_equal(sw(11), eq2(d, 38.0887, 4.9433, -1.5781)) # SW 11
  expect_equal(sw(1), eq2(d, 36.0507, 4.6909, -1.5902))  # SW 1-6, 12-21
  expect_equal(sw(21), sw(1))

  aw <- function(nsr) quiet(hd_huang2016(d, "POPU.TRE", natural_subregion = nsr))$height
  expect_equal(aw(10), eq1(d, 29.2555, 0.0473, 1.1990)) # AW 7 to 10
  expect_equal(aw(NA), eq1(d, 26.3137, 0.0757, 1.2699)) # AW provincial
  expect_equal(
    quiet(hd_huang2016(d, "POPU.BAL", natural_subregion = 11))$height,
    eq1(d, 29.2133, 0.0456, 1.0944) # PB 7 to 11
  )
  expect_equal(
    quiet(hd_huang2016(d, "PICE.MAR", natural_subregion = 9))$height,
    eq2(d, 24.0271, 4.1254, -1.5215) # SB 7 to 10
  )
})

test_that("population predictions use Table 4, not the Table 5 mixed fixed effects", {
  h <- quiet(hd_huang2016(30, "PICE.GLA"))$height
  expect_equal(h, eq2(30, 36.5824, 4.7766, -1.5639))
  expect_false(isTRUE(all.equal(h, eq2(30, 35.9274, 4.4797, -1.4472))))
})

test_that("natural_subregion accepts numeric, character, numeric-string and factor codes", {
  ref <- quiet(hd_huang2016(25, "PICE.MAR", natural_subregion = 11))$height
  expect_equal(quiet(hd_huang2016(25, "PICE.MAR", natural_subregion = "LF"))$height, ref)
  expect_equal(quiet(hd_huang2016(25, "PICE.MAR", natural_subregion = " lf "))$height, ref)
  expect_equal(quiet(hd_huang2016(25, "PICE.MAR", natural_subregion = "11"))$height, ref)
  expect_equal(quiet(hd_huang2016(25, "PICE.MAR", natural_subregion = factor("LF")))$height, ref)
  expect_equal(quiet(hd_huang2016(25, "PICE.MAR", natural_subregion = 11L))$height, ref)
})

test_that("provincial model is used without subregion, for NA, and for provincial-only species", {
  prov <- quiet(hd_huang2016(25, "PICE.GLA"))$height
  expect_equal(quiet(hd_huang2016(25, "PICE.GLA", natural_subregion = NA))$height, prov)
  expect_equal(quiet(hd_huang2016(25, "PICE.GLA", natural_subregion = NA_character_))$height, prov)

  out <- quiet(hd_huang2016(25, "BETU.PAP", natural_subregion = 11, keep_model_id = TRUE))
  expect_identical(out$subregion_group, "provincial")
  expect_equal(out$height, eq1(25, 23.1255, 0.0683, 1.1450))
})

test_that("source species mappings (Table 1) use the group equations", {
  d <- c(10, 30)
  same <- function(a, b, nsr = NULL) {
    expect_equal(
      quiet(hd_huang2016(d, a, natural_subregion = nsr))$height,
      quiet(hd_huang2016(d, b, natural_subregion = nsr))$height
    )
  }
  same("PINU.ALB", "PINU.CON", 8)
  same("PINU.FLE", "PINU.CON")
  same("PICE.ENG", "PICE.GLA", 10)
  same("ABIE.LAS", "ABIE.BAL", 11)
  same("LARI.OCC", "LARI.LAR")
  same("LARI.LYA", "LARI.LAR")
  same("PICEENG", "PICE.GLA")
})

test_that("combined models are selected with DECID / CONIF / ALL", {
  expect_equal(
    quiet(hd_huang2016(30, "DECID"))$height,
    eq1(30, 26.8418, 0.0687, 1.2442)
  )
  expect_equal(
    quiet(hd_huang2016(30, "conif", natural_subregion = 11))$height,
    eq2(30, 35.8728, 4.4998, -1.4848)
  )
  expect_equal(quiet(hd_huang2016(30, "All"))$height, eq2(30, 36.1486, 4.3125, -1.4208))
})

# --- messages and warnings -----------------------------------------------------

test_that("hd_huang2016 reports variant, fallback and species mapping", {
  rlang::local_options(
    rlib_message_verbosity = "verbose",
    CanadaForestAllometry.quiet = FALSE
  )
  msgs <- function(...) paste(capture_messages(hd_huang2016(...)), collapse = "")
  expect_match(msgs(20, "PICE.GLA"), "provincial models")
  expect_match(msgs(20, "PICE.GLA", natural_subregion = 8), "natural-subregion-group models")
  expect_match(
    msgs(20, "BETU.PAP", natural_subregion = 8),
    "no natural-subregion-group models for BETU.PAP"
  )
  expect_match(msgs(20, "PICE.GLA", natural_subregion = c(8, NA)), "NA `natural_subregion`")
  expect_match(
    msgs(20, "PINU.ALB"),
    "PINU.ALB is predicted with the PL \\(PINU.CON\\) equation"
  )
})

test_that("DBH outside the Table 3 range is computed with one warning", {
  # SW provincial DBH range (Table 3): 0.2-78.5 cm.
  expect_warning(
    out <- suppressMessages(hd_huang2016(c(0, 30, 90), "PICE.GLA")),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_true(all(is.finite(out$height)))
  expect_no_warning(suppressMessages(hd_huang2016(c(1, 30, 70), "PICE.GLA")))
})

# --- behaviour / plausibility (deciduous form [1] has no source benchmark) ----

test_that("height starts at 1.3 m and increases with DBH for all species keys", {
  keys <- c(
    "POPU.TRE", "POPU.BAL", "BETU.PAP", "PINU.CON", "PINU.BAN", "PICE.GLA",
    "PICE.MAR", "ABIE.BAL", "PSEU.MEN", "LARI.LAR", "DECID", "CONIF", "ALL"
  )
  d <- c(0, 0.5, seq(1, 80, by = 1))
  for (k in keys) {
    h <- quiet(hd_huang2016(d, k))$height
    # form [2] is ~1.3015 at DBH = 0 because of ln(DBH + 0.01)
    expect_lt(abs(h[1] - 1.3), 0.01)
    expect_true(all(diff(h) > 0), info = k)
    expect_true(all(h < 60), info = k)
  }
})

test_that("aspen provincial predictions are plausible (sanity check, no source benchmark)", {
  h <- quiet(hd_huang2016(c(0.1, 10, 30, 60), "POPU.TRE"))$height
  expect_equal(round(h, 2), c(1.35, 13.08, 24.21, 27.26))
  expect_true(all(h < 1.3 + 26.3137))
})

test_that("hd_huang2016 is registered consistently", {
  info <- get_hd_model_info("huang2016")
  expect_identical(info$engine, "hd_huang2016")
  expect_identical(info$predictors$arg, c("DBH", "natural_subregion"))
  expect_true(all(c("PICE.GLA", "PINU.ALB", "LARI.LYA") %in% info$species))
  expect_false(any(c("DECID", "CONIF", "ALL") %in% info$species))
  expect_true("huang2016" %in% find_hd_models(jurisdiction = "AB", species = "PICE.ENG")$model_id)
})
