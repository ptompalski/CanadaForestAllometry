# hd_huang1994(): Huang, Titus, Lakusta & Held (1994), Alberta T/291.
# No worked example or reference predictions in the source: checks compare
# against the published equation with hand-entered Table 1 / Table 2
# coefficients, plus plausibility.

quiet <- function(expr) suppressMessages(suppressWarnings(expr))

cr <- function(DBH, a, b, c) 1.3 + a * (1 - exp(-b * DBH))^c

test_that("hd_huang1994 returns a well-formed tibble", {
  out <- quiet(hd_huang1994(DBH = c(10, 20), species = "PICE.GLA"))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)

  out <- quiet(hd_huang1994(DBH = 20, species = "PICE.GLA", keep_model_id = TRUE))
  expect_identical(names(out), c("height", "model_id", "subregion_group"))
  expect_identical(out$model_id, "huang1994")
  expect_identical(out$subregion_group, "provincial")
})

test_that("hd_huang1994 handles zero-length input", {
  expect_identical(hd_huang1994(numeric(), "PICE.GLA"), tibble::tibble(height = numeric()))
  out <- hd_huang1994(numeric(), "PICE.GLA", keep_model_id = TRUE)
  expect_identical(names(out), c("height", "model_id", "subregion_group"))
  expect_equal(nrow(out), 0L)
})

test_that("hd_huang1994 recycles inputs and works inside mutate()", {
  out <- quiet(hd_huang1994(DBH = c(10, 20, 30), species = "PICE.MAR", natural_subregion = "LF"))
  expect_equal(nrow(out), 3L)

  trees <- tibble::tibble(
    DBH = c(12, 25, 40),
    species = c("POPU.TRE", "PICE.GLA", "PINU.CON"),
    nsr = c(1L, 10L, 8L)
  )
  res <- quiet(dplyr::mutate(trees, hd_huang1994(DBH, species, natural_subregion = nsr)))
  expect_equal(
    res$height,
    quiet(hd_huang1994(trees$DBH, trees$species, trees$nsr))$height
  )

  expect_error(hd_huang1994(DBH = 1:3, species = c("PICE.GLA", "PICE.MAR")), "Incompatible")
  expect_error(
    hd_huang1994(DBH = 1:3, species = "PICE.GLA", natural_subregion = c(1, 2)),
    "Incompatible"
  )
})

test_that("hd_huang1994 validates inputs", {
  expect_error(hd_huang1994(-1, "PICE.GLA"), ">= 0")
  expect_error(hd_huang1994("10", "PICE.GLA"), "numeric")
  expect_error(hd_huang1994(10, 1), "character")
  expect_error(hd_huang1994(10, "PINU.ALB"), "not covered")
  expect_error(hd_huang1994(10, "ALL"), "Unrecognized")
  expect_error(hd_huang1994(10, "PICE.GLA", keep_model_id = NA), "keep_model_id")
  expect_error(hd_huang1994(10, "PICE.GLA", natural_subregion = 22), "Invalid")
  expect_error(hd_huang1994(10, "PICE.GLA", natural_subregion = "XX"), "Invalid")
})

test_that("NA DBH or species returns NA height", {
  out <- quiet(hd_huang1994(
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

# --- coefficients (Table 2 provincial; Table 1 natural-region groups) --------

test_that("provincial models use Table 2 coefficients", {
  d <- c(5, 20, 45)
  expect_equal(
    quiet(hd_huang1994(d, "PICE.GLA"))$height,
    cr(d, 32.1261, 0.04633, 1.3032)
  )
  expect_equal(
    quiet(hd_huang1994(d, "BETU.PAP"))$height,
    cr(d, 27.9727, 0.03522, 0.8695)
  )
  # Fb and Fa share one fit
  expect_equal(
    quiet(hd_huang1994(d, "ABIE.LAS"))$height,
    cr(d, 24.7532, 0.06615, 1.5695)
  )
  expect_equal(
    quiet(hd_huang1994(d, "ABIEBAL"))$height,
    quiet(hd_huang1994(d, "ABIE.LAS"))$height
  )
})

test_that("natural-region groups select the Table 1 coefficients", {
  d <- 25
  h <- function(sp, nsr) quiet(hd_huang1994(d, sp, natural_subregion = nsr))$height
  expect_equal(h("PICE.GLA", 8), cr(d, 38.3117, 0.02635, 1.1152))
  expect_equal(h("PICE.GLA", "CM"), cr(d, 29.8812, 0.05557, 1.3911))
  expect_equal(h("PICE.GLA", 14), cr(d, 32.4278, 0.05055, 1.3940))
  expect_equal(h("PICE.MAR", 11), cr(d, 24.9305, 0.05281, 1.2552))
  expect_equal(h("POPU.TRE", 2), cr(d, 26.5484, 0.05699, 0.9846))
  expect_equal(h("ABIE.BAL", 10), cr(d, 24.3383, 0.06707, 1.5909))
  expect_equal(h("CONIF", 16), cr(d, 30.7738, 0.06562, 1.6975))
  expect_equal(h("decid", 9), cr(d, 25.8069, 0.06818, 1.2063))

  out <- quiet(hd_huang1994(d, "PICE.GLA", natural_subregion = 9, keep_model_id = TRUE))
  expect_identical(out$subregion_group, "9,11,14")
})

test_that("2006 subregion 21 (UBH) uses the 1994 region-6 models", {
  for (sp in c("PICE.GLA", "PICE.MAR", "POPU.TRE", "CONIF", "DECID")) {
    expect_equal(
      quiet(hd_huang1994(25, sp, natural_subregion = 21))$height,
      quiet(hd_huang1994(25, sp, natural_subregion = 6))$height,
      info = sp
    )
  }
})

test_that("provincial fallback: no subregion, NA, grassland, provincial-only species", {
  prov <- quiet(hd_huang1994(25, "PICE.GLA"))$height
  expect_equal(quiet(hd_huang1994(25, "PICE.GLA", natural_subregion = NA))$height, prov)
  for (g in 17:20) {
    out <- quiet(hd_huang1994(25, "PICE.GLA", natural_subregion = g, keep_model_id = TRUE))
    expect_equal(out$height, prov)
    expect_identical(out$subregion_group, "provincial")
  }
  for (sp in c("BETU.PAP", "PINU.BAN", "PICE.ENG", "PSEU.MEN", "LARI.LAR")) {
    expect_equal(
      quiet(hd_huang1994(25, sp, natural_subregion = 8))$height,
      quiet(hd_huang1994(25, sp))$height,
      info = sp
    )
  }
})

test_that("CONIF/DECID without a regional model return NA with a warning", {
  expect_warning(
    out <- suppressMessages(hd_huang1994(
      c(25, 25, 25, 25), c("CONIF", "DECID", "CONIF", "PICE.GLA"),
      natural_subregion = c(18, NA, 8, 18), keep_model_id = TRUE
    )),
    "2 rows"
  )
  expect_true(all(is.na(out$height[1:2])))
  expect_true(all(is.na(out$model_id[1:2])))
  expect_false(anyNA(out$height[3:4]))
  expect_warning(suppressMessages(hd_huang1994(25, "CONIF")), "CONIF")
})

# --- messages and warnings -----------------------------------------------------

test_that("hd_huang1994 reports variant and fallback", {
  rlang::local_options(
    rlib_message_verbosity = "verbose",
    CanadaForestAllometry.quiet = FALSE
  )
  msgs <- function(...) paste(capture_messages(hd_huang1994(...)), collapse = "")
  expect_match(msgs(20, "PICE.GLA"), "provincial models")
  expect_match(msgs(20, "PICE.GLA", natural_subregion = 8), "natural-region-group models")
  expect_match(
    msgs(20, "BETU.PAP", natural_subregion = 8),
    "no natural-region-group model for BETU.PAP"
  )
  expect_match(
    msgs(20, "PICE.GLA", natural_subregion = 18),
    "no natural-region-group model for PICE.GLA"
  )
  expect_match(msgs(20, "PICE.GLA", natural_subregion = c(8, NA)), "NA `natural_subregion`")
})

test_that("DBH outside the Table 3 range is computed with one warning", {
  # SW DBH range (Table 3): 1.2-78.5 cm.
  expect_warning(
    out <- suppressMessages(hd_huang1994(c(0.5, 30, 90), "PICE.GLA", natural_subregion = 8)),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_true(all(is.finite(out$height)))
  expect_no_warning(suppressMessages(hd_huang1994(c(2, 30, 70), "PICE.GLA")))
  # no published ranges for the groups
  expect_no_warning(suppressMessages(hd_huang1994(c(0.5, 120), "CONIF", natural_subregion = 8)))
})

# --- behaviour / plausibility (no source benchmark) ----------------------------

test_that("height starts at 1.3 m and increases with DBH for every parameter set", {
  pars <- .get_internal_data("parameters_hd_Huang1994")
  d <- c(0, 0.5, seq(1, 80, by = 1))
  sets <- unique(pars[c("species_key", "natural_subregion")])
  for (i in seq_len(nrow(sets))) {
    nsr <- sets$natural_subregion[i]
    h <- quiet(hd_huang1994(d, sets$species_key[i], natural_subregion = nsr))$height
    info <- paste(sets$species_key[i], nsr)
    expect_equal(h[1], 1.3, info = info)
    expect_true(all(diff(h) > 0), info = info)
    expect_true(all(h < 45), info = info)
  }
})

test_that("predictions are close to hd_huang2016 (same data lineage)", {
  d <- c(10, 20, 30)
  for (sp in c("PICE.GLA", "PICE.MAR", "POPU.TRE", "PINU.CON")) {
    h94 <- quiet(hd_huang1994(d, sp))$height
    h16 <- quiet(hd_huang2016(d, sp))$height
    expect_true(all(abs(h94 - h16) / h16 < 0.15), info = sp)
  }
})

test_that("hd_huang1994 is registered consistently", {
  info <- get_hd_model_info("huang1994")
  expect_identical(info$engine, "hd_huang1994")
  expect_identical(info$region_arg, "natural_subregion")
  expect_identical(info$predictors$arg, c("DBH", "natural_subregion"))
  expect_setequal(
    info$species,
    c(
      "ABIE.BAL", "ABIE.LAS", "BETU.PAP", "LARI.LAR", "PICE.ENG", "PICE.GLA",
      "PICE.MAR", "PINU.BAN", "PINU.CON", "POPU.BAL", "POPU.TRE", "PSEU.MEN"
    )
  )
})
