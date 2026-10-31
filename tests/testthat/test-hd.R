test_that("hd() picks the highest-ranked eligible model and matches the engine", {
  out <- hd(20, "PICE.MAR", "ON", HDOM = 18, BA = 25, TPH = 1200)
  expect_identical(out$model_id, "sharma2016_natural")
  expect_equal(
    out$height,
    hd_sharma2016(20, "PICE.MAR", variant = "natural", HDOM = 18, BA = 25, TPH = 1200)$height
  )

  # without stand predictors, a DBH-only model is used
  expect_identical(hd(20, "PICE.MAR", "ON")$model_id, "sharmazhang2004_eq5")
})

test_that("hd() passes each model only its own predictors", {
  out <- hd(20, "PICE.MAR", "AB", HTOP = 15, pick_best = FALSE)[[1]]
  expect_true(all(c("huang2009_m10", "huang2009_m9") %in% out$model_id))
  expect_equal(
    out$height[out$model_id == "huang2009_m10"],
    hd_huang2009(20, "PICE.MAR", HTOP = 15)$height
  )
  expect_equal(
    out$height[out$model_id == "huang2009_m9"],
    hd_huang2009(20, "PICE.MAR")$height
  )
})

test_that("hd() uses optional predictors per tree", {
  out <- hd(c(20, 20), "PICE.GLA", "AB", natural_subregion = c("CM", NA))
  expect_identical(out$model_id, c("huang2016", "huang2016"))
  expect_equal(out$height[1], hd_huang2016(20, "PICE.GLA", natural_subregion = "CM")$height)
  expect_equal(out$height[2], hd_huang2016(20, "PICE.GLA")$height)
})

test_that("hd() falls back to the next model when one fails", {
  out <- hd(20, "PICE.MAR", "AB", natural_subregion = "not_a_region")
  expect_identical(out$model_id, "huang2009_m9")
})

test_that("hd() respects stand_origin", {
  out <- hd(
    c(20, 20), "PICE.MAR", "QC",
    HDOM = 12, DDOM = 15, stand_origin = c("natural", "plantation")
  ) |> suppressWarnings()
  expect_identical(out$model_id, c(NA_character_, "auger2025_dominant"))

  # natural-stand fallback for plantation trees warns
  expect_warning(
    out <- hd(20, "PICE.MAR", "AB", stand_origin = "plantation"),
    "natural-stand model was used"
  )
  expect_identical(out$model_id, "huang2016")

  # plantation trees prefer plantation models but can fall back to natural ones
  out <- hd(20, "PICE.MAR", "ON", HDOM = 18, BA = 25, TPH = 1200,
    stand_origin = "plantation", pick_best = FALSE)[[1]]
  expect_identical(out$model_id[1], "sharma2016_plantation")
  expect_false("sharma2016_natural" %in% out$model_id[1:2])
})

test_that("hd() restricts BC models to coastal or interior trees", {
  out <- hd(c(30, 30, 30), "PSEU.MEN", "BC",
    bec_zone = c("CWH", "idf", NA), bc_region = c(NA, NA, "interior"))
  expect_identical(
    out$model_id,
    c("staudhammerlemay2000_dbh", "temesgen2004_m2", "temesgen2004_m2")
  )

  expect_warning(hd(30, "PSEU.MEN", "BC"), "only part of BC")
  # region inputs are ignored outside BC
  expect_no_warning(hd(20, "PICE.MAR", "AB", bec_zone = "CWH"))

  expect_error(hd(30, "PSEU.MEN", "BC", bec_zone = "XYZ"), "Unknown `bec_zone`")
  expect_error(hd(30, "PSEU.MEN", "BC", bc_region = "north"), "bc_region")
  expect_error(
    hd(30, "PSEU.MEN", "BC", bec_zone = "CWH", bc_region = "interior"),
    "disagree"
  )
})

test_that("hd() returns NA with a warning for uncovered trees (exact species match)", {
  expect_warning(
    out <- hd(c(20, 20, 20), c("PICE.MAR", "ACER.SAC", "BETU.PAP"), c("ON", "SK", "ON")),
    "2 trees"
  )
  expect_identical(is.na(out$height), c(FALSE, TRUE, TRUE))
})

test_that("hd(pick_best = FALSE) returns ordered candidates per tree", {
  all <- hd(c(20, 30), "PICE.MAR", "ON", HDOM = 18, BA = 25, TPH = 1200, pick_best = FALSE)
  best <- hd(c(20, 30), "PICE.MAR", "ON", HDOM = 18, BA = 25, TPH = 1200)
  expect_length(all, 2L)
  expect_identical(vapply(all, function(x) x$model_id[1], ""), best$model_id)
  expect_equal(vapply(all, function(x) x$height[1], 1), best$height)

  no_id <- hd(20, "PICE.MAR", "ON", pick_best = FALSE, keep_model_id = FALSE)
  expect_named(no_id[[1]], "height")
  expect_named(hd(20, "PICE.MAR", "ON", keep_model_id = FALSE), "height")
})

test_that("hd() validates inputs", {
  expect_error(hd(20, "PICE.MAR", "ON", 25), "must be named")
  expect_error(hd(20, "PICE.MAR", "ON", ba = 25), "Unknown predictor")
  expect_error(hd(20, "PICE.MAR", "ON", stand_origin = "planted"), "stand_origin")
  expect_error(hd(c(20, 25, 30), "PICE.MAR", c("ON", "QC")), "length 1 or 3")
})
