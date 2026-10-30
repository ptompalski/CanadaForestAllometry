# hd_auger2016(): Auger (2016), Note de recherche forestière no 146.

# Equation 1 (p. 4), population level.
eq1 <- function(DBH, A, B = 0) {
  L <- log(DBH + 1)
  1.3 + A * L + B * L^2
}

hd <- function(...) suppressMessages(suppressWarnings(hd_auger2016(...)))

test_that("hd_auger2016 returns a well-formed tibble", {
  out <- hd(DBH = c(15, 25), species = "ACER.SAC", BA = 20)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_true(all(out$height > 1.3))

  empty <- hd_auger2016(DBH = numeric(), species = character(), BA = numeric())
  expect_identical(empty, tibble::tibble(height = numeric()))
})

test_that("hd_auger2016 reproduces the Annexe 1 worked example (white spruce)", {
  out <- hd(
    DBH = 20, species = "PICE.GLA", BA = 20, QMD = 15,
    SDOM = "1", VP = "FC1", TMOY = 2.3
  )$height
  expect_equal(out, 12.0, tolerance = 0.05 / 12)

  # Independent recomputation from the transcribed Annexe 1, 3 and 4 values.
  A <- -2.9051 + 0.0252 * 20 - 0.2230 * (20 / 15) + 0.0789 * 2.3 - 0.3581 - 0.4203
  expect_equal(out, eq1(20, A, 2.2411), tolerance = 1e-10)
})

test_that("hd_auger2016 reproduces the effect sizes reported on p. 11", {
  f <- function(species, ...) {
    hd(
      DBH = 20, species = species, BA = 20, QMD = 15.4, MP = 2, PTOT = 1000,
      altitude = 300, disturbance = "aucune", ...
    )$height
  }
  epn <- function(...) f("PICE.MAR", ...)
  sab <- function(...) f("ABIE.BAL", ...)
  ers <- function(...) f("ACER.SAH", ...)

  expect_equal(
    epn(SDOM = "5ouest", VP = "RS2", TMOY = 1) - epn(SDOM = "6est", VP = "RS2", TMOY = 1),
    1.0, tolerance = 0.1
  )
  expect_equal(
    sab(SDOM = "5ouest", VP = "RS2") - sab(SDOM = "6est", VP = "RS2"),
    1.8, tolerance = 0.1 / 1.8
  )
  expect_equal(
    epn(SDOM = "5ouest", VP = "RS2", TMOY = 4) - epn(SDOM = "5ouest", VP = "RS2", TMOY = 1),
    0.9, tolerance = 0.1 / 0.9
  )
  expect_equal(
    ers(SDOM = "3ouest", VP = "FE3", TMOY = 5.5) - ers(SDOM = "3ouest", VP = "FE3", TMOY = 3.5),
    1.2, tolerance = 0.1 / 1.2
  )
  expect_equal(
    epn(SDOM = "5ouest", VP = "RE2", TMOY = 1) - epn(SDOM = "5ouest", VP = "RS2", TMOY = 1),
    0.5, tolerance = 0.1 / 0.5
  )
  expect_equal(
    sab(SDOM = "5ouest", VP = "RE2") - sab(SDOM = "5ouest", VP = "RS2"),
    -0.59, tolerance = 0.1 / 0.59
  )
  expect_equal(
    ers(SDOM = "3ouest", VP = "MJ1", TMOY = 3.5) - ers(SDOM = "3ouest", VP = "FE3", TMOY = 3.5),
    -1.0, tolerance = 0.1
  )
})

test_that("hd_auger2016 matches CFSForestTools for species with published parameters", {
  # CFSForestTools generalhdrelation2014 unit-test predictions (commit
  # fa86f80c), 3 rows per species; full set from
  # tmp/generate_hd_auger2016_comparison_values.R. Other species differ by
  # design (CFSForestTools uses unpublished estimates for them).
  ref <- tibble::tribble(
    ~species, ~DBH, ~BA, ~QMD, ~SDOM, ~MP, ~VP, ~TMOY, ~PTOT, ~altitude, ~expected,
    "PRUN.SER", 11.7, 4.291710, 19.086426, "1", "2", "FE2", 6.1296, 948.2191, 90, 11.7242,
    "PRUN.SER", 32.9, 40.184249, 29.829712, "1", "2", "FE2", 7.0328, 1041.631, 40, 23.5316,
    "PRUN.SER", 18.4, 22.862725, 15.260334, "2EST", "2", "MJ1", 4.9067, 1114.7959, 140, 15.1708,
    "ACER.SAC", 20.2, 23.179574, 18.365542, "1", "8", "FO1", 6.3059, 1001.1513, 10, 17.8708,
    "ACER.SAC", 54.8, 42.522517, 39.330595, "1", "8", "FO1", 6.8033, 982.6539, 20, 30.0355,
    "ACER.SAC", 13.4, 24.738314, 19.842442, "1", "5", "FO1", 6.1952, 995.3925, 10, 14.9948,
    "FRAX.AME", 10.3, 14.066147, 18.469834, "1", "2", "FE2", 6.5147, 1151.9614, 80, 13.3122,
    "FRAX.AME", 15.8, 57.826452, 33.026796, "1", "5", "MS1", 7.2375, 962.0803, 40, 19.5642,
    "FRAX.AME", 9.4, 4.199838, 17.480438, "1", "2", "FE2", 5.3091, 878.7129, 150, 11.8487,
    "FRAX.NIG", 15.3, 20.697166, 16.655156, "1", "8", "MF1", 6.4929, 1024.5266, 30, 15.2777,
    "FRAX.NIG", 31.3, 27.635079, 22.831452, "1", "2", "FE3", 4.9266, 969.4813, 200, 22.3122,
    "FRAX.NIG", 19.9, 32.014184, 21.281637, "1", "5", "FE3", 5.2413, 1053.0521, 140, 19.0523,
    "LARI.LAR", 18.8, 21.560417, 16.998940, "2EST", "5", "MS2", 6.9153, 1001.9301, 50, 17.0634,
    "LARI.LAR", 24.6, 37.381477, 29.417241, "2EST", "2", "MS2", 5.1758, 912.2732, 180, 23.3030,
    "LARI.LAR", 29, 14.633735, 15.028155, "2EST", "7", "RS3", 5.1053, 1065.3375, 50, 18.7250,
    "ULMU.AME", 13.9, 14.066147, 18.469834, "1", "2", "FE3", 6.5147, 1151.9614, 80, 12.3193,
    "ULMU.AME", 22.9, 50.107009, 27.396484, "1", "5", "RS1", 7.2375, 962.0803, 40, 20.7526,
    "ULMU.AME", 13, 14.003276, 15.959558, "1", "2", "MJ1", 5.5221, 843.7713, 120, 11.8186,
    "OSTR.VIR", 11.8, 6.786527, 19.596910, "1", "2", "FE2", 6.5147, 1151.9614, 80, 12.3115,
    "OSTR.VIR", 16.1, 41.401165, 20.959008, "1", "2", "FE5", 5.3562, 905.5936, 190, 16.0167,
    "OSTR.VIR", 16.3, 40.358666, 22.122225, "1", "2", "FE5", 5.3562, 905.5936, 190, 16.0324,
    "PINU.STR", 15, 4.291710, 19.086426, "1", "2", "FE2", 6.1296, 948.2191, 90, 9.9925,
    "PINU.STR", 69.1, 47.458901, 25.558951, "1", "1", "RS5", 6.5315, 1030.8674, 40, 30.6808,
    "PINU.STR", 22.4, 16.550695, 22.952669, "1", "2", "RP1", 5.099, 918.869, 190, 14.8848,
    "PINU.RES", 17.7, 27.905864, 15.263983, "2EST", "2", "RP1", 6.8951, 1000.7111, 50, 13.4124,
    "PINU.RES", 49.2, 41.107386, 24.457438, "2EST", "2", "RP1", 5.543, 910.5235, 140, 26.8729,
    "PINU.RES", 10, 1.362705, 9.957194, "2EST", "1", "RP1", 4.9047, 1038.2486, 110, 5.8220,
    "TILI.AME", 22.9, 30.542583, 21.389394, "1", "2", "FE2", 6.6141, 1090.6744, 50, 19.0886,
    "TILI.AME", 48.5, 36.172062, 22.313661, "1", "2", "FE2", 6.6141, 1090.6744, 50, 26.1109,
    "TILI.AME", 12.4, 17.566588, 16.465381, "1", "0", "MJ1", 5.5221, 843.7713, 120, 12.4540
  )
  out <- dplyr::mutate(
    ref,
    hd(DBH, species, BA, QMD, SDOM, MP, VP, TMOY, PTOT, altitude, disturbance = "NONE")
  )
  expect_lt(max(abs(out$height - out$expected)), 0.02)
})

test_that("hd_auger2016 treats grey birch as linear in ln(DBH + 1)", {
  dbh <- c(10, 20, 30)
  out <- hd(DBH = dbh, species = "BETU.POP", BA = 15)$height
  expect_equal(out, eq1(dbh, 3.6064 + 0.0571 * 15), tolerance = 1e-10)

  era <- hd(DBH = dbh, species = "ACER.SAC", BA = 15)$height
  expect_equal(era, eq1(dbh, 1.6134 + 0.0426 * 15, 0.9252), tolerance = 1e-10)
})

test_that("hd_auger2016 requires only the predictors used by the species present", {
  expect_no_error(hd(DBH = 20, species = c("ACER.SAC", "ULMU.AME"), BA = 20))
  expect_error(
    hd_auger2016(DBH = 20, species = "PICE.MAR", BA = 20),
    "QMD.*PICE.MAR"
  )
  expect_error(
    hd_auger2016(DBH = 20, species = "BETU.PAP", BA = 20, QMD = 15, SDOM = "4est", MP = 2, VP = "MS2", PTOT = 1000, disturbance = "none"),
    "altitude.*BETU.PAP"
  )
  expect_error(
    hd_auger2016(DBH = 20, species = "PINU.BAN", BA = 20, QMD = 15, SDOM = "4est", MP = 2),
    "disturbance.*PINU.BAN"
  )

  # Unused predictors are ignored, including NA values
  base <- hd(DBH = 20, species = "ACER.SAC", BA = 20)$height
  expect_identical(
    hd(DBH = 20, species = "ACER.SAC", BA = 20, QMD = NA_real_, SDOM = NA, MP = 3, VP = "RS2", TMOY = NA_real_, PTOT = NA_real_, altitude = NA_real_, disturbance = NA)$height,
    base
  )
})

test_that("hd_auger2016 returns NA for NA inputs used by the species", {
  args <- list(
    DBH = 20, species = "PICE.MAR", BA = 20, QMD = 15, SDOM = "5ouest",
    MP = 2, VP = "RS2", TMOY = 1, PTOT = 1000, disturbance = "aucune"
  )
  expect_false(is.na(do.call(hd, args)$height))
  for (nm in c("DBH", "BA", "QMD", "SDOM", "MP", "VP", "TMOY", "PTOT", "disturbance")) {
    a <- args
    a[[nm]] <- if (is.numeric(a[[nm]])) NA_real_ else NA
    expect_true(is.na(do.call(hd, a)$height), info = nm)
  }
  bp <- list(
    DBH = 20, species = "BETU.PAP", BA = 20, QMD = 15, SDOM = "4est", MP = 2,
    VP = "MS2", PTOT = 1000, altitude = NA_real_, disturbance = "none"
  )
  expect_true(is.na(do.call(hd, bp)$height))

  expect_identical(
    hd(DBH = c(20, 20), species = c("ACER.SAC", NA), BA = 20)$height[2],
    NA_real_
  )
})

test_that("hd_auger2016 normalizes class codes", {
  p <- function(SDOM = "5ouest", VP = "RS2", disturbance = "aucune", MP = 2) {
    hd(
      DBH = 20, species = "PICE.MAR", BA = 20, QMD = 15, SDOM = SDOM, MP = MP,
      VP = VP, TMOY = 1, PTOT = 1000, disturbance = disturbance
    )$height
  }
  ref <- p()
  expect_identical(p(SDOM = "5OUEST"), ref)
  expect_identical(p(SDOM = " 5Ouest "), ref)
  expect_identical(p(VP = "rs2"), ref)
  expect_identical(p(MP = "2"), ref)
  expect_identical(p(SDOM = factor("5ouest")), ref)
  expect_identical(p(disturbance = "none"), ref)
  expect_identical(p(disturbance = "AUCUNE"), ref)
  expect_identical(p(disturbance = "natural"), p(disturbance = "naturelle"))
  expect_identical(p(disturbance = "anthropogenic"), p(disturbance = "Anthropique"))
  expect_false(identical(p(disturbance = "natural"), ref))
  expect_true(is.na(p(SDOM = "")))
})

test_that("hd_auger2016 aborts on malformed class codes", {
  base <- list(DBH = 20, species = "PICE.MAR", BA = 20, QMD = 15)
  call <- function(...) do.call(hd_auger2016, c(base, list(...)))
  expect_error(call(SDOM = "5nord"), "Invalid `SDOM`.*5nord")
  expect_error(call(MP = 10), "Invalid `MP`")
  expect_error(call(VP = "RS"), "Invalid `VP`")
  expect_error(call(disturbance = "fire"), "Invalid `disturbance`")
  expect_error(call(SDOM = TRUE), "`SDOM` must be a character vector")
})

test_that("hd_auger2016 warns once and returns NA for classes without an estimate", {
  # Red oak has no estimate for physical environment 7 (Annexe 2).
  expect_warning(
    out <- hd_auger2016(DBH = 20, species = "QUER.RUB", BA = 20, MP = c(7, 2)),
    class = "CanadaForestAllometry_hd_uncovered_level"
  )
  expect_true(is.na(out$height[1]))
  expect_false(is.na(out$height[2]))

  w <- expect_warning(
    hd_auger2016(DBH = 20, species = "QUER.RUB", BA = 20, MP = rep(7, 12)),
    class = "CanadaForestAllometry_hd_uncovered_level"
  )
  expect_match(conditionMessage(w), "12 rows")
  expect_match(conditionMessage(w), "and 2 more")
  expect_match(conditionMessage(w), "CHR = 7")
})

test_that("hd_auger2016 validates inputs", {
  expect_error(hd_auger2016(DBH = -1, species = "ACER.SAC", BA = 20), "DBH")
  expect_error(hd_auger2016(DBH = "20", species = "ACER.SAC", BA = 20), "DBH")
  expect_error(hd_auger2016(DBH = 20, species = 1, BA = 20), "species")
  expect_error(hd_auger2016(DBH = 20, species = "ACER.SAC", BA = -1), "BA")
  expect_error(hd_auger2016(DBH = 20, species = "ACER.SAC", BA = 20, QMD = 0), "QMD")
  expect_error(hd_auger2016(DBH = 20, species = "ACER.SAC", BA = 20, PTOT = -1), "PTOT")
  expect_error(hd_auger2016(DBH = 20, species = "PICE.ABI", BA = 20), "not covered")
  expect_error(
    hd_auger2016(DBH = c(10, 20, 30), species = c("ACER.SAC", "ULMU.AME"), BA = 20),
    "length"
  )
})

test_that("hd_auger2016 accepts compact species codes and factors", {
  ref <- hd(DBH = 20, species = "ACER.SAC", BA = 20)
  expect_identical(hd(DBH = 20, species = "ACERSAC", BA = 20), ref)
  expect_identical(hd(DBH = 20, species = factor("ACER.SAC"), BA = 20), ref)
})

test_that("hd_auger2016 recycles inside dplyr::mutate()", {
  trees <- tibble::tibble(
    DBH = c(14, 22, 18),
    species = c("PICE.MAR", "ABIE.BAL", "ACER.SAH"),
    QMD = c(15, 15, 20),
    SDOM = c("5ouest", "5ouest", "3ouest"),
    VP = c("RS2", "RS2", "FE3"),
    TMOY = c(1, 1, 3.5)
  )
  out <- dplyr::mutate(
    trees,
    hd(DBH, species, BA = 22, QMD = QMD, SDOM = SDOM, MP = 2, VP = VP, TMOY = TMOY, PTOT = 1000, altitude = 300, disturbance = "none")
  )
  expect_equal(nrow(out), 3L)
  single <- vapply(seq_len(3), function(i) {
    hd(
      trees$DBH[i], trees$species[i], 22, trees$QMD[i], trees$SDOM[i], 2,
      trees$VP[i], trees$TMOY[i], 1000, 300, "none"
    )$height
  }, numeric(1))
  expect_equal(out$height, single)
})

test_that("hd_auger2016 height increases with DBH and basal area", {
  dbh <- seq(10, 60, by = 5)
  h <- hd(DBH = dbh, species = "ACER.SAC", BA = 20)$height
  expect_true(all(diff(h) > 0))
  h_ba <- hd(DBH = 25, species = "ACER.SAC", BA = c(5, 20, 40))$height
  expect_true(all(diff(h_ba) > 0))
})

test_that("hd_auger2016 warns for inputs outside the fitting range", {
  expect_warning(
    hd_auger2016(DBH = 5, species = "ACER.SAC", BA = 20),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_warning(
    hd_auger2016(
      DBH = 20, species = "BETU.PAP", BA = 20, QMD = 15, SDOM = "4est", MP = 2,
      VP = "MS2", PTOT = 1000, altitude = 5000, disturbance = "none"
    ),
    "altitude"
  )
  # Unused out-of-range predictors are not reported
  expect_no_warning(
    hd_auger2016(DBH = 20, species = "ACER.SAC", BA = 20, altitude = 5000, TMOY = 50, PTOT = 1e5)
  )
})

test_that("hd_auger2016 is consistent with the registry", {
  reg <- hd_model_registry()
  row <- reg[reg$model_id == "auger2016", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$engine, "hd_auger2016")
  expect_identical(row$required_predictors[[1]], c("DBH", "BA"))
  expect_setequal(
    row$optional_predictors[[1]],
    c("QMD", "SDOM", "MP", "VP", "TMOY", "PTOT", "altitude", "disturbance")
  )
  pars <- CanadaForestAllometry:::.get_internal_data("parameters_hd_Auger2016")
  expect_equal(nrow(pars), 27L)
})
