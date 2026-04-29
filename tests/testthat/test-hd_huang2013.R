testthat::test_that("hd_huang2013 returns fixed/base height predictions", {
  out <- CanadaForestAllometry::hd_huang2013(
    DBH = c(20, 25),
    species = c("PICE.GLA", "POPU.TRE"),
    subregion = "Province"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height >= 1.3))
})

testthat::test_that("hd_huang2013 matches report Table 6 white spruce base predictions", {
  # Huang et al. (2013), Table 6: white spruce provincial base model [2].
  ref <- tibble::tibble(
    DBH = c(32.5, 11.9, 36.6, 30.0, 22.0, 29.8),
    height_table = c(25.49, 11.82, 26.93, 24.46, 20.17, 24.38)
  )

  pred <- CanadaForestAllometry::hd_huang2013(
    DBH = ref$DBH,
    species = "PICE.GLA",
    subregion = "Province"
  )

  testthat::expect_equal(pred$height, ref$height_table, tolerance = 0.01)
})

testthat::test_that("hd_huang2013 supports NFI species grouping", {
  pice_gla <- CanadaForestAllometry::hd_huang2013(20, "PICE.GLA")$height
  pice_eng <- CanadaForestAllometry::hd_huang2013(20, "PICE.ENG")$height

  testthat::expect_equal(pice_gla, pice_eng, tolerance = 1e-12)

  pinu_con <- CanadaForestAllometry::hd_huang2013(20, "PINU.CON", subregion = "ALP")$height
  pinu_alb <- CanadaForestAllometry::hd_huang2013(20, "PINU.ALB", subregion = "ALP")$height
  testthat::expect_equal(pinu_con, pinu_alb, tolerance = 1e-12)

  abie_bal <- CanadaForestAllometry::hd_huang2013(20, "ABIE.BAL", subregion = "LF")$height
  abie_las <- CanadaForestAllometry::hd_huang2013(20, "ABIE.LAS", subregion = "LF")$height
  testthat::expect_equal(abie_bal, abie_las, tolerance = 1e-12)
})

testthat::test_that("hd_huang2013 selects subregion groups and fallbacks", {
  # Exact grouped subregion labels and member codes should agree.
  x1 <- CanadaForestAllometry::hd_huang2013(20, "PICE.MAR", subregion = "7 to 10")$height
  x2 <- CanadaForestAllometry::hd_huang2013(20, "PICE.MAR", subregion = "UF")$height
  x3 <- CanadaForestAllometry::hd_huang2013(20, "PICE.MAR", subregion = "M")$height
  x4 <- CanadaForestAllometry::hd_huang2013(20, "PICE.MAR", subregion = "MT")$height
  testthat::expect_equal(x1, x2, tolerance = 1e-12)
  testthat::expect_equal(x1, x3, tolerance = 1e-12)
  testthat::expect_equal(x1, x4, tolerance = 1e-12)

  # No white spruce model for Central Mixedwood: use the report's Others group.
  others <- CanadaForestAllometry::hd_huang2013(20, "PICE.GLA", subregion = "Others")$height
  cm <- CanadaForestAllometry::hd_huang2013(20, "PICE.GLA", subregion = "CM")$height
  testthat::expect_equal(others, cm, tolerance = 1e-12)

  # Species with only provincial parameters ignore non-provincial subregions.
  province <- CanadaForestAllometry::hd_huang2013(20, "BETU.PAP", subregion = "Province")$height
  alp <- CanadaForestAllometry::hd_huang2013(20, "BETU.PAP", subregion = "ALP")$height
  others_direct <- CanadaForestAllometry::hd_huang2013(20, "BETU.PAP", subregion = "Others")$height
  testthat::expect_equal(province, alp, tolerance = 1e-12)
  testthat::expect_equal(province, others_direct, tolerance = 1e-12)
})

testthat::test_that("hd_huang2013 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::hd_huang2013(
      DBH = numeric(0),
      species = character(0),
      subregion = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_huang2013(DBH = 0, species = "PICE.GLA"),
    "DBH.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_huang2013(DBH = 20, species = "BOGUS"),
    "Unrecognized species",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_huang2013(DBH = 20, species = "ACER.RUB"),
    "Unsupported species",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_huang2013(DBH = 20, species = "PICE.GLA", subregion = "bogus"),
    "Invalid subregion",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_huang2013(
      DBH = c(20, 21),
      species = c("PICE.GLA", "PICE.GLA", "PICE.GLA")
    ),
    "length 1 or",
    ignore.case = TRUE
  )
})

testthat::test_that("hd_huang2013 catches defensive non-finite and below-breast-height outputs", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::hd_huang2013(DBH = 20, species = "POPU.TRE"),
      .huang2013_hd_eq1 = function(...) Inf,
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::hd_huang2013(DBH = 20, species = "POPU.TRE"),
      .huang2013_hd_eq1 = function(...) 1,
      .package = "CanadaForestAllometry"
    ),
    "below breast height",
    ignore.case = TRUE
  )
})
