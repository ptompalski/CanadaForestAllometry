testthat::test_that("hd_rijal2012 returns fixed height predictions", {
  out <- CanadaForestAllometry::hd_rijal2012(
    DBH = c(20, 25),
    species = c("ABIE.BAL", "BETU.PAP"),
    CSI = c(15, 17),
    CCF = c(120, 90),
    BAL = c(8, 12)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height >= 1.37))
})

testthat::test_that("hd_rijal2012 matches manual equation evaluation", {
  DBH <- c(20, 25)
  CSI <- c(15, 17)
  CCF <- c(120, 90)
  BAL <- c(8, 12)
  species <- c("ABIE.BAL", "BETU.PAP")

  # Table 5 GNLS parameters.
  c0 <- c(10.7493, 13.0163)
  c1 <- c(0.0722, 0.0504)
  c2 <- c(1.3652, 1.1434)
  c3 <- c(0.7706, 0.7727)
  c4 <- c(0.0076, -0.0070)
  c5 <- c(-0.0046, -0.0034)

  expected <- 1.37 + (c0 + c3 * CSI) *
    (1 - exp(-c1 * DBH))^(c2 + c4 * log(CCF + 1) + c5 * BAL)

  out <- CanadaForestAllometry::hd_rijal2012(
    DBH = DBH,
    species = species,
    CSI = CSI,
    CCF = CCF,
    BAL = BAL
  )

  testthat::expect_equal(out$height, expected, tolerance = 1e-12)
})

testthat::test_that("hd_rijal2012 responds to site and competition covariates", {
  low_csi <- CanadaForestAllometry::hd_rijal2012(
    DBH = 20,
    species = "PICE.GLA",
    CSI = 10,
    CCF = 100,
    BAL = 10
  )$height

  high_csi <- CanadaForestAllometry::hd_rijal2012(
    DBH = 20,
    species = "PICE.GLA",
    CSI = 20,
    CCF = 100,
    BAL = 10
  )$height

  testthat::expect_gt(high_csi, low_csi)
})

testthat::test_that("hd_rijal2012 validates inputs", {
  testthat::expect_error(
    CanadaForestAllometry::hd_rijal2012(
      DBH = numeric(0),
      species = character(0),
      CSI = numeric(0),
      CCF = numeric(0),
      BAL = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_rijal2012(
      DBH = 20,
      species = "ABIE.BAL",
      CSI = 0,
      CCF = 100,
      BAL = 10
    ),
    "CSI.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_rijal2012(
      DBH = 20,
      species = "ABIE.BAL",
      CSI = 15,
      CCF = -1,
      BAL = 10
    ),
    "CCF.*>= 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_rijal2012(
      DBH = 20,
      species = "PINU.BAN",
      CSI = 15,
      CCF = 100,
      BAL = 10
    ),
    "Unsupported species",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_rijal2012(
      DBH = c(20, 21),
      species = c("ABIE.BAL", "ABIE.BAL", "ABIE.BAL"),
      CSI = 15,
      CCF = 100,
      BAL = 10
    ),
    "length 1 or",
    ignore.case = TRUE
  )
})

testthat::test_that("hd_rijal2012 catches defensive non-finite and below-breast-height outputs", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::hd_rijal2012(
        DBH = 20,
        species = "ABIE.BAL",
        CSI = 15,
        CCF = 100,
        BAL = 10
      ),
      .rijal2012_hd_height = function(...) Inf,
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::hd_rijal2012(
        DBH = 20,
        species = "ABIE.BAL",
        CSI = 15,
        CCF = 100,
        BAL = 10
      ),
      .rijal2012_hd_height = function(...) 1,
      .package = "CanadaForestAllometry"
    ),
    "below breast height",
    ignore.case = TRUE
  )
})
