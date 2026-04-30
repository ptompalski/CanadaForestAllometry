testthat::test_that("hd_sharmaparton2007 returns fixed height predictions", {
  out <- CanadaForestAllometry::hd_sharmaparton2007(
    DBH = c(20, 25),
    species = c("PICE.MAR", "POPU.TRE"),
    SHT = 20,
    TPH = 2500,
    BA = 25
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height >= 1.3))
})

testthat::test_that("hd_sharmaparton2007 matches manual equation evaluation", {
  DBH <- c(20, 25)
  SHT <- c(20, 18)
  TPH <- c(2500, 1800)
  BA <- c(25, 22)
  species <- c("PICE.MAR", "POPU.TRE")

  # Table 6, Method 2 (mixed model fixed part).
  u <- c(3.3952, 2.3161)
  d <- c(0.6390, 0.7456)
  b <- c(0.0424, 0.0543)
  w <- c(0.1380, 0.1738)
  g <- c(1.2948, 1.0967)

  expected <- 1.3 + u * (SHT^d) * (1 - exp(-b * ((TPH / BA)^w) * DBH))^g

  out <- CanadaForestAllometry::hd_sharmaparton2007(
    DBH = DBH,
    species = species,
    SHT = SHT,
    TPH = TPH,
    BA = BA
  )

  testthat::expect_equal(out$height, expected, tolerance = 1e-12)
})

testthat::test_that("hd_sharmaparton2007 supports nls_fixed parameter set", {
  mixed <- CanadaForestAllometry::hd_sharmaparton2007(
    DBH = 20,
    species = "PICE.MAR",
    SHT = 20,
    TPH = 2500,
    BA = 25,
    fit = "mixed_fixed"
  )$height

  nls <- CanadaForestAllometry::hd_sharmaparton2007(
    DBH = 20,
    species = "PICE.MAR",
    SHT = 20,
    TPH = 2500,
    BA = 25,
    fit = "nls_fixed"
  )$height

  testthat::expect_false(isTRUE(all.equal(mixed, nls)))
})

testthat::test_that("hd_sharmaparton2007 is sensitive to stand covariates", {
  low_sht <- CanadaForestAllometry::hd_sharmaparton2007(
    DBH = 20,
    species = "PINU.BAN",
    SHT = 15,
    TPH = 2500,
    BA = 25
  )$height

  high_sht <- CanadaForestAllometry::hd_sharmaparton2007(
    DBH = 20,
    species = "PINU.BAN",
    SHT = 25,
    TPH = 2500,
    BA = 25
  )$height

  testthat::expect_gt(high_sht, low_sht)
})

testthat::test_that("hd_sharmaparton2007 validates inputs", {
  testthat::expect_error(
    CanadaForestAllometry::hd_sharmaparton2007(
      DBH = numeric(0),
      species = character(0),
      SHT = numeric(0),
      TPH = numeric(0),
      BA = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_sharmaparton2007(
      DBH = 20,
      species = "PICE.MAR",
      SHT = 0,
      TPH = 2500,
      BA = 25
    ),
    "SHT.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_sharmaparton2007(
      DBH = 20,
      species = "ACER.RUB",
      SHT = 20,
      TPH = 2500,
      BA = 25
    ),
    "Unsupported species",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::hd_sharmaparton2007(
      DBH = c(20, 21),
      species = c("PICE.MAR", "PICE.MAR", "PICE.MAR"),
      SHT = 20,
      TPH = 2500,
      BA = 25
    ),
    "length 1 or",
    ignore.case = TRUE
  )
})

testthat::test_that("hd_sharmaparton2007 catches defensive non-finite and below-breast-height outputs", {
  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::hd_sharmaparton2007(
        DBH = 20,
        species = "PICE.MAR",
        SHT = 20,
        TPH = 2500,
        BA = 25
      ),
      .sharmaparton2007_hd_height = function(...) Inf,
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry::hd_sharmaparton2007(
        DBH = 20,
        species = "PICE.MAR",
        SHT = 20,
        TPH = 2500,
        BA = 25
      ),
      .sharmaparton2007_hd_height = function(...) 1,
      .package = "CanadaForestAllometry"
    ),
    "below breast height",
    ignore.case = TRUE
  )
})
