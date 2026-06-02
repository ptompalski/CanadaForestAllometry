testthat::test_that("ytbh_carmean1989 predicts years-to-breast-height", {
  out <- CanadaForestAllometry::ytbh_carmean1989(
    si = c(18, 20, 22),
    species = c("ACER.SAH", "CHAM.THY", "TSUG.CAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "ytbh")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$ytbh)))
  testthat::expect_true(all(out$ytbh > 0))
})

testthat::test_that("ytbh_carmean1989 returns fixed values for non-cedar species", {
  out <- CanadaForestAllometry::ytbh_carmean1989(
    si = c(12, 18, 24),
    species = c("ACER.SAH", "BETU.ALL", "TSUG.CAN")
  )

  testthat::expect_equal(out$ytbh, c(4, 4, 6), tolerance = 1e-12)
})

testthat::test_that("ytbh_carmean1989 interpolates Atlantic white-cedar values in feet", {
  out_points <- CanadaForestAllometry::ytbh_carmean1989(
    si = c(20, 50, 80) / 3.28084,
    species = "CHAM.THY"
  )
  testthat::expect_equal(out_points$ytbh, c(11, 8, 5), tolerance = 1e-12)

  out_mid <- CanadaForestAllometry::ytbh_carmean1989(
    si = 45 / 3.28084,
    species = "CHAM.THY"
  )
  testthat::expect_equal(out_mid$ytbh[[1]], 8.5, tolerance = 1e-12)
})

testthat::test_that("ytbh_carmean1989 clamps Atlantic white-cedar outside table range", {
  out <- CanadaForestAllometry::ytbh_carmean1989(
    si = c(10, 100) / 3.28084,
    species = "CHAM.THY"
  )

  testthat::expect_equal(out$ytbh, c(11, 5), tolerance = 1e-12)
})

testthat::test_that("ytbh_carmean1989 supports scalar recycling", {
  out <- CanadaForestAllometry::ytbh_carmean1989(
    si = c(12, 15, 18),
    species = "ACER.SAH"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "ytbh")
})

testthat::test_that("ytbh_carmean1989 validates inputs", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = "10",
      species = "ACER.SAH"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = c(10, 12),
      species = c("ACER.SAH", "ACER.SAH", "ACER.SAH")
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = 12,
      species = "XXXX.YYY"
    ),
    "No Carmean1989 YTBH parameters found|Unrecognized species codes",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = 0,
      species = "ACER.SAH"
    ),
    "si.*> 0|si.*values > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_carmean1989 catches missing years-to-bh values for non-cedar species", {
  mock_pars <- dplyr::tibble(
    Species = c("ACER.SAH", "CHAM.THY"),
    years_to_bh = c(NA_real_, NA_real_)
  )

  testthat::local_mocked_bindings(
    .get_internal_data = function(name) {
      testthat::expect_equal(name, "parameters_Carmean1989")
      mock_pars
    },
    .package = "CanadaForestAllometry"
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = 12,
      species = "ACER.SAH"
    ),
    "No Carmean1989 YTBH value available",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_carmean1989 reports missing parameter rows for valid species", {
  mock_pars <- dplyr::tibble(
    Species = "CHAM.THY",
    years_to_bh = NA_real_
  )

  testthat::local_mocked_bindings(
    .get_internal_data = function(name) {
      testthat::expect_equal(name, "parameters_Carmean1989")
      mock_pars
    },
    .package = "CanadaForestAllometry"
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = 12,
      species = "ACER.SAH"
    ),
    "No Carmean1989 YTBH parameters found",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_carmean1989 catches non-finite outputs", {
  mock_inf <- dplyr::tibble(
    Species = "ACER.SAH",
    years_to_bh = Inf
  )

  testthat::local_mocked_bindings(
    .get_internal_data = function(name) {
      testthat::expect_equal(name, "parameters_Carmean1989")
      mock_inf
    },
    .package = "CanadaForestAllometry"
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = 12,
      species = "ACER.SAH"
    ),
    "Non-finite years-to-breast-height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_carmean1989 catches negative outputs", {
  mock_neg <- dplyr::tibble(
    Species = "ACER.SAH",
    years_to_bh = -1
  )

  testthat::local_mocked_bindings(
    .get_internal_data = function(name) {
      testthat::expect_equal(name, "parameters_Carmean1989")
      mock_neg
    },
    .package = "CanadaForestAllometry"
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_carmean1989(
      si = 12,
      species = "ACER.SAH"
    ),
    "Negative years-to-breast-height prediction",
    ignore.case = TRUE
  )
})
