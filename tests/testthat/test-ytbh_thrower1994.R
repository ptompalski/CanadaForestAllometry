testthat::test_that("ytbh_thrower1994 predicts years-to-breast-height", {
  out <- CanadaForestAllometry::ytbh_thrower1994(
    si = c(12, 16, 20, 18),
    species = c("PINU.CON", "THUJ.PLI", "ABIE.LAS", "TSUG.HET")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "ytbh")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_true(all(is.finite(out$ytbh)))
  testthat::expect_true(all(out$ytbh > 0))
})

testthat::test_that("ytbh_thrower1994 matches manual equations by species", {
  si <- c(14, 18, 20, 16)
  sp <- c("PINU.CON", "THUJ.PLI", "ABIE.LAS", "TSUG.HET")

  expected <- c(
    5.6 + 42.64 / si[[1]],
    13.25 - si[[2]] / 6.096,
    42.25 - 10.66 * log(si[[3]]),
    9.43 - 0.043 * (si[[4]] / 0.3048)
  )

  out <- CanadaForestAllometry::ytbh_thrower1994(
    si = si,
    species = sp
  )

  testthat::expect_equal(out$ytbh, expected, tolerance = 1e-10)
})

testthat::test_that("ytbh_thrower1994 supports scalar recycling", {
  out <- CanadaForestAllometry::ytbh_thrower1994(
    si = c(10, 15, 20),
    species = "PINU.CON"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "ytbh")
})

testthat::test_that("ytbh_thrower1994 validates inputs", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_thrower1994(
      si = numeric(0),
      species = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_thrower1994(
      si = "10",
      species = "PINU.CON"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_thrower1994(
      si = c(10, 12),
      species = c("PINU.CON", "PINU.CON", "PINU.CON")
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_thrower1994(
      si = 12,
      species = "XXXX.YYY"
    ),
    "Unrecognized species codes|No Thrower1994 YTBH parameters found",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_thrower1994 catches negative model output", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_thrower1994(
      si = 100,
      species = "THUJ.PLI"
    ),
    "Negative years-to-breast-height prediction",
    ignore.case = TRUE
  )
})
