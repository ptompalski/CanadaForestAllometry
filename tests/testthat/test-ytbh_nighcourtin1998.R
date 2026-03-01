testthat::test_that("ytbh_nighcourtin1998 predicts years-to-breast-height", {
  out <- CanadaForestAllometry::ytbh_nighcourtin1998(
    si = c(12, 20, 28)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "ytbh")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$ytbh)))
  testthat::expect_true(all(out$ytbh > 0))
})

testthat::test_that("ytbh_nighcourtin1998 matches manual piecewise equation", {
  out <- CanadaForestAllometry::ytbh_nighcourtin1998(
    si = c(25, 30)
  )

  testthat::expect_equal(out$ytbh[[1]], 5.494 - 0.1789 * 25, tolerance = 1e-12)
  testthat::expect_equal(out$ytbh[[2]], 1, tolerance = 1e-12)
})

testthat::test_that("ytbh_nighcourtin1998 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nighcourtin1998(
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_nighcourtin1998(
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nighcourtin1998 validates positive finite predictors", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nighcourtin1998(
      si = 0
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_nighcourtin1998(
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nighcourtin1998 caps high SI values at 1 year", {
  out <- CanadaForestAllometry::ytbh_nighcourtin1998(
    si = c(40, 100)
  )

  testthat::expect_equal(out$ytbh, c(1, 1), tolerance = 1e-12)
})
