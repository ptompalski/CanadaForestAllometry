testthat::test_that("ytbh_nigh2000 predicts years-to-breast-height", {
  out <- CanadaForestAllometry::ytbh_nigh2000(
    si = c(10, 15, 20)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "ytbh")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$ytbh)))
  testthat::expect_true(all(out$ytbh > 0))
})

testthat::test_that("ytbh_nigh2000 matches manual equation evaluation", {
  si <- 15
  y_expected <- 18.18 - 0.5526 * si

  out <- CanadaForestAllometry::ytbh_nigh2000(
    si = si
  )

  testthat::expect_equal(out$ytbh[[1]], y_expected, tolerance = 1e-12)
})

testthat::test_that("ytbh_nigh2000 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2000(
      si = c(10, 15),
      c(1, 2, 3)
    ),
    "unused argument",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nigh2000 supports scalar recycling", {
  out <- CanadaForestAllometry::ytbh_nigh2000(
    si = c(10, 15, 20)
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "ytbh")
  testthat::expect_true(all(is.finite(out$ytbh)))
})

testthat::test_that("ytbh_nigh2000 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2000(
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2000(
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nigh2000 validates positive finite predictors", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2000(
      si = 0
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2000(
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )
})
