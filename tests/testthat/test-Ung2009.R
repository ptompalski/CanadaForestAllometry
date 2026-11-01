testthat::test_that("Ung2009 returns expected columns for variant='both'", {
  out <- CanadaForestAllometry::Ung2009(
    species = "ABIE.BAL",
    age = c(10, 20, 30),
    GDD = 1500,
    MAP = 800,
    variant = "both"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_identical(names(out), c("age", "H", "BA", "V", "V2"))
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(out$H > 0))
  testthat::expect_true(all(out$BA > 0))
  testthat::expect_true(all(out$V > 0))
  testthat::expect_true(all(out$V2 > 0))
})

testthat::test_that("Ung2009 supports Nat1 and Nat2 output modes", {
  o1 <- CanadaForestAllometry::Ung2009(
    species = "ABIE.BAL",
    age = 15:17,
    GDD = 1200,
    MAP = 700,
    variant = "Nat1"
  )
  o2 <- CanadaForestAllometry::Ung2009(
    species = "ABIE.BAL",
    age = 15:17,
    GDD = 1200,
    MAP = 700,
    variant = "Nat2"
  )

  testthat::expect_identical(names(o1), c("age", "H", "BA", "V"))
  testthat::expect_identical(names(o2), c("age", "V2"))
})

testthat::test_that("Ung2009 validates inputs", {
  testthat::expect_error(
    CanadaForestAllometry::Ung2009(
      species = "ABIE.BAL",
      age = c(0, 5),
      GDD = 1500,
      MAP = 800
    ),
    "Age must be greater than 0",
    fixed = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::Ung2009(
      species = "ABIE.BAL",
      age = 1:5,
      GDD = "bad",
      MAP = 800
    ),
    "GDD and MAP must be numeric",
    fixed = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::Ung2009(
      species = 1,
      age = 1:5,
      GDD = 1500,
      MAP = 800
    ),
    "Species must be a character string",
    fixed = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::Ung2009(
      species = "NOT.A.SPP",
      age = 1:5,
      GDD = 1500,
      MAP = 800
    ),
    "Invalid species",
    fixed = TRUE
  )
})
