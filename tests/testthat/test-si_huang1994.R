testthat::test_that("si_huang1994 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_huang1994(
    age = c(20, 35, 50),
    si = c(12, 16, 20),
    species = c("PICE.GLA", "PINU.CON", "POPU.TRE"),
    subregion = c("All", "6, 9, 11, 14", "9, 11")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_huang1994 predicts si from height and returns single-column tibble", {
  h_in <- CanadaForestAllometry::si_huang1994(
    age = c(20, 35, 50),
    si = c(12, 16, 20),
    species = c("PICE.GLA", "PINU.CON", "POPU.TRE"),
    subregion = c("All", "6, 9, 11, 14", "9, 11")
  ) |>
    dplyr::pull(height)

  out <- CanadaForestAllometry::si_huang1994(
    age = c(20, 35, 50),
    height = h_in,
    species = c("PICE.GLA", "PINU.CON", "POPU.TRE"),
    subregion = c("All", "6, 9, 11, 14", "9, 11")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_huang1994 matches Huang1994 table reference points", {
  refs <- dplyr::tibble(
    species = c(
      "PSEU.MEN", "PICE.GLA", "PINU.CON", "POPU.TRE",
      "POPU.BAL", "POPU.BAL", "POPU.TRE", "PICE.MAR",
      "ABIE.BAL", "PINU.CON", "ABIE.BAL", "PINU.BAN",
      "PINU.BAN", "PSEU.MEN", "PICE.MAR", "PICE.GLA"
    ),
    subregion = rep("All", 16),
    age = c(25, 100, 100, 100, 25, 100, 25, 25, 25, 25, 100, 25, 100, 100, 100, 25),
    si = c(10, 18, 18, 18, 10, 18, 10, 10, 10, 10, 18, 10, 18, 18, 18, 10),
    height_table = c(5.3, 28.9, 25.4, 26.3, 5.0, 26.5, 5.0, 5.6, 5.0, 5.5, 28.1, 5.6, 24.7, 26.3, 26.5, 4.8)
  )

  pred <- CanadaForestAllometry::si_huang1994(
    age = refs$age,
    si = refs$si,
    species = refs$species,
    subregion = refs$subregion
  )

  abs_error <- abs(pred$height - refs$height_table)
  testthat::expect_true(all(is.finite(abs_error)))
  testthat::expect_lte(max(abs_error), 0.06)
  testthat::expect_lte(mean(abs_error), 0.04)
})

testthat::test_that("si_huang1994 supports provincial subregion aliases", {
  x1 <- CanadaForestAllometry::si_huang1994(
    age = 35,
    si = 16,
    species = "PICE.GLA",
    subregion = "All"
  )$height[[1]]

  x2 <- CanadaForestAllometry::si_huang1994(
    age = 35,
    si = 16,
    species = "PICE.GLA",
    subregion = "provincial"
  )$height[[1]]

  x3 <- CanadaForestAllometry::si_huang1994(
    age = 35,
    si = 16,
    species = "PICE.GLA",
    subregion = "province"
  )$height[[1]]

  testthat::expect_equal(x1, x2, tolerance = 1e-12)
  testthat::expect_equal(x1, x3, tolerance = 1e-12)
})

testthat::test_that("si_huang1994 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_huang1994(
      age = numeric(0),
      si = numeric(0),
      species = character(0),
      subregion = character(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_huang1994(
      age = 20,
      height = 10,
      si = 12,
      species = "PICE.GLA"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_huang1994(
      age = 20,
      si = 12,
      species = "PICE.GLA",
      subregion = "bogus"
    ),
    "No Huang1994 parameters found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_huang1994(
      age = c(20, 30),
      si = c(12, 14, 16),
      species = c("PICE.GLA", "PICE.GLA"),
      subregion = "All"
    ),
    "length 1 or",
    ignore.case = TRUE
  )
})
