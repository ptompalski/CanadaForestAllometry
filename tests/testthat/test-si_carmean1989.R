testthat::test_that("si_carmean1989 predicts height from si and returns single-column tibble", {
  out <- CanadaForestAllometry::si_carmean1989(
    age = c(30, 50, 70),
    si = c(12, 16, 20),
    species = c("ACER.SAH", "BETU.ALL", "TSUG.CAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
  testthat::expect_true(all(out$height > 0))
})

testthat::test_that("si_carmean1989 predicts si from height and returns single-column tibble", {
  out <- CanadaForestAllometry::si_carmean1989(
    age = c(30, 50, 70),
    height = c(8, 15, 22),
    species = c("ACER.SAH", "BETU.ALL", "TSUG.CAN")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 0))
})

testthat::test_that("si_carmean1989 matches manual equation evaluation", {
  age <- 60
  si_m <- 18
  si_ft <- si_m * 3.28084

  h_ft_expected <- 6.1308 *
    (si_ft^0.6904) *
    (1 - exp(-0.0195 * age))^(10.1563 * (si_ft^-0.533))
  h_m_expected <- h_ft_expected / 3.28084

  h_out <- CanadaForestAllometry::si_carmean1989(
    age = age,
    si = si_m,
    species = "ACER.SAH"
  )
  testthat::expect_equal(h_out$height[[1]], h_m_expected, tolerance = 1e-10)

  height_m <- 18
  height_ft <- height_m * 3.28084

  si_ft_expected <- 0.2172 *
    (height_ft^1.1309) *
    (1 - exp(-0.0105 * age))^(-1.912 * (height_ft^-0.1327))
  si_m_expected <- si_ft_expected / 3.28084

  si_out <- CanadaForestAllometry::si_carmean1989(
    age = age,
    height = height_m,
    species = "TSUG.CAN"
  )
  testthat::expect_equal(si_out$si[[1]], si_m_expected, tolerance = 1e-10)
})

testthat::test_that("si_carmean1989 supports scalar recycling", {
  out <- CanadaForestAllometry::si_carmean1989(
    age = c(30, 40, 50),
    si = 18,
    species = "ACER.SAH"
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
})

testthat::test_that("Carmean1989 sugar maple parameters use ACER.SAH", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get("parameters_Carmean1989", envir = ns, inherits = FALSE) |>
    dplyr::as_tibble()

  testthat::expect_true("ACER.SAH" %in% pars$Species)
  testthat::expect_false("ACER.SAC" %in% pars$Species)

  out <- CanadaForestAllometry::si_carmean1989(
    age = 60,
    si = 18,
    species = "ACER.SAH"
  )
  testthat::expect_true(is.finite(out$height[[1]]))

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 60,
      si = 18,
      species = "ACER.SAC"
    ),
    "No Carmean1989 parameters found",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1989 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = c(30, 40),
      si = c(15, 15, 15),
      species = "ACER.SAH"
    ),
    "length 1 or",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = c(30, -1),
      height = c(10, 12),
      species = c("ACER.SAH", "ACER.SAH")
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 30,
      height = 10,
      species = "PINU.BAN"
    ),
    "No Carmean1989 parameters found|Unrecognized species codes",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 30,
      height = 10,
      si = 12,
      species = "ACER.SAH"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 30,
      species = "ACER.SAH"
    ),
    "exactly one of `height` or `si`",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1989 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "age.*length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = "30",
      si = 12,
      species = "ACER.SAH"
    ),
    "age.*numeric",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 30,
      height = "10",
      species = "ACER.SAH"
    ),
    "height.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1989 catches non-finite height predictions", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 20,
      si = 1e308,
      species = "ACER.SAH"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_carmean1989 catches non-finite site-index predictions", {
  testthat::expect_error(
    CanadaForestAllometry::si_carmean1989(
      age = 20,
      height = 1e308,
      species = "ACER.SAH"
    ),
    "Non-finite site index prediction",
    ignore.case = TRUE
  )
})

testthat::test_that(".carmean1989_prepare validates zero-length inputs", {
  testthat::expect_error(
    CanadaForestAllometry:::.carmean1989_prepare(
      age = numeric(0),
      x = numeric(0),
      species = character(0),
      x_name = "si"
    ),
    "age.*length > 0",
    ignore.case = TRUE
  )
})
