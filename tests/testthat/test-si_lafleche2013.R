testthat::test_that("si_lafleche2013 predicts height and IQS for CR, WE, and LIN curves", {
  out <- CanadaForestAllometry::si_lafleche2013(
    age = c(50, 50, 50),
    species = c("ABIE.BAL", "PICE.GLA", "THUJ.OCC"),
    ecological_region = c("3c", "2a", "4f"),
    ecological_type = c("MJ12", "MJ11", "RS12"),
    curve_set = "potential"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("height", "si"))
  testthat::expect_equal(out$height, out$si, tolerance = 1e-10)

  # Table 9 checks:
  # SAB 3c MJ12 -> 17.52 (CR)
  # EPB 2a MJ11 -> 19.64 (WE)
  # THO 4f RS12 -> 9.44 (LIN)
  testthat::expect_equal(out$si, c(17.52, 19.64, 9.44), tolerance = 0.02)
})

testthat::test_that("si_lafleche2013 matches additional Table 9 IQSstation values", {
  out <- CanadaForestAllometry::si_lafleche2013(
    age = c(50, 50, 50, 50, 50, 50),
    species = c("BETU.PAP", "PICE.MAR", "POPU.TRE", "PINU.BAN", "ABIE.BAL", "THUJ.OCC"),
    ecological_region = c("4b", "4c", "5d", "5b", "5e", "3b"),
    ecological_type = c("MJ22", "RE21", "MS22", "RE22", "MS22", "RS10"),
    ecological_subregion = c(NA, NA, NA, NA, "5eT", NA),
    curve_set = "potential"
  )

  # Table 9 checks:
  # BOP 4b MJ22 -> 16.90
  # EPN 4c RE21 -> 15.91
  # PET 5d MS22 -> 19.58
  # PIG 5b RE22 -> 16.11
  # SAB 5e 5eT MS22 -> 14.37
  # THO 3b RS10 -> 9.14
  testthat::expect_equal(
    out$si,
    c(16.90, 15.91, 19.58, 16.11, 14.37, 9.14),
    tolerance = 0.03
  )
})

testthat::test_that("si_lafleche2013 matches manual equation evaluation", {
  age <- c(25, 50)

  expected_cr <- 1 + 31.95904 * (1 - exp(-0.02013 * age))^1.45114
  out_cr <- CanadaForestAllometry::si_lafleche2013(
    age = age,
    species = "ABIE.BAL",
    ecological_region = "3c",
    ecological_type = "MJ12",
    curve_set = "potential"
  )
  testthat::expect_equal(out_cr$height, expected_cr, tolerance = 1e-10)

  expected_we <- 1 + 27.61069 * (1 - exp(-0.00694 * age^1.30025))
  out_we <- CanadaForestAllometry::si_lafleche2013(
    age = age,
    species = "PICE.GLA",
    ecological_region = "2a",
    ecological_type = "MJ11",
    curve_set = "potential"
  )
  testthat::expect_equal(out_we$height, expected_we, tolerance = 1e-10)

  expected_lin <- 1 + 0.16874 * age
  out_lin <- CanadaForestAllometry::si_lafleche2013(
    age = age,
    species = "THUJ.OCC",
    ecological_region = "4f",
    ecological_type = "RS12",
    curve_set = "potential"
  )
  testthat::expect_equal(out_lin$height, expected_lin, tolerance = 1e-10)
})

testthat::test_that("si_lafleche2013 supports metadata output and observed curves", {
  out <- CanadaForestAllometry::si_lafleche2013(
    age = 50,
    species = "BETU.PAP",
    ecological_region = "4d",
    ecological_type = "MS12",
    curve_set = "observed",
    include_metadata = TRUE
  )

  testthat::expect_true(all(c(
    "height",
    "si",
    "curve_set",
    "species_qc",
    "Species",
    "ecological_region_description",
    "ecological_type_description_fr",
    "ecological_type_description_en",
    "equation_used"
  ) %in% names(out)))
  testthat::expect_identical(out$curve_set[[1]], "IQSobserved")
  testthat::expect_identical(out$species_qc[[1]], "BOP")
  testthat::expect_identical(out$Species[[1]], "BETU.PAP")
  testthat::expect_identical(out$equation_used[[1]], "WE")
  testthat::expect_false(is.na(out$ecological_region_description[[1]]))
  testthat::expect_false(is.na(out$ecological_type_description_en[[1]]))
})

testthat::test_that("si_lafleche2013 handles missing matches and unsupported LOGIST3 clearly", {
  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 50,
      species = "PICE.GLA",
      ecological_region = "5e",
      ecological_type = "MS22",
      curve_set = "potential"
    ),
    "No Lafleche2013 IQS curve found",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 50,
      species = "PICE.GLA",
      ecological_region = "5b",
      ecological_type = "MS22",
      curve_set = "potential"
    ),
    "LOGIST3",
    ignore.case = FALSE
  )
})

testthat::test_that("si_lafleche2013 validates inputs", {
  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = numeric(0),
      species = "SAB",
      ecological_region = "3c",
      ecological_type = "MJ12"
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 0,
      species = "SAB",
      ecological_region = "3c",
      ecological_type = "MJ12"
    ),
    "age.*> 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 50,
      species = "EPB",
      ecological_region = "3c",
      ecological_type = "MJ12"
    ),
    "must use NFI species codes",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 50,
      species = "ABIE.BAL",
      ecological_region = "3c",
      ecological_type = "MJ12",
      curve_set = "bad"
    ),
    "curve_set",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 50,
      species = "ABIE.BAL",
      ecological_region = "3c",
      ecological_type = "MJ12",
      base_age = 0
    ),
    "base_age",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::si_lafleche2013(
      age = 50,
      species = "ABIE.BAL",
      ecological_region = "3c",
      ecological_type = "MJ12",
      include_metadata = c(TRUE, FALSE)
    ),
    "include_metadata",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lafleche2013 exposes internal validation branches", {
  testthat::expect_error(
    CanadaForestAllometry:::.lafleche2013_prepare(
      age = 50,
      species = factor("ABIE.BAL"),
      ecological_region = "3c",
      ecological_type = "MJ12",
      ecological_subregion = NULL,
      curve_set = "IQSstation"
    ),
    "species",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry:::.lafleche2013_prepare(
      age = 50,
      species = "ABIE.BAL",
      ecological_region = factor("3c"),
      ecological_type = "MJ12",
      ecological_subregion = NULL,
      curve_set = "IQSstation"
    ),
    "ecological_region",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry:::.lafleche2013_prepare(
      age = 50,
      species = "ABIE.BAL",
      ecological_region = "3c",
      ecological_type = factor("MJ12"),
      ecological_subregion = NULL,
      curve_set = "IQSstation"
    ),
    "ecological_type",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry:::.lafleche2013_prepare(
      age = 50,
      species = "ABIE.BAL",
      ecological_region = "3c",
      ecological_type = "MJ12",
      ecological_subregion = 1,
      curve_set = "IQSstation"
    ),
    "ecological_subregion",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lafleche2013 handles supplied ecological_subregion and duplicate-key guard", {
  out <- CanadaForestAllometry:::.lafleche2013_prepare(
    age = c(50, 50),
    species = c("ABIE.BAL", "PICE.GLA"),
    ecological_region = c("5e", "5f"),
    ecological_type = c("MS22", "MS12"),
    ecological_subregion = c("5eT", "5fS"),
    curve_set = "IQSstation"
  )

  testthat::expect_equal(out$ecological_subregion, c("5eT", "5fS"))

  dup_params <- dplyr::bind_rows(
    CanadaForestAllometry:::parameters_QC_IQS2013,
    CanadaForestAllometry:::parameters_QC_IQS2013[1, ]
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      CanadaForestAllometry:::.lafleche2013_prepare(
        age = 50,
        species = "BETU.PAP",
        ecological_region = "2c",
        ecological_type = "FE22",
        ecological_subregion = NULL,
        curve_set = "IQSobserved"
      ),
      parameters_QC_IQS2013 = dup_params,
      .package = "CanadaForestAllometry"
    ),
    "duplicate ecological keys",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lafleche2013 covers output guards via mocked height engine", {
  base_args <- list(
    age = 50,
    species = "ABIE.BAL",
    ecological_region = "3c",
    ecological_type = "MJ12"
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      do.call(CanadaForestAllometry::si_lafleche2013, base_args),
      .lafleche2013_height = function(...) c(Inf),
      .package = "CanadaForestAllometry"
    ),
    "Non-finite height prediction",
    ignore.case = TRUE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      do.call(CanadaForestAllometry::si_lafleche2013, base_args),
      .lafleche2013_height = function(...) c(-1),
      .package = "CanadaForestAllometry"
    ),
    "Negative height prediction",
    ignore.case = TRUE
  )

  counter <- 0L
  testthat::expect_error(
    testthat::with_mocked_bindings(
      do.call(CanadaForestAllometry::si_lafleche2013, base_args),
      .lafleche2013_height = function(...) {
        counter <<- counter + 1L
        if (counter == 1L) {
          10
        } else {
          Inf
        }
      },
      .package = "CanadaForestAllometry"
    ),
    "Non-finite IQS prediction",
    ignore.case = TRUE
  )

  counter <- 0L
  testthat::expect_error(
    testthat::with_mocked_bindings(
      do.call(CanadaForestAllometry::si_lafleche2013, base_args),
      .lafleche2013_height = function(...) {
        counter <<- counter + 1L
        if (counter == 1L) {
          10
        } else {
          -1
        }
      },
      .package = "CanadaForestAllometry"
    ),
    "Negative IQS prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_lafleche2013 helper branches are covered", {
  testthat::expect_error(
    CanadaForestAllometry:::.lafleche2013_normalize_curve_set(1),
    "curve_set",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry:::.lafleche2013_height(
      age = 10,
      equation_used = "BOGUS",
      b1 = 1,
      b2 = 1,
      b3 = 1
    ),
    "Unsupported Lafleche2013 equation form",
    ignore.case = TRUE
  )
})
