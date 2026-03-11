test_that("translate_species_code handles core NFI, CANFI, and jurisdiction lookups", {
  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "scientificname"),
    "Abies balsamea"
  )

  expect_equal(
    translate_species_code("Picea mariana", from = "scientificname"),
    "PICE.MAR"
  )

  expect_equal(
    translate_species_code("black spruce", from = "englishname"),
    "PICE.MAR"
  )

  expect_equal(
    translate_species_code("épinette noire", from = "frenchname"),
    "PICE.MAR"
  )

  expect_equal(
    translate_species_code("302", from = "canfi"),
    "ABIE.BAL"
  )

  expect_equal(
    translate_species_code("BF", from = "jurisdiction", jurisdiction = "ON"),
    "ABIE.BAL"
  )

  expect_equal(
    translate_species_code("SW", from = "jurisdiction", jurisdiction = "BC"),
    "PICE.GLA"
  )
})


test_that("translate_species_code handles auto detection for real package data", {
  msgs <- testthat::capture_messages(
    out_canfi <- translate_species_code("302", from = "auto")
  )
  expect_true(any(grepl("Auto-detected input type: canfi", msgs, fixed = TRUE)))
  expect_equal(out_canfi, "ABIE.BAL")

  msgs <- testthat::capture_messages(
    out_nfi <- translate_species_code("PICE.GLA", from = "auto", to = "englishname")
  )
  expect_true(any(grepl("Auto-detected input type: nfi", msgs, fixed = TRUE)))
  expect_equal(out_nfi, "white spruce")

  msgs <- testthat::capture_messages(
    out_jur <- translate_species_code(c("BF", "PJ", "SB"), from = "auto")
  )
  expect_true(any(grepl("Auto-detected input type: jurisdiction (on)", msgs, fixed = TRUE)))
  expect_equal(out_jur, c("ABIE.BAL", "PINU.BAN", "PICE.MAR"))

  expect_equal(
    translate_species_code(
      c("BF", "PJ", "SB"),
      from = "auto",
      jurisdiction = "ON",
      verbose = FALSE
    ),
    c("ABIE.BAL", "PINU.BAN", "PICE.MAR")
  )
})


test_that("translate_species_code accepts friendly aliases in `to`", {
  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "englishname"),
    "balsam fir"
  )

  expect_equal(
    translate_species_code("PICE.MAR", from = "nfi", to = "frenchname"),
    "Ã©pinette noire"
  )

  expect_equal(
    translate_species_code("black spruce", from = "englishname", to = "nfi"),
    "PICE.MAR"
  )

  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "sci"),
    "Abies balsamea"
  )

  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "eng"),
    "balsam fir"
  )

  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "canfi"),
    "302"
  )

  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "jurisdiction", jurisdiction = "ON"),
    "BF"
  )
})


test_that("translate_species_code handles ambiguity explicitly for CANFI codes", {
  expect_error(
    translate_species_code("104", from = "canfi"),
    class = "ctae_ambiguous_species_code"
  )

  expect_equal(
    translate_species_code("104", from = "canfi", multiple = "first"),
    "PICE.ENG"
  )

  expect_true(
    all(c("PICE.ENG", "PICE.ENG.GLA") %in%
      translate_species_code("104", from = "canfi", multiple = "all")[[1]])
  )
})


test_that("translate_species_code validates inputs and handles unmatched values", {
  expect_error(
    translate_species_code("BF", from = "jurisdiction"),
    "must be provided",
    fixed = TRUE
  )

  expect_error(
    translate_species_code(302, from = "canfi"),
    "`code`",
    fixed = TRUE
  )

  expect_error(
    translate_species_code("302", from = "canfi", verbose = c(TRUE, FALSE)),
    "`verbose`",
    fixed = TRUE
  )

  expect_error(
    translate_species_code("ABIE.BAL", from = "nfi", to = "not_a_column")
  )

  expect_error(
    translate_species_code("302", from = "canfi", jurisdiction = "ON")
  )

  expect_error(
    translate_species_code("black spruce", from = "englishname", jurisdiction = "ON")
  )

  expect_error(
    translate_species_code("ABIE.BAL", from = "nfi", to = "jurisdiction"),
    "Target jurisdiction is required.",
    fixed = TRUE
  )

  expect_error(
    translate_species_code("ABIE.BAL", from = "auto", jurisdiction = "ON"),
    "auto-detected"
  )

  expect_error(
    translate_species_code("302", from = "auto", jurisdiction = "ON"),
    "auto-detected"
  )

  expect_error(
    translate_species_code("ZZ", from = "auto"),
    class = "ctae_unknown_species_code"
  )

  expect_error(
    translate_species_code("ZZ", from = "jurisdiction", jurisdiction = "ON"),
    "No match found for `ZZ` for jurisdiction `on`.",
    fixed = TRUE
  )

  expect_equal(
    translate_species_code(
      c("ZZ", "BF"),
      from = "jurisdiction",
      jurisdiction = "ON",
      unmatched = "NA"
    ),
    c(NA_character_, "ABIE.BAL")
  )
})


test_that("translate_species_code normalizes name lookup inputs", {
  expect_equal(
    translate_species_code("  picea   mariana  ", from = "scientificname"),
    "PICE.MAR"
  )

  expect_equal(
    translate_species_code("EPINETTE NOIRE", from = "frenchname"),
    "PICE.MAR"
  )
})


test_that("translate_species_code auto detection errors for mixed or non-unique inputs", {
  expect_error(
    translate_species_code(c("BF", "302"), from = "auto"),
    class = "ctae_ambiguous_species_source"
  )

  expect_error(
    translate_species_code(c("BF", "RM"), from = "auto"),
    class = "ctae_ambiguous_species_source"
  )

  expect_error(
    translate_species_code(c("RM", "PJ"), from = "auto"),
    class = "ctae_ambiguous_species_source"
  )
})
