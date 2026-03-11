test_that("translate_species_code handles core NFI, CANFI, and jurisdiction lookups", {
  expect_equal(
    translate_species_code("ABIE.BAL", from = "nfi", to = "ScientificName"),
    "Abies balsamea"
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
  expect_equal(
    translate_species_code("302", from = "auto"),
    "ABIE.BAL"
  )

  expect_equal(
    translate_species_code("PICE.GLA", from = "auto", to = "CommonNameEnglish"),
    "white spruce"
  )

  expect_equal(
    translate_species_code(c("BF", "PJ", "SB"), from = "auto"),
    c("ABIE.BAL", "PINU.BAN", "PICE.MAR")
  )

  expect_equal(
    translate_species_code(c("BF", "PJ", "SB"), from = "auto", jurisdiction = "ON"),
    c("ABIE.BAL", "PINU.BAN", "PICE.MAR")
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
    translate_species_code("ABIE.BAL", from = "nfi", to = "not_a_column")
  )

  expect_error(
    translate_species_code("302", from = "canfi", jurisdiction = "ON")
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
