testthat::test_that("vol_ung2013 dispatches to the correct model", {
  DBH <- c(20, 30)
  sp <- c("PICE.GLA", "ABIE.BAL")
  ju <- "QC"

  # height omitted -> DBH-only
  out1 <- vol_ung2013(DBH = DBH, species = sp, jurisdiction = ju)
  testthat::expect_s3_class(out1, "tbl_df")
  testthat::expect_named(out1, c("vol_merchantable", "vol_total"))
  testthat::expect_equal(nrow(out1), 2L)
  testthat::expect_true(all(out1$vol_total >= 0))
  testthat::expect_true(all(out1$vol_merchantable >= 0))
  testthat::expect_true(all(out1$vol_merchantable <= out1$vol_total))

  # height provided -> DBH+H
  out2 <- vol_ung2013(
    DBH = DBH,
    height = c(18, 24),
    species = sp,
    jurisdiction = ju
  )
  testthat::expect_s3_class(out2, "tbl_df")
  testthat::expect_named(out2, c("vol_merchantable", "vol_total"))
  testthat::expect_equal(nrow(out2), 2L)
  testthat::expect_true(all(out2$vol_total >= 0))
  testthat::expect_true(all(out2$vol_merchantable >= 0))
  testthat::expect_true(all(out2$vol_merchantable <= out2$vol_total))

  # all-NA height -> treat as missing -> DBH-only
  out3 <- vol_ung2013(
    DBH = DBH,
    height = c(NA_real_, NA_real_),
    species = sp,
    jurisdiction = ju
  )
  testthat::expect_equal(out3, out1)
})

testthat::test_that("vol_ung2013 validates inputs", {
  testthat::expect_error(
    vol_ung2013(DBH = "20", species = "PICE.GLA", jurisdiction = "ON"),
    "`DBH` must be numeric"
  )

  testthat::expect_error(
    vol_ung2013(
      DBH = 20,
      height = "18",
      species = "PICE.GLA",
      jurisdiction = "NL"
    ),
    "`height` must be numeric"
  )
})

testthat::test_that("vol_ung2013 recycles vectors consistently", {
  out <- vol_ung2013(
    DBH = c(20, 30, 40),
    height = 25,
    species = "PICE.GLA",
    jurisdiction = c("NS", "PE", "NB")
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(out$vol_merchantable <= out$vol_total))
})


.local_data <- function(obj_name) {
  e <- new.env(parent = emptyenv())
  utils::data(list = obj_name, package = "CanadaForestAllometry", envir = e)
  if (!exists(obj_name, envir = e, inherits = FALSE)) {
    testthat::skip(paste0(
      "Dataset `",
      obj_name,
      "` not available via utils::data()."
    ))
  }
  get(obj_name, envir = e, inherits = FALSE)
}

testthat::test_that("Ung2013 volumes are reasonable across all species (from parameter tables)", {
  params_dbh <- CanadaForestAllometry:::get_params_tbl(
    "parameters_NationalTaperModelsDBH"
  )
  params_dbhht <- CanadaForestAllometry:::get_params_tbl(
    "parameters_NationalTaperModelsDBHHT"
  )

  testthat::expect_true("Species" %in% names(params_dbh))
  testthat::expect_true("Species" %in% names(params_dbhht))

  sp_dbh <- sort(unique(params_dbh$Species))
  sp_dbhht <- sort(unique(params_dbhht$Species))

  testthat::expect_equal(length(sp_dbh), 34L)
  testthat::expect_equal(length(sp_dbhht), 34L)
  testthat::expect_equal(sp_dbh, sp_dbhht)

  sp_list <- sp_dbh
  ju <- "ON"

  # ---------- DBH-only checks ----------
  grid_dbh <- tidyr::expand_grid(
    Species = sp_list,
    DBH = c(10, 20, 40, 60)
  )

  out_dbh <- suppressWarnings(
    vol_ung2013(
      DBH = grid_dbh$DBH,
      species = grid_dbh$Species,
      jurisdiction = ju
    )
  )

  testthat::expect_equal(nrow(out_dbh), nrow(grid_dbh))

  # Invariants
  testthat::expect_true(all(out_dbh$vol_total >= 0 | is.na(out_dbh$vol_total)))
  testthat::expect_true(all(
    out_dbh$vol_merchantable >= 0 | is.na(out_dbh$vol_merchantable)
  ))
  testthat::expect_true(all(
    out_dbh$vol_merchantable <= out_dbh$vol_total | is.na(out_dbh$vol_total)
  ))

  # Plausibility guardrail (wide; catches unit/integration/join bugs)
  testthat::expect_true(all(out_dbh$vol_total <= 30 | is.na(out_dbh$vol_total)))

  # Monotonicity in DBH per species
  mono_dbh <- dplyr::bind_cols(grid_dbh, out_dbh) |>
    dplyr::arrange(.data$Species, .data$DBH) |>
    dplyr::group_by(.data$Species) |>
    dplyr::summarise(
      ok = all(diff(.data$vol_total) >= -1e-8, na.rm = TRUE),
      .groups = "drop"
    )
  testthat::expect_true(all(mono_dbh$ok))

  # ---------- DBH + height checks ----------
  grid_dbh_ht <- tidyr::expand_grid(
    Species = sp_list,
    DBH = c(10, 20, 40, 60),
    height = c(12, 18, 28)
  )

  out_dbh_ht <- suppressWarnings(
    vol_ung2013(
      DBH = grid_dbh_ht$DBH,
      height = grid_dbh_ht$height,
      species = grid_dbh_ht$Species,
      jurisdiction = ju
    )
  )

  testthat::expect_equal(nrow(out_dbh_ht), nrow(grid_dbh_ht))

  # Invariants
  testthat::expect_true(all(
    out_dbh_ht$vol_total >= 0 | is.na(out_dbh_ht$vol_total)
  ))
  testthat::expect_true(all(
    out_dbh_ht$vol_merchantable >= 0 | is.na(out_dbh_ht$vol_merchantable)
  ))
  testthat::expect_true(all(
    out_dbh_ht$vol_merchantable <= out_dbh_ht$vol_total |
      is.na(out_dbh_ht$vol_total)
  ))

  testthat::expect_true(all(
    out_dbh_ht$vol_total <= 30 | is.na(out_dbh_ht$vol_total)
  ))

  # Height effect sanity check (per species+DBH)
  mono_ht <- dplyr::bind_cols(grid_dbh_ht, out_dbh_ht) |>
    dplyr::arrange(.data$Species, .data$DBH, .data$height) |>
    dplyr::group_by(.data$Species, .data$DBH) |>
    dplyr::summarise(
      ok = all(diff(.data$vol_total) >= -1e-8, na.rm = TRUE),
      .groups = "drop"
    )
  testthat::expect_true(all(mono_ht$ok))
})

testthat::test_that("vol_ung2013 returns NA rows for non-finite DBH/height and zero rows for degenerate height model cases", {
  out_dbh_na <- vol_ung2013(
    DBH = c(20, NA_real_),
    species = c("PICE.GLA", "PICE.GLA"),
    jurisdiction = c("ON", "ON")
  )
  testthat::expect_true(is.finite(out_dbh_na$vol_total[[1]]))
  testthat::expect_true(is.na(out_dbh_na$vol_total[[2]]))
  testthat::expect_true(is.na(out_dbh_na$vol_merchantable[[2]]))

  out_dbh_zero <- vol_ung2013(
    DBH = 0,
    species = "PICE.GLA",
    jurisdiction = "ON"
  )
  testthat::expect_identical(out_dbh_zero$vol_total[[1]], 0)
  testthat::expect_identical(out_dbh_zero$vol_merchantable[[1]], 0)

  out_ht_na <- vol_ung2013(
    DBH = c(20, 20),
    height = c(18, NA_real_),
    species = c("PICE.GLA", "PICE.GLA"),
    jurisdiction = c("ON", "ON")
  )
  testthat::expect_true(is.finite(out_ht_na$vol_total[[1]]))
  testthat::expect_true(is.na(out_ht_na$vol_total[[2]]))
  testthat::expect_true(is.na(out_ht_na$vol_merchantable[[2]]))

  out_ht_zero <- vol_ung2013(
    DBH = 20,
    height = 0.5,
    species = "PICE.GLA",
    jurisdiction = "ON"
  )
  testthat::expect_identical(out_ht_zero$vol_total[[1]], 0)
  testthat::expect_identical(out_ht_zero$vol_merchantable[[1]], 0)
})

testthat::test_that(".ctae_vol_from_segments covers low-information and no-topdbh branches", {
  v0 <- CanadaForestAllometry:::.ctae_vol_from_segments(
    H = c(0.3, 0.4),
    D2C = c(0, 0),
    stp = 0.1,
    topdbh = 10,
    mindbh = 5,
    dbh_i = 20
  )
  testthat::expect_identical(as.numeric(v0[["vol_total"]]), 0)
  testthat::expect_identical(as.numeric(v0[["vol_merch"]]), 0)

  v_all <- CanadaForestAllometry:::.ctae_vol_from_segments(
    H = c(0.3, 0.4, 0.5),
    D2C = c(100, 90, 80),
    stp = 0.1,
    topdbh = 0,
    mindbh = 0,
    dbh_i = 20
  )
  testthat::expect_true(is.finite(v_all[["vol_total"]]))
  testthat::expect_equal(v_all[["vol_merch"]], v_all[["vol_total"]], tolerance = 1e-12)

  v_mindbh <- CanadaForestAllometry:::.ctae_vol_from_segments(
    H = c(0.3, 0.4, 0.5),
    D2C = c(100, 90, 80),
    stp = 0.1,
    topdbh = 10,
    mindbh = 30,
    dbh_i = 20
  )
  testthat::expect_true(is.finite(v_mindbh[["vol_total"]]))
  testthat::expect_identical(as.numeric(v_mindbh[["vol_merch"]]), 0)
})
