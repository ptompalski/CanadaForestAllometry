# hd_fortin2009(): Fortin et al. (2009), Mémoire de recherche forestière no 153.

hd <- function(...) suppressWarnings(hd_fortin2009(...))

# Reference tree of Figure 5 (p. 13): balsam fir, DBH 20, BA 20, QMD = DBH
# (ISS 0), Centre ("4d"), mesic, MAT 0, no disturbance.
fig5 <- function(...) {
  args <- list(
    DBH = 20, species = "ABIE.BAL", BA = 20, QMD = 20, ecoregion = "4d",
    drainage = "mesic", MAT = 0, disturbance = "none"
  )
  args[names(list(...))] <- list(...)
  do.call(hd, args)$height
}

test_that("hd_fortin2009 returns a well-formed tibble", {
  out <- hd(c(15, 25), "PICE.MAR", 20, 18, "5a", "mesic", 1, "none")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_true(all(out$height > 1.3))

  empty <- hd_fortin2009(
    numeric(), character(), numeric(), numeric(), character(), character(),
    numeric(), character()
  )
  expect_identical(empty, tibble::tibble(height = numeric()))
})

test_that("hd_fortin2009 reproduces the CFSForestTools unit tests", {
  # aDeadTreeOnly (Fortin2009HeightPredictorTest.java, commit fa86f80c): BOJ,
  # DBH 25, BA 0, region 3a, mesic, MAT 2, no disturbance. The harness passes
  # SSI = DBH - QMD = 25 (unsquared), i.e. ISS = 25 -> QMD = 20 here.
  expect_equal(
    hd(25, "BETU.ALL", 0, 20, "3a", "mesic", 2, "none")$height,
    15.129429409429527,
    tolerance = 1e-8
  )

  # refPredictions.xml minus the plot BLUPs (population prediction), region
  # 1a, mesic, no disturbance; QMD_equiv gives the harness's unsquared SSI.
  # One tree per species; full set (58 trees, 11 species) from
  # tmp/generate_hd_fortin2009_comparison_values.R.
  ref <- tibble::tribble(
    ~species, ~DBH, ~BA, ~QMD_equiv, ~MAT, ~expected,
    "ACER.RUB", 18.7, 20.6971658310, 17.2700194666, 6.4929, 16.752898857040201,
    "FRAX.NIG", 26.8, 20.6971658310, 23.6149027761, 6.4929, 20.155142551418301,
    "BETU.PAP", 19.7, 20.6971658310, 17.9550517698, 6.4929, 16.484533436187899,
    "POPU.TRE", 36.4, 29.4872439010, 32.8200770859, 6.5209, 24.849102530356401,
    "POPU.GRA", 33.1, 29.4872439010, 30.0152231732, 6.5209, 24.324541384253699,
    "FAGU.GRA", 38.4, 46.9557360570, 34.7976536528, 6.3506, 25.203950939318801,
    "QUER.RUB", 51.8, 46.9557360570, 46.6641554536, 6.3506, 25.302654771705200,
    "TSUG.CAN", 37.2, 46.9557360570, 33.7682512905, 6.3506, 20.694324147067402,
    "TILI.AME", 32.4, 36.5010458500, 28.8627543553, 6.8338, 24.952448793025201,
    "ACER.SAH", 28, 36.5010458500, 25.1518239607, 6.8338, 21.576264406057099,
    "THUJ.OCC", 19.1, 27.5684180130, 18.3289127066, 7.2190, 12.818629153421400
  )
  out <- hd(
    ref$DBH, ref$species, ref$BA, ref$QMD_equiv, "1a", "mesic", ref$MAT, "none"
  )$height
  expect_equal(out, ref$expected, tolerance = 1e-8)
})

test_that("hd_fortin2009 reproduces Figure 5 (figure-read values)", {
  # Values read from the figure (+/- 0.05 m); tolerance 0.1 m absolute.
  tol <- function(x) 0.1 / x
  expect_equal(fig5(), 13.45, tolerance = tol(13.45))
  expect_equal(
    vapply(c(-4, -2, 2, 4), function(t) fig5(MAT = t), numeric(1)),
    c(12.4, 12.9, 14.0, 14.5),
    tolerance = tol(13)
  )
  expect_equal(
    vapply(c(10, 30, 40), function(b) fig5(BA = b), numeric(1)),
    c(12.6, 14.3, 15.2),
    tolerance = tol(14)
  )
  expect_equal(
    vapply(c("3a", "6h", "5h"), function(e) fig5(ecoregion = e), numeric(1)),
    c(14.3, 13.45, 12.45),
    tolerance = tol(13),
    ignore_attr = TRUE
  )
  expect_equal(
    vapply(c("xeric", "subhydric", "hydric"), function(d) fig5(drainage = d), numeric(1)),
    c(13.0, 13.35, 13.35),
    tolerance = tol(13),
    ignore_attr = TRUE
  )
  expect_equal(
    vapply(c("anthropogenic", "natural"), function(f) fig5(disturbance = f), numeric(1)),
    c(13.55, 13.5),
    tolerance = tol(13),
    ignore_attr = TRUE
  )
  # Panel (e) shows ~12.7 m for DBH - QMD = +/-10 cm; the coefficients (and
  # CFSForestTools) give ~13.16 m. Documented discrepancy; follow the coefficients.
  expect_equal(fig5(QMD = 10), fig5(QMD = 30))
  expect_equal(fig5(QMD = 10), 13.16, tolerance = 0.01 / 13.16)
})

test_that("hd_fortin2009 agrees with the printed Tableaux 4-5 coefficients", {
  # Independent recomputation of eq. (6) with the printed values (SAB, Tableau
  # 4; Tableau 5): BA 25, QMD 15, DBH 22, Ouest, xeric, MAT 1.5, natural.
  L <- log(22 + 1)
  ISS <- (22 - 15)^2
  A <- -2.6584 + 0.02748 * 25 + 0.08828 * 1.5 - 0.1117 + 0.6008 + 0.02191 - 0.00235 * ISS
  B <- 1.8726 + 0.000551 * 25 + 0.000483 * ISS
  expect_equal(
    hd(22, "ABIE.BAL", 25, 15, "5c", "xeric", 1.5, "natural")$height,
    1.3 + A * L + B * L^2,
    tolerance = 0.01 / 15
  )
})

test_that("hd_fortin2009 maps ecological regions to subdivisions", {
  eco <- CanadaForestAllometry:::.get_internal_data("hd_fortin2009_ecoregions")
  expect_equal(nrow(eco), 48L)
  h <- hd(20, "ABIE.BAL", 20, 20, eco$ecoregion, "mesic", 0, "none")$height
  by_sub <- split(h, eco$ecological_subdivision)
  expect_true(all(vapply(by_sub, function(x) all(x == x[1]), logical(1))))
  expect_equal(unname(by_sub$centre[1]), fig5())
  expect_equal(fig5(ecoregion = "6M"), fig5(ecoregion = "5h"))
  expect_equal(fig5(ecoregion = " 7c "), fig5(ecoregion = "6h"))
})

test_that("hd_fortin2009 normalizes class names", {
  expect_equal(fig5(drainage = "X\u00e9rique"), fig5(drainage = "xeric"))
  expect_equal(fig5(drainage = "MESIQUE"), fig5())
  expect_equal(fig5(drainage = "subhydrique"), fig5(drainage = "subhydric"))
  expect_equal(fig5(drainage = "Hydrique"), fig5(drainage = "hydric"))
  expect_equal(fig5(disturbance = "anthropique"), fig5(disturbance = "anthropogenic"))
  expect_equal(fig5(disturbance = "Naturelle"), fig5(disturbance = "natural"))
  expect_equal(fig5(disturbance = "aucune"), fig5())
  expect_equal(fig5(drainage = factor("mesic")), fig5())
})

test_that("hd_fortin2009 aborts on invalid class values", {
  expect_error(fig5(ecoregion = "8a"), "ecoregion")
  expect_error(fig5(ecoregion = "ouest"), "ecoregion")
  expect_error(fig5(drainage = "wet"), "drainage")
  expect_error(fig5(disturbance = "fire"), "disturbance")
  expect_error(fig5(drainage = 2), "character")
})

test_that("hd_fortin2009 returns NA for NA inputs", {
  out <- hd(
    DBH = c(NA, 20, 20, 20, 20, 20, 20, 20),
    species = c("ABIE.BAL", NA, rep("ABIE.BAL", 6)),
    BA = c(20, 20, NA, 20, 20, 20, 20, 20),
    QMD = c(20, 20, 20, NA, 20, 20, 20, 20),
    ecoregion = c("4d", "4d", "4d", "4d", NA, "", "4d", "4d"),
    drainage = c(rep("mesic", 6), NA, "mesic"),
    MAT = 0,
    disturbance = c(rep("none", 7), NA)
  )$height
  expect_true(all(is.na(out)))
  expect_true(is.na(fig5(drainage = NA)))
  expect_false(is.na(fig5(ecoregion = "4d")))
})

test_that("hd_fortin2009 validates inputs", {
  args <- list(20, "ABIE.BAL", 20, 20, "4d", "mesic", 0, "none")
  call_with <- function(i, value) {
    a <- args
    a[i] <- list(value)
    do.call(hd_fortin2009, a)
  }
  expect_error(call_with(1, -1), "DBH")
  expect_error(call_with(1, "20"), "DBH")
  expect_error(call_with(2, 1), "species")
  expect_error(call_with(2, "PINU.RES"), "not covered")
  expect_error(call_with(3, -1), "BA")
  expect_error(call_with(4, 0), "QMD")
  expect_error(call_with(7, "0"), "MAT")
  expect_error(
    hd_fortin2009(c(10, 20, 30), c("ABIE.BAL", "PICE.MAR"), 20, 20, "4d", "mesic", 0, "none"),
    "length"
  )
  expect_error(
    hd_fortin2009(20, "ABIE.BAL", 20, 20, "4d", "mesic", 0, "none", keep_model_id = "yes"),
    "keep_model_id"
  )
})

test_that("hd_fortin2009 accepts compact species codes and factors", {
  ref <- fig5()
  expect_identical(fig5(species = "ABIEBAL"), ref)
  expect_identical(fig5(species = factor("ABIE.BAL")), ref)
})

test_that("hd_fortin2009 recycles inside dplyr::mutate()", {
  trees <- tibble::tibble(
    DBH = c(14, 22, 30),
    species = c("PICE.MAR", "ABIE.BAL", "BETU.ALL"),
    ecoregion = c("5a", "5a", "3b"),
    drainage = c("subhydrique", "mesique", "mesique"),
    MAT = c(0.5, 0.5, 3)
  )
  out <- dplyr::mutate(
    trees,
    hd(DBH, species, 22, 18, ecoregion, drainage, MAT, "aucune")
  )
  expect_equal(nrow(out), 3L)
  single <- vapply(seq_len(3), function(i) {
    hd(
      trees$DBH[i], trees$species[i], 22, 18, trees$ecoregion[i],
      trees$drainage[i], trees$MAT[i], "aucune"
    )$height
  }, numeric(1))
  expect_equal(out$height, single)
})

test_that("hd_fortin2009 height increases with DBH, basal area and temperature", {
  h <- fig5(DBH = seq(10, 50, by = 5), QMD = seq(10, 50, by = 5))
  expect_true(all(diff(h) > 0))
  expect_true(all(diff(fig5(BA = c(5, 20, 40))) > 0))
  expect_true(all(diff(fig5(MAT = c(-3, 0, 3))) > 0))
})

test_that("hd_fortin2009 warns for inputs outside the fitting range", {
  args <- list(
    DBH = 20, species = "ABIE.BAL", BA = 20, QMD = 20, ecoregion = "4d",
    drainage = "mesic", MAT = 0, disturbance = "none"
  )
  expect_no_warning(do.call(hd_fortin2009, args))
  expect_warning(
    do.call(hd_fortin2009, utils::modifyList(args, list(DBH = 5))),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_warning(do.call(hd_fortin2009, utils::modifyList(args, list(BA = 70))), "BA")
  expect_warning(do.call(hd_fortin2009, utils::modifyList(args, list(MAT = 8))), "MAT")
})

test_that("hd_fortin2009 is consistent with the registry", {
  reg <- hd_model_registry()
  row <- reg[reg$model_id == "fortin2009", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$engine, "hd_fortin2009")
  expect_setequal(
    row$required_predictors[[1]],
    c("DBH", "BA", "QMD", "ecoregion", "drainage", "MAT", "disturbance")
  )
  expect_length(row$optional_predictors[[1]], 0L)
  expect_setequal(get_hd_model_info("fortin2009")$species, c(
    "ABIE.BAL", "ACER.RUB", "ACER.SAH", "BETU.ALL", "BETU.PAP", "FAGU.GRA",
    "FRAX.NIG", "LARI.LAR", "OSTR.VIR", "PICE.GLA", "PICE.MAR", "PICE.RUB",
    "PINU.BAN", "PINU.STR", "POPU.GRA", "POPU.TRE", "QUER.RUB", "THUJ.OCC",
    "TILI.AME", "TSUG.CAN"
  ))
})

test_that("keep_model_id adds model_id", {
  out <- hd_fortin2009(
    c(20, 20), c("ABIE.BAL", NA), 20, 20, "4d", "mesic", 0, "none",
    keep_model_id = TRUE
  )
  expect_identical(out$model_id, c("fortin2009", NA))
  expect_identical(
    hd_fortin2009(
      numeric(), "ABIE.BAL", numeric(), numeric(), "4d", "mesic", numeric(), "none",
      keep_model_id = TRUE
    ),
    tibble::tibble(height = numeric(), model_id = character())
  )
})
