# hd_huang2009(): Huang, Meng & Yang (2009), Modern Applied Science 3(4):3-18.

# Source eqs. 9-11 (b = 0) and Table 2 (p. 13), for independent checks.
cr <- function(DBH, a, b2, b3) 1.3 + a * (1 - exp(-b2 * DBH))^b3
t2 <- list(
  m9 = c(b1 = 25.4440, b2 = 0.04734, b3 = 1.0748),
  m10 = c(b1 = 9.1617, b2 = 0.07668, b3 = 1.2266, b4 = 0.6745),
  m11 = c(b1 = 8.9909, b2 = 0.07127, b3 = 1.1617, b4 = 0.6752, b5 = 0.03388)
)

hd <- function(...) suppressMessages(suppressWarnings(hd_huang2009(...)))

test_that("hd_huang2009 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PICE.MAR")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd(c(10, NA), c("PICE.MAR", NA), keep_model_id = TRUE)$model_id,
    c("huang2009_m9", NA)
  )
  expect_identical(
    hd_huang2009(numeric(), "PICE.MAR"),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_huang2009(numeric(), "PICE.MAR", keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("variant is inferred from HTOP and BA and reported once", {
  for (v in c("m9", "m10", "m11")) rlang::reset_message_verbosity(paste0("hd_huang2009_", v))
  expect_message(hd_huang2009(20, "PICE.MAR"), "model 9")
  expect_no_message(hd_huang2009(20, "PICE.MAR"))
  expect_message(hd_huang2009(20, "PICE.MAR", HTOP = 15), "model 10")
  expect_message(hd_huang2009(20, "PICE.MAR", HTOP = 15, BA = 20), "model 11")

  id <- function(...) hd(20, "PICE.MAR", ..., keep_model_id = TRUE)$model_id
  expect_identical(id(), "huang2009_m9")
  expect_identical(id(HTOP = 15), "huang2009_m10")
  expect_identical(id(HTOP = 15, BA = 20), "huang2009_m11")
  expect_error(hd(20, "PICE.MAR", BA = 20), "requires")
})

test_that("hd_huang2009 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(8, 15, 22), species = "PICE.MAR",
    HTOP = c(14, 14, 19), BA = c(10, 10, 25)
  )
  res <- suppressMessages(dplyr::mutate(trees, hd_huang2009(DBH, species, HTOP, BA)))
  expect_equal(res$height, hd(trees$DBH, trees$species, trees$HTOP, trees$BA)$height)
  expect_equal(nrow(hd(c(10, 20, 30), "PICE.MAR", 15, 20)), 3L)
  expect_error(hd(1:3, c("PICE.MAR", "PICE.MAR")), "Incompatible")
  expect_error(hd(1:3, "PICE.MAR", c(15, 16)), "Incompatible")
  expect_error(hd(1:3, "PICE.MAR", 15, c(10, 20)), "Incompatible")
})

test_that("hd_huang2009 validates inputs", {
  expect_error(hd(-1, "PICE.MAR"), ">= 0")
  expect_error(hd("10", "PICE.MAR"), "numeric")
  expect_error(hd(Inf, "PICE.MAR"), "finite")
  expect_error(hd(10, 1), "character")
  expect_error(hd(10, "PICE.MAR", HTOP = 0), "HTOP")
  expect_error(hd(10, "PICE.MAR", HTOP = 15, BA = 0), "BA")
  expect_error(hd(10, "PICE.GLA"), "not covered")
  expect_error(hd(10, "PICE.MAR", keep_model_id = NA), "keep_model_id")
})

test_that("NA in any input returns NA height", {
  expect_true(all(is.na(hd(c(NA, 20), c("PICE.MAR", NA))$height)))
  out <- hd(
    DBH = c(NA, 20, 20, 20), species = "PICE.MAR",
    HTOP = c(15, NA, 15, 15), BA = c(20, 20, NA, 20)
  )
  expect_identical(is.na(out$height), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(is.na(hd(c(20, 20), "PICE.MAR", c(NA, 15))$height), c(TRUE, FALSE))
})

test_that("predictions use the Table 2 coefficients", {
  DBH <- c(1.3, 10, 25, 39.4)
  p <- t2$m9
  expect_equal(hd(DBH, "PICE.MAR")$height, cr(DBH, p[["b1"]], p[["b2"]], p[["b3"]]))
  p <- t2$m10
  expect_equal(
    hd(DBH, "PICE.MAR", 18)$height,
    cr(DBH, p[["b1"]] + p[["b4"]] * 18, p[["b2"]], p[["b3"]])
  )
  p <- t2$m11
  expect_equal(
    hd(DBH, "PICE.MAR", 18, 30)$height,
    cr(DBH, p[["b1"]] + p[["b4"]] * 18 + p[["b5"]] * 30, p[["b2"]], p[["b3"]])
  )
  expect_equal(hd(20, "PICEMAR"), hd(20, "PICE.MAR"))
})

# --- TIER 1: fidelity (Table 3 worked example, p. 14) ------------------------

test_that("reproduces Table 3 fixed-only predictions (H_fix)", {
  # Example plot TopH and BA are not published; back-solved from H_fix:
  # HTOP = 14.5 m (model 10), BA = 7.75 m2/ha (model 11). See
  # tmp/generate_hd_huang2009_comparison_values.R.
  DBH <- c(7.9, 10.7, 20.3)
  expect_equal(hd(DBH, "PICE.MAR")$height, c(8.577, 10.738, 16.455), tolerance = 5e-4)
  expect_lte(
    max(abs(hd(DBH, "PICE.MAR", 14.5)$height - c(8.497, 10.597, 15.467))),
    0.002
  )
  expect_lte(
    max(abs(hd(DBH, "PICE.MAR", 14.5, 7.75)$height - c(8.454, 10.479, 15.244))),
    0.002
  )
})

# --- behaviour / plausibility -------------------------------------------------

test_that("height starts at 1.3 m, increases with DBH, HTOP and BA", {
  d <- c(0, 0.5, seq(1, 60, by = 1))
  for (h in list(hd(d, "PICE.MAR"), hd(d, "PICE.MAR", 15), hd(d, "PICE.MAR", 15, 20))) {
    expect_equal(h$height[1], 1.3)
    expect_true(all(diff(h$height) > 0))
    expect_true(all(h$height < 35))
  }
  expect_true(all(diff(hd(20, "PICE.MAR", c(6, 12, 18, 24))$height) > 0))
  expect_true(all(diff(hd(20, "PICE.MAR", 15, c(1, 20, 50))$height) > 0))
})

# --- warnings ----------------------------------------------------------------

test_that("inputs outside the fitting-data ranges are computed with one warning", {
  # Table 1 (p. 12), fitting data: DBH 1.3-39.4; TopH 5.5-23.55; BA 0.643-54.608.
  expect_no_warning(suppressMessages(hd_huang2009(c(2, 20, 39), "PICE.MAR", 15, 20)))
  cases <- list(
    list(DBH = 1, HTOP = NULL, BA = NULL),
    list(DBH = 40, HTOP = NULL, BA = NULL),
    list(DBH = 20, HTOP = 5, BA = NULL),
    list(DBH = 20, HTOP = 24, BA = NULL),
    list(DBH = 20, HTOP = 15, BA = 0.5),
    list(DBH = 20, HTOP = 15, BA = 55)
  )
  for (a in cases) {
    expect_warning(
      out <- suppressMessages(hd_huang2009(a$DBH, "PICE.MAR", a$HTOP, a$BA)),
      class = "CanadaForestAllometry_hd_out_of_range"
    )
    expect_true(is.finite(out$height))
  }
})

test_that("hd_huang2009 is registered consistently", {
  ids <- paste0("huang2009_", c("m9", "m10", "m11"))
  preds <- list(c("DBH"), c("DBH", "HTOP"), c("DBH", "HTOP", "BA"))
  for (i in seq_along(ids)) {
    info <- get_hd_model_info(ids[i])
    expect_identical(info$engine, "hd_huang2009")
    expect_identical(info$predictors$arg, preds[[i]])
    expect_true(all(info$predictors$required))
    expect_identical(info$species, "PICE.MAR")
  }
  found <- find_hd_models(jurisdiction = "AB", species = "PICE.MAR", available = c("DBH", "HTOP"))
  expect_true(all(c("huang2009_m9", "huang2009_m10") %in% found$model_id[found$usable]))
  expect_false("huang2009_m11" %in% found$model_id[found$usable])
})
