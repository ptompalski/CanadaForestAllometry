# hd_sharmazhang2004(): Sharma & Zhang (2004), Scand. J. For. Res. 19:442-451.
# No source benchmark (no worked example or prediction table): checks are
# against an independent implementation of eqs. (2)/(5) with the Table 3/4
# coefficients, plus plausibility (Tier 2) sanity checks.

eq2 <- function(DBH, BA, TPH, alpha, beta, delta, phi) {
  1.3 + alpha * BA^delta * (1 - exp(-beta * TPH^phi * DBH))
}
eq5 <- function(DBH, alpha, beta, gamma) {
  1.3 + alpha * (1 - exp(-beta * DBH))^gamma
}

# Table 3 (jack pine) and Table 4 (black spruce), p. 445.
t_eq2 <- tibble::tribble(
  ~species, ~alpha, ~beta, ~delta, ~phi,
  "PINU.BAN", 5.2067, 2.4280, 0.4177, -0.3292,
  "PICE.MAR", 14.6726, 0.1175, 0.2176, -0.1146
)
t_eq5 <- tibble::tribble(
  ~species, ~alpha, ~beta, ~gamma,
  "PINU.BAN", 30.7546, 0.0370, 1.1342,
  "PICE.MAR", 28.9282, 0.0431, 1.2191
)

hd <- function(...) suppressWarnings(hd_sharmazhang2004(...))

test_that("hd_sharmazhang2004 reproduces eqs. (2) and (5) with Tables 3-4", {
  grid <- expand.grid(DBH = c(2.5, 10, 25, 40), BA = c(5, 25, 50), TPH = c(300, 2500, 15000))
  for (i in seq_len(nrow(t_eq2))) {
    cf <- t_eq2[i, ]
    expect_equal(
      hd(grid$DBH, cf$species, BA = grid$BA, TPH = grid$TPH)$height,
      eq2(grid$DBH, grid$BA, grid$TPH, cf$alpha, cf$beta, cf$delta, cf$phi),
      tolerance = 1e-12
    )
    cf5 <- t_eq5[i, ]
    expect_equal(
      hd(grid$DBH, cf5$species, variant = "eq5")$height,
      eq5(grid$DBH, cf5$alpha, cf5$beta, cf5$gamma),
      tolerance = 1e-12
    )
  }
})

test_that("hd_sharmazhang2004 curves are plausible (Tier 2 sanity checks)", {
  d <- c(0, seq(1, 45, by = 1))
  for (sp in c("PINU.BAN", "PICE.MAR")) {
    for (v in c("eq2", "eq5")) {
      h <- hd(d, sp, variant = v, BA = if (v == "eq2") 25, TPH = if (v == "eq2") 1500)$height
      expect_equal(h[1], 1.3)
      expect_true(all(diff(h) > 0))
      expect_true(max(h) < 35)
    }
  }
  # Height increases with BA (delta > 0); Fig. 3: jack pine flattens beyond
  # ~14 cm while black spruce keeps rising to 40 cm.
  ba <- hd(20, "PICE.MAR", BA = c(12.5, 25, 37.5, 50), TPH = 1500)$height
  expect_true(all(diff(ba) > 0))
  jp <- hd(c(14, 40), "PINU.BAN", BA = 25, TPH = 1500)$height
  bs <- hd(c(14, 40), "PICE.MAR", BA = 25, TPH = 1500)$height
  expect_lt(diff(jp), 2)
  expect_gt(diff(bs), 5)
})

test_that("hd_sharmazhang2004 is broadly consistent with other Ontario models", {
  # Cross-model sanity check only (different models, data and predictors).
  d <- c(10, 20, 30)
  for (sp in c("PINU.BAN", "PICE.MAR")) {
    new <- hd(d, sp, BA = 25, TPH = 2000)$height
    ref <- hd_sharmaparton2007(d, sp, HDOM = 18, BA = 25, TPH = 2000)$height
    expect_true(all(abs(new / ref - 1) < 0.3))
  }
})

test_that("hd_sharmazhang2004 returns a well-formed tibble", {
  out <- hd(c(10, 20), "PICE.MAR", BA = 25, TPH = 2000)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_identical(
    hd_sharmazhang2004(numeric(), "PICE.MAR", BA = 25, TPH = 2000),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_sharmazhang2004(numeric(), "PICE.MAR", "eq5", keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
  ids <- hd(c(10, 20), c("PINU.BAN", NA), "eq5", keep_model_id = TRUE)
  expect_identical(ids$model_id, c("sharmazhang2004_eq5", NA))
  expect_true(is.na(ids$height[2]))
  expect_identical(
    hd(10, "PINUBAN", BA = 25, TPH = 2000, keep_model_id = TRUE)$model_id,
    "sharmazhang2004_eq2"
  )
})

test_that("hd_sharmazhang2004 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18),
    species = c("PINU.BAN", "PICE.MAR", "PICE.MAR"),
    BA = c(30, 30, 18), TPH = c(3000, 3000, 1200)
  )
  res <- dplyr::mutate(trees, hd_sharmazhang2004(DBH, species, BA = BA, TPH = TPH))
  expect_equal(
    res$height,
    hd_sharmazhang2004(trees$DBH, trees$species, BA = trees$BA, TPH = trees$TPH)$height
  )
  expect_true(is.na(hd(c(10, NA), "PICE.MAR", BA = c(NA, 25), TPH = 2000)$height[1]))
  expect_error(hd(1:3, c("PICE.MAR", "PINU.BAN"), BA = 25, TPH = 2000), "Incompatible")
  expect_error(hd(1:3, "PICE.MAR", BA = c(20, 25), TPH = 2000), "Incompatible")
})

test_that("hd_sharmazhang2004 validates inputs and variant predictors", {
  expect_error(hd_sharmazhang2004(-1, "PICE.MAR", "eq5"), ">= 0")
  expect_error(hd_sharmazhang2004("10", "PICE.MAR", "eq5"), "numeric")
  expect_error(hd_sharmazhang2004(10, 1, "eq5"), "character")
  expect_error(hd_sharmazhang2004(10, "PICE.MAR", "eq3"), "must be one of")
  expect_error(hd_sharmazhang2004(10, "PICE.MAR", "eq5", keep_model_id = NA), "TRUE or FALSE")
  expect_error(hd_sharmazhang2004(10, "PICE.MAR", BA = 0, TPH = 2000), "BA")
  expect_error(hd_sharmazhang2004(10, "PICE.MAR", BA = 25, TPH = -5), "TPH")
  expect_error(hd_sharmazhang2004(10, "PICE.MAR"), "requires")
  expect_error(hd_sharmazhang2004(10, "PICE.MAR", BA = 25), "TPH")
  expect_error(hd_sharmazhang2004(10, "ABIE.BAL", "eq5"), "not covered")
  expect_warning(
    out <- hd_sharmazhang2004(10, "PICE.MAR", "eq5", BA = 25, TPH = 2000),
    class = "CanadaForestAllometry_hd_unused_predictor"
  )
  expect_equal(out, hd_sharmazhang2004(10, "PICE.MAR", "eq5"))
})

test_that("hd_sharmazhang2004 warns outside the data range (Tables 1-2)", {
  expect_no_warning(hd_sharmazhang2004(20, "PICE.MAR", BA = 25, TPH = 2000))
  expect_warning(
    hd_sharmazhang2004(50, "PICE.MAR", "eq5"),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  # BA 1 is inside the jack pine range (0.25) but below black spruce (1.76).
  expect_no_warning(hd_sharmazhang2004(20, "PINU.BAN", BA = 1, TPH = 2000))
  expect_warning(
    hd_sharmazhang2004(20, "PICE.MAR", BA = 1, TPH = 100),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
})
