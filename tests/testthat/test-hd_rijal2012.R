# hd_rijal2012(): Rijal, Weiskittel & Kershaw (2012), Forestry 85:379-389.

# Eq. 4 (p. 381), for independent checks.
eq <- function(DBH, CSI, CCF, BAL, c0, c1, c2, c3, c4, c5) {
  1.37 + (c0 + CSI^c3) * (1 - exp(-c1 * DBH))^(c2 + c4 * log(CCF + 1) + c5 * BAL)
}

# Table 5 (GNLS) and Table 6 (NLME), p. 387, selected rows; "-" = 0.
gnls <- tibble::tribble(
  ~species, ~c0, ~c1, ~c2, ~c3, ~c4, ~c5,
  "FAGU.GRA", 11.8567, 0.0717, 2.8805, 0.6286, -0.2668, -0.0058,
  "ACER.SAH", 9.0164, 0.0488, 2.2266, 0.9057, -0.2037, -0.0021,
  "PICE.MAR", 17.6022, 0.0457, 1.2197, 0.6272, 0.0099, -0.0082,
  "PICE.GLA", 20.5678, 0.0275, 1.4422, 0.7615, -0.0643, -0.0041
)
nlme <- tibble::tribble(
  ~species, ~c0, ~c1, ~c2, ~c3, ~c4, ~c5,
  "QUER.RUB", 9.5568, 0.1128, 3.4246, 0.7446, -0.2805, 0,
  "PICE.RUB", 12.3532, 0.0715, 1.9533, 0.6443, -0.1206, -0.0016,
  "THUJ.OCC", 8.9351, 0.0918, 2.2097, 0.6864, -0.1124, -0.0025
)

hd <- function(...) suppressWarnings(hd_rijal2012(...))

test_that("hd_rijal2012 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PICE.RUB", CSI = 15, CCF = 150, BAL = 10)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd(10, "PICE.RUB", 15, 150, 10, variant = "nlme", keep_model_id = TRUE)$model_id,
    "rijal2012_nlme"
  )
  expect_identical(
    hd(c(10, NA), c("PICE.RUB", NA), 15, 150, 10, keep_model_id = TRUE)$model_id,
    c("rijal2012_gnls", NA)
  )
  expect_identical(
    hd_rijal2012(numeric(), "PICE.RUB", 15, 150, 10),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_rijal2012(numeric(), "PICE.RUB", 15, 150, 10, keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("hd_rijal2012 reproduces eq. 4 with the GNLS and NLME coefficients", {
  x <- list(DBH = c(5, 18, 35), CSI = c(12, 15, 18), CCF = c(80, 150, 250), BAL = c(30, 10, 0))
  for (i in seq_len(nrow(gnls))) {
    r <- gnls[i, ]
    expect_equal(
      hd(x$DBH, r$species, x$CSI, x$CCF, x$BAL)$height,
      eq(x$DBH, x$CSI, x$CCF, x$BAL, r$c0, r$c1, r$c2, r$c3, r$c4, r$c5),
      tolerance = 1e-12
    )
  }
  for (i in seq_len(nrow(nlme))) {
    r <- nlme[i, ]
    expect_equal(
      hd(x$DBH, r$species, x$CSI, x$CCF, x$BAL, variant = "nlme")$height,
      eq(x$DBH, x$CSI, x$CCF, x$BAL, r$c0, r$c1, r$c2, r$c3, r$c4, r$c5),
      tolerance = 1e-12
    )
  }
  # Hand-computed regression value (red spruce, GNLS, DBH 20, CSI 15, CCF 150, BAL 10).
  expect_equal(hd(20, "PICE.RUB", 15, 150, 10)$height, 13.335502, tolerance = 1e-6)
})

test_that("all 15 species are covered by both variants", {
  sp <- c(
    "FAGU.GRA", "BETU.POP", "BETU.PAP", "POPU.TRE", "ACER.RUB", "QUER.RUB",
    "ACER.SAH", "BETU.ALL", "ABIE.BAL", "PICE.MAR", "TSUG.CAN", "PICE.RUB",
    "THUJ.OCC", "PINU.STR", "PICE.GLA"
  )
  for (v in c("gnls", "nlme")) {
    h <- hd(20, sp, 15, 150, 10, variant = v)$height
    expect_true(all(is.finite(h) & h > 1.37))
  }
  expect_equal(
    hd(20, "PICERUB", 15, 150, 10)$height,
    hd(20, "PICE.RUB", 15, 150, 10)$height
  )
})

test_that("NLME BAL coefficient is zero for grey birch and red oak", {
  for (s in c("BETU.POP", "QUER.RUB")) {
    expect_equal(
      hd(20, s, 15, 150, 0, variant = "nlme")$height,
      hd(20, s, 15, 150, 40, variant = "nlme")$height
    )
    expect_false(isTRUE(all.equal(
      hd(20, s, 15, 150, 0)$height, hd(20, s, 15, 150, 40)$height
    )))
  }
})

test_that("plausibility: GNLS reproduces observed mean heights (Table 1)", {
  # Sanity check, not a fidelity test: the source publishes no benchmark.
  t1 <- tibble::tribble(
    ~species, ~DBH, ~H,
    "FAGU.GRA", 17.8, 12.67, "BETU.POP", 14.3, 10.89, "BETU.PAP", 14.3, 10.98,
    "POPU.TRE", 17.6, 12.42, "ACER.RUB", 16.3, 11.81, "QUER.RUB", 18.5, 12.32,
    "ACER.SAH", 16.9, 11.76, "BETU.ALL", 17.7, 12.03, "ABIE.BAL", 12.8, 9.41,
    "PICE.MAR", 10.7, 8.04, "TSUG.CAN", 19.3, 12.17, "PICE.RUB", 16.7, 11.65,
    "THUJ.OCC", 19.8, 11.81, "PINU.STR", 19.2, 12.42, "PICE.GLA", 15.8, 10.78
  )
  h <- hd(t1$DBH, t1$species, CSI = 15, CCF = 150, BAL = 10)$height
  expect_true(all(abs(h - t1$H) < 2))
})

test_that("structural behaviour follows eq. 4", {
  expect_equal(hd(0, "PICE.RUB", 15, 150, 10)$height, 1.37)
  d <- seq(1, 60, by = 1)
  for (v in c("gnls", "nlme")) {
    expect_true(all(diff(hd(d, "PICE.RUB", 15, 150, 10, variant = v)$height) > 0))
  }
  # Asymptote 1.37 + c0 + CSI^c3.
  expect_equal(
    hd(1e4, "PICE.GLA", 15, 150, 10)$height,
    1.37 + 20.5678 + 15^0.7615,
    tolerance = 1e-8
  )
  expect_gt(hd(20, "PICE.RUB", 18, 150, 10)$height, hd(20, "PICE.RUB", 12, 150, 10)$height)
  # c4 < 0: height increases with CCF; c4 > 0 (black spruce GNLS): decreases.
  expect_gt(hd(20, "PICE.RUB", 15, 300, 10)$height, hd(20, "PICE.RUB", 15, 50, 10)$height)
  expect_lt(hd(20, "PICE.MAR", 15, 300, 10)$height, hd(20, "PICE.MAR", 15, 50, 10)$height)
  # c5 < 0: height increases with BAL.
  expect_gt(hd(20, "PICE.RUB", 15, 150, 30)$height, hd(20, "PICE.RUB", 15, 150, 0)$height)
})

test_that("hd_rijal2012 recycles inputs, handles NA and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18),
    species = c("PICE.RUB", "TSUG.CAN", "BETU.ALL"),
    CSI = c(14, 14, 17), CCF = c(180, 180, 120), BAL = c(18, 6, 12)
  )
  res <- dplyr::mutate(trees, hd_rijal2012(DBH, species, CSI, CCF, BAL))
  expect_equal(
    res$height,
    hd(trees$DBH, trees$species, trees$CSI, trees$CCF, trees$BAL)$height
  )
  expect_equal(
    hd(c(10, 20), "PICE.RUB", 15, 150, 10)$height,
    hd(c(10, 20), c("PICE.RUB", "PICE.RUB"), c(15, 15), c(150, 150), c(10, 10))$height
  )
  h <- hd(c(10, NA, 10, 10, 10), "PICE.RUB", c(15, 15, NA, 15, 15), c(150, 150, 150, NA, 150),
          c(10, 10, 10, 10, NA))$height
  expect_true(!is.na(h[1]) && all(is.na(h[2:5])))
  expect_error(hd(c(10, 20, 30), "PICE.RUB", c(15, 16), 150, 10))
})

test_that("hd_rijal2012 validates inputs", {
  expect_error(hd(-1, "PICE.RUB", 15, 150, 10), "DBH")
  expect_error(hd(10, 1, 15, 150, 10), "species")
  expect_error(hd(10, "PICE.RUB", 0, 150, 10), "CSI")
  expect_error(hd(10, "PICE.RUB", 15, -1, 10), "CCF")
  expect_error(hd(10, "PICE.RUB", 15, 150, -1), "BAL")
  expect_error(hd(10, "PICE.RUB", 15, 150), "BAL")
  expect_error(hd(10, "PICE.RUB", 15, 150, 10, variant = "ols"), "variant")
  expect_error(hd(10, "PICE.RUB", 15, 150, 10, keep_model_id = NA), "keep_model_id")
  expect_error(hd(10, "PINU.BAN", 15, 150, 10), "not covered")
  expect_error(hd(10, "FAGU.GRA", 15, 150, 600), "non-positive")
})

test_that("DBH outside the fitting range warns", {
  expect_warning(hd_rijal2012(60, "PICE.MAR", 15, 150, 10), "DBH")
  expect_no_warning(hd_rijal2012(20, "PICE.MAR", 15, 150, 10))
})
