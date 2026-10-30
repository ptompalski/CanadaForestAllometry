# hd_temesgen2004(): Temesgen & von Gadow (2004), Eur. J. For. Res. 123:45-51.

# Models 2 and 7 (pp. 46-47) with the exponent sign corrected to exp(-b D^c),
# for independent checks.
eq <- function(DBH, a, b, c) 1.3 + a * (1 - exp(-b * DBH^c))

# Table 5 (Model 2, p. 51) and Table 4 (Model 7, p. 50); blank (dropped) = 0.
m2 <- tibble::tribble(
  ~species, ~a, ~b, ~c,
  "POPU.TRE", 20.9169, 0.0281, 1.3791,
  "THUJ.PLI", 22.0648, 0.0094, 1.6327,
  "BETU.PAP", 23.4556, 0.0247, 1.4218,
  "PSEU.MEN", 34.1073, 0.0181, 1.1583,
  "LARI.OCC", 36.8541, 0.0244, 1.1126,
  "PINU.CON", 27.7486, 0.0216, 1.2828,
  "PINU.PON", 32.2084, 0.0174, 1.1072,
  "PICE.ENG", 28.3578, 0.0074, 1.5965
)
m7 <- tibble::tribble(
  ~species, ~a1, ~a2, ~a3, ~a4, ~b, ~a5, ~a6,
  "POPU.TRE", 20.6548, 0, 0, 0.08724, 0.01509, 1.39936, 0,
  "THUJ.PLI", 17.9468, 0, -0.0009, 0.14087, 0.03497, 1.30435, 0,
  "BETU.PAP", 20.4464, 0, -0.0007, 0.13355, 0.03576, 1.26223, 0,
  "PSEU.MEN", 32.037, -0.3504, -0.0007, 0.18308, 0.01797, 1.09263, 0.00802,
  "LARI.OCC", 41.7918, 0, 0, 0, 0.01709, 1.1185, 0.00404,
  "PINU.CON", 20.852, 0.3168, -0.0004, 0.23962, 0.03184, 1.0875, -0.0014,
  "PINU.PON", 32.2082, 0, 0, 0, 0.01738, 1.10721, 0,
  "PICE.ENG", 17.0803, 0.0932, 0, 0.34276, 0.01073, 1.46187, 0
)

hd <- function(...) suppressMessages(suppressWarnings(hd_temesgen2004(...)))

test_that("hd_temesgen2004 returns a well-formed tibble", {
  out <- hd(DBH = c(10, 20), species = "PSEU.MEN")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 2L)
  expect_identical(
    hd(10, "PSEU.MEN", BAL = 10, TPH = 800, BA = 25, keep_model_id = TRUE)$model_id,
    "temesgen2004_m7"
  )
  expect_identical(
    hd(c(10, NA), c("PSEU.MEN", NA), keep_model_id = TRUE)$model_id,
    c("temesgen2004_m2", NA)
  )
  expect_identical(
    hd_temesgen2004(numeric(), "PSEU.MEN"),
    tibble::tibble(height = numeric())
  )
  expect_identical(
    hd_temesgen2004(numeric(), "PSEU.MEN", keep_model_id = TRUE),
    tibble::tibble(height = numeric(), model_id = character())
  )
})

test_that("variant is inferred from the stand predictors and reported once", {
  rlang::reset_message_verbosity("hd_temesgen2004_m2")
  rlang::reset_message_verbosity("hd_temesgen2004_m7")
  expect_message(hd_temesgen2004(20, "PSEU.MEN"), "Model 2")
  expect_no_message(hd_temesgen2004(20, "PSEU.MEN"))
  expect_message(hd_temesgen2004(20, "PSEU.MEN", 10, 800, 25), "Model 7")

  expect_error(hd(20, "PSEU.MEN", BAL = 10), "together")
  expect_error(hd(20, "PSEU.MEN", BAL = 10, TPH = 800), "BA")
  expect_error(hd(20, "PSEU.MEN", TPH = 800, BA = 25), "BAL")
})

test_that("hd_temesgen2004 recycles inputs and works inside mutate()", {
  trees <- tibble::tibble(
    DBH = c(12, 25, 18),
    species = c("PSEU.MEN", "LARI.OCC", "POPU.TRE"),
    BAL = c(18, 6, 12), TPH = c(900, 900, 600), BA = c(24, 24, 15)
  )
  res <- suppressMessages(
    dplyr::mutate(trees, hd_temesgen2004(DBH, species, BAL, TPH, BA))
  )
  expect_equal(
    res$height,
    hd(trees$DBH, trees$species, trees$BAL, trees$TPH, trees$BA)$height
  )
  expect_equal(nrow(hd(c(10, 20, 30), "PSEU.MEN", 10, 800, 25)), 3L)
  expect_error(hd(1:3, c("PSEU.MEN", "PINU.CON")), "Incompatible")
  expect_error(hd(1:3, "PSEU.MEN", c(10, 12), 800, 25), "Incompatible")
})

test_that("hd_temesgen2004 validates inputs", {
  expect_error(hd(-1, "PSEU.MEN"), ">= 0")
  expect_error(hd("10", "PSEU.MEN"), "numeric")
  expect_error(hd(Inf, "PSEU.MEN"), "finite")
  expect_error(hd(10, 1), "character")
  expect_error(hd(10, "PSEU.MEN", -1, 800, 25), "BAL")
  expect_error(hd(10, "PSEU.MEN", 10, 0, 25), "TPH")
  expect_error(hd(10, "PSEU.MEN", 10, 800, 0), "BA")
  expect_error(hd(10, "PICE.MAR"), "not covered")
  expect_error(hd(10, "PSEU.MEN", keep_model_id = NA), "keep_model_id")
  # Extreme density makes the cedar asymptote negative (a3 < 0).
  expect_error(hd(10, "THUJ.PLI", 5, 30000, 20), "non-positive")
  expect_no_error(hd(0, "PSEU.MEN", 0, 800, 25))
})

test_that("NA in any input returns NA height (incl. dropped-term species)", {
  out <- hd(c(NA, 20), c("PSEU.MEN", NA))
  expect_true(all(is.na(out$height)))
  for (sp in c("PSEU.MEN", "PINU.PON")) {
    out <- hd(
      DBH = c(NA, 20, 20, 20, 20),
      species = sp,
      BAL = c(10, NA, 10, 10, 10),
      TPH = c(800, 800, NA, 800, 800),
      BA = c(25, 25, 25, NA, 25)
    )
    expect_identical(is.na(out$height), c(rep(TRUE, 4), FALSE), info = sp)
  }
})

test_that("DBH outside the Table 1 range warns once and is still computed", {
  expect_warning(
    out <- suppressMessages(hd_temesgen2004(c(1, 20, 40), "POPU.TRE")),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  expect_false(anyNA(out$height))
})

# --- coefficient wiring (Tables 4 and 5) ---------------------------------------

test_that("predictions use the Table 5 (Model 2) and Table 4 (Model 7) coefficients", {
  DBH <- c(2, 10, 25, 50)
  for (i in seq_len(nrow(m2))) {
    r <- m2[i, ]
    expect_equal(hd(DBH, r$species)$height, eq(DBH, r$a, r$b, r$c), info = r$species)
  }
  BAL <- 12
  TPH <- 900
  BA <- 28
  for (i in seq_len(nrow(m7))) {
    r <- m7[i, ]
    expect_equal(
      hd(DBH, r$species, BAL, TPH, BA)$height,
      eq(
        DBH,
        r$a1 + r$a2 * BAL + r$a3 * TPH + r$a4 * BA,
        r$b,
        r$a5 + r$a6 * BAL
      ),
      info = r$species
    )
  }
  expect_equal(hd(20, "PSEUMEN"), hd(20, "PSEU.MEN"))
})

test_that("the source 'spruce' coefficients serve the Engelmann/white complex", {
  sp <- c("PICE.ENG", "PICE.GLA", "PICE.ENG.GLA")
  h2 <- hd(25, sp)$height
  h7 <- hd(25, sp, 10, 800, 25)$height
  expect_equal(h2, rep(h2[1], 3))
  expect_equal(h7, rep(h7[1], 3))
})

# --- TIER 2: plausibility (no exact source benchmark) -------------------------

test_that("Model 2 at the mean DBH reproduces the observed mean height (Table 1)", {
  # Table 1 (p. 50): mean DBH and mean height of the fitting data. A concave
  # curve evaluated at the mean DBH slightly over-predicts the mean height.
  tab1 <- tibble::tribble(
    ~species, ~DBH, ~height,
    "POPU.TRE", 12.8, 13.0,
    "THUJ.PLI", 19.1, 15.1,
    "BETU.PAP", 15.1, 16.1,
    "PSEU.MEN", 19.4, 14.7,
    "LARI.OCC", 20.4, 19.0,
    "PINU.CON", 17.0, 15.7,
    "PINU.PON", 22.0, 14.0,
    "PICE.ENG", 13.8, 11.3
  )
  pred <- hd(tab1$DBH, tab1$species)$height
  expect_true(all(pred - tab1$height > 0 & pred - tab1$height < 1.5))
})

test_that("curves start at 1.3 m, increase with DBH and approach 1.3 + a", {
  DBH <- c(0, 5, 10, 20, 40, 80)
  for (sp in m2$species) {
    h2 <- hd(DBH, sp)$height
    h7 <- hd(DBH, sp, 10, 800, 25)$height
    expect_equal(h2[1], 1.3, info = sp)
    expect_equal(h7[1], 1.3, info = sp)
    expect_true(all(diff(h2) > 0) && all(diff(h7) > 0), info = sp)
  }
  expect_equal(hd(1000, "PSEU.MEN")$height, 1.3 + 34.1073)
  # Ponderosa pine: all stand terms dropped, so Model 7 ~ Model 2.
  expect_equal(
    hd(c(10, 30, 50), "PINU.PON", 10, 800, 25)$height,
    hd(c(10, 30, 50), "PINU.PON")$height,
    tolerance = 1e-3
  )
})

test_that("Model 7 responds to stand density as published", {
  # Douglas-fir: a decreases with TPH (a3 < 0) and increases with BA (a4 > 0).
  # BAL acts on both a (a2 < 0) and c (a6 > 0), so its net effect varies.
  h <- function(BAL, TPH, BA) hd(20, "PSEU.MEN", BAL, TPH, BA)$height
  expect_lt(h(10, 2000, 30), h(10, 500, 30))
  expect_gt(h(10, 800, 40), h(10, 800, 20))
})
