# assertions.R internal helpers

testthat::test_that("assert_len_compat handles validation and recycling", {
  f <- CanadaForestAllometry:::assert_len_compat

  testthat::expect_error(
    f(),
    "At least one input vector"
  )

  testthat::expect_error(
    f(a = 1:2, .n = -1),
    "non-negative integer"
  )

  testthat::expect_error(
    f(a = 1:2, b = 1:3, .n = 3),
    "must have length 1 or 3"
  )

  out <- f(age = 50, si = c(12, 14, 16), .n = 3)
  testthat::expect_named(out, c("age", "si"))
  testthat::expect_identical(out$age, c(50, 50, 50))
  testthat::expect_identical(out$si, c(12, 14, 16))

  out_no_recycle <- f(age = 50, si = c(12, 14, 16), .n = 3, .recycle = FALSE)
  testthat::expect_identical(out_no_recycle$age, 50)

  # unnamed inputs force fallback arg naming and implicit .n from max length
  out_unnamed <- f(1, 1:2)
  testthat::expect_named(out_unnamed, c("arg1", "arg2"))
  testthat::expect_identical(out_unnamed$arg1, c(1, 1))
  testthat::expect_identical(out_unnamed$arg2, 1:2)
})


testthat::test_that("assert_numeric_vec validates constraints", {
  f <- CanadaForestAllometry:::assert_numeric_vec

  testthat::expect_error(f(NULL, "x"), "cannot be NULL")
  testthat::expect_invisible(f(NULL, "x", allow_null = TRUE))

  testthat::expect_error(f("a", "x"), "must be numeric")
  testthat::expect_error(f(c(1, NA_real_), "x"), "cannot contain NA")
  testthat::expect_error(f(c(1, Inf), "x"), "finite values")

  testthat::expect_error(
    f(1, "x", gt = 0, gte = 0),
    "Use only one of"
  )

  testthat::expect_error(
    f(1, "x", gt = c(0, 1)),
    "gt.*single numeric"
  )
  testthat::expect_error(
    f(c(0, 1), "x", gt = 0),
    "values > 0"
  )

  testthat::expect_error(
    f(1, "x", gte = c(0, 1)),
    "gte.*single numeric"
  )
  testthat::expect_error(
    f(c(-1, 1), "x", gte = 0),
    "values >= 0"
  )

  testthat::expect_invisible(
    f(c(NA_real_, Inf), "x", finite = FALSE, allow_na = TRUE)
  )
})


testthat::test_that("assert_chr_scalar validates scalar character inputs", {
  f <- CanadaForestAllometry:::assert_chr_scalar

  testthat::expect_error(f(1, "species"), "single character")
  testthat::expect_error(f(NA_character_, "species"), "cannot be NA")
  testthat::expect_error(f("   ", "species"), "cannot be empty")

  testthat::expect_invisible(f(NA_character_, "species", allow_na = TRUE))
  testthat::expect_invisible(f("   ", "species", non_empty = FALSE))
})


testthat::test_that("assert_choice validates choices and cardinality", {
  f <- CanadaForestAllometry:::assert_choice

  testthat::expect_error(
    f("a", "model", choices = character(0)),
    "non-empty character vector"
  )
  testthat::expect_error(
    f(1, "model", choices = c("a", "b")),
    "must be character"
  )
  testthat::expect_error(
    f(c("a", "b"), "model", choices = c("a", "b")),
    "length 1"
  )
  testthat::expect_error(
    f(NA_character_, "model", choices = c("a", "b")),
    "cannot contain NA"
  )
  testthat::expect_error(
    f("c", "model", choices = c("a", "b")),
    "invalid choice"
  )

  out <- f(c("a", NA_character_), "model", c("a", "b"), multiple = TRUE, allow_na = TRUE)
  testthat::expect_identical(out, c("a", NA_character_))
})


testthat::test_that("assert_required_cols and assert_nrow validate data frames", {
  f_cols <- CanadaForestAllometry:::assert_required_cols
  f_nrow <- CanadaForestAllometry:::assert_nrow

  testthat::expect_error(f_cols(1, "x"), "data.frame")
  testthat::expect_error(f_cols(data.frame(x = 1), character(0)), "non-empty character")
  testthat::expect_error(f_cols(data.frame(x = 1), c("x", "y"), object = "tbl"), "Missing required columns")
  testthat::expect_invisible(f_cols(data.frame(x = 1, y = 2), c("x", "y")))

  testthat::expect_error(f_nrow(1, 1), "data.frame")
  testthat::expect_error(f_nrow(data.frame(x = 1), -1), "non-negative integer")
  testthat::expect_error(
    f_nrow(data.frame(x = 1:2), 1, object = "params", context = "species=PICE.MAR"),
    "species=PICE\\.MAR"
  )
  testthat::expect_invisible(f_nrow(data.frame(x = 1:2), 2))
})


testthat::test_that("assert_finite_params validates row bounds and scalar finiteness", {
  f <- CanadaForestAllometry:::assert_finite_params

  df <- data.frame(a = c(1, 2), b = c(3, 4))
  testthat::expect_invisible(f(df, c("a", "b"), object = "pars", row = 2))

  testthat::expect_error(
    f(df, c("a", "z")),
    "Missing required columns"
  )
  testthat::expect_error(
    f(df, c("a", "b"), row = 0),
    "positive integer"
  )
  testthat::expect_error(
    f(df, c("a", "b"), row = 3),
    "out of bounds"
  )

  bad <- data.frame(a = c(1, Inf), b = I(list(2, c(3, 4))))
  testthat::expect_error(
    f(bad, c("a", "b"), row = 2),
    "non-finite or non-scalar"
  )
})


testthat::test_that("abort_row formats context-rich errors", {
  f <- CanadaForestAllometry:::abort_row

  testthat::expect_error(
    f("si_nigh2000", 3, "bad input"),
    "failed for row 3: bad input"
  )

  testthat::expect_error(
    f("si_nigh2000", 4, "bad input", species = "PICE.MAR", numeric(0), c("a", "b")),
    "species=PICE\\.MAR.*arg2=<empty>.*arg3=a\\|b"
  )

  testthat::expect_error(
    f("si_nigh2000", 5, "bad input", 1, 2),
    "arg1=1.*arg2=2"
  )
})
