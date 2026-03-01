testthat::test_that("inform_once respects quiet option", {
  old_quiet <- getOption("CanadaForestAllometry.quiet")
  withr::defer(options(CanadaForestAllometry.quiet = old_quiet))

  id <- paste0("test_zzz_quiet_", as.integer(stats::runif(1, 1, 1e9)))

  options(CanadaForestAllometry.quiet = TRUE)
  msgs_quiet <- testthat::capture_messages(
    out_quiet <- CanadaForestAllometry:::inform_once("hello", .id = id)
  )
  testthat::expect_length(msgs_quiet, 0L)
  testthat::expect_identical(out_quiet, FALSE)

  options(CanadaForestAllometry.quiet = FALSE)
  msgs_loud <- testthat::capture_messages(
    CanadaForestAllometry:::inform_once("hello", .id = paste0(id, "_loud"))
  )
  testthat::expect_lte(length(msgs_loud), 1L)
})
