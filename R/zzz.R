inform_once <- function(..., .id) {
  if (isTRUE(getOption("CanadaForestAllometry.quiet", FALSE))) {
    return(invisible(FALSE))
  }

  rlang::inform(
    message = cli::format_message(...),
    class = "CanadaForestAllometry_message",
    .frequency = "once",
    .frequency_id = .id
  )
}

# internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

utils::globalVariables(c(".data", ".env"))
