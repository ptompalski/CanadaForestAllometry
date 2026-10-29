# Shared internal helpers for DBH–height (hd_) models.

# internal
# All-or-none check for a group of optional predictors that together enable a
# model variant (e.g. HDOM + BA + TPH). Returns TRUE if every member is
# supplied (non-NULL), FALSE if none is, and aborts on partial supply.
.hd_check_predictor_group <- function(args, variant = NULL, call = rlang::caller_env()) {
  if (!is.list(args) || length(args) == 0L || is.null(names(args)) ||
    any(!nzchar(names(args)))) {
    cli::cli_abort("{.arg args} must be a non-empty named list.")
  }

  supplied <- !vapply(args, is.null, logical(1))
  if (all(supplied)) {
    return(TRUE)
  }
  if (!any(supplied)) {
    return(FALSE)
  }

  grp <- names(args)
  what <- if (is.null(variant)) "" else paste0(" (", variant, ")")
  cli::cli_abort(
    c(
      "{.arg {grp}} must be supplied together{what}, or not at all.",
      "v" = "Supplied: {.arg {grp[supplied]}}.",
      "x" = "Missing: {.arg {grp[!supplied]}}."
    ),
    call = call
  )
}

# internal
# Emits ONE warning per call listing rows whose inputs fall outside the
# source's published applicability range. Predictions are still computed.
#
# x:        named list of numeric vectors (all length n).
# lower, upper: named lists (subset of names(x)) of numeric vectors of length 1 or
#           n; NA means "no limit" for that row.
# Returns the affected row indices invisibly.
.hd_warn_out_of_range <- function(x, lower = list(), upper = list(), fn, max_rows = 10L) {
  if (!is.list(x) || length(x) == 0L || is.null(names(x))) {
    cli::cli_abort("{.arg x} must be a non-empty named list.")
  }
  extra <- setdiff(c(names(lower), names(upper)), names(x))
  if (length(extra)) {
    cli::cli_abort("Limits supplied for unknown input{?s}: {.arg {extra}}.")
  }

  bullets <- character()
  rows_all <- integer()
  for (nm in names(x)) {
    v <- x[[nm]]
    lo <- lower[[nm]] %||% NA_real_
    hi <- upper[[nm]] %||% NA_real_
    out <- !is.na(v) & ((!is.na(lo) & v < lo) | (!is.na(hi) & v > hi))
    rows <- which(out)
    if (length(rows) == 0L) next

    rows_all <- union(rows_all, rows)
    shown <- rows[seq_len(min(length(rows), max_rows))]
    more <- if (length(rows) > max_rows) paste0(" and ", length(rows) - max_rows, " more") else ""
    bullets <- c(bullets, paste0(
      "`", nm, "`: ", length(rows), " row", if (length(rows) > 1L) "s" else "",
      " (", paste(shown, collapse = ", "), more, ")"
    ))
  }

  if (length(bullets) == 0L) {
    return(invisible(integer()))
  }

  n_out <- length(rows_all)
  names(bullets) <- rep("*", length(bullets))
  cli::cli_warn(
    c(
      "{.fn {fn}}: {n_out} row{?s} outside the source's published applicability range; predictions are extrapolated.",
      bullets
    ),
    class = "CanadaForestAllometry_hd_out_of_range"
  )
  invisible(sort(rows_all))
}
