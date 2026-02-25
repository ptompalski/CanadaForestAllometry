# internal
assert_len_compat <- function(..., .n = NULL, .recycle = TRUE) {
  vals <- list(...)
  if (length(vals) == 0L) {
    cli::cli_abort("At least one input vector must be supplied.")
  }

  arg_names <- names(vals)
  if (is.null(arg_names) || any(!nzchar(arg_names))) {
    arg_names <- paste0("arg", seq_along(vals))
  }

  lens <- vapply(vals, length, integer(1))

  if (is.null(.n)) {
    .n <- max(lens)
  }

  if (!is.numeric(.n) || length(.n) != 1L || is.na(.n) || .n < 0 || .n != as.integer(.n)) {
    cli::cli_abort("{.arg .n} must be a single non-negative integer.")
  }
  .n <- as.integer(.n)

  bad <- arg_names[!lens %in% c(1L, .n)]
  if (length(bad) > 0L) {
    bad_txt <- paste(sprintf("`%s`", bad), collapse = ", ")
    cli::cli_abort(c(
      "Incompatible input lengths.",
      "x" = paste0(bad_txt, " must have length 1 or ", .n, ".")
    ))
  }

  if (isTRUE(.recycle) && .n > 0L) {
    vals <- Map(
      function(v) {
        if (length(v) == 1L && .n != 1L) {
          rep(v, .n)
        } else {
          v
        }
      },
      vals
    )
  }

  names(vals) <- arg_names
  vals
}


# internal
assert_numeric_vec <- function(
    x,
    arg,
    finite = TRUE,
    gt = NULL,
    gte = NULL,
    allow_na = FALSE,
    allow_null = FALSE
) {
  if (is.null(x)) {
    if (isTRUE(allow_null)) {
      return(invisible(TRUE))
    }
    cli::cli_abort("{.arg {arg}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be numeric.")
  }

  if (!isTRUE(allow_na) && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} cannot contain NA values.")
  }

  if (isTRUE(finite) && any(!is.na(x) & !is.finite(x))) {
    cli::cli_abort("{.arg {arg}} must contain only finite values.")
  }

  if (!is.null(gt) && !is.null(gte)) {
    cli::cli_abort("Use only one of {.arg gt} or {.arg gte}.")
  }

  if (!is.null(gt)) {
    if (!is.numeric(gt) || length(gt) != 1L || is.na(gt)) {
      cli::cli_abort("{.arg gt} must be a single numeric value.")
    }
    if (any(!is.na(x) & x <= gt)) {
      cli::cli_abort("{.arg {arg}} must contain values > {gt}.")
    }
  }

  if (!is.null(gte)) {
    if (!is.numeric(gte) || length(gte) != 1L || is.na(gte)) {
      cli::cli_abort("{.arg gte} must be a single numeric value.")
    }
    if (any(!is.na(x) & x < gte)) {
      cli::cli_abort("{.arg {arg}} must contain values >= {gte}.")
    }
  }

  invisible(TRUE)
}


# internal
assert_chr_scalar <- function(x, arg, allow_na = FALSE, non_empty = TRUE) {
  if (!is.character(x) || length(x) != 1L) {
    cli::cli_abort("{.arg {arg}} must be a single character value.")
  }

  if (!isTRUE(allow_na) && is.na(x)) {
    cli::cli_abort("{.arg {arg}} cannot be NA.")
  }

  if (isTRUE(non_empty) && !is.na(x) && !nzchar(trimws(x))) {
    cli::cli_abort("{.arg {arg}} cannot be empty.")
  }

  invisible(TRUE)
}


# internal
assert_choice <- function(x, arg, choices, multiple = FALSE, allow_na = FALSE) {
  if (!is.character(choices) || length(choices) == 0L) {
    cli::cli_abort("{.arg choices} must be a non-empty character vector.")
  }

  if (!is.character(x)) {
    cli::cli_abort("{.arg {arg}} must be character.")
  }

  if (!isTRUE(multiple) && length(x) != 1L) {
    cli::cli_abort("{.arg {arg}} must be length 1.")
  }

  if (!isTRUE(allow_na) && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} cannot contain NA values.")
  }

  bad <- unique(x[!is.na(x) & !x %in% choices])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.arg {arg}} contains invalid choice(s).",
      "x" = paste0("Invalid: ", paste(bad, collapse = ", ")),
      "i" = paste0("Valid choices: ", paste(choices, collapse = ", "))
    ))
  }

  invisible(x)
}


# internal
assert_required_cols <- function(df, cols, object = "data") {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg object} must be a data.frame/tibble.")
  }

  if (!is.character(cols) || length(cols) == 0L) {
    cli::cli_abort("{.arg cols} must be a non-empty character vector.")
  }

  miss <- setdiff(cols, names(df))
  if (length(miss) > 0L) {
    cli::cli_abort(c(
      paste0("Missing required columns in ", object, "."),
      "x" = paste(miss, collapse = ", ")
    ))
  }

  invisible(TRUE)
}


# internal
assert_nrow <- function(df, n, object = "data", context = NULL) {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg object} must be a data.frame/tibble.")
  }

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    cli::cli_abort("{.arg n} must be a single non-negative integer.")
  }
  n <- as.integer(n)

  actual <- nrow(df)
  if (!identical(actual, n)) {
    msg <- paste0(object, " must have exactly ", n, " row(s); found ", actual, ".")
    if (!is.null(context) && length(context) == 1L && nzchar(context)) {
      msg <- paste0(msg, " ", context)
    }
    cli::cli_abort(msg)
  }

  invisible(TRUE)
}


# internal
assert_finite_params <- function(df, cols, object = "parameters", row = 1L) {
  assert_required_cols(df, cols, object = object)

  if (!is.numeric(row) || length(row) != 1L || is.na(row) || row < 1 || row != as.integer(row)) {
    cli::cli_abort("{.arg row} must be a single positive integer.")
  }
  row <- as.integer(row)

  if (nrow(df) < row) {
    cli::cli_abort("{.arg row} ({row}) is out of bounds for {.val {object}}.")
  }

  bad <- cols[!vapply(
    cols,
    function(nm) {
      v <- df[[nm]][[row]]
      is.numeric(v) && length(v) == 1L && is.finite(v)
    },
    logical(1)
  )]

  if (length(bad) > 0L) {
    cli::cli_abort(c(
      paste0(object, " has non-finite or non-scalar numeric values at row ", row, "."),
      "x" = paste(bad, collapse = ", ")
    ))
  }

  invisible(TRUE)
}


# internal
abort_row <- function(fn, i, msg, ...) {
  ctx <- list(...)

  if (length(ctx) == 0L) {
    cli::cli_abort("{.fn {fn}} failed for row {i}: {msg}")
  }

  ctx_names <- names(ctx)
  if (is.null(ctx_names)) {
    ctx_names <- rep("", length(ctx))
  }
  ctx_names[!nzchar(ctx_names)] <- paste0("arg", which(!nzchar(ctx_names)))

  fmt_one <- function(v) {
    if (length(v) == 0L) {
      return("<empty>")
    }
    if (length(v) == 1L) {
      return(as.character(v))
    }
    paste(as.character(v), collapse = "|")
  }

  ctx_txt <- paste0(
    ctx_names,
    "=",
    vapply(ctx, fmt_one, character(1)),
    collapse = ", "
  )

  cli::cli_abort("{.fn {fn}} failed for row {i} ({ctx_txt}): {msg}")
}
