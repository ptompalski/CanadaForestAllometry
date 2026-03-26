#' Universal species translator across codes and names
#'
#' Translate species identifiers across supported code systems and name fields.
#' The function can translate among NFI, CANFI, and jurisdiction-specific
#' species codes, and can also resolve scientific names, English common names,
#' and French common names to NFI codes, CANFI codes, jurisdiction-specific
#' codes, or supported name fields.
#'
#' By default, translations return NFI codes, but the `to` argument can be used
#' to return CANFI codes, jurisdiction-specific codes, English common names,
#' French common names, or scientific names.
#'
#' When `from = "auto"`, the function tries to infer the input type from the
#' supplied values. Numeric inputs are treated as CANFI codes, NFI-formatted
#' values are treated as NFI codes, and other code-like inputs are treated as
#' jurisdiction codes. For jurisdiction-style inputs, the full input vector is
#' used to infer a single shared jurisdiction when possible. If the lookup does
#' not support a unique interpretation, the function errors and asks the user
#' to supply `from` or `jurisdiction` explicitly.
#'
#' @param code A character vector of species codes or names to translate.
#' @param from Source code system or name field: one of `"auto"`, `"nfi"`,
#'   `"canfi"`, `"jurisdiction"`, `"scientificname"`, `"englishname"`, or
#'   `"frenchname"`.
#' @param jurisdiction Jurisdiction for provincial or territorial codes. Required
#'   when `from = "jurisdiction"` and also when `to = "jurisdiction"` unless
#'   `from = "auto"` successfully infers a single shared jurisdiction. May be
#'   length 1 or the same length as `code`.
#' @param to Target field to return: one of `"nfi"`, `"canfi"`,
#'   `"jurisdiction"`, `"scientificname"`, `"englishname"`, or `"frenchname"`.
#'   Defaults to `"nfi"`.
#' @param multiple How to handle ambiguous matches: `"error"`, `"all"`, or
#'   `"first"`.
#' @param unmatched How to handle unmatched inputs: `"error"` or `"NA"`.
#' @param verbose Logical. If `TRUE` and `from = "auto"`, report the inferred
#'   input type once per function call. Defaults to `TRUE`.
#'
#' @return
#' A character vector when `multiple` is `"error"` or `"first"`. A list of
#' character vectors when `multiple = "all"`.
#'
#' @examples
#' # Translate NFI codes to English or French common names
#' translate_species_code("ABIE.BAL", from = "nfi", to = "englishname")
#' translate_species_code("ABIE.BAL", from = "nfi", to = "frenchname")
#'
#' # Translate NFI codes to CANFI or jurisdiction-specific codes
#' translate_species_code("ABIE.BAL", from = "nfi", to = "canfi")
#' translate_species_code("ABIE.BAL", from = "nfi", to = "jurisdiction", jurisdiction = "ON")
#'
#' # Translate from scientific, English, or French names
#' translate_species_code("Picea mariana", from = "scientificname")
#' translate_species_code("black spruce", from = "englishname")
#' translate_species_code("epinette noire", from = "frenchname")
#'
#' # Translate from jurisdiction, CANFI, or auto-detected inputs
#' translate_species_code("BF", from = "jurisdiction", jurisdiction = "ON")
#' translate_species_code("302", from = "canfi")
#' translate_species_code("302", from = "auto")
#'
#' # Translate between name fields
#' translate_species_code("black spruce", from = "englishname", to = "frenchname")
#' translate_species_code("Picea mariana", from = "scientificname", to = "englishname")
#' translate_species_code("PICE.GLA", from = "auto", to = "scientificname")
#'
#' # Partial argument values also work via match.arg()
#' # Here, `to = "e"` resolves to `englishname`
#' translate_species_code(c("ABIE.BAL", "PICE.MAR", "PINU.CON"), to = "e")
#'
#' # Auto-detect a shared jurisdiction from a vector of input codes
#' translate_species_code(c("BF", "PJ", "SB"), from = "auto")
#' translate_species_code(
#'   "SW",
#'   from = "jurisdiction",
#'   jurisdiction = "BC",
#'   to = "scientificname"
#' )
#'
#' # Ambiguous CANFI code (several matches): errors by default
#' try(
#'   translate_species_code("104", from = "canfi")
#' )
#'
#' # Several matches - return the first match
#' translate_species_code("104", from = "canfi", multiple = "first")
#'
#' # Several matches - return all matches as a list
#' translate_species_code("104", from = "canfi", multiple = "all")
#'
#' # Vectorized input with several matches
#' translate_species_code(
#'   c("302", "104"),
#'   from = "canfi",
#'   multiple = "first"
#' )
#'
#' translate_species_code(
#'   c("302", "104"),
#'   from = "canfi",
#'   multiple = "all"
#' )
#'
#' @export
translate_species_code <- function(
  code,
  from = c(
    "auto",
    "nfi",
    "canfi",
    "jurisdiction",
    "scientificname",
    "englishname",
    "frenchname"
  ),
  jurisdiction = NULL,
  to = c(
    "nfi",
    "canfi",
    "jurisdiction",
    "scientificname",
    "englishname",
    "frenchname"
  ),
  multiple = c("error", "all", "first"),
  unmatched = c("error", "NA"),
  verbose = TRUE
) {
  from <- match.arg(
    tolower(from),
    c(
      "auto",
      "nfi",
      "canfi",
      "jurisdiction",
      "scientificname",
      "englishname",
      "frenchname"
    )
  )
  to <- match.arg(
    tolower(to),
    c(
      "nfi",
      "canfi",
      "jurisdiction",
      "scientificname",
      "englishname",
      "frenchname"
    )
  )
  multiple <- match.arg(tolower(multiple), c("error", "all", "first"))
  unmatched <- match.arg(tolower(unmatched), c("error", "na"))
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    cli::cli_abort("{.arg verbose} must be a single TRUE or FALSE value.")
  }

  if (!is.character(code)) {
    cli::cli_abort("{.arg code} must be a character vector.")
  }

  dictionary <- .get_package_data("species_dictionary")
  lookup <- .get_package_data("species_code_lookup")

  assert_required_cols(
    dictionary,
    c(
      "NFI_code",
      "CommonNameEnglish",
      "CommonNameFrench",
      "ScientificName",
      "Genus",
      "Species",
      "Var"
    ),
    object = "species_dictionary"
  )

  assert_required_cols(
    lookup,
    c("code_system", "jurisdiction", "code", "NFI_code"),
    object = "species_code_lookup"
  )

  lookup_jurisdiction <- as.character(lookup$jurisdiction)
  has_jurisdiction <- !is.na(lookup_jurisdiction)
  lookup_jurisdiction[has_jurisdiction] <- stringr::str_to_lower(
    standardize_jurisdiction_code(lookup_jurisdiction[has_jurisdiction])
  )

  lookup <- lookup |>
    dplyr::mutate(
      code_system = stringr::str_to_lower(as.character(.data$code_system)),
      jurisdiction = lookup_jurisdiction,
      code = stringr::str_to_upper(stringr::str_trim(as.character(.data$code))),
      NFI_code = stringr::str_to_upper(
        stringr::str_trim(as.character(.data$NFI_code))
      )
    )

  if (identical(from, "auto")) {
    inferred <- .infer_species_code_input(
      code = code,
      lookup = lookup,
      jurisdiction = jurisdiction
    )
    from <- inferred$from
    jurisdiction <- inferred$jurisdiction
    if (isTRUE(verbose)) {
      msg <- if (identical(from, "jurisdiction")) {
        paste0(
          "Auto-detected input type: jurisdiction (",
          jurisdiction[[1]],
          ")"
        )
      } else {
        paste0("Auto-detected input type: ", from)
      }
      cli::cli_inform(msg)
    }
  }

  name_from_map <- c(
    scientificname = "ScientificName",
    englishname = "CommonNameEnglish",
    frenchname = "CommonNameFrench"
  )
  to_map <- c(
    nfi = "NFI_code",
    scientificname = "ScientificName",
    englishname = "CommonNameEnglish",
    frenchname = "CommonNameFrench"
  )
  to_resolved <- if (to %in% names(to_map)) unname(to_map[[to]]) else to

  if (from == "jurisdiction") {
    if (is.null(jurisdiction)) {
      cli::cli_abort(
        "{.arg jurisdiction} must be provided when {.arg from} is {.val jurisdiction}."
      )
    }
    recycled <- assert_len_compat(
      code = code,
      jurisdiction = jurisdiction
    )
    code <- recycled$code
    jurisdiction <- tolower(standardize_jurisdiction_code(
      recycled$jurisdiction
    ))
  } else {
    if (!is.null(jurisdiction) && to != "jurisdiction") {
      cli::cli_abort(
        "{.arg jurisdiction} must be NULL unless {.arg from} or {.arg to} is {.val jurisdiction}."
      )
    }
    if (is.null(jurisdiction)) {
      jurisdiction <- rep(NA_character_, length(code))
    } else {
      recycled <- assert_len_compat(code = code, jurisdiction = jurisdiction)
      jurisdiction <- tolower(standardize_jurisdiction_code(
        recycled$jurisdiction
      ))
    }
  }

  code_std <- if (from %in% names(name_from_map)) {
    .normalize_species_name(code)
  } else {
    switch(
      from,
      nfi = standardize_species_code(code, keep_all = FALSE),
      canfi = stringr::str_to_upper(stringr::str_trim(as.character(code))),
      jurisdiction = stringr::str_to_upper(stringr::str_trim(as.character(
        code
      )))
    )
  }

  key <- tibble::tibble(
    .row = seq_along(code_std),
    code = code_std,
    jurisdiction = jurisdiction
  )

  matches <- if (from == "nfi") {
    key |>
      dplyr::transmute(.row, jurisdiction, NFI_code = code) |>
      dplyr::left_join(dictionary, by = "NFI_code")
  } else if (from %in% names(name_from_map)) {
    from_col <- name_from_map[[from]]

    key |>
      dplyr::rename(.name_key = code) |>
      dplyr::left_join(
        dictionary |>
          dplyr::mutate(.name_key = .normalize_species_name(.data[[from_col]])),
        by = ".name_key"
      ) |>
      dplyr::arrange(.data$.row)
  } else {
    lookup_sub <- lookup |>
      dplyr::filter(.data$code_system == from)

    if (from == "jurisdiction") {
      lookup_sub <- lookup_sub |>
        dplyr::filter(.data$jurisdiction %in% unique(jurisdiction))
    }

    key |>
      dplyr::left_join(lookup_sub, by = c("code", "jurisdiction")) |>
      dplyr::left_join(dictionary, by = "NFI_code")
  }

  out <- vector("list", length(code_std))

  for (i in seq_along(out)) {
    row_matches <- matches |>
      dplyr::filter(.data$.row == i)

    if (from == "jurisdiction") {
      row_matches <- .collapse_jurisdiction_species_matches(row_matches)
    }

    nfi_vals <- row_matches |>
      dplyr::pull(.data$NFI_code) |>
      unique()

    vals <- if (to %in% names(to_map)) {
      row_matches |>
        dplyr::pull(!!rlang::sym(to_resolved)) |>
        unique()
    } else if (to == "canfi") {
      lookup |>
        dplyr::filter(
          .data$code_system == "canfi",
          .data$NFI_code %in% nfi_vals
        ) |>
        dplyr::pull(.data$code) |>
        unique()
    } else {
      if (is.na(jurisdiction[i])) {
        cli::cli_abort(
          c(
            "Target jurisdiction is required.",
            "x" = "{.arg jurisdiction} must be supplied when {.arg to} is {.val jurisdiction}.",
            "i" = "Provide a jurisdiction explicitly, or use {.arg from = \"auto\"} with inputs that infer one."
          )
        )
      }

      lookup |>
        dplyr::filter(
          .data$code_system == "jurisdiction",
          !is.na(.data$jurisdiction)
        ) |>
        dplyr::transmute(
          jurisdiction = stringr::str_to_lower(
            stringr::str_trim(as.character(.data$jurisdiction))
          ),
          NFI_code = stringr::str_to_upper(
            stringr::str_trim(as.character(.data$NFI_code))
          ),
          code = stringr::str_to_upper(
            stringr::str_trim(as.character(.data$code))
          )
        ) |>
        dplyr::filter(
          .data$jurisdiction == stringr::str_to_lower(.env$jurisdiction[i]),
          .data$NFI_code %in% stringr::str_to_upper(
            stringr::str_trim(as.character(.env$nfi_vals))
          )
        ) |>
        dplyr::pull(.data$code) |>
        unique()
    }

    vals <- vals[!is.na(vals)]

    if (length(vals) == 0L) {
      if (identical(unmatched, "na")) {
        out[[i]] <- NA_character_
        next
      }

      ctx <- paste0("`", code[i], "`")
      if (from == "jurisdiction") {
        ctx <- paste0(ctx, " for jurisdiction `", jurisdiction[i], "`")
      }

      cli::cli_abort(
        c(
          "Unknown species code.",
          "x" = paste0("No match found for ", ctx, ".")
        ),
        class = "ctae_unknown_species_code"
      )
    }

    if (length(vals) > 1L) {
      if (identical(multiple, "error")) {
        ctx <- paste0("`", code[i], "`")
        if (from == "jurisdiction") {
          # nocov start
          ctx <- paste0(ctx, " for jurisdiction `", jurisdiction[i], "`")
          # nocov end
        }

        cli::cli_abort(
          c(
            "Ambiguous species code.",
            "x" = paste0("Species code ", ctx, " maps to multiple values."),
            "i" = paste0("Requested field: ", to_resolved),
            "i" = paste0("Matches: ", paste(vals, collapse = ", "))
          ),
          class = "ctae_ambiguous_species_code"
        )
      }

      if (identical(multiple, "first")) {
        out[[i]] <- vals[[1]]
        next
      }

      out[[i]] <- vals
      next
    }

    out[[i]] <- vals[[1]]
  }

  if (identical(multiple, "all")) {
    return(out)
  }

  unlist(out, use.names = FALSE)
}


# internal
.normalize_species_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_squish() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower()
}


# internal
.collapse_jurisdiction_species_matches <- function(tbl) {
  if (!"NFI_code" %in% names(tbl) || nrow(tbl) <= 1L) {
    return(tbl)
  }

  nfi_vals <- tbl |>
    dplyr::pull(.data$NFI_code) |>
    unique()

  nfi_vals <- nfi_vals[!is.na(nfi_vals)]
  if (length(nfi_vals) <= 1L) {
    return(tbl)
  }

  child_vals <- vapply(
    nfi_vals,
    function(one_code) {
      any(startsWith(nfi_vals, paste0(one_code, ".")))
    },
    logical(1)
  )

  parent_vals <- nfi_vals[child_vals]
  if (length(parent_vals) == 0L) {
    return(tbl)
  }

  tbl |>
    dplyr::filter(is.na(.data$NFI_code) | .data$NFI_code %in% parent_vals)
}


# internal
.infer_species_code_input <- function(code, lookup, jurisdiction = NULL) {
  code_trim <- stringr::str_to_upper(stringr::str_trim(as.character(code)))

  is_nfi <- !is.na(code_trim) &
    stringr::str_detect(
      code_trim,
      "^[A-Z]{4}\\.(?:[A-Z]{3}|SPP)(?:\\.[A-Z]{3})?$"
    )
  is_canfi <- !is.na(code_trim) & stringr::str_detect(code_trim, "^[0-9]+$")

  code_class <- ifelse(is_nfi, "nfi", ifelse(is_canfi, "canfi", "other"))
  distinct_classes <- unique(code_class[!is.na(code_class)])

  if (length(distinct_classes) == 1L && identical(distinct_classes, "nfi")) {
    if (!is.null(jurisdiction)) {
      cli::cli_abort(
        "{.arg jurisdiction} must be NULL when auto-detected {.arg from} is {.val nfi}."
      )
    }
    return(list(from = "nfi", jurisdiction = NULL))
  }

  if (length(distinct_classes) == 1L && identical(distinct_classes, "canfi")) {
    if (!is.null(jurisdiction)) {
      cli::cli_abort(
        "{.arg jurisdiction} must be NULL when auto-detected {.arg from} is {.val canfi}."
      )
    }
    return(list(from = "canfi", jurisdiction = NULL))
  }

  if (
    length(distinct_classes) > 1L &&
      any(distinct_classes %in% c("nfi", "canfi"))
  ) {
    cli::cli_abort(
      c(
        "Cannot auto-detect a single source system.",
        "x" = "Input codes mix NFI, CANFI, or other code formats.",
        "i" = "Supply {.arg from} explicitly."
      ),
      class = "ctae_ambiguous_species_source"
    )
  }

  if (!is.null(jurisdiction)) {
    jurisdiction_std <- tolower(standardize_jurisdiction_code(jurisdiction))
    return(list(from = "jurisdiction", jurisdiction = jurisdiction_std))
  }

  juris_lookup <- lookup |>
    dplyr::filter(.data$code_system == "jurisdiction") |>
    dplyr::mutate(code = stringr::str_to_upper(stringr::str_trim(.data$code)))

  possible_jurisdictions <- lapply(code_trim, function(one_code) {
    juris_lookup |>
      dplyr::filter(.data$code == one_code) |>
      dplyr::pull(.data$jurisdiction) |>
      unique()
  })

  empty_matches <- vapply(possible_jurisdictions, length, integer(1)) == 0L
  if (any(empty_matches)) {
    bad <- unique(code[empty_matches])
    cli::cli_abort(
      c(
        "Cannot auto-detect jurisdiction.",
        "x" = paste0(
          "No jurisdiction match found for: ",
          paste(bad, collapse = ", ")
        ),
        "i" = "Supply {.arg from} and {.arg jurisdiction} explicitly."
      ),
      class = "ctae_unknown_species_code"
    )
  }

  common_jurisdictions <- Reduce(intersect, possible_jurisdictions)

  if (length(common_jurisdictions) == 1L) {
    return(list(from = "jurisdiction", jurisdiction = common_jurisdictions))
  }

  if (length(common_jurisdictions) == 0L) {
    cli::cli_abort(
      c(
        "Cannot auto-detect a common jurisdiction.",
        "x" = "The supplied codes do not resolve to a single shared jurisdiction.",
        "i" = "Supply {.arg jurisdiction} explicitly."
      ),
      class = "ctae_ambiguous_species_source"
    )
  }

  cli::cli_abort(
    c(
      "Cannot auto-detect jurisdiction uniquely.",
      "x" = paste0(
        "Possible jurisdictions: ",
        paste(sort(common_jurisdictions), collapse = ", ")
      ),
      "i" = "Supply {.arg jurisdiction} explicitly."
    ),
    class = "ctae_ambiguous_species_source"
  )
}
