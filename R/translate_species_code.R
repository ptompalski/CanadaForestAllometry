#' Translate species codes across supported code systems
#'
#' Translate species codes from NFI, CANFI, or jurisdiction-specific code
#' systems into canonical NFI codes or species metadata stored in the package
#' dictionary.
#'
#' @param code A character vector of species codes to translate.
#' @param from Source code system: one of `"auto"`, `"nfi"`, `"canfi"`, or
#'   `"jurisdiction"`.
#' @param jurisdiction Jurisdiction for provincial or territorial codes. Required
#'   when `from = "jurisdiction"`. May be length 1 or the same length as `code`.
#' @param to Target field to return. Must be one of the columns in
#'   `species_dictionary`. Defaults to `"NFI_code"`.
#' @param multiple How to handle ambiguous matches: `"error"`, `"all"`, or
#'   `"first"`.
#' @param unmatched How to handle unmatched inputs: `"error"` or `"NA"`.
#'
#' @return
#' A character vector when `multiple` is `"error"` or `"first"`. A list of
#' character vectors when `multiple = "all"`.
#'
#' @examples
#' translate_species_code("ABIE.BAL", from = "nfi", to = "CommonNameEnglish")
#' translate_species_code("ABIE.BAL", from = "nfi", to = "CommonNameFrench")
#' translate_species_code("BF", from = "jurisdiction", jurisdiction = "ON")
#' translate_species_code("302", from = "canfi")
#' translate_species_code("302", from = "auto")
#' translate_species_code("PICE.GLA", from = "auto", to = "ScientificName")
#' translate_species_code(c("BF", "PJ", "SB"))
#' translate_species_code(
#'   "SW",
#'   from = "jurisdiction",
#'   jurisdiction = "BC",
#'   to = "ScientificName"
#' )
#'
#' # Ambiguous CANFI code: errors by default
#' try(
#'   translate_species_code("104", from = "canfi")
#' )
#'
#' # Return the first match
#' translate_species_code("104", from = "canfi", multiple = "first")
#'
#' # Return all matches as a list
#' translate_species_code("104", from = "canfi", multiple = "all")
#'
#' # Vectorized input with mixed ambiguity handling
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
  from = c("auto", "nfi", "canfi", "jurisdiction"),
  jurisdiction = NULL,
  to = "NFI_code",
  multiple = c("error", "all", "first"),
  unmatched = c("error", "NA")
) {
  from <- match.arg(tolower(from), c("auto", "nfi", "canfi", "jurisdiction"))
  multiple <- match.arg(tolower(multiple), c("error", "all", "first"))
  unmatched <- match.arg(tolower(unmatched), c("error", "na"))

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

  assert_choice(to, "to", names(dictionary))

  if (identical(from, "auto")) {
    inferred <- .infer_species_code_input(
      code = code,
      lookup = lookup,
      jurisdiction = jurisdiction
    )
    from <- inferred$from
    jurisdiction <- inferred$jurisdiction
  }

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
    if (!is.null(jurisdiction)) {
      cli::cli_abort(
        "{.arg jurisdiction} must be NULL unless {.arg from} is {.val jurisdiction}."
      )
    }
    jurisdiction <- rep(NA_character_, length(code))
  }

  code_std <- switch(
    from,
    nfi = standardize_species_code(code, keep_all = FALSE),
    canfi = stringr::str_to_upper(stringr::str_trim(as.character(code))),
    jurisdiction = stringr::str_to_upper(stringr::str_trim(as.character(code)))
  )

  key <- tibble::tibble(
    .row = seq_along(code_std),
    code = code_std,
    jurisdiction = jurisdiction
  )

  matches <- if (from == "nfi") {
    key %>%
      dplyr::transmute(.row, NFI_code = code) %>%
      dplyr::left_join(dictionary, by = "NFI_code")
  } else {
    lookup_sub <- lookup %>%
      dplyr::filter(.data$code_system == from)

    if (from == "jurisdiction") {
      lookup_sub <- lookup_sub %>%
        dplyr::filter(.data$jurisdiction %in% unique(jurisdiction))
    }

    key %>%
      dplyr::left_join(lookup_sub, by = c("code", "jurisdiction")) %>%
      dplyr::left_join(dictionary, by = "NFI_code")
  }

  out <- vector("list", length(code_std))

  for (i in seq_along(out)) {
    vals <- matches %>%
      dplyr::filter(.data$.row == i) %>%
      dplyr::pull(!!rlang::sym(to)) %>%
      unique()

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
            "i" = paste0("Requested field: ", to),
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

  juris_lookup <- lookup %>%
    dplyr::filter(.data$code_system == "jurisdiction") %>%
    dplyr::mutate(code = stringr::str_to_upper(stringr::str_trim(.data$code)))

  possible_jurisdictions <- lapply(code_trim, function(one_code) {
    juris_lookup %>%
      dplyr::filter(.data$code == one_code) %>%
      dplyr::pull(.data$jurisdiction) %>%
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
