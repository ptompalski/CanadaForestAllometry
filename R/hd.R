#' Predict tree height: choose best available DBH–height model or run all candidates
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `hd()` is a convenience wrapper that predicts total tree height (m) from DBH
#' by selecting, per tree, one (or more) of the DBH–height (`hd_*()`) models
#' listed in [hd_model_registry()]. It is the DBH–height counterpart of
#' [vol()].
#'
#' @section Model selection logic:
#' Selection is registry-driven and occurs per input tree:
#' \enumerate{
#'   \item Geographic applicability: the model's `province_scope` must include
#'     `jurisdiction`.
#'   \item Species coverage: the model must have coefficients for `species`
#'     (exact match; no genus-level fallback).
#'   \item Stand origin: plantation-only models are used only when
#'     `stand_origin = "plantation"`. For plantation trees, models fitted to
#'     plantations (or to both origins) are preferred; natural-stand models are
#'     used only as a fallback, with a warning.
#'   \item British Columbia coast vs interior: models fitted to coastal or
#'     interior BC only (see `bc_region` in [hd_model_registry()]) are
#'     restricted to trees in that region when `bec_zone` or `bc_region` is
#'     given. When neither is given, they remain eligible and a warning is
#'     issued if one is used.
#'   \item Required predictors: every required predictor of the model (see
#'     [get_hd_model_info()]) must be supplied in `...` and non-missing for the
#'     tree. Optional predictors (e.g. a region code) are passed on when
#'     non-missing.
#'   \item Ranking: among eligible models the highest `rank` is used. If the
#'     model fails for a tree (e.g. a species-specific predictor is missing, or
#'     a region code is not recognised), the next-ranked model is tried.
#' }
#'
#' @section Important limitations and recommended use:
#' \itemize{
#'   \item Predictor definitions differ between sources. For example, basal
#'     area (`BA`) may be all-species or species-specific and computed with
#'     different minimum DBH thresholds; dominant height (`HDOM`) is defined
#'     differently by each source. `hd()` passes the same value to every
#'     model, so check the definitions with [get_hd_model_info()] before
#'     supplying stand-level predictors.
#'   \item The ranking is a pragmatic default (more stand information, newer
#'     and more local models are preferred). If you require a specific model,
#'     call the `hd_*()` function directly.
#'   \item Run with `pick_best = FALSE` during exploratory analyses to inspect
#'     which models are eligible and how sensitive results are to model choice.
#'   \item Models' out-of-range warnings are passed through unchanged; their
#'     informational messages (e.g. which internal variant was used) are
#'     suppressed, since `model_id` reports the model used.
#' }
#'
#' @param DBH Numeric vector. Diameter at breast height (cm).
#' @param species Character vector. NFI species codes (e.g. `"PICE.MAR"`).
#' @param jurisdiction Character vector. Province/territory code (e.g. `"ON"`).
#' @param ... Named predictors, using the `hd_*()` argument names (e.g.
#'   `BA`, `HDOM`, `TPH`, `natural_subregion`, `ecoregion`). Each must have
#'   length 1 or `length(DBH)`; `NA` marks the predictor as unavailable for
#'   that tree. See [hd_model_registry()] for the predictors each model uses.
#' @param bec_zone Optional character vector. BC biogeoclimatic zone code
#'   (e.g. `"CWH"`, `"IDF"`), used to infer `bc_region`: `CWH`, `CDF`, `MH`
#'   and `CMA` are coastal; `BG`, `BWBS`, `ESSF`, `ICH`, `IDF`, `IMA`, `MS`,
#'   `PP`, `SBPS`, `SBS`, `SWB` and `BAFA` are interior. Ignored outside BC.
#' @param bc_region Optional character vector, `"coastal"` or `"interior"`.
#'   Alternative to `bec_zone`; if both are given they must agree. Ignored
#'   outside BC.
#' @param stand_origin Character vector, `"natural"` (default) or
#'   `"plantation"`. `NA` is treated as `"natural"`.
#' @param pick_best Logical. If `TRUE` (default), use the single best model per
#'   tree. If `FALSE`, run all eligible models and return a list (one tibble per
#'   tree), suitable as a list-column.
#' @param keep_model_id Logical. If `TRUE` (default), include a `model_id`
#'   column with the registry model ID used.
#'
#' @return
#' If `pick_best = TRUE`, a tibble with one row per tree and column `height`
#' (m), plus `model_id` if requested. Trees for which no model is eligible or
#' succeeds get `NA` (with a warning). Warnings are also issued when a
#' plantation tree uses a natural-stand model, or when a coastal- or
#' interior-only BC model is used without `bec_zone`/`bc_region`.
#'
#' If `pick_best = FALSE`, a list with one tibble per tree containing the
#' outputs of every successful candidate model (`height`, and optionally
#' `model_id`), ordered from most to least preferred.
#'
#' @seealso [hd_model_registry()], [find_hd_models()], [get_hd_model_info()]
#' @examples
#' ## DBH only: best available DBH-only model
#' hd(DBH = c(12, 25), species = "PICE.MAR", jurisdiction = c("ON", "AB"))
#'
#' ## Stand predictors unlock stand-level models where available
#' hd(
#'   DBH = 20, species = "PICE.MAR", jurisdiction = "ON",
#'   HDOM = 18, BA = 25, TPH = 1200
#' )
#'
#' ## BC: the BEC zone (or bc_region) selects coastal vs interior models
#' hd(DBH = c(30, 30), species = "PSEU.MEN", jurisdiction = "BC", bec_zone = c("CWH", "IDF"))
#'
#' ## Compare all eligible models
#' hd(
#'   DBH = 20, species = "PICE.MAR", jurisdiction = "ON",
#'   HDOM = 18, BA = 25, TPH = 1200, pick_best = FALSE
#' )[[1]]
#'
#' ## Inside dplyr::mutate()
#' trees <- tibble::tibble(
#'   DBH = c(15, 22, 30),
#'   species = c("PINU.BAN", "PICE.GLA", "PICE.MAR"),
#'   jurisdiction = c("ON", "AB", "AB"),
#'   natural_subregion = c(NA, "CM", "LF")
#' )
#' trees |>
#'   dplyr::mutate(hd(DBH, species, jurisdiction,
#'     natural_subregion = natural_subregion
#'   ))
#' @export
hd <- function(
  DBH,
  species,
  jurisdiction,
  ...,
  bec_zone = NA_character_,
  bc_region = NA_character_,
  stand_origin = "natural",
  pick_best = TRUE,
  keep_model_id = TRUE
) {
  assert_numeric_vec(DBH, "DBH")
  if (!is.logical(pick_best) || length(pick_best) != 1L || is.na(pick_best)) {
    cli::cli_abort("{.arg pick_best} must be TRUE or FALSE.")
  }
  if (!is.logical(keep_model_id) || length(keep_model_id) != 1L || is.na(keep_model_id)) {
    cli::cli_abort("{.arg keep_model_id} must be TRUE or FALSE.")
  }

  n <- length(DBH)
  if (n == 0L) {
    cli::cli_abort("{.arg DBH} must have length > 0.")
  }

  preds <- .hd_model_predictors()
  dots <- list(...)
  .hd_check_dots(dots, valid = setdiff(unique(preds$arg), c("DBH", "bec_zone")))

  recycle <- function(x, arg) {
    if (length(x) == 1L) return(rep(x, n))
    if (length(x) == n) return(x)
    cli::cli_abort("{.arg {arg}} must have length 1 or {n} (got {length(x)}).")
  }
  species <- recycle(species, "species")
  jurisdiction <- recycle(jurisdiction, "jurisdiction")
  stand_origin <- recycle(stand_origin, "stand_origin")
  bec_zone <- recycle(bec_zone, "bec_zone")
  bc_region <- recycle(bc_region, "bc_region")
  dots <- purrr::imap(dots, recycle)

  if (!is.character(species) && !is.factor(species)) {
    cli::cli_abort("{.arg species} must be a character vector.")
  }
  species <- standardize_species_code(species, keep_all = FALSE)

  if (!is.character(jurisdiction)) {
    cli::cli_abort("{.arg jurisdiction} must be a character vector.")
  }
  ok_j <- !is.na(jurisdiction)
  jurisdiction[ok_j] <- standardize_jurisdiction_code(jurisdiction[ok_j])

  stand_origin <- as.character(stand_origin)
  stand_origin[is.na(stand_origin)] <- "natural"
  bad <- setdiff(unique(stand_origin), c("natural", "plantation"))
  if (length(bad)) {
    cli::cli_abort(c(
      "{.arg stand_origin} must be {.val natural} or {.val plantation}.",
      "x" = "Invalid value{?s}: {.val {bad}}."
    ))
  }

  tree_bc_region <- .hd_tree_bc_region(bec_zone, bc_region, jurisdiction)
  if (any(!is.na(bec_zone))) dots$bec_zone <- bec_zone

  reg <- hd_model_registry_species()
  cand <- .hd_candidates(
    reg, DBH, species, jurisdiction, stand_origin, tree_bc_region, dots
  )

  if (isTRUE(pick_best)) {
    height <- rep(NA_real_, n)
    model_id <- rep(NA_character_, n)
    sel <- rep(NA_integer_, n)
    pending <- cand
    while (nrow(pending) > 0L) {
      # each unresolved tree tries its next-best candidate
      pending <- pending[order(pending$.row, -pending$.score), ]
      try_now <- pending[!duplicated(pending$.row), ]
      pending <- pending[duplicated(pending$.row), ]

      for (m in unique(try_now$.reg_i)) {
        rows <- try_now$.row[try_now$.reg_i == m]
        h <- .hd_run_model(reg[m, ], rows, DBH, species, dots)
        ok <- is.finite(h)
        height[rows[ok]] <- h[ok]
        model_id[rows[ok]] <- reg$model_id[[m]]
        sel[rows[ok]] <- m
      }
      pending <- pending[is.na(height[pending$.row]), ]
    }

    sel_origin <- reg$stand_origin[sel]
    fallback <- which(!is.na(sel) & stand_origin == "plantation" & sel_origin == "natural")
    if (length(fallback)) {
      rows_txt <- .hd_fmt_rows(fallback)
      cli::cli_warn(c(
        "{.fn hd}: no plantation model was available for {length(fallback)} plantation tree{?s}; a natural-stand model was used.",
        "i" = "Rows: {rows_txt}."
      ))
    }

    sel_bc <- reg$bc_region[sel]
    bc_unknown <- which(!is.na(sel) & !is.na(sel_bc) & is.na(tree_bc_region))
    if (length(bc_unknown)) {
      rows_txt <- .hd_fmt_rows(bc_unknown)
      used <- unique(paste0(reg$model_id[sel[bc_unknown]], " (", sel_bc[bc_unknown], " BC)"))
      cli::cli_warn(c(
        "{.fn hd}: {length(bc_unknown)} BC tree{?s} used a model fitted to only part of BC: {used}.",
        "i" = "Rows: {rows_txt}.",
        "i" = "Supply {.arg bec_zone} or {.arg bc_region} to restrict models to the tree's region."
      ))
    }

    missing <- which(is.na(height) & !is.na(DBH))
    if (length(missing)) {
      rows_txt <- .hd_fmt_rows(missing)
      cli::cli_warn(c(
        "{.fn hd}: no DBH\u2013height model was eligible or succeeded for {length(missing)} tree{?s}; height set to NA.",
        "i" = "Rows: {rows_txt}.",
        "i" = "Use {.fn find_hd_models} to see which models cover these species/jurisdictions and which predictors they need."
      ))
    }

    out <- tibble::tibble(height = height)
    if (isTRUE(keep_model_id)) out$model_id <- model_id
    return(out)
  }

  # ---- pick_best = FALSE: run every candidate ----
  res <- vector("list", length(unique(cand$.reg_i)))
  for (k in seq_along(res)) {
    m <- unique(cand$.reg_i)[[k]]
    sub <- cand[cand$.reg_i == m, ]
    sub$height <- .hd_run_model(reg[m, ], sub$.row, DBH, species, dots)
    sub$model_id <- reg$model_id[[m]]
    res[[k]] <- sub
  }
  res <- dplyr::bind_rows(res)
  res <- res[is.finite(res$height), ]
  res <- res[order(res$.row, -res$.score), ]

  proto <- tibble::tibble(height = numeric(0), model_id = character(0))
  if (!isTRUE(keep_model_id)) proto$model_id <- NULL
  out <- vector("list", n)
  split_res <- split(res, res$.row)
  for (i in seq_len(n)) {
    df <- split_res[[as.character(i)]]
    out[[i]] <- if (is.null(df)) {
      proto
    } else {
      tibble::tibble(height = df$height, model_id = df$model_id)[names(proto)]
    }
  }
  out
}

# internal
.hd_fmt_rows <- function(rows, max_rows = 10L) {
  paste0(
    paste(utils::head(rows, max_rows), collapse = ", "),
    if (length(rows) > max_rows) ", ..." else ""
  )
}

# internal
# BEC zone -> BC coastal/interior. "AT" (legacy Alpine Tundra) spans both.
.hd_bec_regions <- c(
  CWH = "coastal", CDF = "coastal", MH = "coastal", CMA = "coastal",
  BG = "interior", BWBS = "interior", ESSF = "interior", ICH = "interior",
  IDF = "interior", IMA = "interior", MS = "interior", PP = "interior",
  SBPS = "interior", SBS = "interior", SWB = "interior", BAFA = "interior",
  AT = NA_character_
)

# internal
# Per-tree BC region ("coastal"/"interior"/NA) from `bec_zone` and/or
# `bc_region`; NA outside BC.
.hd_tree_bc_region <- function(bec_zone, bc_region, jurisdiction,
                               call = rlang::caller_env()) {
  bec <- toupper(trimws(as.character(bec_zone)))
  bad <- setdiff(unique(bec[!is.na(bec)]), names(.hd_bec_regions))
  if (length(bad)) {
    cli::cli_abort(c(
      "Unknown {.arg bec_zone} code{?s}: {.val {bad}}.",
      "i" = "Valid codes: {.val {names(.hd_bec_regions)}}."
    ), call = call)
  }
  from_bec <- unname(.hd_bec_regions[bec])

  region <- tolower(trimws(as.character(bc_region)))
  bad <- setdiff(unique(region[!is.na(region)]), c("coastal", "interior"))
  if (length(bad)) {
    cli::cli_abort(
      "{.arg bc_region} must be {.val coastal} or {.val interior}, not {.val {bad}}.",
      call = call
    )
  }

  clash <- which(!is.na(from_bec) & !is.na(region) & from_bec != region)
  if (length(clash)) {
    rows_txt <- .hd_fmt_rows(clash)
    cli::cli_abort(c(
      "{.arg bec_zone} and {.arg bc_region} disagree for {length(clash)} tree{?s}.",
      "i" = "Rows: {rows_txt}."
    ), call = call)
  }

  out <- ifelse(is.na(region), from_bec, region)
  out[is.na(jurisdiction) | jurisdiction != "BC"] <- NA_character_
  out
}

# internal
.hd_check_dots <- function(dots, valid, call = rlang::caller_env()) {
  if (length(dots) == 0L) return(invisible())
  nms <- names(dots)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort("All predictors in {.arg ...} must be named.", call = call)
  }
  if (anyDuplicated(nms)) {
    cli::cli_abort("Duplicated predictor{?s} in {.arg ...}: {.arg {unique(nms[duplicated(nms)])}}.", call = call)
  }
  unknown <- setdiff(nms, valid)
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unknown predictor{?s} in {.arg ...}: {.arg {unknown}}.",
      "i" = "Valid predictors (case-sensitive): {.arg {sort(valid)}}."
    ), call = call)
  }
  invisible()
}

# internal
# One row per eligible (tree, registry row) pair with a preference score.
.hd_candidates <- function(reg, DBH, species, jurisdiction, stand_origin,
                           tree_bc_region, dots) {
  n <- length(DBH)
  avail <- function(arg) {
    v <- dots[[arg]]
    if (is.null(v)) rep(FALSE, n) else !is.na(v)
  }

  out <- vector("list", nrow(reg))
  for (m in seq_len(nrow(reg))) {
    prov <- normalize_province_scope(reg$province_scope[[m]])
    origin <- reg$stand_origin[[m]]

    ok <- !is.na(DBH) &
      jurisdiction %in% prov &
      species %in% reg$species[[m]] &
      (origin != "plantation" | stand_origin == "plantation") &
      (is.na(reg$bc_region[[m]]) | is.na(tree_bc_region) |
        tree_bc_region == reg$bc_region[[m]])
    for (arg in setdiff(reg$required_predictors[[m]], "DBH")) {
      ok <- ok & avail(arg)
    }

    rows <- which(ok)
    if (length(rows) == 0L) next
    # plantation trees: prefer plantation/both models over natural-only ones
    origin_match <- origin == "both" | origin == stand_origin[rows]
    out[[m]] <- tibble::tibble(
      .row = rows,
      .reg_i = m,
      .score = origin_match * 1000 + reg$rank[[m]]
    )
  }
  dplyr::bind_rows(
    tibble::tibble(.row = integer(), .reg_i = integer(), .score = numeric()),
    out
  )
}

# internal
# Runs one registry model on the given rows. Returns heights (NA where the
# model failed). Rows are grouped by which optional predictors are available,
# so each engine call sees only the predictors it should use.
.hd_run_model <- function(reg_row, rows, DBH, species, dots) {
  fn <- get(reg_row$engine[[1]], envir = asNamespace("CanadaForestAllometry"))
  req <- setdiff(reg_row$required_predictors[[1]], "DBH")
  opt <- intersect(reg_row$optional_predictors[[1]], names(dots))

  pattern <- if (length(opt)) {
    do.call(paste, c(lapply(opt, function(a) !is.na(dots[[a]][rows])), sep = "|"))
  } else {
    rep("", length(rows))
  }

  call_engine <- function(idx) {
    use_opt <- opt[vapply(opt, function(a) !is.na(dots[[a]][idx[[1]]]), logical(1))]
    args <- c(
      list(DBH = DBH[idx], species = species[idx]),
      lapply(dots[c(req, use_opt)], `[`, idx),
      reg_row$fixed_args[[1]]
    )
    h <- tryCatch(
      suppressMessages(do.call(fn, args))$height,
      error = function(e) NULL
    )
    if (length(h) != length(idx)) NULL else as.numeric(h)
  }

  height <- rep(NA_real_, length(rows))
  for (key in unique(pattern)) {
    pos <- which(pattern == key)
    h <- call_engine(rows[pos])
    if (is.null(h) && length(pos) > 1L) {
      # isolate trees that make the engine fail (e.g. species-specific predictors)
      h <- vapply(pos, function(p) call_engine(rows[p]) %||% NA_real_, numeric(1))
    }
    if (!is.null(h)) height[pos] <- h
  }
  height
}
