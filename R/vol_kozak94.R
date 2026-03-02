#' Estimate tree volume using Kozak (1994) taper model (BC, BEC-zone specific)
#'
#' Implements the Kozak (1994) taper/volume formulation for British Columbia,
#' using BEC-zone–specific parameter sets. Total volume is computed independent
#' of merchantability; merchantable volume is set to 0 when DBH is below the
#' jurisdictional minimum DBH criterion.
#'
#' @param DBH Numeric vector of diameter at breast height (cm).
#' @param height Numeric vector of total tree height (m).
#' @param species Character vector of species codes (e.g. "TSUG.HET").
#' @param BEC_zone Character vector of BEC zone codes (e.g., "CWH", "ICH", "IDF").
#'
#' @return A tibble with volumes (m^3): total, merchantable.
#' @export
vol_kozak94 <- function(DBH, height, species, BEC_zone) {
  n <- length(DBH)
  if (length(height) != n || length(species) != n || length(BEC_zone) != n) {
    rlang::abort(
      "DBH, height, species, and BEC_zone must have the same length."
    )
  }

  species_std <- standardize_species_code(species)
  bec_std <- stringr::str_squish(stringr::str_to_upper(BEC_zone))

  # ---- constants from original Kozak94 implementation ----
  SECLENGTH <- 1
  LOGLENGTH <- 5
  cons <- 0.00007854
  per <- (1.0 - sqrt(0.01))

  # ---- internal: structured abort with context ----
  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_kozak94() failed for row ",
      i,
      " (species=",
      species_std[i],
      ", BEC_zone=",
      bec_std[i],
      ", DBH_cm=",
      DBH[i],
      ", ht_m=",
      height[i],
      "): ",
      msg
    ))
  }

  # ---- internal: DIB at height h (m), inside bark (cm) ----
  kozak_dib <- function(h, HT, dbhht, ff, p, bias_factor) {
    xq <- h / HT
    # Internal-call guard: h is always > 0 in current call paths and HT is
    # validated/clamped > 0, so this is defensive-only.
    # nocov start
    if (!is.finite(xq) || xq <= 0) {
      return(NA_real_)
    }
    # nocov end

    xl <- 1.0 - sqrt(xq)
    xx <- xl / per
    # Internal-call guard: integration bounds keep h <= HT, so xx < 0 is
    # defensive-only for unexpected parameter/state corruption.
    # nocov start
    if (xx < 0) {
      xx <- 0
    }
    # nocov end
    xl <- asin(xl)
    xb <- 1.0 / (xq + dbhht)

    expon <- p$b0 +
      p$b1 * xq^0.25 +
      p$b2 * xq^(1 / 3) +
      p$b3 * xq^0.5 +
      p$b4 * xl +
      p$b5 * xb +
      p$b6 * HT

    dib <- bias_factor * ff * xx^expon
    if (!is.finite(dib)) {
      return(NA_real_)
    }
    dib
  }

  # ---- internal: Smalian integration from x1 to xt (m) ----
  kozak_smalian <- function(x1, xt, di1, p, HT, dbhht, ff, bias_factor) {
    # Internal-call guard: callers enforce ordered, finite segment bounds.
    # nocov start
    if (!is.finite(x1) || !is.finite(xt) || xt < x1) {
      return(c(v = NA_real_, di2 = NA_real_))
    }
    # nocov end
    # Internal-call guard: callers provide non-negative, finite starting DIB.
    # nocov start
    if (!is.finite(di1) || di1 < 0) {
      return(c(v = NA_real_, di2 = NA_real_))
    }
    # nocov end

    v <- 0.0
    repeat {
      x2 <- x1 + SECLENGTH
      if (x2 > xt) {
        x2 <- xt
      }

      di2 <- kozak_dib(x2, HT, dbhht, ff, p, bias_factor)
      # nocov start
      # Defensive-only: internal callers pass validated parameters and segment
      # bounds, so di2 non-finite is not expected with shipped parameter tables.
      if (!is.finite(di2)) {
        return(c(v = NA_real_, di2 = NA_real_))
      }

      inc <- cons * (x2 - x1) * (di1^2 + di2^2) * 0.5
      # Defensive-only for unexpected numeric overflow/underflow.
      if (!is.finite(inc)) {
        return(c(v = NA_real_, di2 = NA_real_))
      }
      # nocov end

      v <- v + inc

      if ((x1 + SECLENGTH) > xt) {
        break
      }
      x1 <- x2
      di1 <- di2
    }
    c(v = v, di2 = di2)
  }

  # ---- internal: solve relative height hm1 where DIB == topdbh ----
  solve_hm1 <- function(p, HT, dbhht, ff, bias_factor, topdbh) {
    hm1 <- 0.9
    maxiter <- 50
    tolcrit <- 1e-4

    for (nn in seq_len(maxiter)) {
      xl <- asin(1.0 - sqrt(hm1))
      xb <- 1 / (hm1 + dbhht)

      expon <- p$b0 +
        p$b1 * hm1^0.25 +
        p$b2 * hm1^(1 / 3) +
        p$b3 * hm1^0.5 +
        p$b4 * xl +
        p$b5 * xb +
        p$b6 * HT

      if (!is.finite(expon) || expon == 0) {
        return(NA_real_)
      }

      hm2 <- (topdbh / (bias_factor * ff))^(1 / expon)
      hm2 <- (1 - hm2 * per)^2

      # nocov start
      # Defensive-only: with validated inputs and shipped coefficients, hm2 is
      # expected finite within solver iterations.
      if (!is.finite(hm2)) {
        return(NA_real_)
      }
      # nocov end
      if (abs(hm2 - hm1) < tolcrit) {
        break
      }

      hm1 <- hm1 + (hm2 - hm1) / 2
      hm1 <- max(min(hm1, 1), 0)
    }

    hm1
  }

  # ---- cache merch criteria once per species x BEC_zone ----
  # Use `.data$...` inside dplyr verbs to avoid R CMD check NSE notes.
  mc_cache <- dplyr::tibble(
    .species_key = species_std,
    .bec_key = bec_std
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      mc = purrr::map2(
        .data$.species_key,
        .data$.bec_key,
        ~ get_merch_criteria(
          "BC",
          species = .x,
          BEC_zone = .y,
          verbose = FALSE
        ) |>
          dplyr::slice(1)
      )
    ) |>
    tidyr::unnest(mc)

  req <- c(".species_key", "stumpht_m", "topdbh_cm", "mindbh_cm")
  miss <- setdiff(req, names(mc_cache))
  if (length(miss) > 0) {
    rlang::abort(paste0(
      "get_merch_criteria() missing columns: ",
      paste(miss, collapse = ", ")
    ))
  }

  # ---- outputs ----
  vol_total <- numeric(n)
  vol_merch <- numeric(n)
  vol_stump <- numeric(n)
  vol_top <- numeric(n)

  for (i in seq_len(n)) {
    dbh <- DBH[i]
    HT <- height[i]

    if (is.na(dbh) || is.na(HT) || !is.finite(dbh) || !is.finite(HT)) {
      abort_i(i, "DBH and height must be finite numeric values (not NA/Inf).")
    }
    if (dbh <= 0) {
      abort_i(i, "DBH must be > 0.")
    }
    if (HT <= 0) {
      abort_i(i, "height must be > 0.")
    }
    if (HT < 1.3) {
      HT <- 1.3
    }

    # merch criteria
    mc <- mc_cache |>
      dplyr::filter(
        .data$.species_key == species_std[i],
        .data$.bec_key == bec_std[i]
      ) |>
      dplyr::slice(1)

    if (nrow(mc) == 0) {
      abort_i(i, "No merchantability criteria found for this species in BC.")
    }
    stumpht <- mc$stumpht_m[[1]]
    topdbh <- mc$topdbh_cm[[1]]
    mindbh <- mc$mindbh_cm[[1]]

    if (!is.finite(stumpht) || stumpht < 0) {
      abort_i(i, "Invalid stumpht_m in merch criteria.")
    }
    if (!is.finite(topdbh) || topdbh <= 0) {
      abort_i(i, "Invalid topdbh_cm in merch criteria.")
    }
    if (!is.finite(mindbh) || mindbh < 0) {
      abort_i(i, "Invalid mindbh_cm in merch criteria.")
    }

    # mindbh is a merchantability rule: it should NOT affect total volume
    allow_merch <- (dbh >= mindbh)

    # parameters
    p <- get_volume_params(
      model_id = "regional_kozak94",
      species = species_std[i],
      subregion = bec_std[i]
    )

    if (nrow(p) == 0) {
      abort_i(i, "No Kozak94 parameters found for this species x BEC_zone.")
    }
    if (nrow(p) > 1) {
      abort_i(
        i,
        "Multiple Kozak94 parameter rows returned; expected exactly one."
      )
    }

    # validate required columns
    need_p <- c(
      "a0",
      "a1",
      "a2",
      "b0",
      "b1",
      "b2",
      "b3",
      "b4",
      "b5",
      "b6",
      "log_bias_factor"
    )
    miss_p <- setdiff(need_p, names(p))
    if (length(miss_p) > 0) {
      abort_i(
        i,
        paste0("Missing parameter columns: ", paste(miss_p, collapse = ", "))
      )
    }

    # validate finite params
    for (nm in need_p) {
      if (!is.finite(p[[nm]][[1]])) {
        abort_i(i, paste0("Parameter '", nm, "' is not finite (NA/Inf)."))
      }
    }

    bias_factor <- p$log_bias_factor[[1]]
    if (bias_factor <= 0) {
      abort_i(i, "log_bias_factor must be > 0.")
    }

    dbhht <- dbh / HT
    # Unreachable with validated inputs: DBH > 0 and HT is clamped >= 1.3.
    # nocov start
    if (!is.finite(dbhht)) {
      abort_i(i, "dbh/height ratio is not finite.")
    }
    # nocov end

    ff <- p$a0[[1]] * dbh^(p$a1[[1]]) * (p$a2[[1]]^dbh)
    if (!is.finite(ff) || ff <= 0) {
      abort_i(i, "Form factor is non-finite or <= 0 (check a0/a1/a2).")
    }

    # stump
    dib3 <- kozak_dib(0.3, HT, dbhht, ff, p, bias_factor)
    if (!is.finite(dib3) || dib3 < 0) {
      abort_i(i, "Failed computing DIB at 0.3 m (non-finite/negative).")
    }

    if (stumpht <= 0.3) {
      stmv <- cons * dib3^2 * stumpht
      dsh <- dib3
    } else {
      # nocov start
      # Defensive branch for jurisdictions where stump height exceeds 0.3 m;
      # current BC merch tables use stumpht_m = 0.3.
      stmv <- cons * dib3^2 * stumpht
      v <- kozak_smalian(0.3, stumpht, dib3, p, HT, dbhht, ff, bias_factor)
      if (!is.finite(v[["v"]]) || !is.finite(v[["di2"]])) {
        abort_i(i, "Smalian integration failed for stump section.")
      }
      dsh <- v[["di2"]]
      stmv <- stmv + v[["v"]]
      # nocov end
    }

    # nocov start
    # Defensive-only: stump integration above enforces finite, non-negative
    # volumes/diameters under validated inputs and parameters.
    if (!is.finite(stmv) || stmv < 0) {
      abort_i(i, "Computed stump volume is non-finite/negative.")
    }
    if (!is.finite(dsh) || dsh < 0) {
      abort_i(i, "Computed stump-top diameter is non-finite/negative.")
    }
    # nocov end

    # merchantable + top
    # For extremely short trees, force the non-merchantable path. This avoids
    # platform-dependent tiny positive merchantable volumes near the 1.3 m
    # height clamp where branch decisions can differ by math-library rounding.
    min_merch_length_m <- 1.05
    if (HT <= (stumpht + min_merch_length_m)) {
      v <- kozak_smalian(stumpht, HT, dsh, p, HT, dbhht, ff, bias_factor)
      # nocov start
      if (!is.finite(v[["v"]])) {
        abort_i(
          i,
          "Smalian integration failed for top section (whole stem above stump)."
        )
      }
      # nocov end
      volm <- 0
      topv <- v[["v"]]
    } else {
      # Treat near-threshold stump diameters as non-merchantable to avoid
      # platform-dependent branch flips from tiny floating-point differences.
      topdbh_tol_cm <- 0.05
      if (dsh <= (topdbh + topdbh_tol_cm)) {
        v <- kozak_smalian(stumpht, HT, dsh, p, HT, dbhht, ff, bias_factor)
        # nocov start
        # Defensive-only: whole-stem-above-stump integration is expected finite
        # under validated inputs and shipped parameter rows.
        if (!is.finite(v[["v"]])) {
          abort_i(
            i,
            "Smalian integration failed for top section (whole stem above stump)."
          )
        }
        # nocov end
        volm <- 0
        topv <- v[["v"]]
      } else {
        hm1 <- solve_hm1(p, HT, dbhht, ff, bias_factor, topdbh)
        if (!is.finite(hm1)) {
          abort_i(i, "Merchantable height solver failed (non-finite hm1).")
        }

        hm <- hm1 * HT
        # nocov start
        # Defensive-only: solver and clamps are expected to keep hm finite and
        # above stump height for valid parameter rows.
        if (!is.finite(hm) || hm < stumpht) {
          abort_i(i, "Computed merchantable height is invalid (< stump height).")
        }
        # nocov end

        numlogs <- floor((hm - stumpht) / LOGLENGTH + 1)
        numlogs <- max(numlogs, 1)

        vol_logs <- numeric(numlogs)
        di1 <- dsh
        x1 <- stumpht

        for (k in seq_len(numlogs)) {
          xt <- x1 + LOGLENGTH
          if (xt >= hm) {
            xt <- hm
          }

          v <- kozak_smalian(x1, xt, di1, p, HT, dbhht, ff, bias_factor)
          # nocov start
          # Defensive-only: merch-log integrations are expected finite with valid
          # inputs and shipped parameter tables.
          if (!is.finite(v[["v"]]) || !is.finite(v[["di2"]])) {
            abort_i(
              i,
              paste0("Smalian integration failed for merch log ", k, ".")
            )
          }
          # nocov end

          vol_logs[k] <- v[["v"]]
          di1 <- v[["di2"]]
          x1 <- xt
          if (x1 >= hm) break
        }

        volm <- sum(vol_logs, na.rm = TRUE)
        # nocov start
        # Defensive-only: sum of validated positive log sections should remain
        # finite and non-negative.
        if (!is.finite(volm) || volm < 0) {
          abort_i(i, "Computed merchantable volume is non-finite/negative.")
        }
        # nocov end

        vtop <- kozak_smalian(hm, HT, topdbh, p, HT, dbhht, ff, bias_factor)
        # nocov start
        # Defensive-only: top section integration is expected finite for valid
        # geometry and parameter values.
        if (!is.finite(vtop[["v"]])) {
          abort_i(i, "Smalian integration failed for top section (hm -> tip).")
        }
        # nocov end
        topv <- vtop[["v"]]
      }
    }

    if (!allow_merch) {
      volm <- 0
    }

    # nocov start
    # Defensive-only: top volume is constructed from validated integrations.
    if (!is.finite(topv) || topv < 0) {
      abort_i(i, "Computed top volume is non-finite/negative.")
    }
    # nocov end

    vol_stump[i] <- stmv
    vol_merch[i] <- volm
    vol_top[i] <- topv
    vol_total[i] <- stmv + volm + topv

    # nocov start
    # Defensive-only: total is a sum of finite component volumes.
    if (!is.finite(vol_total[i])) {
      abort_i(i, "Computed total volume is non-finite.")
    }
    # nocov end
  }

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}
