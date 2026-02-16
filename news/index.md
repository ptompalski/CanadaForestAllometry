# Changelog

## CanadaForestAllometry 0.7.1

### New: Newfoundland and Labrador volume model (`vol_nl()`)

- Added a new regional volume model for Newfoundland and Labrador based
  on:
  - Ker (1974) — *N-X-122* (province-wide total volume)
  - Warren & Meades (1986) — *N-X-242* (district-level total, gross and
    net volume)
  - Honer (1967) — *N-X-67* (merchantable conversion factors)
- The implementation is based on the original OSM (C#) code (thank you
  Chris Hennigar):
  - District-specific equations are used only for districts 2 and 4–18
  - District 19 is intentionally excluded (unstable behaviour noted in
    original implementation)
  - All other districts (including 1, 3, 20–24) fall back to
    province-wide equations
  - Net merchantable volume (when available) is internally constrained
    to ≥95% of gross volume
- Default behavior:
  - `subregion = "Province"` uses province-wide parameters
  - Output includes `vol_total` and `vol_merchantable`, consistent with
    other volume models
  - Optional `keep_net = TRUE` exposes gross and net merchantable
    components

### Parameter storage refactor

- Model parameters are now stored as internal datasets

## CanadaForestAllometry 0.7.0

- **Renamed package**: *CTAE* is now **CanadaForestAllometry**.
- This is a naming change only
- All existing functions are available under the new package name.
