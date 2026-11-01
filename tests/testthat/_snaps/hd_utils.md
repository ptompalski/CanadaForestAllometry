# .hd_warn_out_of_range() emits exactly one warning listing rows

    Code
      out <- .hd_warn_out_of_range(x, lower = list(DBH = 2, BA = 5), upper = list(
        DBH = c(60, 60, 60, 60), BA = 85), fn = "hd_x")
    Condition
      Warning:
      `hd_x()`: 3 rows outside the source's published applicability range; predictions are extrapolated.
      * `DBH`: 2 rows (1, 3)
      * `BA`: 1 row (2)

