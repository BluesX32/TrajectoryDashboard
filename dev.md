# TrajectoryDashboard — Developer Notes

## Known gotchas

### R `seq(from, to)` counts DOWN when `from > to`

**File:** `R/trajectory.R` — `.consolidate_phases()`

`seq(2L, nrow(wdf))` looks like "iterate rows 2…n", but when `nrow(wdf) == 1`
it evaluates to `c(2L, 1L)` (R counts down by 1). The loop then tries
`wdf$phase[2]` on a 1-row data frame, which returns `NA`, and `NA == cur_phase`
crashes with *"missing value where TRUE/FALSE needed"*.

**Rule:** Never use `seq(k, n)` as a loop range — use `seq_len(n)[-k+1]` or
guard with `if (n < k) return(...)` first. In `.consolidate_phases` the guard
is `if (nrow(wdf) <= 1L) return(wdf)`.

Use `identical(a, b)` instead of `a == b` inside `if()` whenever `a` or `b`
could be `NA` — `identical` always returns a scalar `TRUE`/`FALSE`.

---

### `DBI::dbGetQuery` vs `DatabaseConnector::querySql` for JDBC connections

SAFER Desktop / Discovery HPC connections are raw RJDBC `JDBCConnection`
objects. `DatabaseConnector::querySql()` internally calls `dbms(conn)`, which
returns `character(0)` for raw JDBC objects → `if (character(0) == ...)` →
*"argument is of length zero"*.

**Rule:** Always dispatch through `.exec_sql()` (defined in `R/cohort.R`):

```r
.exec_sql <- function(conn, sql) {
  if (inherits(conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(conn, sql))
  } else {
    DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = FALSE)
  }
}
```

The same pattern exists in `R/sql_helpers.R` — follow it there too.

---

### Tibble column access warns on missing columns

`tibble[[col]]` emits a warning for non-existent columns (unlike base
`data.frame`, which silently returns `NULL`). Always ensure columns like
`label_display` exist before accessing them (e.g. add them in `.make_flag()`).

---

### loess requires ≥ 6 points with span = 0.4

`stats::loess()` with `span = 0.4` on fewer than ~6 observations causes
singularity warnings. Guard: `if (nrow(df) >= 6L)` before calling loess, and
wrap the call in `suppressWarnings()` for the rare borderline case.
