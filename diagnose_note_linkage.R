# diagnose_note_linkage.R
# Run this BEFORE analysis_2by2_reach.R.
# Goal: find which database table/column maps the JSONL note_ids to person_id.
#
# Steps:
#   1. Load a sample of JSONL note_ids
#   2. Show what the OMOP NOTE table actually contains (range, count, format)
#   3. Search INFORMATION_SCHEMA for every table that has a note_id-like column
#   4. Spot-check JSONL note_ids against each candidate table
#
# Read the printed output and identify which table returns matches.
# Then tell the developer which table/column to use in analysis_2by2_reach.R.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")
library(rlang, lib.loc = "~/R/win-library/4.5")
library(dplyr, lib.loc = "C:/Program Files/RPackages")
library(jsonlite)

con   <- TrajectoryDashboard::create_safer_connection("R.env")
cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

run_sql <- function(con, sql_template, ...) {
  params <- list(...)
  sql    <- do.call(SqlRender::render, c(list(sql = sql_template), params))
  sql    <- SqlRender::translate(sql, targetDialect = con$dbms %||% "sql server")
  result <- if (inherits(con$conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(con$conn, sql))
  } else {
    DatabaseConnector::querySql(con$conn, sql, snakeCaseToCamelCase = FALSE)
  }
  names(result) <- tolower(names(result))
  result
}

# ── Sample JSONL note_ids ────────────────────────────────────────────────────
read_jsonl <- function(path) {
  lines <- readLines(path, warn = FALSE)
  dplyr::bind_rows(lapply(lines, function(l) {
    as.data.frame(jsonlite::fromJSON(l)$data, stringsAsFactors = FALSE)
  }))
}

llm_dir  <- file.path("output", "LLM")
raw      <- read_jsonl(file.path(llm_dir, "infection_final.jsonl"))
note_ids <- unique(as.character(raw$note_id))
probe    <- note_ids[1:min(10, length(note_ids))]   # first 10 for spot-checks

cat("\n=== JSONL note_ids (first 10) ===\n")
print(probe)
cat("Total unique note_ids in JSONL:", length(note_ids), "\n")

# ── 1. OMOP NOTE table overview ──────────────────────────────────────────────
cat("\n=== 1. OMOP NOTE table: row count, note_id range, source_value sample ===\n")

note_overview <- run_sql(con, "
SELECT
  COUNT(*)                          AS total_rows,
  MIN(note_id)                      AS min_note_id,
  MAX(note_id)                      AS max_note_id,
  COUNT(DISTINCT person_id)         AS distinct_persons
FROM @cdm_schema.note",
  cdm_schema = cdm)
print(note_overview)

note_sample <- run_sql(con, "
SELECT TOP 5 note_id, person_id, note_date, note_source_value
FROM @cdm_schema.note
ORDER BY note_id",
  cdm_schema = cdm)
cat("Sample rows from NOTE table:\n")
print(note_sample)

# ── 2. Direct match: JSONL ids vs NOTE.note_id ───────────────────────────────
cat("\n=== 2. How many JSONL note_ids match NOTE.note_id? ===\n")
match_note_id <- run_sql(con, "
SELECT COUNT(*) AS matched
FROM @cdm_schema.note
WHERE CAST(note_id AS VARCHAR) IN (@note_ids)",
  cdm_schema = cdm, note_ids = probe)
print(match_note_id)

# ── 3. Direct match: JSONL ids vs NOTE.note_source_value ────────────────────
cat("\n=== 3. How many JSONL note_ids match NOTE.note_source_value? ===\n")
match_src <- run_sql(con, "
SELECT COUNT(*) AS matched
FROM @cdm_schema.note
WHERE note_source_value IN (@note_ids)",
  cdm_schema = cdm, note_ids = probe)
print(match_src)

# ── 4. Search INFORMATION_SCHEMA for all tables with a note_id-like column ──
cat("\n=== 4. All tables with a column named like '%note%id%' ===\n")
candidates <- run_sql(con, "
SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE COLUMN_NAME LIKE '%note%id%'
   OR COLUMN_NAME LIKE '%note_id%'
ORDER BY TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME")
print(candidates)

# ── 5. Search for tables with both a note-id-like column AND person_id ───────
cat("\n=== 5. Tables that have both a note-like column AND person_id ===\n")
candidates_with_person <- run_sql(con, "
SELECT c1.TABLE_SCHEMA, c1.TABLE_NAME, c1.COLUMN_NAME AS note_col
FROM INFORMATION_SCHEMA.COLUMNS c1
JOIN INFORMATION_SCHEMA.COLUMNS c2
  ON c1.TABLE_SCHEMA = c2.TABLE_SCHEMA
 AND c1.TABLE_NAME   = c2.TABLE_NAME
 AND c2.COLUMN_NAME  = 'person_id'
WHERE c1.COLUMN_NAME LIKE '%note%'
ORDER BY c1.TABLE_SCHEMA, c1.TABLE_NAME")
print(candidates_with_person)

# ── 6. Spot-check probe ids against each candidate table ─────────────────────
cat("\n=== 6. Spot-checking probe note_ids against each candidate table ===\n")

if (nrow(candidates_with_person) > 0) {
  for (i in seq_len(nrow(candidates_with_person))) {
    schema   <- candidates_with_person$table_schema[i]
    tbl      <- candidates_with_person$table_name[i]
    note_col <- candidates_with_person$note_col[i]
    full_tbl <- paste0(schema, ".", tbl)

    tryCatch({
      res <- run_sql(con, sprintf("
        SELECT COUNT(*) AS matched
        FROM %s
        WHERE CAST(%s AS VARCHAR) IN (@note_ids)",
        full_tbl, note_col),
        note_ids = probe)
      cat(sprintf("  %-50s  %-25s  matched = %d\n",
                  full_tbl, note_col, res$matched))
    }, error = function(e) {
      cat(sprintf("  %-50s  %-25s  ERROR: %s\n",
                  full_tbl, note_col, conditionMessage(e)))
    })
  }
} else {
  cat("No candidate tables found via INFORMATION_SCHEMA.\n")
  cat("The NOTE table may be in a different catalog or schema.\n")
}

cat("\n=== Done. Share the output above to identify the correct join. ===\n")
