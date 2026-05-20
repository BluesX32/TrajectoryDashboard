# analysis_2by2_reach.R
# 2×3 contingency tables: ICD-code-based vs LLM-based classification
# for shingles INFECTION and shingles VACCINE.
#
# Unit of analysis: one NOTE (N ≈ 1000 notes processed by the LLM).
#
# OMOP linkage chain:
#   JSONL note_id
#     → NOTE.note_id  →  NOTE.person_id, NOTE.visit_occurrence_id
#     → CONDITION_OCCURRENCE (via visit_occurrence_id) → ICD infection flag
#     → DRUG_EXPOSURE / PROCEDURE_OCCURRENCE (via visit_occurrence_id) → ICD vaccine flag
#
# ICD flag = did THIS visit (the one the note belongs to) have a matching code?
#
# Table layout (one for infection, one for vaccine):
#
#                         ┌──────── LLM Classification ────────┐
#                         │  No       Yes      Uncertain        │ Row Total
#   ICD: Absent           │ n (row%)  n (row%)  n (row%)        │   n
#   ICD: Present          │ n (row%)  n (row%)  n (row%)        │   n
#   Column Total          │   n          n          n           │   N
#
# LLM per-note rules:
#   Infection: Yes = has_shingles_infection=="yes"
#              No  = has_shingles_infection=="no"
#   Vaccine:   Yes       = shingles_vaccine_received=="yes"
#              Uncertain = vaccine_event_type %in% {ordered_or_recommended, due_or_needed}
#              No        = vaccine_event_type=="absent"
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang,     lib.loc = "~/R/win-library/4.5")
library(dplyr,     lib.loc = "C:/Program Files/RPackages")
library(jsonlite)
library(gt)

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

# ============================================================================
# 1. Load LLM output — one row per note, classify directly
# ============================================================================

read_jsonl <- function(path) {
  lines <- readLines(path, warn = FALSE)
  dplyr::bind_rows(lapply(lines, function(l) {
    as.data.frame(jsonlite::fromJSON(l)$data, stringsAsFactors = FALSE)
  }))
}

llm_dir <- file.path("output", "LLM")

vaccine_notes <- read_jsonl(file.path(llm_dir, "vaccine_final.jsonl")) |>
  mutate(
    note_id     = as.integer(note_id),
    llm_vaccine = case_when(
      shingles_vaccine_received == "yes"                                    ~ "yes",
      vaccine_event_type %in% c("ordered_or_recommended", "due_or_needed") ~ "uncertain",
      TRUE                                                                  ~ "no"
    )
  ) |>
  select(note_id, llm_vaccine)

infection_notes <- read_jsonl(file.path(llm_dir, "infection_final.jsonl")) |>
  mutate(
    note_id       = as.integer(note_id),
    llm_infection = case_when(
      has_shingles_infection == "yes" ~ "yes",
      has_shingles_infection == "no"  ~ "no",
      TRUE                            ~ "uncertain"
    )
  ) |>
  select(note_id, llm_infection)

all_note_ids <- unique(c(vaccine_notes$note_id, infection_notes$note_id))
message(length(all_note_ids), " unique notes in LLM output.")

# ============================================================================
# 2. NOTE.note_id → person_id + visit_occurrence_id
# ============================================================================

note_visits <- run_sql(con,
  "SELECT note_id, person_id, visit_occurrence_id
   FROM @cdm_schema.note
   WHERE note_id IN (@note_ids)",
  cdm_schema = cdm,
  note_ids   = all_note_ids
)

message(nrow(note_visits), " notes linked to person_id and visit_occurrence_id.")
message(n_distinct(note_visits$person_id), " unique patients.")

visit_ids <- unique(note_visits$visit_occurrence_id[!is.na(note_visits$visit_occurrence_id)])

# ============================================================================
# 3. ICD infection flag — VZV diagnosis in CONDITION_OCCURRENCE for this visit
# ============================================================================

icd_infection_visits <- run_sql(con, "
WITH vzv AS (
  SELECT DISTINCT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741, 141374, 37165237,
    4221382, 4066727, 37165216, 4080937, 4299673, 37110753,
    4064036, 4067067, 40175007, 37165342, 4080929, 4063440,
    4272156, 4033204, 4033778, 4206461, 135618, 4033777
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741
  ) AND c.invalid_reason IS NULL
)
SELECT DISTINCT co.visit_occurrence_id
FROM @cdm_schema.condition_occurrence co
JOIN vzv ON co.condition_concept_id = vzv.concept_id
WHERE co.visit_occurrence_id IN (@visit_ids)",
  cdm_schema = cdm, vocab_schema = vocab,
  visit_ids  = visit_ids
)$visit_occurrence_id

message(length(icd_infection_visits), " visits with a VZV ICD code.")

# ============================================================================
# 4. ICD vaccine flag — Shingrix/Zostavax in DRUG_EXPOSURE or
#    PROCEDURE_OCCURRENCE for this visit
# ============================================================================

icd_vaccine_visits <- run_sql(con, "
WITH vc AS (
  SELECT DISTINCT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103) AND invalid_reason IS NULL
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
    AND ca.descendant_concept_id NOT IN (40213260, 706104, 40213255, 40213256)
)
SELECT DISTINCT visit_occurrence_id FROM (
  SELECT de.visit_occurrence_id
  FROM @cdm_schema.drug_exposure de
  JOIN vc ON de.drug_concept_id = vc.concept_id
  WHERE de.visit_occurrence_id IN (@visit_ids)
  UNION
  SELECT po.visit_occurrence_id
  FROM @cdm_schema.procedure_occurrence po
  JOIN vc ON po.procedure_concept_id = vc.concept_id
  WHERE po.visit_occurrence_id IN (@visit_ids)
) x",
  cdm_schema = cdm, vocab_schema = vocab,
  visit_ids  = visit_ids
)$visit_occurrence_id

message(length(icd_vaccine_visits), " visits with a vaccine record.")

# ============================================================================
# 5. Assemble note-level datasets
#    note_id → visit_occurrence_id → ICD flag
# ============================================================================

LLM_LEVELS <- c("no", "yes", "uncertain")
LLM_LABELS <- c("No", "Yes", "Uncertain")

infection_df <- infection_notes |>
  inner_join(note_visits, by = "note_id") |>
  mutate(
    llm = factor(llm_infection, levels = LLM_LEVELS, labels = LLM_LABELS),
    icd = factor(
      if_else(!is.na(visit_occurrence_id) & visit_occurrence_id %in% icd_infection_visits,
              "Present", "Absent"),
      levels = c("Absent", "Present")
    )
  )

vaccine_df <- vaccine_notes |>
  inner_join(note_visits, by = "note_id") |>
  mutate(
    llm = factor(llm_vaccine, levels = LLM_LEVELS, labels = LLM_LABELS),
    icd = factor(
      if_else(!is.na(visit_occurrence_id) & visit_occurrence_id %in% icd_vaccine_visits,
              "Present", "Absent"),
      levels = c("Absent", "Present")
    )
  )

message(nrow(infection_df), " notes in infection table.")
message(nrow(vaccine_df),   " notes in vaccine table.")

# ============================================================================
# 6. Build 2×3 gt table
# ============================================================================

make_2x3_gt <- function(df, title, subtitle) {
  ct       <- table(df$icd, df$llm)
  row_tots <- rowSums(ct)
  col_tots <- colSums(ct)
  N        <- sum(ct)

  fmt <- function(n, denom) sprintf("%d (%.1f%%)", n, 100 * n / max(denom, 1L))

  body <- purrr::map_dfr(rownames(ct), function(r) {
    tibble::tibble(
      ICD         = r,
      No          = fmt(ct[r, "No"],       row_tots[r]),
      Yes         = fmt(ct[r, "Yes"],       row_tots[r]),
      Uncertain   = fmt(ct[r, "Uncertain"], row_tots[r]),
      `Row Total` = as.character(row_tots[r])
    )
  })

  totals <- tibble::tibble(
    ICD         = "Column Total",
    No          = as.character(col_tots["No"]),
    Yes         = as.character(col_tots["Yes"]),
    Uncertain   = as.character(col_tots["Uncertain"]),
    `Row Total` = as.character(N)
  )

  bind_rows(body, totals) |>
    gt(rowname_col = "ICD") |>
    tab_header(title = title, subtitle = md(subtitle)) |>
    tab_spanner(label = md("**LLM Classification**"),
                columns = c(No, Yes, Uncertain)) |>
    cols_label(
      No          = md("**No**"),
      Yes         = md("**Yes**"),
      Uncertain   = md("**Uncertain**"),
      `Row Total` = md("**Row Total**")
    ) |>
    tab_stubhead(label = md("**ICD-Based**")) |>
    cols_align(align = "center", columns = c(No, Yes, Uncertain, `Row Total`)) |>
    tab_style(
      style     = list(cell_fill(color = "#eaf2fb"), cell_text(weight = "bold")),
      locations = list(cells_body(rows = nrow(body) + 1L),
                       cells_stub(rows = "Column Total"))
    ) |>
    tab_style(style = cell_fill(color = "#fef9e7"), locations = cells_body(rows = 1L)) |>
    tab_style(style = cell_fill(color = "#eafaf1"), locations = cells_body(rows = 2L)) |>
    tab_footnote(
      footnote = paste0(
        "n (row%). ICD flag is visit-level: the note's visit_occurrence_id is matched ",
        "to CONDITION_OCCURRENCE (infection) or DRUG_EXPOSURE / PROCEDURE_OCCURRENCE (vaccine)."
      ),
      locations = cells_column_spanners()
    ) |>
    tab_options(
      table.font.names                  = "Arial",
      table.font.size                   = 12,
      column_labels.font.weight         = "bold",
      heading.title.font.size           = 14,
      heading.title.font.weight         = "bold",
      stub.border.width                 = px(1),
      stub.border.color                 = "#6c757d",
      table.border.top.color            = "#2c3e50",
      table.border.top.width            = px(2),
      table.border.bottom.color         = "#2c3e50",
      table.border.bottom.width         = px(2),
      column_labels.border.bottom.color = "#6c757d",
      column_labels.border.bottom.width = px(1),
      table.width                       = pct(60)
    )
}

table_infection <- make_2x3_gt(
  infection_df,
  title    = "Table A. Shingles Infection: ICD-Code vs LLM (Note Level)",
  subtitle = sprintf("Notes linked to EHR visits (N = %d notes, %d patients)",
                     nrow(infection_df), n_distinct(infection_df$person_id))
)

table_vaccine <- make_2x3_gt(
  vaccine_df,
  title    = "Table B. Shingles Vaccine: ICD-Code vs LLM (Note Level)",
  subtitle = sprintf("Notes linked to EHR visits (N = %d notes, %d patients)",
                     nrow(vaccine_df), n_distinct(vaccine_df$person_id))
)

print(table_infection)
print(table_vaccine)

# ============================================================================
# 7. Save
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output")) dir.create("output")

gtsave(table_infection, file.path("output", paste0("table_2x3_infection_reach_", dt, ".html")))
gtsave(table_vaccine,   file.path("output", paste0("table_2x3_vaccine_reach_",   dt, ".html")))
gtsave(table_infection, file.path("output", paste0("table_2x3_infection_reach_", dt, ".docx")))
gtsave(table_vaccine,   file.path("output", paste0("table_2x3_vaccine_reach_",   dt, ".docx")))

message("Done.")
