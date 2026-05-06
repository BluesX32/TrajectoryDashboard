# analysis_2by2.R
# 2×3 contingency tables comparing ICD-code-based vs LLM-based classification
# for shingles INFECTION and shingles VACCINE in the base cohort.
#
# Table layout (produced separately for infection and vaccine):
#
#                         ┌──────── LLM Classification ────────┐
#                         │ No        Yes       Uncertain       │ Row Total
#   ICD: Absent           │ n (row%)  n (row%)  n (row%)        │   n
#   ICD: Present          │ n (row%)  n (row%)  n (row%)        │   n
#   Column Total          │   n          n          n           │   N
#
# ICD identification (same concept sets as preliminary_tables.R):
#   Infection : VZV / herpes zoster condition_occurrence — no antiviral req.
#   Vaccine   : Shingrix/Zostavax drug_exposure or procedure_occurrence,
#               ancestors 44808679 / 21601361 / 706103, minus live-zoster excl.
#
# LLM per-note classification rules:
#   Infection: yes       = has_shingles_infection == "yes"  &  confidence == "high"
#              uncertain = has_shingles_infection == "yes"  &  confidence == "medium"
#              no        = has_shingles_infection == "no"
#   Vaccine:   yes       = shingles_vaccine_received == "yes"
#              uncertain = vaccine_event_type %in% {ordered_or_recommended,
#                                                   due_or_needed}
#              no        = vaccine_event_type == "absent"
#
# Patient-level aggregation from note-level results:
#   any "yes" across notes → Yes;  else any "uncertain" → Uncertain;  else No.
#
# Run interactively in RStudio.  Requires DatabaseConnector, SqlRender,
# jsonlite, gt, dplyr.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

# install.packages(c("jsonlite", "gt", "dplyr"))
library(rlang,     lib.loc = "~/R/win-library/4.5")
library(dplyr,     lib.loc = "C:/Program Files/RPackages")
library(jsonlite)
library(gt)

# ----------------------------------------------------------------------------
# Connection — choose SAFER or local, comment out the other
# ----------------------------------------------------------------------------
# con <- TrajectoryDashboard::create_connection_from_env(".env")
con <- TrajectoryDashboard::create_safer_connection("R.env")

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

# ============================================================================
# Helper: render + translate + execute an inline SQL template
# ============================================================================

run_sql <- function(con, sql_template, ...) {
  params <- list(...)
  sql    <- do.call(SqlRender::render, c(list(sql = sql_template), params))
  dbms   <- con$dbms %||% "sql server"
  sql    <- SqlRender::translate(sql, targetDialect = dbms)
  result <- if (inherits(con$conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(con$conn, sql))
  } else {
    DatabaseConnector::querySql(con$conn, sql, snakeCaseToCamelCase = FALSE)
  }
  names(result) <- tolower(names(result))
  result
}

# ============================================================================
# STEP 1: Load LLM output files
# ============================================================================

message("Loading LLM output files...")

llm_dir <- file.path("output", "LLM")

read_jsonl <- function(path) {
  lines <- readLines(path, warn = FALSE)
  dplyr::bind_rows(lapply(lines, function(l) {
    as.data.frame(jsonlite::fromJSON(l)$data, stringsAsFactors = FALSE)
  }))
}

vaccine_raw   <- read_jsonl(file.path(llm_dir, "vaccine_final.jsonl"))
infection_raw <- read_jsonl(file.path(llm_dir, "infection_final.jsonl"))

message(nrow(vaccine_raw),   " vaccine note records loaded.")
message(nrow(infection_raw), " infection note records loaded.")

# ============================================================================
# STEP 2: Per-note LLM classification  →  yes / uncertain / no
# ============================================================================

# Vaccine
#   yes       = vaccine was confirmed received in the note
#   uncertain = vaccine mentioned but only as ordered, recommended, or due
#   no        = no vaccine mention (absent)
vaccine_notes <- vaccine_raw |>
  mutate(
    note_id      = as.integer(note_id),
    llm_vaccine  = case_when(
      shingles_vaccine_received == "yes"                                          ~ "yes",
      vaccine_event_type %in% c("ordered_or_recommended", "due_or_needed")       ~ "uncertain",
      TRUE                                                                        ~ "no"
    )
  ) |>
  select(note_id, llm_vaccine)

# Infection
#   yes       = high-confidence positive mention of shingles infection
#   uncertain = positive mention but only medium confidence
#   no        = no shingles infection detected
infection_notes <- infection_raw |>
  mutate(
    note_id        = as.integer(note_id),
    llm_infection  = case_when(
      has_shingles_infection == "yes" & confidence == "high"   ~ "yes",
      has_shingles_infection == "yes" & confidence == "medium" ~ "uncertain",
      TRUE                                                      ~ "no"
    )
  ) |>
  select(note_id, llm_infection)

# ============================================================================
# STEP 3: Link note_id → person_id via NOTE table
# ============================================================================

message("Fetching note → person mapping from NOTE table...")

all_note_ids <- unique(c(vaccine_notes$note_id, infection_notes$note_id))

note_person_sql <- "
SELECT note_id, person_id
FROM @cdm_schema.note
WHERE note_id IN (@note_ids)
"

note_person <- run_sql(con, note_person_sql,
                       cdm_schema = cdm,
                       note_ids   = all_note_ids)

message(n_distinct(note_person$person_id), " unique patients linked to LLM notes.")

# ============================================================================
# STEP 4: Patient-level LLM aggregation
#   Priority: yes (2) > uncertain (1) > no (0)
#   Take the maximum priority across all notes for a patient.
# ============================================================================

llm_priority   <- c("yes" = 2L, "uncertain" = 1L, "no" = 0L)
priority_label <- c("2" = "yes", "1" = "uncertain", "0" = "no")

aggregate_llm <- function(note_df, llm_col, note_person_df) {
  note_df |>
    inner_join(note_person_df, by = "note_id") |>
    mutate(.prio = llm_priority[.data[[llm_col]]]) |>
    group_by(person_id) |>
    summarise(.max_prio = max(.prio, na.rm = TRUE), .groups = "drop") |>
    mutate(llm_status = priority_label[as.character(.max_prio)]) |>
    select(person_id, llm_status)
}

pt_vaccine_llm   <- aggregate_llm(vaccine_notes,   "llm_vaccine",   note_person) |>
  rename(llm_vaccine   = llm_status)
pt_infection_llm <- aggregate_llm(infection_notes, "llm_infection", note_person) |>
  rename(llm_infection = llm_status)

llm_person_ids <- unique(note_person$person_id)
message(length(llm_person_ids), " patients in LLM cohort.")

# ============================================================================
# STEP 5: ICD-based infection status
#   Any VZV / herpes zoster diagnosis in condition_occurrence (no antiviral
#   requirement — this is for direct comparison with the NLP signal).
# ============================================================================

message("Querying ICD-based shingles infection status...")

icd_infection_sql <- "
WITH vzv_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
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
  )
  AND c.invalid_reason IS NULL
)
SELECT DISTINCT co.person_id
FROM @cdm_schema.condition_occurrence co
JOIN vzv_concepts vc ON co.condition_concept_id = vc.concept_id
WHERE co.person_id IN (@person_ids)
"

icd_infection_pts <- run_sql(con, icd_infection_sql,
                              cdm_schema   = cdm,
                              vocab_schema = vocab,
                              person_ids   = llm_person_ids)$person_id

message(length(icd_infection_pts), " / ", length(llm_person_ids),
        " LLM-cohort patients have a VZV ICD diagnosis.")

# ============================================================================
# STEP 6: ICD-based vaccine status
#   Shingrix/Zostavax in drug_exposure or procedure_occurrence.
#   Concept set: ancestors 44808679 / 21601361 / 706103,
#   excluding live-zoster vaccines 40213260 / 706104 / 40213255 / 40213256.
# ============================================================================

message("Querying ICD-based shingles vaccine status...")

icd_vaccine_sql <- "
WITH vaccine_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
    AND invalid_reason IS NULL
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
    AND ca.descendant_concept_id NOT IN (40213260, 706104, 40213255, 40213256)
)
SELECT DISTINCT person_id
FROM (
  SELECT de.person_id
  FROM @cdm_schema.drug_exposure de
  JOIN vaccine_concepts vc ON de.drug_concept_id = vc.concept_id
  WHERE de.person_id IN (@person_ids)
  UNION
  SELECT po.person_id
  FROM @cdm_schema.procedure_occurrence po
  JOIN vaccine_concepts vc ON po.procedure_concept_id = vc.concept_id
  WHERE po.person_id IN (@person_ids)
) combined
"

icd_vaccine_pts <- run_sql(con, icd_vaccine_sql,
                            cdm_schema   = cdm,
                            vocab_schema = vocab,
                            person_ids   = llm_person_ids)$person_id

message(length(icd_vaccine_pts), " / ", length(llm_person_ids),
        " LLM-cohort patients have a Shingrix/Zostavax vaccine record.")

# ============================================================================
# STEP 7: Assemble analysis datasets (one per task)
# ============================================================================

message("Assembling analysis datasets...")

all_llm_pts <- data.frame(person_id = llm_person_ids)

LLM_LEVELS   <- c("no", "yes", "uncertain")
LLM_LABELS   <- c("No", "Yes", "Uncertain")
ICD_LEVELS   <- c("Absent", "Present")

infection_analysis <- all_llm_pts |>
  left_join(pt_infection_llm, by = "person_id") |>
  mutate(
    llm_infection = factor(
      coalesce(llm_infection, "no"),
      levels = LLM_LEVELS, labels = LLM_LABELS
    ),
    icd_infection = factor(
      if_else(person_id %in% icd_infection_pts, "Present", "Absent"),
      levels = ICD_LEVELS
    )
  )

vaccine_analysis <- all_llm_pts |>
  left_join(pt_vaccine_llm, by = "person_id") |>
  mutate(
    llm_vaccine = factor(
      coalesce(llm_vaccine, "no"),
      levels = LLM_LEVELS, labels = LLM_LABELS
    ),
    icd_vaccine = factor(
      if_else(person_id %in% icd_vaccine_pts, "Present", "Absent"),
      levels = ICD_LEVELS
    )
  )

# ============================================================================
# STEP 8: Build 2×3 gt table
#   Rows    (2) : ICD-based — Absent | Present
#   Columns (3) : LLM-based — No | Yes | Uncertain
#   Cells       : n (row%)
#   Extras      : row totals, column totals row
# ============================================================================

make_2x3_gt <- function(df, icd_col, llm_col, title, subtitle) {
  ct        <- table(df[[icd_col]], df[[llm_col]])   # 2 × 3 matrix
  row_tots  <- rowSums(ct)
  col_tots  <- colSums(ct)
  N         <- sum(ct)

  fmt_cell  <- function(n, row_tot)
    sprintf("%d (%.1f%%)", n, 100 * n / max(row_tot, 1L))

  fmt_total <- function(n) as.character(n)

  # Main body rows
  body_rows <- purrr::map_dfr(rownames(ct), function(icd_lbl) {
    row  <- ct[icd_lbl, ]
    rtot <- row_tots[icd_lbl]
    tibble::tibble(
      ICD         = icd_lbl,
      No          = fmt_cell(row["No"],        rtot),
      Yes         = fmt_cell(row["Yes"],        rtot),
      Uncertain   = fmt_cell(row["Uncertain"],  rtot),
      `Row Total` = fmt_total(rtot)
    )
  })

  # Column totals row
  total_row <- tibble::tibble(
    ICD         = "Column Total",
    No          = fmt_total(col_tots["No"]),
    Yes         = fmt_total(col_tots["Yes"]),
    Uncertain   = fmt_total(col_tots["Uncertain"]),
    `Row Total` = fmt_total(N)
  )

  bind_rows(body_rows, total_row) |>
    gt(rowname_col = "ICD") |>
    tab_header(
      title    = title,
      subtitle = md(subtitle)
    ) |>
    tab_spanner(
      label   = md("**LLM Classification**"),
      columns = c(No, Yes, Uncertain)
    ) |>
    cols_label(
      No           = md("**No**"),
      Yes          = md("**Yes**"),
      Uncertain    = md("**Uncertain**"),
      `Row Total`  = md("**Row Total**")
    ) |>
    tab_stubhead(label = md("**ICD-Based**")) |>
    cols_align(align = "center", columns = c(No, Yes, Uncertain, `Row Total`)) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_stub(rows = "Column Total")
    ) |>
    tab_style(
      style     = cell_fill(color = "#eaf2fb"),
      locations = cells_stub(rows = "Column Total")
    ) |>
    tab_style(
      style     = cell_fill(color = "#eaf2fb"),
      locations = cells_body(rows = nrow(body_rows) + 1L)
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(rows = nrow(body_rows) + 1L)
    ) |>
    tab_style(
      style     = cell_fill(color = "#fef9e7"),
      locations = cells_body(rows = 1L)
    ) |>
    tab_style(
      style     = cell_fill(color = "#eafaf1"),
      locations = cells_body(rows = 2L)
    ) |>
    tab_footnote(
      footnote  = md("Cell values: n (row %). Row % = n within ICD row / row total × 100."),
      locations = cells_column_spanners()
    ) |>
    tab_footnote(
      footnote = md(paste0(
        "**ICD Absent**: no VZV / herpes zoster ICD code in EHR. ",
        "**ICD Present**: ≥1 matching condition_occurrence (infection) ",
        "or drug_exposure / procedure_occurrence (vaccine)."
      )),
      locations = cells_stubhead()
    ) |>
    tab_footnote(
      footnote = md(paste0(
        "LLM classification aggregated to patient level from note-level NLP results ",
        "(output/LLM/). Priority: Yes > Uncertain > No. ",
        "Infection — Yes: has_shingles_infection = yes & confidence = high; ",
        "Uncertain: has_shingles_infection = yes & confidence = medium. ",
        "Vaccine   — Yes: shingles_vaccine_received = yes; ",
        "Uncertain: vaccine_event_type ∈ {ordered_or_recommended, due_or_needed}."
      )),
      locations = cells_column_labels(columns = Uncertain)
    ) |>
    tab_options(
      table.font.names                    = "Arial",
      table.font.size                     = 12,
      column_labels.font.size             = 12,
      column_labels.font.weight           = "bold",
      heading.title.font.size             = 14,
      heading.title.font.weight           = "bold",
      stub.border.width                   = px(1),
      stub.border.color                   = "#6c757d",
      table.border.top.width              = px(2),
      table.border.top.color              = "#2c3e50",
      table.border.bottom.width           = px(2),
      table.border.bottom.color           = "#2c3e50",
      column_labels.border.bottom.width   = px(1),
      column_labels.border.bottom.color   = "#6c757d",
      table.width                         = pct(65)
    )
}

# ============================================================================
# Build and print tables
# ============================================================================

message("Rendering 2x3 tables...")

n_inf <- nrow(infection_analysis)
n_vax <- nrow(vaccine_analysis)

table_infection <- make_2x3_gt(
  infection_analysis, "icd_infection", "llm_infection",
  title    = "Table A. Shingles Infection: ICD-Code vs LLM Classification",
  subtitle = sprintf(
    "Base cohort patients with NLP-analyzed notes (N = %d) — VZV / herpes zoster diagnosis (no antiviral requirement)",
    n_inf
  )
)

table_vaccine <- make_2x3_gt(
  vaccine_analysis, "icd_vaccine", "llm_vaccine",
  title    = "Table B. Shingles Vaccine: ICD-Code vs LLM Classification",
  subtitle = sprintf(
    "Base cohort patients with NLP-analyzed notes (N = %d) — Shingrix / Zostavax in drug_exposure or procedure_occurrence",
    n_vax
  )
)

print(table_infection)
print(table_vaccine)

# ============================================================================
# Save output
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output")) dir.create("output")

gtsave(table_infection, file.path("output", paste0("table_2x3_infection_", dt, ".html")))
gtsave(table_vaccine,   file.path("output", paste0("table_2x3_vaccine_",   dt, ".html")))
gtsave(table_infection, file.path("output", paste0("table_2x3_infection_", dt, ".docx")))
gtsave(table_vaccine,   file.path("output", paste0("table_2x3_vaccine_",   dt, ".docx")))

saveRDS(infection_analysis, file.path("output", paste0("data_2x3_infection_", dt, ".rds")))
saveRDS(vaccine_analysis,   file.path("output", paste0("data_2x3_vaccine_",   dt, ".rds")))

message("Done. Tables saved to output/.")
