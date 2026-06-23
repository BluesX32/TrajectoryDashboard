# analysis_risk_factors_vzv.R
# Exploratory analysis of clinical factors associated with three VZV outcomes
# among shingles patients in the prevalent RD base cohort.
#
# ── Outcomes ──────────────────────────────────────────────────────────────────
#   (1) Post-herpetic neuralgia (PHN)
#   (2) Organ invasive / disseminated VZV
#   (3) Post-vaccine VZV breakthrough  (vaccinated patients only)
#
# ── Clinical factors ──────────────────────────────────────────────────────────
#   Underlying RD (SLE / DM/Myositis / SSc / GCA / RA / SpA / ANCA vasculitis)
#   DMARD class in 90d before vaccination  (biologic / JAK / csDMARD)
#   DMARD class in 30d after vaccination
#   N distinct DMARDs in 90d before vaccination
#   N distinct DMARDs in 30d after vaccination
#   Lymphopenia  (lymphocytes < 1.0 × 10⁹/L nearest to reference date ± 90d)
#     Reference date: first Shingrix dose (vaccinated) or first shingles
#     episode date (unvaccinated)
#
# ── How to run ────────────────────────────────────────────────────────────────
# Option A: run preliminary_tables_shingles.R first — this script reuses its
#   objects (shingles_ids, phn_pts, organ_pts, json_phn_ids,
#   json_vzv_morbidity_ids, ep_post, vacc_bulk, etc.).
#   Guards with exists() skip re-fetching if already in environment.
# Option B: set STANDALONE <- TRUE to build everything from scratch.
#
# NOTE: phn_pts uses a 2-arm definition (PHN diagnosis OR shingles + PHN
#   treatment drug within 120 days) per cohort_PrevalenceRD_VZV_PHN.json.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang, lib.loc = "~/R/win-library/4.5")
library(dplyr, lib.loc = "C:/Program Files/RPackages")
library(gtsummary)
library(gt)
library(labelled)

STANDALONE <- FALSE   # set TRUE to rebuild cohort without running prelim script first

LYMPHOPENIA_THRESHOLD <- 1.0   # lymphocytes < 1.0 × 10⁹/L
VACC_ONSET_DAYS       <- 14L   # days after first vaccine before event = post-vaccine
SHINGLES_GAP_DAYS     <- 90L   # episode collapsing gap

# ── DMARD ancestor → class mapping ───────────────────────────────────────────
# Biologics: TNF / IL-6 / IL12-23 / IL-17 / Type1-IFN / T-cell / BAFF / CD19-20
BIOLOGIC_ANCESTORS <- c(
  1119119L, 937368L, 19041065L, 912263L, 1151789L,   # TNF-i
  1594587L, 40171288L,                                # IL-6-i
  40161532L, 1511348L, 1593700L,                      # IL-12/23-i
  45892883L, 35603563L, 746895L,                      # IL-17-i
  701470L,                                            # Type1 IFN-i
  1186087L,                                           # T-cell co-stim
  40236987L,                                          # BAFF-i
  1314273L, 44507676L                                 # CD19/CD20
)
# JAK inhibitors
JAK_ANCESTORS <- c(1361580L, 42904205L, 40244464L, 1510627L)
# Conventional (csDMARD + antimetabolite + CNI + alkylating)
CSDMARD_ANCESTORS <- c(
  1305058L, 1101898L, 964339L, 1777087L,             # csDMARD
  19014878L, 19068900L, 19003999L,                   # antimetabolite
  950637L, 739590L,                                  # CNI
  1310317L                                            # alkylating
)
ALL_DMARD_ANCESTORS <- c(BIOLOGIC_ANCESTORS, JAK_ANCESTORS, CSDMARD_ANCESTORS)

# ============================================================================
# Prerequisites — skip if objects already in environment from prelim script
# ============================================================================

if (STANDALONE || !exists("con")) {
  con <- TrajectoryDashboard::create_safer_connection("R.env")
}

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

if (STANDALONE || !exists("shingles_ids") || !exists("phn_pts")) {
  message("Building shingles cohort (run preliminary_tables_shingles.R first to skip this)...")
  source("preliminary_tables_shingles.R")
}

# ============================================================================
# STEP 1  Per-patient reference dates
# Vaccinated → first Shingrix dose date
# Unvaccinated → earliest collapsed shingles episode date
# ============================================================================

first_vacc <- vacc_bulk |>
  group_by(person_id) |>
  summarise(first_vacc_date = min(vacc_date), .groups = "drop")

first_shingles <- episodes_cl |>
  group_by(person_id) |>
  summarise(first_shingles_date = min(condition_start_date), .groups = "drop")

ref_dates <- base_cohort |>
  filter(person_id %in% shingles_ids) |>
  select(person_id) |>
  left_join(first_vacc,    by = "person_id") |>
  left_join(first_shingles, by = "person_id") |>
  mutate(
    is_vaccinated = !is.na(first_vacc_date),
    ref_date      = if_else(is_vaccinated, first_vacc_date, first_shingles_date)
  )

# ============================================================================
# STEP 2  DMARD class and count variables around vaccination date
# Windows are anchored to first_vacc_date; NA for unvaccinated patients.
# ============================================================================

classify_dmard_class <- function(anc_id) {
  case_when(
    anc_id %in% BIOLOGIC_ANCESTORS ~ "biologic",
    anc_id %in% JAK_ANCESTORS      ~ "jak",
    TRUE                            ~ "csdmard"
  )
}

# 90d before vaccination
dmard_pre_vacc <- dmard_exposures |>
  inner_join(first_vacc, by = "person_id") |>
  filter(drug_date >= first_vacc_date - 90L,
         drug_date <= first_vacc_date) |>
  distinct(person_id, ancestor_concept_id) |>
  mutate(drug_class = classify_dmard_class(ancestor_concept_id)) |>
  group_by(person_id) |>
  summarise(
    any_biologic_pre_vacc  = any(drug_class == "biologic"),
    any_jak_pre_vacc       = any(drug_class == "jak"),
    any_csdmard_pre_vacc   = any(drug_class == "csdmard"),
    n_dmards_pre_vacc      = n_distinct(ancestor_concept_id),
    .groups = "drop"
  )

# 30d after vaccination
dmard_post_vacc <- dmard_exposures |>
  inner_join(first_vacc, by = "person_id") |>
  filter(drug_date > first_vacc_date,
         drug_date <= first_vacc_date + 30L) |>
  distinct(person_id, ancestor_concept_id) |>
  mutate(drug_class = classify_dmard_class(ancestor_concept_id)) |>
  group_by(person_id) |>
  summarise(
    any_biologic_post_vacc = any(drug_class == "biologic"),
    any_jak_post_vacc      = any(drug_class == "jak"),
    any_csdmard_post_vacc  = any(drug_class == "csdmard"),
    n_dmards_post_vacc     = n_distinct(ancestor_concept_id),
    .groups = "drop"
  )

# ============================================================================
# STEP 3  Lymphocyte measurements — nearest to ref_date within ±90d
# ============================================================================

message("Fetching lymphocyte measurements for shingles patients...")

lymph_sql <- "
SELECT m.person_id,
  CAST(m.measurement_date AS DATE) AS meas_date,
  m.value_as_number,
  m.unit_source_value
FROM @cdm_schema.measurement m
WHERE m.person_id IN (@person_ids)
  AND m.value_as_number IS NOT NULL
  AND m.measurement_concept_id IN (
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
    WHERE ca.ancestor_concept_id IN (4206426, 3004327)
      AND c.invalid_reason IS NULL
    UNION
    SELECT concept_id FROM @vocab_schema.concept
    WHERE concept_id IN (4206426, 3004327, 3006505, 37045722)
  )
"

lymph_raw <- run_sql(con, lymph_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = shingles_ids) |>
  mutate(meas_date = as.Date(meas_date))

lymph_near_ref <- ref_dates |>
  select(person_id, ref_date) |>
  left_join(lymph_raw, by = "person_id") |>
  filter(!is.na(meas_date),
         abs(as.integer(meas_date - ref_date)) <= 90L) |>
  mutate(days_offset = abs(as.integer(meas_date - ref_date))) |>
  group_by(person_id) |>
  slice_min(days_offset, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(lymphopenia = value_as_number < LYMPHOPENIA_THRESHOLD) |>
  select(person_id, lymphocytes = value_as_number, lymphopenia)

# ============================================================================
# STEP 4  Assemble per-patient analysis dataset
# ============================================================================

message("Assembling VZV risk-factor analysis dataset...")

vzv_analysis <- base_cohort |>
  filter(person_id %in% shingles_ids) |>
  mutate(
    age = as.integer(format(obs_start, "%Y")) - year_of_birth,
    sex = if_else(gender_concept_id == 8532L, "Female", "Male")
  ) |>
  # Outcomes
  mutate(
    outcome_phn       = person_id %in% phn_pts$person_id,
    outcome_organ     = person_id %in% organ_pts$person_id,
    outcome_post_vacc = person_id %in% unique(ep_post$person_id)
  ) |>
  # Disease flags
  left_join(disease_flags, by = "person_id") |>
  mutate(across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L)),
         across(starts_with("dx_"), as.logical)) |>
  # Vaccination status and DMARD windows
  left_join(ref_dates |> select(person_id, is_vaccinated, first_vacc_date), by = "person_id") |>
  left_join(dmard_pre_vacc,  by = "person_id") |>
  left_join(dmard_post_vacc, by = "person_id") |>
  mutate(
    across(starts_with("any_"), \(x) coalesce(x, FALSE)),
    n_dmards_pre_vacc  = if_else(is_vaccinated, coalesce(n_dmards_pre_vacc,  0L), NA_integer_),
    n_dmards_post_vacc = if_else(is_vaccinated, coalesce(n_dmards_post_vacc, 0L), NA_integer_),
    n_dmards_pre_vacc_cat = factor(
      case_when(
        is.na(n_dmards_pre_vacc)    ~ NA_character_,
        n_dmards_pre_vacc == 0L     ~ "0",
        n_dmards_pre_vacc == 1L     ~ "1",
        n_dmards_pre_vacc >= 2L     ~ "2+"
      ), levels = c("0", "1", "2+")
    ),
    n_dmards_post_vacc_cat = factor(
      case_when(
        is.na(n_dmards_post_vacc)   ~ NA_character_,
        n_dmards_post_vacc == 0L    ~ "0",
        n_dmards_post_vacc == 1L    ~ "1",
        n_dmards_post_vacc >= 2L    ~ "2+"
      ), levels = c("0", "1", "2+")
    )
  ) |>
  # Lymphopenia
  left_join(lymph_near_ref, by = "person_id") |>
  select(
    person_id,
    age, sex,
    outcome_phn, outcome_organ, outcome_post_vacc,
    is_vaccinated,
    dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
    any_biologic_pre_vacc, any_jak_pre_vacc, any_csdmard_pre_vacc,
    any_biologic_post_vacc, any_jak_post_vacc, any_csdmard_post_vacc,
    n_dmards_pre_vacc_cat, n_dmards_post_vacc_cat,
    lymphocytes, lymphopenia
  )

message(sprintf("Analysis dataset: %d shingles patients.", nrow(vzv_analysis)))
message(sprintf("  PHN: %d | Organ invasive: %d | Post-vaccine: %d",
                sum(vzv_analysis$outcome_phn),
                sum(vzv_analysis$outcome_organ),
                sum(vzv_analysis$outcome_post_vacc)))

# ============================================================================
# Helper: build a gtsummary comparison table for a given binary outcome
# ============================================================================

# shared variable labels
VAR_LABELS <- list(
  age                     = "Age, years",
  dx_sle                  = "SLE",
  dx_dm_myositis          = "Dermatomyositis / Myositis",
  dx_ssc                  = "Systemic Sclerosis (SSc)",
  dx_gca                  = "Giant Cell Arteritis (GCA)",
  dx_ra                   = "Rheumatoid Arthritis (RA)",
  dx_spa                  = "Spondyloarthropathy (SpA)",
  dx_vasculitis           = "ANCA-Associated Vasculitis",
  any_biologic_pre_vacc   = "Any biologic (90d pre-vaccine)",
  any_jak_pre_vacc        = "Any JAK inhibitor (90d pre-vaccine)",
  any_csdmard_pre_vacc    = "Any csDMARD (90d pre-vaccine)",
  any_biologic_post_vacc  = "Any biologic (30d post-vaccine)",
  any_jak_post_vacc       = "Any JAK inhibitor (30d post-vaccine)",
  any_csdmard_post_vacc   = "Any csDMARD (30d post-vaccine)",
  n_dmards_pre_vacc_cat   = "N DMARDs prescribed (90d pre-vaccine)",
  n_dmards_post_vacc_cat  = "N DMARDs prescribed (30d post-vaccine)",
  lymphopenia             = "Lymphopenia (<1.0 × 10⁹/L near reference date)"
)

make_vzv_table <- function(data, outcome_col, outcome_labels,
                            title, subtitle, pop_note = NULL) {
  factor_col <- paste0(outcome_col, "_fct")
  data[[factor_col]] <- factor(
    if_else(data[[outcome_col]], outcome_labels[2], outcome_labels[1]),
    levels = outcome_labels
  )

  tbl_data <- data |>
    select(
      !!sym(factor_col),
      age,
      dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
      any_biologic_pre_vacc, any_jak_pre_vacc, any_csdmard_pre_vacc,
      any_biologic_post_vacc, any_jak_post_vacc, any_csdmard_post_vacc,
      n_dmards_pre_vacc_cat, n_dmards_post_vacc_cat,
      lymphopenia
    )
  tbl_data <- set_variable_labels(tbl_data,
    .labels = VAR_LABELS[intersect(names(VAR_LABELS), names(tbl_data))])

  tbl <- tbl_data |>
    tbl_summary(
      by        = !!sym(factor_col),
      statistic = list(
        age               ~ "{median} ({p25}, {p75})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits    = list(age ~ c(0, 0, 0), all_categorical() ~ c(0, 1)),
      missing   = "ifany",
      type      = list(
        age               ~ "continuous",
        where(is.logical) ~ "dichotomous",
        where(is.factor)  ~ "categorical"
      ),
      value     = list(where(is.logical) ~ TRUE)
    ) |>
    add_overall(last = FALSE) |>
    add_p(
      test = list(
        age               ~ "wilcox.test",
        all_categorical() ~ "chisq.test"
      )
    ) |>
    bold_labels() |>
    modify_header(
      label  ~ "**Characteristic**",
      stat_0 ~ "**Overall**\n(N = {N})",
      stat_1 ~ paste0("**", outcome_labels[1], "**\n(n = {n})"),
      stat_2 ~ paste0("**", outcome_labels[2], "**\n(n = {n})")
    ) |>
    modify_spanning_header(c(stat_1, stat_2) ~ paste0("**", title, "**")) |>
    modify_table_body(
      \(x) x |> mutate(groupname_col = case_when(
        variable == "age"                              ~ "Demographics",
        grepl("^dx_", variable)                        ~ "Rheumatologic Diagnosis",
        grepl("_pre_vacc",  variable)                  ~ "DMARD Use – 90d Pre-Vaccination¹",
        grepl("_post_vacc", variable)                  ~ "DMARD Use – 30d Post-Vaccination¹",
        variable == "lymphopenia"                      ~ "Immunologic Status",
        TRUE                                           ~ NA_character_
      ))
    ) |>
    as_gt() |>
    tab_style(
      style     = cell_text(weight = "bold", color = "#2c3e50"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style     = cell_fill(color = "#f8f9fa"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.font.names = "Arial", table.font.size = 12,
      column_labels.font.weight = "bold",
      heading.title.font.size   = 14, heading.title.font.weight = "bold",
      table.border.top.color    = "#2c3e50", table.border.top.width  = px(2),
      table.border.bottom.color = "#2c3e50", table.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#6c757d",
      column_labels.border.bottom.width = px(1)
    ) |>
    tab_header(title = title, subtitle = md(subtitle)) |>
    tab_footnote(
      footnote  = paste0(
        "Vaccination window anchored to first Shingrix dose date. ",
        if (!is.null(pop_note)) pop_note else
          "Unvaccinated patients are NA for all vaccination-window variables."
      ),
      locations = cells_row_groups(groups = c(
        "DMARD Use – 90d Pre-Vaccination¹",
        "DMARD Use – 30d Post-Vaccination¹"
      ))
    )
  tbl
}

# ============================================================================
# TABLE A  PHN outcome  (all shingles patients)
# ============================================================================

message("Building Table A: PHN outcome...")

table_a_phn <- make_vzv_table(
  data            = vzv_analysis,
  outcome_col     = "outcome_phn",
  outcome_labels  = c("No PHN", "PHN"),
  title           = "Table A. Clinical Factors by Post-Herpetic Neuralgia (PHN) Status",
  subtitle        = sprintf(
    "Among %d shingles patients; %d (%.1f%%) developed PHN",
    nrow(vzv_analysis),
    sum(vzv_analysis$outcome_phn),
    100 * mean(vzv_analysis$outcome_phn)
  )
)
print(table_a_phn)

# ============================================================================
# TABLE B  Organ invasive / disseminated VZV  (all shingles patients)
# ============================================================================

message("Building Table B: Organ invasive VZV outcome...")

table_b_organ <- make_vzv_table(
  data            = vzv_analysis,
  outcome_col     = "outcome_organ",
  outcome_labels  = c("No organ involvement", "Organ invasive VZV"),
  title           = "Table B. Clinical Factors by Organ Invasive / Disseminated VZV Status",
  subtitle        = sprintf(
    "Among %d shingles patients; %d (%.1f%%) had organ invasive/disseminated disease",
    nrow(vzv_analysis),
    sum(vzv_analysis$outcome_organ),
    100 * mean(vzv_analysis$outcome_organ)
  )
)
print(table_b_organ)

# ============================================================================
# TABLE C  Post-vaccine VZV breakthrough  (vaccinated shingles patients only)
# ============================================================================

message("Building Table C: Post-vaccine VZV outcome (vaccinated patients only)...")

vzv_vaccinated <- vzv_analysis |> filter(is_vaccinated)

table_c_postvax <- make_vzv_table(
  data            = vzv_vaccinated,
  outcome_col     = "outcome_post_vacc",
  outcome_labels  = c("Pre-vaccine shingles", "Post-vaccine shingles"),
  title           = "Table C. Clinical Factors by Post-Vaccine VZV Breakthrough Status",
  subtitle        = sprintf(
    "Among %d vaccinated shingles patients; %d (%.1f%%) had post-vaccine breakthrough",
    nrow(vzv_vaccinated),
    sum(vzv_vaccinated$outcome_post_vacc),
    100 * mean(vzv_vaccinated$outcome_post_vacc)
  ),
  pop_note        = paste0(
    "All patients in this table received Shingrix; window anchored to first dose. ",
    "Post-vaccine = shingles episode ≥", VACC_ONSET_DAYS,
    " days after first dose."
  )
)
print(table_c_postvax)

# ============================================================================
# Save
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output/VZV_RiskFactors")) dir.create("output/VZV_RiskFactors", recursive = TRUE)

gtsave(table_a_phn,      file.path("output/VZV_RiskFactors", paste0("tableA_phn_",          dt, ".html")))
gtsave(table_b_organ,    file.path("output/VZV_RiskFactors", paste0("tableB_organ_",         dt, ".html")))
gtsave(table_c_postvax,  file.path("output/VZV_RiskFactors", paste0("tableC_post_vaccine_",  dt, ".html")))
gtsave(table_a_phn,      file.path("output/VZV_RiskFactors", paste0("tableA_phn_",           dt, ".docx")))
gtsave(table_b_organ,    file.path("output/VZV_RiskFactors", paste0("tableB_organ_",          dt, ".docx")))
gtsave(table_c_postvax,  file.path("output/VZV_RiskFactors", paste0("tableC_post_vaccine_",   dt, ".docx")))

message("Done. Outputs saved to output/VZV_RiskFactors/")
