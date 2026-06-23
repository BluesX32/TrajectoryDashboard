# analysis_cox_shingles_incident.R
# Cox proportional hazards models for three VZV outcomes — INCIDENT RD cohort.
#
# ── Outcomes ─────────────────────────────────────────────────────────────────
#   1. Post-vaccine shingles (breakthrough VZV ≥14d after first Shingrix dose)
#      Cohort: vaccinated incident shingles patients
#      Entry:  first_vacc_date + VACC_ONSET_DAYS
#
#   2. PHN (total — vaccinated + unvaccinated) [larger N justifies total count]
#      Cohort: ALL incident shingles patients
#      Entry:  first collapsed shingles episode date
#
#   3. Organ-involved / disseminated VZV (total — post-vacc n too small)
#      Cohort: ALL incident shingles patients (same as outcome 2)
#      Entry:  first collapsed shingles episode date
#
# ── Predictors ───────────────────────────────────────────────────────────────
#   Outcome 1 (vaccinated cohort):
#     age · rd_category · dmard_class_pre (90d before vacc) · dmard_class_post
#     (30d after vacc) · n_dmards_pre_cat · lymphopenia (±90d from vacc date)
#
#   Outcomes 2 & 3 (full shingles cohort):
#     age · rd_category · dmard_class_shingles (90d before first episode) ·
#     n_dmards_shingles_cat · is_vaccinated · lymphopenia (±90d from episode)
#
# ── MICE ─────────────────────────────────────────────────────────────────────
#   Two separate MICE runs (m=5) for lymphopenia:
#   one anchored at vacc date (outcome 1), one at first shingles date (2 & 3).
#
# ── How to run ───────────────────────────────────────────────────────────────
#   Run preliminary_tables_shingles_incident.R first, or STANDALONE <- TRUE.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang,   lib.loc = "~/R/win-library/4.5")
library(dplyr,   lib.loc = "C:/Program Files/RPackages")
library(tidyr,   lib.loc = "C:/Program Files/RPackages")
library(ggplot2, lib.loc = "C:/Program Files/RPackages")
library(survival)
library(mice)

STANDALONE <- FALSE

LYMPHOPENIA_THRESHOLD <- 1.0   # lymphocytes < 1.0 × 10⁹/L
VACC_ONSET_DAYS       <- 14L

# ── DMARD ancestor constants (corrected to match inst/json) ──────────────────
BIOLOGIC_ANCESTORS <- c(
  1119119L, 937368L, 19041065L, 912263L, 1151789L,
  1594587L, 40171288L,
  40161532L, 1511348L, 1593700L,
  45892883L, 35603563L, 746895L,
  701470L,
  1186087L,
  40236987L,
  1314273L, 44507676L
)
JAK_ANCESTORS     <- c(1361580L, 42904205L, 40244464L, 1510627L)
CSDMARD_ANCESTORS <- c(
  1305058L, 1101898L, 964339L, 1777087L,
  19014878L, 19068900L, 19003999L,
  950637L, 739590L,
  1310317L
)
ALL_DMARD_ANCESTORS <- c(BIOLOGIC_ANCESTORS, JAK_ANCESTORS, CSDMARD_ANCESTORS)

DMARD_CLASS_LEVELS <- c("None", "csDMARD", "Biologic", "JAK inhibitor", "Multiple classes")

RD_LEVELS <- c(
  "SLE", "Dermatomyositis / Myositis", "Systemic Sclerosis (SSc)",
  "Giant Cell Arteritis (GCA)", "Rheumatoid Arthritis (RA)",
  "Spondyloarthropathy (SpA)", "ANCA-Associated Vasculitis",
  "More than 1 diagnosis"
)

# ============================================================================
# Prerequisites
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

if (STANDALONE || !exists("shingles_vaccine_ids") || !exists("episodes_cl")) {
  message("Sourcing preliminary_tables_shingles_incident.R ...")
  source("preliminary_tables_shingles_incident.R")
}

# ============================================================================
# STEP 1  Reference dates
# first_shingles_date: entry point for PHN / organ models (outcomes 2 & 3)
# first_vacc_date:     entry point for post-vaccine shingles model (outcome 1)
# base_cohort from incident prelim already contains obs_start and obs_end.
# ============================================================================

first_shingles <- episodes_cl |>
  group_by(person_id) |>
  summarise(first_shingles_date = min(condition_start_date), .groups = "drop")

first_vacc <- vacc_bulk |>
  group_by(person_id) |>
  summarise(first_vacc_date = min(vacc_date), .groups = "drop")

# ── rd_category derivation helper ────────────────────────────────────────────
add_rd_category <- function(df) {
  df |>
    left_join(disease_flags, by = "person_id") |>
    mutate(across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L)),
           across(starts_with("dx_"), as.logical)) |>
    mutate(
      n_dx = rowSums(across(starts_with("dx_"))),
      rd_category = factor(case_when(
        n_dx > 1L      ~ "More than 1 diagnosis",
        dx_sle         ~ "SLE",
        dx_dm_myositis ~ "Dermatomyositis / Myositis",
        dx_ssc         ~ "Systemic Sclerosis (SSc)",
        dx_gca         ~ "Giant Cell Arteritis (GCA)",
        dx_ra          ~ "Rheumatoid Arthritis (RA)",
        dx_spa         ~ "Spondyloarthropathy (SpA)",
        dx_vasculitis  ~ "ANCA-Associated Vasculitis",
        TRUE           ~ NA_character_
      ), levels = RD_LEVELS)
    )
}

# ── DMARD broad-class helper ──────────────────────────────────────────────────
classify_broad <- function(anc_id) {
  case_when(
    anc_id %in% JAK_ANCESTORS      ~ "JAK inhibitor",
    anc_id %in% BIOLOGIC_ANCESTORS ~ "Biologic",
    TRUE                            ~ "csDMARD"
  )
}

make_dmard_class <- function(exposures, dates_df, ref_col, lo, hi, out_col) {
  hits <- exposures |>
    inner_join(dates_df |> select(person_id, .ref = !!rlang::sym(ref_col)),
               by = "person_id") |>
    filter(drug_date >= .ref + lo, drug_date <= .ref + hi) |>
    distinct(person_id, ancestor_concept_id) |>
    mutate(cls = classify_broad(ancestor_concept_id)) |>
    group_by(person_id) |>
    summarise(
      n_cls = n_distinct(cls),
      top   = if_else(n_cls > 1L, "Multiple classes",
               if_else(any(cls == "JAK inhibitor"), "JAK inhibitor",
               if_else(any(cls == "Biologic"),      "Biologic", "csDMARD"))),
      .groups = "drop"
    )
  dates_df |>
    select(person_id) |>
    left_join(hits |> select(person_id, top), by = "person_id") |>
    mutate(!!out_col := factor(coalesce(top, "None"), levels = DMARD_CLASS_LEVELS)) |>
    select(person_id, !!out_col)
}

make_n_dmards <- function(exposures, dates_df, ref_col, lo, hi, out_col) {
  exposures |>
    inner_join(dates_df |> select(person_id, .ref = !!rlang::sym(ref_col)),
               by = "person_id") |>
    filter(drug_date >= .ref + lo, drug_date <= .ref + hi) |>
    distinct(person_id, ancestor_concept_id) |>
    count(person_id, name = "n_drugs") |>
    right_join(dates_df |> select(person_id), by = "person_id") |>
    mutate(!!out_col := factor(
      case_when(
        is.na(n_drugs) | n_drugs == 0L ~ "0",
        n_drugs == 1L                  ~ "1",
        n_drugs == 2L                  ~ "2",
        n_drugs >= 3L                  ~ "3+"
      ),
      levels = c("0", "1", "2", "3+")
    )) |>
    select(person_id, !!out_col)
}

# ============================================================================
# STEP 2  Lymphocyte measurements for all incident shingles patients
# Single SQL query; slice differently per outcome below.
# ============================================================================

message("Fetching lymphocyte measurements for incident shingles patients...")

lymph_sql <- "
SELECT m.person_id,
  CAST(m.measurement_date AS DATE) AS meas_date,
  m.value_as_number
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

nearest_lymph <- function(ref_df, ref_col, window = 90L) {
  ref_df |>
    select(person_id, .ref = !!rlang::sym(ref_col)) |>
    left_join(lymph_raw, by = "person_id") |>
    filter(!is.na(meas_date),
           abs(as.integer(meas_date - .ref)) <= window) |>
    mutate(days_off = abs(as.integer(meas_date - .ref))) |>
    group_by(person_id) |>
    slice_min(days_off, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    mutate(lymphopenia = value_as_number < LYMPHOPENIA_THRESHOLD) |>
    select(person_id, lymphopenia)
}

# ============================================================================
# STEP 3  OUTCOME 1 — Post-vaccine shingles (vaccinated cohort)
# ============================================================================

message("Building vaccinated cohort (outcome 1: post-vaccine shingles) ...")

first_vzv_postvacc <- ep_post |>
  group_by(person_id) |>
  summarise(vzv_date = min(condition_start_date), .groups = "drop")

vacc_cox_df <- base_cohort |>
  filter(person_id %in% shingles_vaccine_ids) |>
  select(person_id, year_of_birth, obs_end) |>
  inner_join(first_vacc, by = "person_id") |>
  mutate(
    entry_date = first_vacc_date + VACC_ONSET_DAYS,
    age        = as.integer(format(first_vacc_date, "%Y")) - year_of_birth
  ) |>
  filter(obs_end > entry_date) |>
  left_join(first_vzv_postvacc, by = "person_id") |>
  mutate(
    time_vzv  = as.integer(coalesce(vzv_date, obs_end) - entry_date),
    event_vzv = !is.na(vzv_date)
  ) |>
  filter(time_vzv > 0L) |>
  add_rd_category() |>
  left_join(make_dmard_class(dmard_exposures, first_vacc,
                              "first_vacc_date", -90L, 0L, "dmard_class_pre"),
            by = "person_id") |>
  left_join(make_dmard_class(dmard_exposures, first_vacc,
                              "first_vacc_date",  1L, 30L, "dmard_class_post"),
            by = "person_id") |>
  left_join(make_n_dmards(dmard_exposures, first_vacc,
                           "first_vacc_date", -90L, 0L, "n_dmards_pre_cat"),
            by = "person_id") |>
  left_join(nearest_lymph(first_vacc, "first_vacc_date"), by = "person_id") |>
  transmute(
    person_id,
    time_vzv, event_vzv,
    age,
    rd_category,
    dmard_class_pre  = factor(coalesce(as.character(dmard_class_pre),  "None"),
                               levels = DMARD_CLASS_LEVELS),
    dmard_class_post = factor(coalesce(as.character(dmard_class_post), "None"),
                               levels = DMARD_CLASS_LEVELS),
    n_dmards_pre_cat,
    lymphopenia
  )

message(sprintf(
  "Outcome 1 — vaccinated cohort: %d patients, post-vacc VZV events: %d",
  nrow(vacc_cox_df), sum(vacc_cox_df$event_vzv)
))

# ============================================================================
# STEP 4  OUTCOMES 2 & 3 — PHN total and Organ-involvement total
# Cohort: all incident shingles patients; entry = first shingles episode date.
# ============================================================================

message("Building full shingles cohort (outcomes 2 & 3: PHN + organ total) ...")

post_phn   <- phn_pts   |> select(person_id, phn_date   = complication_date)
post_organ <- organ_pts |> select(person_id, organ_date = complication_date)

shingles_cox_df <- base_cohort |>
  filter(person_id %in% shingles_ids) |>
  select(person_id, year_of_birth, obs_end) |>
  inner_join(first_shingles, by = "person_id") |>
  mutate(age = as.integer(format(first_shingles_date, "%Y")) - year_of_birth) |>
  filter(obs_end > first_shingles_date) |>
  # PHN: earliest complication date at or after first shingles episode
  left_join(
    post_phn |>
      inner_join(first_shingles, by = "person_id") |>
      filter(phn_date >= first_shingles_date) |>
      group_by(person_id) |>
      slice_min(phn_date, n = 1L, with_ties = FALSE) |>
      ungroup() |>
      select(person_id, phn_date),
    by = "person_id"
  ) |>
  # Organ involvement: same logic
  left_join(
    post_organ |>
      inner_join(first_shingles, by = "person_id") |>
      filter(organ_date >= first_shingles_date) |>
      group_by(person_id) |>
      slice_min(organ_date, n = 1L, with_ties = FALSE) |>
      ungroup() |>
      select(person_id, organ_date),
    by = "person_id"
  ) |>
  mutate(
    time_phn    = as.integer(coalesce(phn_date,   obs_end) - first_shingles_date),
    event_phn   = !is.na(phn_date),
    time_organ  = as.integer(coalesce(organ_date, obs_end) - first_shingles_date),
    event_organ = !is.na(organ_date),
    is_vaccinated = person_id %in% shingles_vaccine_ids
  ) |>
  filter(time_phn > 0L) |>
  add_rd_category() |>
  left_join(make_dmard_class(dmard_exposures, first_shingles,
                              "first_shingles_date", -90L, 0L, "dmard_class_shingles"),
            by = "person_id") |>
  left_join(make_n_dmards(dmard_exposures, first_shingles,
                           "first_shingles_date", -90L, 0L, "n_dmards_shingles_cat"),
            by = "person_id") |>
  left_join(nearest_lymph(first_shingles, "first_shingles_date"), by = "person_id") |>
  transmute(
    person_id,
    time_phn, event_phn,
    time_organ, event_organ,
    age,
    rd_category,
    dmard_class_shingles = factor(coalesce(as.character(dmard_class_shingles), "None"),
                                   levels = DMARD_CLASS_LEVELS),
    n_dmards_shingles_cat,
    is_vaccinated,
    lymphopenia
  )

message(sprintf(
  "Outcomes 2/3 — full shingles cohort: %d patients | PHN events: %d | Organ events: %d",
  nrow(shingles_cox_df),
  sum(shingles_cox_df$event_phn),
  sum(shingles_cox_df$event_organ)
))

# ============================================================================
# STEP 5  MICE — two separate imputation runs for lymphopenia
# ============================================================================

run_mice <- function(df, imp_vars) {
  d <- df |> select(all_of(imp_vars))
  init <- mice::mice(d, maxit = 0, print = FALSE)
  meth <- init$method
  meth[names(meth) != "lymphopenia"] <- ""
  meth["lymphopenia"] <- "logreg"
  mice::mice(d, m = 5, maxit = 10, method = meth, seed = 42, printFlag = FALSE)
}

message("MICE imputation for outcome 1 (vacc cohort) ...")
imp_vacc <- run_mice(
  vacc_cox_df,
  c("age", "rd_category", "dmard_class_pre", "dmard_class_post",
    "n_dmards_pre_cat", "lymphopenia")
)

message("MICE imputation for outcomes 2 & 3 (full shingles cohort) ...")
imp_shingles <- run_mice(
  shingles_cox_df,
  c("age", "rd_category", "dmard_class_shingles", "n_dmards_shingles_cat",
    "is_vaccinated", "lymphopenia")
)

# Attach survival columns to each imputed dataset
splice_surv <- function(imp, source_df, surv_cols) {
  lapply(seq_len(imp$m), function(i) {
    d <- mice::complete(imp, i)
    for (col in surv_cols) d[[col]] <- source_df[[col]]
    d
  })
}

dfs_vacc     <- splice_surv(imp_vacc, vacc_cox_df,
                             c("time_vzv", "event_vzv"))
dfs_shingles <- splice_surv(imp_shingles, shingles_cox_df,
                             c("time_phn", "event_phn", "time_organ", "event_organ"))

# ============================================================================
# STEP 6  Fit Cox models + Rubin's rules pooling
# ============================================================================

pool_rubin <- function(fits, outcome_label) {
  m <- length(fits)
  bind_rows(lapply(fits, function(f) {
    tibble(term = names(coef(f)), est = coef(f), se = sqrt(diag(vcov(f))))
  }), .id = "imp") |>
    group_by(term) |>
    summarise(
      Q_bar = mean(est),
      U_bar = mean(se^2),
      B     = var(est),
      T_var = U_bar + (1 + 1/m) * B,
      .groups = "drop"
    ) |>
    mutate(
      se_pool = sqrt(T_var),
      hr      = exp(Q_bar),
      ci_lo   = exp(Q_bar - 1.96 * se_pool),
      ci_hi   = exp(Q_bar + 1.96 * se_pool),
      p_value = 2 * pnorm(abs(Q_bar / se_pool), lower.tail = FALSE),
      outcome = outcome_label
    )
}

cox_rhs <- function(rhs_str, time_col, event_col) {
  as.formula(paste0("Surv(", time_col, ", ", event_col, ") ~ ", rhs_str))
}

message("Fitting Cox models ...")

# Outcome 1: post-vaccine shingles
fits_vzv <- lapply(dfs_vacc, function(d) {
  coxph(cox_rhs("age + rd_category + dmard_class_pre + dmard_class_post +
                  n_dmards_pre_cat + lymphopenia",
                "time_vzv", "event_vzv"),
        data = d, ties = "efron")
})

# Outcome 2: PHN total
fits_phn <- lapply(dfs_shingles, function(d) {
  coxph(cox_rhs("age + rd_category + dmard_class_shingles +
                  n_dmards_shingles_cat + is_vaccinated + lymphopenia",
                "time_phn", "event_phn"),
        data = d, ties = "efron")
})

# Outcome 3: organ involvement total
fits_organ <- lapply(dfs_shingles, function(d) {
  coxph(cox_rhs("age + rd_category + dmard_class_shingles +
                  n_dmards_shingles_cat + is_vaccinated + lymphopenia",
                "time_organ", "event_organ"),
        data = d, ties = "efron")
})

hr_vzv   <- pool_rubin(fits_vzv,   "Post-vaccine shingles")
hr_phn   <- pool_rubin(fits_phn,   "PHN (total)")
hr_organ <- pool_rubin(fits_organ, "Organ-involved VZV (total)")

hr_all_1  <- hr_vzv   |> select(outcome, term, hr, ci_lo, ci_hi, p_value)
hr_all_23 <- bind_rows(hr_phn, hr_organ) |> select(outcome, term, hr, ci_lo, ci_hi, p_value)

message("\n── Pooled HRs: Outcome 1 (post-vaccine shingles) ──")
print(hr_all_1 |> mutate(across(c(hr, ci_lo, ci_hi), \(x) round(x, 3)),
                          p_value = round(p_value, 4)), n = Inf)

message("\n── Pooled HRs: Outcomes 2 & 3 (PHN and organ total) ──")
print(hr_all_23 |> mutate(across(c(hr, ci_lo, ci_hi), \(x) round(x, 3)),
                           p_value = round(p_value, 4)), n = Inf)

# ============================================================================
# STEP 7  Forest plots
# ============================================================================

LABELS_VACC <- c(
  "age"                                        = "Age (per year)",
  "rd_categorySLE"                             = "RD: SLE",
  "rd_categoryDermatomyositis / Myositis"      = "RD: DM / Myositis",
  "rd_categorySystemic Sclerosis (SSc)"        = "RD: SSc",
  "rd_categoryGiant Cell Arteritis (GCA)"      = "RD: GCA",
  "rd_categorySpondyloarthropathy (SpA)"       = "RD: SpA",
  "rd_categoryANCA-Associated Vasculitis"      = "RD: AAV",
  "rd_categoryMore than 1 diagnosis"           = "RD: Multiple diagnoses",
  "dmard_class_precsDMARD"                     = "Pre-vacc: csDMARD",
  "dmard_class_preBiologic"                    = "Pre-vacc: Biologic",
  "dmard_class_preJAK inhibitor"               = "Pre-vacc: JAK inhibitor",
  "dmard_class_preMultiple classes"            = "Pre-vacc: Multiple",
  "dmard_class_postcsDMARD"                    = "Post-vacc: csDMARD",
  "dmard_class_postBiologic"                   = "Post-vacc: Biologic",
  "dmard_class_postJAK inhibitor"              = "Post-vacc: JAK inhibitor",
  "dmard_class_postMultiple classes"           = "Post-vacc: Multiple",
  "n_dmards_pre_cat1"                          = "N DMARDs pre-vacc: 1",
  "n_dmards_pre_cat2"                          = "N DMARDs pre-vacc: 2",
  "n_dmards_pre_cat3+"                         = "N DMARDs pre-vacc: 3+",
  "lymphopeniaTRUE"                            = "Lymphopenia (<1.0 ×10⁹/L)"
)

LABELS_SHINGLES <- c(
  "age"                                        = "Age (per year)",
  "rd_categorySLE"                             = "RD: SLE",
  "rd_categoryDermatomyositis / Myositis"      = "RD: DM / Myositis",
  "rd_categorySystemic Sclerosis (SSc)"        = "RD: SSc",
  "rd_categoryGiant Cell Arteritis (GCA)"      = "RD: GCA",
  "rd_categorySpondyloarthropathy (SpA)"       = "RD: SpA",
  "rd_categoryANCA-Associated Vasculitis"      = "RD: AAV",
  "rd_categoryMore than 1 diagnosis"           = "RD: Multiple diagnoses",
  "dmard_class_shinglescsDMARD"                = "DMARD at shingles: csDMARD",
  "dmard_class_shinglesBiologic"               = "DMARD at shingles: Biologic",
  "dmard_class_shinglesJAK inhibitor"          = "DMARD at shingles: JAK inhibitor",
  "dmard_class_shinglesMultiple classes"       = "DMARD at shingles: Multiple",
  "n_dmards_shingles_cat1"                     = "N DMARDs at shingles: 1",
  "n_dmards_shingles_cat2"                     = "N DMARDs at shingles: 2",
  "n_dmards_shingles_cat3+"                    = "N DMARDs at shingles: 3+",
  "is_vaccinatedTRUE"                          = "Shingrix vaccinated",
  "lymphopeniaTRUE"                            = "Lymphopenia (<1.0 ×10⁹/L)"
)

make_forest <- function(hr_df, labels_map, title, subtitle, color_vals) {
  fd <- hr_df |>
    filter(!grepl("Intercept", term)) |>
    mutate(
      label   = coalesce(labels_map[term], term),
      label   = factor(label, levels = rev(labels_map)),
      outcome = factor(outcome, levels = unique(outcome))
    )

  ggplot(fd, aes(y = label, x = hr, xmin = ci_lo, xmax = ci_hi,
                 color = outcome, shape = outcome)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray55") +
    geom_errorbarh(height = 0.25, position = position_dodge(0.55), linewidth = 0.6) +
    geom_point(size = 2.5, position = position_dodge(0.55)) +
    scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8),
                  labels = c("0.25", "0.5", "1", "2", "4", "8")) +
    scale_color_manual(values = color_vals) +
    labs(title    = title,
         subtitle = subtitle,
         x = "Hazard Ratio (log scale, 95% CI — pooled over 5 MICE datasets)",
         y = NULL, color = NULL, shape = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position  = "top",
          panel.grid.minor = element_blank(),
          axis.text.y      = element_text(size = 9))
}

p_vacc <- make_forest(
  hr_all_1,
  LABELS_VACC,
  title    = "Cox regression: post-vaccine shingles (incident RD cohort)",
  subtitle = sprintf("Vaccinated incident shingles patients (N = %d, events = %d) | Ref: RA, no DMARD",
                     nrow(vacc_cox_df), sum(vacc_cox_df$event_vzv)),
  color_vals = c("Post-vaccine shingles" = "#2C6FAC")
)

p_shingles <- make_forest(
  hr_all_23,
  LABELS_SHINGLES,
  title    = "Cox regression: PHN and organ involvement — total (incident RD cohort)",
  subtitle = sprintf("All incident shingles patients (N = %d) | PHN events: %d | Organ events: %d | Ref: RA, no DMARD",
                     nrow(shingles_cox_df), sum(shingles_cox_df$event_phn),
                     sum(shingles_cox_df$event_organ)),
  color_vals = c("PHN (total)"              = "#D94F3D",
                 "Organ-involved VZV (total)" = "#4CAF50")
)

print(p_vacc)
print(p_shingles)

# ============================================================================
# STEP 8  Save outputs
# ============================================================================

out_dir <- "output/Shingles_Incident"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "cox_forest_postvacc_shingles.png"),  p_vacc,
       width = 11, height = 9, dpi = 150)
ggsave(file.path(out_dir, "cox_forest_phn_organ_total.png"), p_shingles,
       width = 11, height = 9, dpi = 150)

write.csv(
  bind_rows(hr_all_1, hr_all_23) |>
    mutate(across(c(hr, ci_lo, ci_hi, p_value), \(x) round(x, 4))),
  file.path(out_dir, "cox_hr_incident.csv"),
  row.names = FALSE
)

message(sprintf("Saved to %s/", out_dir))
