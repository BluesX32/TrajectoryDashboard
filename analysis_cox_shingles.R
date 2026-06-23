# analysis_cox_shingles.R
# Cox proportional hazards models for three VZV outcomes among vaccinated
# shingles patients in the prevalent RD cohort.
#
# ── Outcomes (follow-up starts at first_vacc_date + VACC_ONSET_DAYS) ─────────
#   1. Post-vaccine shingles episode (breakthrough VZV)
#   2. Post-herpetic neuralgia (PHN)
#   3. Organ-involved / disseminated VZV
#
# ── Predictors ───────────────────────────────────────────────────────────────
#   age            (continuous, at vaccination date)
#   rd_category    (factor: 7 RD classes + "More than 1 diagnosis"; ref = RA)
#   dmard_class_pre  (dominant DMARD class 90d before vaccination; ref = None)
#   dmard_class_post (dominant DMARD class 30d after vaccination;  ref = None)
#   n_dmards_pre_cat (N distinct drugs 90d pre-vaccine; factor 0/1/2/3+)
#   lymphopenia    (< 1.0 ×10⁹/L nearest to vacc date ±90d; MICE imputed)
#
# ── Cohort ───────────────────────────────────────────────────────────────────
#   Vaccinated RD patients who had shingles (shingles_vaccine_ids from prelim).
#   This is a subset of shingles_ids and dmard_exposures covers all of them.
#
# ── How to run ───────────────────────────────────────────────────────────────
#   Run preliminary_tables_shingles.R first (or set STANDALONE <- TRUE).
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang,     lib.loc = "~/R/win-library/4.5")
library(dplyr,     lib.loc = "C:/Program Files/RPackages")
library(tidyr,     lib.loc = "C:/Program Files/RPackages")
library(ggplot2,   lib.loc = "C:/Program Files/RPackages")
library(survival)
library(mice)

STANDALONE <- FALSE

LYMPHOPENIA_THRESHOLD <- 1.0   # lymphocytes < 1.0 × 10⁹/L
VACC_ONSET_DAYS       <- 14L   # days after first dose before event counts as post-vaccine

# ── DMARD ancestor → broad class mapping (corrected, matches inst/json) ───────
BIOLOGIC_ANCESTORS <- c(
  1119119L, 937368L, 19041065L, 912263L, 1151789L,   # TNF-i
  1594587L, 40171288L,                                # IL-6-i
  40161532L, 1511348L, 1593700L,                      # IL-12/23-i
  45892883L, 35603563L, 746895L,                      # IL-17-i
  701470L,                                            # Type 1 IFN-i
  1186087L,                                           # T-cell co-stim
  40236987L,                                          # BAFF-i
  1314273L, 44507676L                                 # CD19/CD20
)
JAK_ANCESTORS <- c(1361580L, 42904205L, 40244464L, 1510627L)
CSDMARD_ANCESTORS <- c(
  1305058L, 1101898L, 964339L, 1777087L,             # csDMARD
  19014878L, 19068900L, 19003999L,                   # antimetabolite
  950637L, 739590L,                                  # CNI
  1310317L                                            # alkylating
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

if (STANDALONE || !exists("shingles_vaccine_ids") || !exists("ep_post")) {
  message("Sourcing preliminary_tables_shingles.R ...")
  source("preliminary_tables_shingles.R")
}

# ============================================================================
# STEP 1  Vaccinated cohort + observation end dates
# shingles_vaccine_ids are all in shingles_ids so dmard_exposures covers them.
# ============================================================================

obs_end_sql <- "
SELECT person_id,
  CAST(MAX(observation_period_end_date) AS DATE) AS obs_end
FROM @cdm_schema.observation_period
WHERE person_id IN (@person_ids)
GROUP BY person_id
"

message("Fetching observation end dates for vaccinated shingles patients...")
obs_end_df <- run_sql(con, obs_end_sql,
                      cdm_schema = cdm,
                      person_ids = shingles_vaccine_ids) |>
  mutate(obs_end = as.Date(obs_end))

first_vacc <- vacc_bulk |>
  group_by(person_id) |>
  summarise(first_vacc_date = min(vacc_date), .groups = "drop")

vacc_cohort <- base_cohort |>
  filter(person_id %in% shingles_vaccine_ids) |>
  select(person_id, year_of_birth, gender_concept_id, obs_start) |>
  inner_join(first_vacc,  by = "person_id") |>
  left_join(obs_end_df,   by = "person_id") |>
  mutate(
    entry_date = first_vacc_date + VACC_ONSET_DAYS,
    age        = as.integer(format(first_vacc_date, "%Y")) - year_of_birth
  ) |>
  filter(!is.na(obs_end), obs_end > entry_date)

# ============================================================================
# STEP 2  Time-to-event for each outcome (all measured from entry_date)
# ============================================================================

# Outcome 1: first post-vaccine shingles episode
first_vzv <- ep_post |>
  filter(person_id %in% vacc_cohort$person_id) |>
  group_by(person_id) |>
  summarise(vzv_date = min(condition_start_date), .groups = "drop")

# Outcome 2: first post-entry PHN
post_phn <- phn_pts |>
  inner_join(vacc_cohort |> select(person_id, entry_date), by = "person_id") |>
  filter(complication_date >= entry_date) |>
  group_by(person_id) |>
  summarise(phn_date = min(complication_date), .groups = "drop")

# Outcome 3: first post-entry organ-involved VZV
post_organ <- organ_pts |>
  inner_join(vacc_cohort |> select(person_id, entry_date), by = "person_id") |>
  filter(complication_date >= entry_date) |>
  group_by(person_id) |>
  summarise(organ_date = min(complication_date), .groups = "drop")

cox_base <- vacc_cohort |>
  left_join(first_vzv,  by = "person_id") |>
  left_join(post_phn,   by = "person_id") |>
  left_join(post_organ, by = "person_id") |>
  mutate(
    time_vzv    = as.integer(coalesce(vzv_date,   obs_end) - entry_date),
    event_vzv   = !is.na(vzv_date),
    time_phn    = as.integer(coalesce(phn_date,   obs_end) - entry_date),
    event_phn   = !is.na(phn_date),
    time_organ  = as.integer(coalesce(organ_date, obs_end) - entry_date),
    event_organ = !is.na(organ_date)
  ) |>
  filter(time_vzv > 0L)

message(sprintf(
  "Vaccinated cohort: %d | Events — breakthrough VZV: %d  PHN: %d  Organ: %d",
  nrow(cox_base), sum(cox_base$event_vzv),
  sum(cox_base$event_phn), sum(cox_base$event_organ)
))

# ============================================================================
# STEP 3  Disease flags → rd_category
# ============================================================================

cox_base <- cox_base |>
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

# ============================================================================
# STEP 4  DMARD class factor (pre-vacc 90d / post-vacc 30d) and drug count
# Dominant class priority: JAK > Biologic > csDMARD.
# If ≥2 classes present: "Multiple classes".
# ============================================================================

classify_broad <- function(anc_id) {
  dplyr::case_when(
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

dmard_class_pre  <- make_dmard_class(dmard_exposures, first_vacc,
                                      "first_vacc_date", -90L, 0L, "dmard_class_pre")
dmard_class_post <- make_dmard_class(dmard_exposures, first_vacc,
                                      "first_vacc_date",  1L, 30L, "dmard_class_post")

n_dmards_pre <- dmard_exposures |>
  inner_join(first_vacc, by = "person_id") |>
  filter(drug_date >= first_vacc_date - 90L, drug_date <= first_vacc_date) |>
  distinct(person_id, ancestor_concept_id) |>
  count(person_id, name = "n_pre")

cox_base <- cox_base |>
  left_join(dmard_class_pre,  by = "person_id") |>
  left_join(dmard_class_post, by = "person_id") |>
  left_join(n_dmards_pre,     by = "person_id") |>
  mutate(
    dmard_class_pre  = factor(coalesce(as.character(dmard_class_pre),  "None"),
                               levels = DMARD_CLASS_LEVELS),
    dmard_class_post = factor(coalesce(as.character(dmard_class_post), "None"),
                               levels = DMARD_CLASS_LEVELS),
    n_dmards_pre_cat = factor(
      case_when(
        is.na(n_pre) | n_pre == 0L ~ "0",
        n_pre == 1L                ~ "1",
        n_pre == 2L                ~ "2",
        n_pre >= 3L                ~ "3+"
      ),
      levels = c("0", "1", "2", "3+")
    )
  )

# ============================================================================
# STEP 5  Lymphopenia (nearest to vaccination ±90d)
# ============================================================================

message("Fetching lymphocyte measurements for vaccinated shingles patients...")

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
                     person_ids   = shingles_vaccine_ids) |>
  mutate(meas_date = as.Date(meas_date))

lymph_near_vacc <- first_vacc |>
  left_join(lymph_raw, by = "person_id") |>
  filter(!is.na(meas_date),
         abs(as.integer(meas_date - first_vacc_date)) <= 90L) |>
  mutate(days_offset = abs(as.integer(meas_date - first_vacc_date))) |>
  group_by(person_id) |>
  slice_min(days_offset, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(lymphopenia = value_as_number < LYMPHOPENIA_THRESHOLD) |>
  select(person_id, lymphopenia)

cox_base <- cox_base |>
  left_join(lymph_near_vacc, by = "person_id")

pct_lymph <- round(100 * mean(!is.na(cox_base$lymphopenia)), 1)
message(sprintf("Lymphopenia data available: %.1f%% of vaccinated patients", pct_lymph))

# ============================================================================
# STEP 6  Final analysis dataset
# ============================================================================

cox_df <- cox_base |>
  transmute(
    person_id,
    time_vzv, event_vzv,
    time_phn, event_phn,
    time_organ, event_organ,
    age,
    rd_category,
    dmard_class_pre,
    dmard_class_post,
    n_dmards_pre_cat,
    lymphopenia
  )

# ============================================================================
# STEP 7  MICE multiple imputation — lymphopenia only
# 5 datasets, binary logistic regression; all other predictors are complete.
# ============================================================================

message("Running MICE (m=5) for lymphopenia ...")

imp_vars  <- cox_df |>
  select(age, rd_category, dmard_class_pre, dmard_class_post,
         n_dmards_pre_cat, lymphopenia)

mice_init <- mice::mice(imp_vars, maxit = 0, print = FALSE)
meth <- mice_init$method
meth[names(meth) != "lymphopenia"] <- ""
meth["lymphopenia"] <- "logreg"

imp <- mice::mice(imp_vars, m = 5, maxit = 10, method = meth,
                  seed = 42, printFlag = FALSE)

# Bind survival columns back onto each completed dataset
completed_dfs <- lapply(seq_len(imp$m), function(i) {
  d <- mice::complete(imp, i)
  d$time_vzv    <- cox_df$time_vzv
  d$event_vzv   <- cox_df$event_vzv
  d$time_phn    <- cox_df$time_phn
  d$event_phn   <- cox_df$event_phn
  d$time_organ  <- cox_df$time_organ
  d$event_organ <- cox_df$event_organ
  d
})

# ============================================================================
# STEP 8  Fit Cox models on each imputed dataset, pool via Rubin's rules
# ============================================================================

COX_RHS <- ~ age + rd_category + dmard_class_pre + dmard_class_post +
              n_dmards_pre_cat + lymphopenia

fit_pooled_cox <- function(data_list, time_col, event_col) {
  fits <- lapply(data_list, function(d) {
    frm <- as.formula(
      paste0("Surv(", time_col, ", ", event_col, ") ",
             paste(deparse(COX_RHS), collapse = ""))
    )
    survival::coxph(frm, data = d, ties = "efron")
  })
  fits
}

message("Fitting Cox models on 5 imputed datasets × 3 outcomes...")

fits_vzv   <- fit_pooled_cox(completed_dfs, "time_vzv",   "event_vzv")
fits_phn   <- fit_pooled_cox(completed_dfs, "time_phn",   "event_phn")
fits_organ <- fit_pooled_cox(completed_dfs, "time_organ", "event_organ")

# Rubin's rules (manual, to avoid mice mids compatibility issues with added cols)
pool_rubin <- function(fits, outcome_label) {
  m <- length(fits)
  est_list <- lapply(fits, function(f) {
    tibble::tibble(term = names(coef(f)), est = coef(f), se = sqrt(diag(vcov(f))))
  })
  dplyr::bind_rows(est_list, .id = "imp") |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      Q_bar   = mean(est),
      U_bar   = mean(se^2),
      B       = var(est),
      T_var   = U_bar + (1 + 1/m) * B,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      se_pool = sqrt(T_var),
      hr      = exp(Q_bar),
      ci_lo   = exp(Q_bar - 1.96 * se_pool),
      ci_hi   = exp(Q_bar + 1.96 * se_pool),
      p_value = 2 * pnorm(abs(Q_bar / se_pool), lower.tail = FALSE),
      outcome = outcome_label
    )
}

hr_vzv   <- pool_rubin(fits_vzv,   "Post-vaccine shingles")
hr_phn   <- pool_rubin(fits_phn,   "Post-herpetic neuralgia")
hr_organ <- pool_rubin(fits_organ, "Organ-involved VZV")

hr_all <- dplyr::bind_rows(hr_vzv, hr_phn, hr_organ) |>
  dplyr::select(outcome, term, hr, ci_lo, ci_hi, p_value)

message("\n── Pooled Hazard Ratios (exponentiated) ──")
print(hr_all |> dplyr::mutate(dplyr::across(c(hr, ci_lo, ci_hi), \(x) round(x, 3)),
                               p_value = round(p_value, 4)),
      n = Inf)

# ============================================================================
# STEP 9  Forest plot
# ============================================================================

TERM_LABELS <- c(
  "age"                                           = "Age (per year)",
  "rd_categorySLE"                                = "RD: SLE",
  "rd_categoryDermatomyositis / Myositis"         = "RD: DM / Myositis",
  "rd_categorySystemic Sclerosis (SSc)"           = "RD: SSc",
  "rd_categoryGiant Cell Arteritis (GCA)"         = "RD: GCA",
  "rd_categorySpondyloarthropathy (SpA)"          = "RD: SpA",
  "rd_categoryANCA-Associated Vasculitis"         = "RD: AAV",
  "rd_categoryMore than 1 diagnosis"              = "RD: Multiple diagnoses",
  "dmard_class_precsDMARD"                        = "Pre-vacc DMARD: csDMARD",
  "dmard_class_preBiologic"                       = "Pre-vacc DMARD: Biologic",
  "dmard_class_preJAK inhibitor"                  = "Pre-vacc DMARD: JAK inhibitor",
  "dmard_class_preMultiple classes"               = "Pre-vacc DMARD: Multiple",
  "dmard_class_postcsDMARD"                       = "Post-vacc DMARD: csDMARD",
  "dmard_class_postBiologic"                      = "Post-vacc DMARD: Biologic",
  "dmard_class_postJAK inhibitor"                 = "Post-vacc DMARD: JAK inhibitor",
  "dmard_class_postMultiple classes"              = "Post-vacc DMARD: Multiple",
  "n_dmards_pre_cat1"                             = "N DMARDs pre-vacc: 1",
  "n_dmards_pre_cat2"                             = "N DMARDs pre-vacc: 2",
  "n_dmards_pre_cat3+"                            = "N DMARDs pre-vacc: 3+",
  "lymphopeniaTRUE"                               = "Lymphopenia (<1.0 ×10⁹/L)"
)

# Fixed term order (same across outcomes)
TERM_ORDER <- names(TERM_LABELS)

forest_df <- hr_all |>
  dplyr::filter(!grepl("Intercept", term)) |>
  dplyr::mutate(
    label   = dplyr::coalesce(TERM_LABELS[term], term),
    label   = factor(label, levels = rev(TERM_LABELS)),
    outcome = factor(outcome, levels = c("Post-vaccine shingles",
                                          "Post-herpetic neuralgia",
                                          "Organ-involved VZV"))
  )

p_forest <- ggplot2::ggplot(
    forest_df,
    ggplot2::aes(y = label, x = hr, xmin = ci_lo, xmax = ci_hi,
                 color = outcome, shape = outcome)
  ) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray55") +
  ggplot2::geom_errorbarh(height = 0.25,
                           position = ggplot2::position_dodge(0.55),
                           linewidth = 0.6) +
  ggplot2::geom_point(size = 2.5,
                       position = ggplot2::position_dodge(0.55)) +
  ggplot2::scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 4, 8),
    labels = c("0.25", "0.5", "1", "2", "4", "8")
  ) +
  ggplot2::scale_color_manual(values = c(
    "Post-vaccine shingles"   = "#2C6FAC",
    "Post-herpetic neuralgia" = "#D94F3D",
    "Organ-involved VZV"      = "#4CAF50"
  )) +
  ggplot2::labs(
    title    = "Cox regression: hazard ratios for VZV outcomes",
    subtitle = "Vaccinated RD patients | Reference: RA, no DMARD, 0 DMARDs, no lymphopenia",
    x        = "Hazard Ratio (log scale, 95% CI — pooled over 5 MICE datasets)",
    y        = NULL,
    color    = NULL, shape = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position  = "top",
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 9)
  )

print(p_forest)

# ============================================================================
# STEP 10  Save outputs
# ============================================================================

if (!dir.exists("output/Shingles")) dir.create("output/Shingles", recursive = TRUE)

ggplot2::ggsave("output/Shingles/cox_forest_vzv.png", p_forest,
                width = 12, height = 9, dpi = 150)
message("Saved: output/Shingles/cox_forest_vzv.png")

write.csv(
  hr_all |> dplyr::mutate(dplyr::across(c(hr, ci_lo, ci_hi, p_value), \(x) round(x, 4))),
  "output/Shingles/cox_hr_vzv.csv",
  row.names = FALSE
)
message("Saved: output/Shingles/cox_hr_vzv.csv")
