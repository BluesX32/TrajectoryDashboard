# analysis_cox_pjp.R
# Cox proportional hazards model for time-to-PJP in the full prevalent RD cohort.
#
# ── Outcome ──────────────────────────────────────────────────────────────────
#   First PJP infection (pneumocystis pneumonia)
#   Time at risk: cohort entry (obs_start) → first PJP date, or obs_end if none.
#
# ── Predictors ───────────────────────────────────────────────────────────────
#   rd_category   (factor: 7 RD classes + "More than 1 diagnosis"; ref = RA)
#   any_dmard     (y/n): any DMARD exposure in 90 days after cohort entry
#
# ── DMARD note ───────────────────────────────────────────────────────────────
#   The user's target is "DMARD 90d before PJP infection."  For Cox regression
#   this cannot be measured at the event date for non-PJP patients without
#   introducing bias.  Baseline DMARD exposure at cohort entry (obs_start,
#   +90d window) is used instead: it captures the patient's treatment status
#   at the start of follow-up and avoids immortal-time bias.
#
# ── How to run ───────────────────────────────────────────────────────────────
#   Run preliminary_tables_pjp.R first (or set STANDALONE <- TRUE).
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang,   lib.loc = "~/R/win-library/4.5")
library(dplyr,   lib.loc = "C:/Program Files/RPackages")
library(tidyr,   lib.loc = "C:/Program Files/RPackages")
library(ggplot2, lib.loc = "C:/Program Files/RPackages")
library(survival)

STANDALONE <- FALSE

ALL_DMARD_ANCESTORS <- c(
  # csDMARD
  1305058L, 1101898L, 964339L, 1777087L,
  # Antimetabolite
  19014878L, 19068900L, 19003999L,
  # CNI
  950637L, 739590L,
  # Alkylating
  1310317L,
  # TNF-i
  1119119L, 937368L, 19041065L, 912263L, 1151789L,
  # IL-6-i
  1594587L, 40171288L,
  # IL-12/23-i
  40161532L, 1511348L, 1593700L,
  # IL-17-i
  45892883L, 35603563L, 746895L,
  # Type 1 IFN-i
  701470L,
  # JAK-i
  1361580L, 42904205L, 40244464L, 1510627L,
  # T-cell co-stim
  1186087L,
  # BAFF-i
  40236987L,
  # CD19/CD20
  1314273L, 44507676L
)

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

if (STANDALONE || !exists("base_cohort") || !exists("pjp_cohort")) {
  message("Sourcing preliminary_tables_pjp.R ...")
  source("preliminary_tables_pjp.R")
}

# ============================================================================
# STEP 1  Survival data: time from obs_start to PJP or obs_end
# base_cohort from PJP prelim has obs_start and obs_end.
# ============================================================================

cox_pjp_df <- base_cohort |>
  select(person_id, obs_start, obs_end) |>
  mutate(
    pjp_date  = pjp_cohort$index_date[match(person_id, pjp_cohort$person_id)],
    event_pjp = person_id %in% pjp_ids,
    exit_date = dplyr::if_else(event_pjp, pjp_date, obs_end),
    time_pjp  = as.integer(exit_date - obs_start)
  ) |>
  filter(time_pjp > 0L)

message(sprintf(
  "PJP Cox cohort: %d patients — PJP events: %d (%.1f%%)",
  nrow(cox_pjp_df),
  sum(cox_pjp_df$event_pjp),
  100 * mean(cox_pjp_df$event_pjp)
))

# ============================================================================
# STEP 2  rd_category (mutually exclusive)
# ============================================================================

cox_pjp_df <- cox_pjp_df |>
  dplyr::left_join(disease_flags, by = "person_id") |>
  dplyr::mutate(dplyr::across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L)),
                dplyr::across(starts_with("dx_"), as.logical)) |>
  dplyr::mutate(
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
# STEP 3  Baseline DMARD exposure (any DMARD in 90d from obs_start)
# Runs for all cohort members — the PJP prelim only fetched for pjp_ids.
# ============================================================================

message("Fetching baseline DMARD exposures for the full RD cohort...")

dmard_baseline_sql <- "
SELECT DISTINCT de.person_id,
  1 AS any_dmard
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
JOIN @cdm_schema.observation_period op
  ON de.person_id = op.person_id
  AND de.drug_exposure_start_date BETWEEN op.observation_period_start_date
                                      AND DATEADD(day, 90, op.observation_period_start_date)
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (
    1305058,  1101898,  964339,   1777087,
    19014878, 19068900, 19003999,
    950637,   739590,
    1310317,
    1119119,  937368,   19041065, 912263,   1151789,
    1594587,  40171288,
    40161532, 1511348,  1593700,
    45892883, 35603563, 746895,
    701470,
    1361580,  42904205, 40244464, 1510627,
    1186087,
    40236987,
    1314273,  44507676
  )
"

dmard_baseline <- run_sql(con, dmard_baseline_sql,
                           cdm_schema   = cdm,
                           vocab_schema = vocab,
                           person_ids   = cohort_ids)

cox_pjp_df <- cox_pjp_df |>
  dplyr::left_join(dmard_baseline, by = "person_id") |>
  dplyr::mutate(any_dmard = !is.na(any_dmard))

message(sprintf(
  "Baseline DMARD exposure (90d from cohort entry): %d / %d (%.1f%%)",
  sum(cox_pjp_df$any_dmard), nrow(cox_pjp_df),
  100 * mean(cox_pjp_df$any_dmard)
))

# ============================================================================
# STEP 4  Fit Cox model
# ============================================================================

cox_pjp_final <- cox_pjp_df |>
  dplyr::transmute(
    person_id,
    time_pjp, event_pjp,
    rd_category,
    any_dmard
  ) |>
  tidyr::drop_na(rd_category)

cox_fit <- survival::coxph(
  Surv(time_pjp, event_pjp) ~ rd_category + any_dmard,
  data  = cox_pjp_final,
  ties  = "efron"
)

message("\n── Cox model summary ──")
print(summary(cox_fit))

# ============================================================================
# STEP 5  Extract HR table
# ============================================================================

hr_df <- broom::tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::rename(hr = estimate, ci_lo = conf.low, ci_hi = conf.high) |>
  dplyr::select(term, hr, ci_lo, ci_hi, p.value)

TERM_LABELS_PJP <- c(
  "rd_categorySLE"                       = "RD: SLE",
  "rd_categoryDermatomyositis / Myositis"= "RD: DM / Myositis",
  "rd_categorySystemic Sclerosis (SSc)"  = "RD: SSc",
  "rd_categoryGiant Cell Arteritis (GCA)"= "RD: GCA",
  "rd_categorySpondyloarthropathy (SpA)" = "RD: SpA",
  "rd_categoryANCA-Associated Vasculitis"= "RD: AAV",
  "rd_categoryMore than 1 diagnosis"     = "RD: Multiple diagnoses",
  "any_dmardTRUE"                        = "Any DMARD at cohort entry (90d)"
)

hr_labeled <- hr_df |>
  dplyr::mutate(
    label = dplyr::coalesce(TERM_LABELS_PJP[term], term),
    label = factor(label, levels = rev(TERM_LABELS_PJP))
  )

message("\n── Hazard Ratios ──")
print(hr_labeled |>
        dplyr::select(label, hr, ci_lo, ci_hi, p.value) |>
        dplyr::mutate(dplyr::across(c(hr, ci_lo, ci_hi), \(x) round(x, 3)),
                      p.value = round(p.value, 4)),
      n = Inf)

# ============================================================================
# STEP 6  Forest plot
# ============================================================================

n_total  <- nrow(cox_pjp_final)
n_events <- sum(cox_pjp_final$event_pjp)

p_forest_pjp <- ggplot2::ggplot(
    hr_labeled |> dplyr::filter(!is.na(label)),
    ggplot2::aes(y = label, x = hr, xmin = ci_lo, xmax = ci_hi)
  ) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray55") +
  ggplot2::geom_errorbarh(height = 0.3, color = "#2C6FAC", linewidth = 0.7) +
  ggplot2::geom_point(size = 3, color = "#2C6FAC") +
  ggplot2::scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 4, 8, 16),
    labels = c("0.25", "0.5", "1", "2", "4", "8", "16")
  ) +
  ggplot2::labs(
    title    = "Cox regression: hazard ratios for PJP infection",
    subtitle = sprintf(
      "Full RD cohort (N = %d, events = %d) | Reference: RA, no baseline DMARD",
      n_total, n_events
    ),
    x = "Hazard Ratio (log scale, 95% CI)",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 10)
  )

print(p_forest_pjp)

# ============================================================================
# STEP 7  Save outputs
# ============================================================================

if (!dir.exists("output/PJP")) dir.create("output/PJP", recursive = TRUE)

ggplot2::ggsave("output/PJP/cox_forest_pjp.png", p_forest_pjp,
                width = 9, height = 6, dpi = 150)
message("Saved: output/PJP/cox_forest_pjp.png")

write.csv(
  hr_labeled |>
    dplyr::select(term, label, hr, ci_lo, ci_hi, p.value) |>
    dplyr::mutate(dplyr::across(c(hr, ci_lo, ci_hi, p.value), \(x) round(x, 4))),
  "output/PJP/cox_hr_pjp.csv",
  row.names = FALSE
)
message("Saved: output/PJP/cox_hr_pjp.csv")
