# analysis_dx_overlap_shingles_incident.R
# Disease diagnosis overlap among INCIDENT shingles patients.
#
# ── Outputs ───────────────────────────────────────────────────────────────────
#   1. Console summary  : # / % patients with >1 RD diagnosis
#   2. Distribution table: n patients with exactly 1, 2, 3, … diagnoses
#   3. UpSet plot        : intersection sizes across all 7 RD disease flags
#                          (replaces Venn diagram — 7 sets are unreadable as Venn)
#   4. Co-occurrence heatmap: pairwise count of patients sharing both diagnoses
#
# ── How to run ────────────────────────────────────────────────────────────────
# Option A: run preliminary_tables_shingles_incident.R first — this script
#   reuses shingles_ids and disease_flags already in the environment.
# Option B: set STANDALONE <- TRUE to source the prerequisite automatically.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang,    lib.loc = "~/R/win-library/4.5")
library(dplyr,    lib.loc = "C:/Program Files/RPackages")
library(tidyr,    lib.loc = "C:/Program Files/RPackages")
library(ggplot2,  lib.loc = "C:/Program Files/RPackages")
library(UpSetR)

STANDALONE <- FALSE

if (STANDALONE || !exists("shingles_ids") || !exists("disease_flags")) {
  message("Sourcing preliminary_tables_shingles_incident.R ...")
  source("preliminary_tables_shingles_incident.R")
}

# ============================================================================
# Config
# ============================================================================

DX_COLS <- c(
  "dx_sle", "dx_dm_myositis", "dx_ssc",
  "dx_gca", "dx_ra", "dx_spa", "dx_vasculitis"
)

DX_LABELS <- c(
  dx_sle         = "SLE",
  dx_dm_myositis = "DM / Myositis",
  dx_ssc         = "SSc",
  dx_gca         = "GCA",
  dx_ra          = "RA",
  dx_spa         = "SpA",
  dx_vasculitis  = "AAV"
)

# ============================================================================
# STEP 1  Filter disease flags to shingles patients and count diagnoses
# ============================================================================

shingles_dx <- disease_flags |>
  filter(person_id %in% shingles_ids) |>
  mutate(across(all_of(DX_COLS), as.integer)) |>
  mutate(n_dx = rowSums(across(all_of(DX_COLS))))

n_total  <- nrow(shingles_dx)
n_multi  <- sum(shingles_dx$n_dx > 1)

message(sprintf(
  "\n%d / %d (%.1f%%) incident shingles patients have MORE than 1 RD diagnosis.\n",
  n_multi, n_total, 100 * n_multi / n_total
))

# ============================================================================
# STEP 2  Distribution table: n patients by number of diagnoses
# ============================================================================

dist_tbl <- shingles_dx |>
  count(n_dx, name = "n_patients") |>
  arrange(n_dx) |>
  mutate(
    pct        = sprintf("%.1f%%", 100 * n_patients / n_total),
    n_dx_label = paste0(n_dx, " diagnosis", if_else(n_dx == 1, "", "es"))
  ) |>
  select(n_dx_label, n_patients, pct)

message("── Distribution of RD diagnosis count among incident shingles patients ──")
print(dist_tbl, n = Inf)

# ============================================================================
# STEP 3  UpSet plot — intersection sizes across all 7 diseases
# ============================================================================

upset_data <- shingles_dx |>
  select(all_of(DX_COLS)) |>
  rename(!!!setNames(DX_COLS, DX_LABELS[DX_COLS])) |>
  as.data.frame()

upset(
  upset_data,
  sets              = rev(DX_LABELS),       # display order (top = largest set)
  order.by          = "freq",
  decreasing        = TRUE,
  mb.ratio          = c(0.6, 0.4),
  number.angles     = 0,
  point.size        = 3,
  line.size         = 1,
  mainbar.y.label   = "Intersection size (patients)",
  sets.x.label      = "Patients per disease",
  text.scale        = c(1.4, 1.2, 1.2, 1, 1.4, 1.1),
  main.bar.color    = "#2C6FAC",
  sets.bar.color    = "#6BAED6",
  matrix.color      = "#2C6FAC"
)

# Save
if (!dir.exists("output")) dir.create("output")
png("output/upset_dx_overlap_shingles_incident.png",
    width = 1800, height = 1000, res = 150)
upset(
  upset_data,
  sets              = rev(DX_LABELS),
  order.by          = "freq",
  decreasing        = TRUE,
  mb.ratio          = c(0.6, 0.4),
  number.angles     = 0,
  point.size        = 3,
  line.size         = 1,
  mainbar.y.label   = "Intersection size (patients)",
  sets.x.label      = "Patients per disease",
  text.scale        = c(1.4, 1.2, 1.2, 1, 1.4, 1.1),
  main.bar.color    = "#2C6FAC",
  sets.bar.color    = "#6BAED6",
  matrix.color      = "#2C6FAC"
)
dev.off()
message("Saved: output/upset_dx_overlap_shingles_incident.png")

# ============================================================================
# STEP 4  Pairwise co-occurrence heatmap
# ============================================================================

# Build symmetric n×n co-occurrence matrix (off-diagonal = both dx TRUE)
dx_mat <- shingles_dx |>
  select(all_of(DX_COLS)) |>
  mutate(across(everything(), as.logical)) |>
  as.matrix()

co_mat <- matrix(0L, nrow = 7, ncol = 7,
                 dimnames = list(DX_LABELS, DX_LABELS))

for (i in seq_len(7)) {
  for (j in seq_len(7)) {
    co_mat[i, j] <- sum(dx_mat[, i] & dx_mat[, j], na.rm = TRUE)
  }
}

# Convert to tidy for ggplot
co_long <- as.data.frame(co_mat) |>
  tibble::rownames_to_column("disease_x") |>
  pivot_longer(-disease_x, names_to = "disease_y", values_to = "n") |>
  mutate(
    disease_x = factor(disease_x, levels = DX_LABELS),
    disease_y = factor(disease_y, levels = rev(DX_LABELS)),
    label     = if_else(n > 0, as.character(n), "")
  )

heatmap_plot <- ggplot(co_long, aes(x = disease_x, y = disease_y, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#EFF3FF", high = "#084594",
                      name = "Patients\nwith both") +
  labs(
    title    = "Pairwise RD diagnosis co-occurrence",
    subtitle = sprintf("Incident shingles patients (N = %d)", n_total),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x      = element_text(angle = 35, hjust = 1),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

print(heatmap_plot)

ggsave("output/heatmap_dx_overlap_shingles_incident.png",
       heatmap_plot, width = 7, height = 6, dpi = 150)
message("Saved: output/heatmap_dx_overlap_shingles_incident.png")
