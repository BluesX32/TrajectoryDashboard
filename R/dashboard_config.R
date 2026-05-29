# dashboard_config.R
# Disease-agnostic configuration object for the TrajectoryDashboard.
# Encapsulates lab/drug concepts, UI labels, event SQL, and workup concepts so
# the same dashboard engine can be used for any OMOP CDM population.
#
# Quick-start examples:
#   config <- myositis_config()         # default myositis behaviour (unchanged)
#   config <- ra_config()               # RA-specific concepts
#   config <- dashboard_config(         # fully custom
#     primary_lab  = "crp",
#     lab_concepts = list(crp = c(3020460L), esr = c(3009542L)),
#     event_row_label = "Flares"
#   )

# ---------------------------------------------------------------------------
# Sub-config constructors
# ---------------------------------------------------------------------------

#' Phase detection rules
#'
#' Controls the rolling-window trajectory algorithm and treatment episode
#' gap-bridging. Pass to [dashboard_config()] via `phase_rules = phase_rules(...)`.
#'
#' @param window_days Integer. Rolling window width in days. Default `90L`.
#' @param min_observations Integer. Minimum lab points in a window to assign a
#'   phase. Fewer points → `"sparse"`. Default `2L`.
#' @param slope_threshold_pct Numeric. Slope is considered rising/falling only
#'   when `|slope| > slope_threshold_pct × ULN` per day. Default `0.05`.
#' @param flare_multiplier Numeric. Mean lab `> N × ULN` **and** rising → `"flare"`.
#'   Default `3.0`.
#' @param worsening_multiplier Numeric. Mean lab `> N × ULN` **and** rising →
#'   `"worsening"`. Default `1.0`.
#' @param response_drop_pct Numeric. Fractional drop from flare/worsening peak
#'   required to classify a window as `"response"`. Default `0.30`.
#' @param treatment_gap_days Integer. Maximum gap (days) between drug records
#'   to bridge into a single episode in [compute_treatment_phases()]. Default `30L`.
#' @return A named list of class `"phase_rules"`.
#' @export
phase_rules <- function(
    window_days          = 90L,
    min_observations     = 2L,
    slope_threshold_pct  = 0.05,
    flare_multiplier     = 3.0,
    worsening_multiplier = 1.0,
    response_drop_pct    = 0.30,
    treatment_gap_days   = 30L
) {
  structure(
    list(
      window_days          = as.integer(window_days),
      min_observations     = as.integer(min_observations),
      slope_threshold_pct  = as.numeric(slope_threshold_pct),
      flare_multiplier     = as.numeric(flare_multiplier),
      worsening_multiplier = as.numeric(worsening_multiplier),
      response_drop_pct    = as.numeric(response_drop_pct),
      treatment_gap_days   = as.integer(treatment_gap_days)
    ),
    class = "phase_rules"
  )
}

#' Clinical decision detection rules
#'
#' Controls which clinical decision points are flagged and how they are detected.
#' Pass to [dashboard_config()] via `decision_rules = decision_rules(...)`.
#'
#' @param escalation_lookback_days Integer. Look back this many days before a
#'   flare/worsening event for a recent medication start. If found, the event is
#'   not flagged as an escalation point. Default `30L`.
#' @param taper_watch_families Character vector. Drug families whose active exposure
#'   during a `"response"` phase triggers a taper-opportunity marker. The label
#'   uses the matched family name: `"Lab response — consider [family] taper"`.
#'   Default `c("Corticosteroids")`.
#' @param medication_change_skip_families Character vector or `NULL`. Drug families
#'   excluded from generic "Started: X" medication-change markers (they are already
#'   captured by taper detection). `NULL` inherits `taper_watch_families`. Default `NULL`.
#' @param admission_visit_types Character vector. OMOP visit concept labels that
#'   count as admissions.
#' @param referral_trigger Character(1). Regex that must match a note for it to be
#'   considered a referral event at all (prevents false positives from notes that
#'   merely mention a specialty). Default `"\\brefer|\\breferral|\\bconsult\\b"`.
#' @param referral_to_specialties Named character vector. Names are display labels;
#'   values are regex patterns matched against note text to identify the destination
#'   specialty. Multiple matches are collapsed: `"Referral to Pulmonology, Neurology"`.
#' @param referral_from_specialties Named character vector. Names are display labels;
#'   values are regex patterns for the referring provider/specialty. The first match
#'   is used. When found, label becomes `"Referral from [From] to [To]"`.
#' @param show_medication_changes Logical. Show generic medication-start markers.
#'   Default `TRUE`.
#' @param show_admissions Logical. Show admission markers. Default `TRUE`.
#' @param show_referrals Logical. Show referral markers. Default `TRUE`.
#' @return A named list of class `"decision_rules"`.
#' @export
decision_rules <- function(
    escalation_lookback_days        = 30L,
    taper_watch_families            = c("Corticosteroids"),
    medication_change_skip_families = NULL,
    admission_visit_types = c("Inpatient Visit",
                               "Emergency Room Visit",
                               "Emergency Room and Inpatient Visit"),
    referral_trigger = "\\brefer|\\breferral|\\bconsult\\b",
    referral_to_specialties = c(
      "Pulmonology"       = "pulmonolog",
      "Neurology"         = "\\bneurol",
      "Rheumatology"      = "rheumatolog",
      "Cardiology"        = "cardiol",
      "Gastroenterology"  = "gastroenterol",
      "Nephrology"        = "nephrol",
      "Hematology"        = "hematol",
      "Oncology"          = "oncol",
      "Dermatology"       = "dermatol"
    ),
    referral_from_specialties = c(
      "Primary Care"      = "primary care|\\bpcp\\b|family medicine|general practice",
      "Internal Medicine" = "internal medicine"
    ),
    show_medication_changes = TRUE,
    show_admissions         = TRUE,
    show_referrals          = TRUE
) {
  skip <- medication_change_skip_families %||% taper_watch_families
  structure(
    list(
      escalation_lookback_days        = as.integer(escalation_lookback_days),
      taper_watch_families            = taper_watch_families,
      medication_change_skip_families = skip,
      admission_visit_types           = admission_visit_types,
      referral_trigger                = referral_trigger,
      referral_to_specialties         = referral_to_specialties,
      referral_from_specialties       = referral_from_specialties,
      show_medication_changes         = isTRUE(show_medication_changes),
      show_admissions                 = isTRUE(show_admissions),
      show_referrals                  = isTRUE(show_referrals)
    ),
    class = "decision_rules"
  )
}

#' Default toxicity monitoring rules
#'
#' Returns the three built-in rules (hepatotoxicity, lymphopenia, nephrotoxicity)
#' that match the original hardcoded behaviour. Pass a modified or extended list to
#' [dashboard_config()] via `toxicity_rules = list(...)`.
#'
#' **Rule structure**: each element of the list is a named list with fields:
#' \describe{
#'   \item{`name`}{Character. Display label for this toxicity.}
#'   \item{`drug_selector`}{Named list with optional fields: `families` (character
#'     vector of `drug_family` values), `name_pattern` (regex on `drug_name`),
#'     `concept_ids` (integer vector of OMOP `drug_concept_id`). Fields are OR'd —
#'     a drug matches if it satisfies ANY non-`NULL` field. All `NULL` means every
#'     drug in the patient record is watched.}
#'   \item{`lab_keys`}{Character vector of keys into `config$lab_concepts`. Multiple
#'     keys are OR'd (any matching lab can trigger the rule).}
#'   \item{`threshold_type`}{One of `"x_uln"` (value > N × ULN), `"absolute_low"`
#'     (value < N), `"absolute_high"` (value > N), `"pct_rise"` (value >
#'     baseline × (1 + N)), `"pct_drop"` (value < baseline × (1 − N)).}
#'   \item{`threshold_value`}{Numeric. The threshold in units matching `threshold_type`.}
#'   \item{`window_days`}{Integer. Labs within this many days of drug exposure are checked.}
#'   \item{`severity_levels`}{Named list with `warning` and `alert` thresholds in
#'     the same units as `threshold_type`.}
#' }
#'
#' Set `toxicity_rules = NULL` in [dashboard_config()] to disable all monitoring.
#'
#' @return A list of three toxicity rule objects.
#' @export
default_toxicity_rules <- function() {
  list(
    list(
      name          = "Hepatotoxicity",
      drug_selector = list(
        families     = c("Methotrexate", "Azathioprine"),
        name_pattern = NULL,
        concept_ids  = NULL
      ),
      lab_keys        = c("alt", "ast"),
      threshold_type  = "x_uln",
      threshold_value = 3.0,
      window_days     = 30L,
      severity_levels = list(warning = 3.0, alert = 5.0)
    ),
    list(
      name          = "Lymphopenia",
      drug_selector = list(
        families     = c("Azathioprine", "Methotrexate", "Mycophenolate",
                          "Rituximab", "JAK inhibitors", "Corticosteroids",
                          "Other IST"),
        name_pattern = NULL,
        concept_ids  = NULL
      ),
      lab_keys        = "lymphocytes",
      threshold_type  = "absolute_low",
      threshold_value = 0.5,
      window_days     = 14L,
      severity_levels = list(warning = 0.5, alert = 0.2)
    ),
    list(
      name          = "Nephrotoxicity",
      drug_selector = list(
        families     = NULL,
        name_pattern = "cyclosporine|tacrolimus",
        concept_ids  = NULL
      ),
      lab_keys        = "creatinine",
      threshold_type  = "pct_rise",
      threshold_value = 0.25,
      window_days     = 30L,
      severity_levels = list(warning = 0.25, alert = 0.50)
    )
  )
}

#' Display configuration
#'
#' Controls phase colours and label overrides shown in the dashboard UI.
#' Pass to [dashboard_config()] via `display = display_config(...)`.
#'
#' @param phase_colors Named list. One hex colour string per phase: `flare`,
#'   `worsening`, `stable`, `response`, `relapse`, `sparse`.
#' @param phase_labels Named list. Display strings per phase. Override any subset
#'   to rename phases in the UI (e.g. `flare = "Active Disease"`).
#' @return A named list of class `"display_config"`.
#' @export
display_config <- function(
    phase_colors = list(
      flare     = "#D32F2F",
      worsening = "#F57C00",
      stable    = "#388E3C",
      response  = "#0288D1",
      relapse   = "#7B1FA2",
      sparse    = "#BDBDBD"
    ),
    phase_labels = list(
      flare     = "Flare",
      worsening = "Worsening",
      stable    = "Stable",
      response  = "Response",
      relapse   = "Relapse",
      sparse    = "Sparse"
    )
) {
  structure(
    list(phase_colors = phase_colors, phase_labels = phase_labels),
    class = "display_config"
  )
}

# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

#' Create a dashboard configuration object
#'
#' Returns an S3 object of class `dashboard_config` that controls which
#' concepts are queried, how the UI is labelled, and what optional panels are
#' shown. Pass it to [launch_trajectory_dashboard()] via the `config` argument.
#'
#' @param primary_lab Character(1). Key from `lab_concepts` used as the default
#'   lab driving the trajectory curve.
#' @param lab_concepts Named list of integer vectors. Keys are short lab names
#'   (e.g. `"crp"`, `"ck"`); values are OMOP `measurement_concept_id` vectors.
#'   Defaults to [MYOSITIS_LAB_CONCEPTS].
#' @param drug_concepts Named list of integer vectors. Keys are drug names;
#'   values are OMOP `drug_concept_id` vectors. Defaults to
#'   [MYOSITIS_DRUG_CONCEPTS].
#' @param drug_families Named list mapping display family names to character
#'   vectors of drug concept keys (entries in `drug_concepts`). Controls the
#'   medication checkbox panel. Defaults to the built-in myositis map.
#' @param lab_uln Named numeric vector. Override default upper limits of normal
#'   for specific labs. Names must match keys in `lab_concepts`. `NULL` uses
#'   built-in defaults.
#' @param disease_name Character(1). Display name used in the dashboard title and
#'   axis labels. Default `"Myositis"`.
#' @param event_json_path Character(1) or `NULL`. Path to a JSON file that
#'   defines disease-specific events for the timeline row. The JSON must contain
#'   `domain` (OMOP table name), `concept_ids` (integer vector), and optionally
#'   `include_descendants` (logical). SQL is generated automatically from the
#'   JSON so no hand-written SQL is required. `NULL` disables the generic events
#'   row (myositis configs use their own specialised event functions instead).
#' @param event_row_label Character(1). Label shown on the y-axis for the
#'   disease event row (e.g. `"Shingles"`, `"Flares"`, `"Infections"`).
#'   Only used when `event_json_path` is non-`NULL` or for myositis configs.
#' @param condition_categories Named list or `NULL`. Each element is an integer
#'   vector of `condition_concept_id` values; the name becomes the badge label
#'   in the patient summary bar. `NULL` hides the condition-category panel
#'   (myositis configs use `fetch_rheumatic_diagnoses()` automatically).
#' @param workup_concepts Named list or `NULL`. Each element is an integer
#'   vector of `measurement_concept_id` values for a workup test type; the name
#'   becomes the decision-point label (e.g. `"Anti-Jo-1"`). `NULL` disables
#'   workup detection (myositis configs use antibody concepts automatically).
#' @param phase_rules A [phase_rules()] object. Controls trajectory window size,
#'   flare/worsening thresholds, response sensitivity, and treatment gap bridging.
#'   Default: [phase_rules()] with all defaults.
#' @param decision_rules A [decision_rules()] object. Controls escalation lookback,
#'   taper-watch drug families, admission visit types, and referral detection patterns.
#'   Default: [decision_rules()] with all defaults.
#' @param toxicity_rules A list of toxicity rule objects (see [default_toxicity_rules()])
#'   or `NULL` to disable toxicity monitoring. Each rule specifies a drug selector,
#'   a lab key, a threshold type, and severity levels.
#'   Default: [default_toxicity_rules()].
#' @param display A [display_config()] object. Controls phase colours and label
#'   overrides. Default: [display_config()] with all defaults.
#' @param reviewer_name Character(1) or `NULL`. Name shown on exported review
#'   sheets. Defaults to `Sys.info()[["user"]]` at export time when `NULL`.
#' @param research_table `data.frame` or `NULL`. Displayed in a collapsible
#'   cohort-level research panel below the patient view. `NULL` hides the
#'   panel.
#' @param research_table_title Character(1). Heading for the research panel.
#'
#' @return An S3 object of class `dashboard_config`.
#' @export
#'
#' @examples
#' # Minimal RA-style config
#' cfg <- dashboard_config(
#'   primary_lab  = "crp",
#'   lab_concepts = list(crp = c(3020460L), esr = c(3009542L),
#'                        rf  = c(3033408L)),
#'   event_row_label = "RA Flares"
#' )
#'
#' # Use the myositis preset (identical to the pre-config default behaviour)
#' cfg <- myositis_config()
dashboard_config <- function(
    disease_name         = "Myositis",
    primary_lab          = "ck",
    lab_concepts         = MYOSITIS_LAB_CONCEPTS,
    drug_concepts        = MYOSITIS_DRUG_CONCEPTS,
    drug_families        = .DRUG_FAMILY_MAP,
    lab_uln              = NULL,
    event_json_path      = NULL,
    event_row_label      = "Events",
    condition_categories = NULL,
    workup_concepts      = NULL,
    phase_rules          = phase_rules(),
    decision_rules       = decision_rules(),
    toxicity_rules       = default_toxicity_rules(),
    display              = display_config(),
    reviewer_name        = NULL,
    research_table       = NULL,
    research_table_title = "Research Panel"
) {
  config <- structure(
    list(
      disease_name         = disease_name,
      primary_lab          = primary_lab,
      lab_concepts         = lab_concepts,
      drug_concepts        = drug_concepts,
      drug_families        = drug_families,
      lab_uln              = lab_uln,
      event_json_path      = event_json_path,
      event_row_label      = event_row_label,
      condition_categories = condition_categories,
      workup_concepts      = workup_concepts,
      phase_rules          = phase_rules,
      decision_rules       = decision_rules,
      toxicity_rules       = toxicity_rules,
      display              = display,
      reviewer_name        = reviewer_name,
      research_table       = research_table,
      research_table_title = research_table_title
    ),
    class = "dashboard_config"
  )
  validate_dashboard_config(config)
}

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

#' @noRd
validate_dashboard_config <- function(config) {
  stopifnot(inherits(config, "dashboard_config"))

  if (!is.character(config$disease_name) || length(config$disease_name) != 1L)
    stop("'disease_name' must be a single character string.", call. = FALSE)

  if (!is.character(config$primary_lab) || length(config$primary_lab) != 1L)
    stop("'primary_lab' must be a single character string.", call. = FALSE)

  if (!is.list(config$lab_concepts) || is.null(names(config$lab_concepts)))
    stop("'lab_concepts' must be a named list.", call. = FALSE)

  if (!config$primary_lab %in% names(config$lab_concepts))
    warning(sprintf(
      "primary_lab '%s' is not a key in lab_concepts — will fall back to first key.",
      config$primary_lab
    ), call. = FALSE)

  if (!is.list(config$drug_concepts) || is.null(names(config$drug_concepts)))
    stop("'drug_concepts' must be a named list.", call. = FALSE)

  if (!is.null(config$event_json_path) &&
      (!is.character(config$event_json_path) ||
       !file.exists(config$event_json_path)))
    stop(sprintf(
      "'event_json_path' does not exist: %s", config$event_json_path
    ), call. = FALSE)

  if (!is.null(config$condition_categories) &&
      (!is.list(config$condition_categories) ||
       is.null(names(config$condition_categories))))
    stop("'condition_categories' must be a named list or NULL.", call. = FALSE)

  if (!is.null(config$workup_concepts) &&
      (!is.list(config$workup_concepts) ||
       is.null(names(config$workup_concepts))))
    stop("'workup_concepts' must be a named list or NULL.", call. = FALSE)

  if (!inherits(config$phase_rules, "phase_rules"))
    stop("'phase_rules' must be a phase_rules() object.", call. = FALSE)

  if (!inherits(config$decision_rules, "decision_rules"))
    stop("'decision_rules' must be a decision_rules() object.", call. = FALSE)

  if (!is.null(config$toxicity_rules) && !is.list(config$toxicity_rules))
    stop("'toxicity_rules' must be a list of rule objects or NULL.", call. = FALSE)

  if (!inherits(config$display, "display_config"))
    stop("'display' must be a display_config() object.", call. = FALSE)

  if (!is.null(config$research_table) && !is.data.frame(config$research_table))
    stop("'research_table' must be a data.frame or NULL.", call. = FALSE)

  config
}

# ---------------------------------------------------------------------------
# S3 print method
# ---------------------------------------------------------------------------

#' @export
print.dashboard_config <- function(x, ...) {
  pr <- x$phase_rules
  dr <- x$decision_rules
  cat(sprintf(paste0(
    "<dashboard_config>\n",
    "  disease     : %s\n",
    "  primary_lab : %s\n",
    "  lab keys    : %s\n",
    "  drug keys   : %d\n",
    "  event row   : %s (%s)\n",
    "  workup      : %s\n",
    "  phase_rules : window=%dd  flare=%.1fx  worsening=%.1fx  response_drop=%.0f%%\n",
    "  dec_rules   : lookback=%dd  taper_watch=[%s]  referrals=%s\n",
    "  tox_rules   : %s\n",
    "  research tbl: %s\n"
  ),
    x$disease_name,
    x$primary_lab,
    paste(names(x$lab_concepts), collapse = ", "),
    length(x$drug_concepts),
    x$event_row_label,
    if (!is.null(x$event_json_path)) basename(x$event_json_path) else "none",
    if (!is.null(x$workup_concepts)) paste(names(x$workup_concepts), collapse = ", ") else "(none)",
    pr$window_days, pr$flare_multiplier, pr$worsening_multiplier,
    pr$response_drop_pct * 100,
    dr$escalation_lookback_days,
    paste(dr$taper_watch_families, collapse = ", "),
    if (dr$show_referrals) "on" else "off",
    if (is.null(x$toxicity_rules)) "disabled"
    else paste0(length(x$toxicity_rules), " rules: ",
                paste(vapply(x$toxicity_rules, `[[`, "", "name"), collapse = ", ")),
    if (!is.null(x$research_table))
      paste0(x$research_table_title, " (", nrow(x$research_table), " rows)")
    else "(none)"
  ))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Disease presets
# ---------------------------------------------------------------------------

#' Myositis dashboard configuration (default)
#'
#' Returns the full myositis-specific config that replicates the pre-config
#' behaviour of the dashboard: CK-driven trajectory, myositis antibody workup
#' points, Shingles/VZV event row, and rheumatic-disease condition categories.
#'
#' @return A `dashboard_config` object with class
#'   `c("myositis_dashboard_config", "dashboard_config")`.
#' @export
myositis_config <- function() {
  cfg <- dashboard_config(
    disease_name  = "Myositis",
    primary_lab   = "ck",
    lab_concepts  = MYOSITIS_LAB_CONCEPTS,
    drug_concepts = MYOSITIS_DRUG_CONCEPTS,
    drug_families = .DRUG_FAMILY_MAP,
    lab_uln       = NULL,
    event_json_path  = NULL,   # server detects myositis class → specialised fetchers
    event_row_label  = "Shingles",
    condition_categories = NULL,  # server detects myositis class → fetch_rheumatic_diagnoses()
    workup_concepts = list(
      "Anti-Jo-1"   = MYOSITIS_LAB_CONCEPTS$anti_jo1,
      "Anti-Mi-2"   = MYOSITIS_LAB_CONCEPTS$anti_mi2,
      "Anti-MDA5"   = MYOSITIS_LAB_CONCEPTS$anti_mda5,
      "Anti-TIF1-γ" = MYOSITIS_LAB_CONCEPTS$anti_tif1,
      "Anti-HMGCR"  = MYOSITIS_LAB_CONCEPTS$anti_hmgcr,
      "Anti-SRP"    = MYOSITIS_LAB_CONCEPTS$anti_srs,
      "Anti-NXP2"   = MYOSITIS_LAB_CONCEPTS$anti_nxp2,
      "Anti-PM/Scl" = MYOSITIS_LAB_CONCEPTS$anti_pm_scl
    ),
    phase_rules    = phase_rules(),
    decision_rules = decision_rules(),
    toxicity_rules = default_toxicity_rules(),
    display        = display_config(),
    research_table       = NULL,
    research_table_title = "Post-Vaccine Shingles Cohort"
  )
  class(cfg) <- c("myositis_dashboard_config", "dashboard_config")
  cfg
}

#' RA dashboard configuration preset
#'
#' A starting-point config for rheumatoid arthritis cohorts. Uses CRP as the
#' primary biomarker with RF and anti-CCP as workup concepts. Reuses the
#' standard myositis DMARD list since it covers all common RA medications.
#'
#' @return A `dashboard_config` object.
#' @export
ra_config <- function() {
  dashboard_config(
    disease_name = "Rheumatoid Arthritis",
    primary_lab  = "crp",
    lab_concepts = list(
      crp         = c(3020460L, 3034963L),
      esr         = c(3009542L),
      rf          = c(3033408L),
      anti_ccp    = c(3010148L),
      wbc         = c(3010813L),
      lymphocytes = c(3004327L),
      hemoglobin  = c(3000963L),
      creatinine  = c(3051825L)
    ),
    drug_concepts = MYOSITIS_DRUG_CONCEPTS,
    drug_families = .DRUG_FAMILY_MAP,
    event_row_label      = "RA Events",
    condition_categories = NULL,
    workup_concepts = list(
      "Rheumatoid Factor" = c(3033408L),
      "Anti-CCP"          = c(3010148L)
    ),
    # CRP does not scale 3× ULN for flare the same way CK does
    phase_rules = phase_rules(flare_multiplier = 2.0),
    decision_rules = decision_rules(
      referral_to_specialties = c(
        "Pulmonology"       = "pulmonolog",
        "Neurology"         = "\\bneurol",
        "Rheumatology"      = "rheumatolog",
        "Cardiology"        = "cardiol",
        "Gastroenterology"  = "gastroenterol",
        "Nephrology"        = "nephrol",
        "Hematology"        = "hematol",
        "Oncology"          = "oncol",
        "Dermatology"       = "dermatol",
        "Immunology"        = "immunolog"
      )
    ),
    toxicity_rules       = default_toxicity_rules(),
    display              = display_config(),
    research_table_title = "RA Cohort Panel"
  )
}

#' SLE dashboard configuration preset
#'
#' A starting-point config for systemic lupus erythematosus cohorts. Uses CRP
#' as the primary biomarker with anti-dsDNA and complement levels as workup
#' concepts.
#'
#' @return A `dashboard_config` object.
#' @export
sle_config <- function() {
  dashboard_config(
    disease_name = "Systemic Lupus Erythematosus",
    primary_lab  = "crp",
    lab_concepts = list(
      crp         = c(3020460L, 3034963L),
      esr         = c(3009542L),
      anti_dsdna  = c(3003999L),
      c3          = c(3013429L),
      c4          = c(3003714L),
      wbc         = c(3010813L),
      lymphocytes = c(3004327L),
      creatinine  = c(3051825L),
      hemoglobin  = c(3000963L)
    ),
    drug_concepts = MYOSITIS_DRUG_CONCEPTS,
    drug_families = .DRUG_FAMILY_MAP,
    event_row_label      = "Lupus Events",
    condition_categories = NULL,
    workup_concepts = list(
      "Anti-dsDNA" = c(3003999L),
      "C3"         = c(3013429L),
      "C4"         = c(3003714L)
    ),
    # CRP-based; lupus labs are typically ordered frequently → tighter window
    phase_rules = phase_rules(flare_multiplier = 2.0, window_days = 60L),
    decision_rules = decision_rules(
      referral_to_specialties = c(
        "Pulmonology"       = "pulmonolog",
        "Neurology"         = "\\bneurol",
        "Rheumatology"      = "rheumatolog",
        "Cardiology"        = "cardiol",
        "Nephrology"        = "nephrol",
        "Hematology"        = "hematol",
        "Gastroenterology"  = "gastroenterol",
        "Oncology"          = "oncol",
        "Dermatology"       = "dermatol"
      )
    ),
    toxicity_rules       = default_toxicity_rules(),
    display              = display_config(),
    research_table_title = "SLE Cohort Panel"
  )
}

# ---------------------------------------------------------------------------
# Generic disease-events fetcher
# ---------------------------------------------------------------------------

#' Build a per-patient event query from a JSON definition
#'
#' Reads a simple event definition JSON and returns a ready-to-execute SQL
#' string (already rendered and translated). The JSON must contain:
#' - `domain`: OMOP table name (e.g. `"condition_occurrence"`)
#' - `concept_ids`: integer array of standard concept IDs
#' - `include_descendants`: (optional, default `false`) expand via
#'   `concept_ancestor`
#'
#' @param json_path Path to the event definition JSON file.
#' @param cdm_schema OMOP CDM schema.
#' @param vocab_schema Vocabulary schema (defaults to `cdm_schema`).
#' @param person_id Integer patient ID.
#' @param dbms SqlRender target dialect (default `"sql server"`).
#' @return A translated SQL string returning `event_date`, `event_label`,
#'   `event_detail` for the given patient.
#' @export
build_event_sql <- function(json_path,
                             cdm_schema,
                             vocab_schema = cdm_schema,
                             person_id,
                             dbms         = "sql server") {
  if (!requireNamespace("jsonlite",   quietly = TRUE))
    stop("Package 'jsonlite' required.",   call. = FALSE)
  if (!requireNamespace("SqlRender",  quietly = TRUE))
    stop("Package 'SqlRender' required.",  call. = FALSE)

  if (!file.exists(json_path))
    stop(sprintf("Event JSON not found: %s", json_path), call. = FALSE)

  def          <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)
  domain       <- def$domain %||% "condition_occurrence"
  concept_ids  <- as.integer(def$concept_ids)
  include_desc <- isTRUE(def$include_descendants)

  supported <- c("condition_occurrence", "drug_exposure",
                  "measurement", "observation", "procedure_occurrence")
  if (!domain %in% supported)
    stop(sprintf(
      "Unsupported domain '%s'. Choose one of: %s",
      domain, paste(supported, collapse = ", ")
    ), call. = FALSE)

  ids_str     <- paste(unique(concept_ids), collapse = ", ")
  date_col    <- .domain_date_col(domain)
  concept_col <- .domain_concept_col(domain)
  src_col     <- .domain_source_col(domain)

  cte <- paste0(
    "event_concepts AS (\n",
    "  SELECT DISTINCT concept_id\n",
    "  FROM @vocab_schema.concept\n",
    sprintf("  WHERE concept_id IN (%s)", ids_str)
  )
  if (include_desc) {
    cte <- paste0(cte,
      "\n  UNION\n",
      "  SELECT DISTINCT ca.descendant_concept_id\n",
      "  FROM @vocab_schema.concept_ancestor ca\n",
      "  JOIN @vocab_schema.concept c\n",
      "    ON ca.descendant_concept_id = c.concept_id\n",
      sprintf("  WHERE ca.ancestor_concept_id IN (%s)\n", ids_str),
      "    AND c.invalid_reason IS NULL"
    )
  }
  cte <- paste0(cte, "\n)")

  sql <- paste0(
    "WITH\n", cte, "\n\n",
    "SELECT\n",
    sprintf("  t.%s AS event_date,\n",   date_col),
    "  c2.concept_name AS event_label,\n",
    sprintf("  t.%s AS event_detail\n",  src_col),
    sprintf("FROM @cdm_schema.%s t\n", domain),
    sprintf("JOIN event_concepts ec ON t.%s = ec.concept_id\n", concept_col),
    sprintf("JOIN @vocab_schema.concept c2 ON t.%s = c2.concept_id\n", concept_col),
    "WHERE t.person_id = @person_id\n",
    sprintf("ORDER BY t.%s", date_col)
  )

  sql <- SqlRender::render(sql,
                           cdm_schema   = cdm_schema,
                           vocab_schema = vocab_schema,
                           person_id    = as.integer(person_id))
  SqlRender::translate(sql, targetDialect = dbms)
}

#' @noRd
.domain_source_col <- function(domain) {
  switch(domain,
    condition_occurrence = "condition_source_value",
    drug_exposure        = "drug_source_value",
    measurement          = "value_source_value",
    observation          = "value_source_value",
    procedure_occurrence = "procedure_source_value",
    "source_value"
  )
}

#' Fetch disease events from a JSON definition
#'
#' Generic replacement for the myositis-specific `fetch_shingles_events()` /
#' `fetch_phn_events()` / `fetch_vzv_organ_events()`. Builds platform-agnostic
#' SQL from the event definition JSON at `event_json_path` (set in
#' [dashboard_config()]) and returns a standardised tibble. No hand-written SQL
#' is required — the JSON specifies domain and concept IDs only.
#'
#' @param connector A `trajectory_connector`.
#' @param person_id Integer patient identifier.
#' @param event_json_path Character(1). Path to the event definition JSON file.
#' @param cdm_schema CDM schema (inferred from connector when `NULL`).
#' @param vocab_schema Vocabulary schema (inferred from connector when `NULL`).
#' @param dbms Target dialect (default `"sql server"`).
#'
#' @return A tibble with columns: `event_date` (Date), `event_label`
#'   (character), `event_detail` (character). Zero rows if no events found.
#' @export
fetch_disease_events <- function(connector, person_id, event_json_path,
                                  cdm_schema   = NULL,
                                  vocab_schema = NULL,
                                  dbms         = "sql server") {
  if (!requireNamespace("DatabaseConnector", quietly = TRUE))
    stop("Package 'DatabaseConnector' required.", call. = FALSE)

  if (!is.character(event_json_path) || !file.exists(event_json_path))
    stop(sprintf("'event_json_path' does not exist: %s", event_json_path),
         call. = FALSE)

  if (inherits(connector, "trajectory_connector")) {
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
  } else {
    if (is.null(cdm_schema))
      stop("'cdm_schema' is required when 'connector' is a plain connection.",
           call. = FALSE)
    vocab_schema <- vocab_schema %||% cdm_schema
  }

  .run <- function(conn, actual_dbms) {
    sql <- build_event_sql(event_json_path,
                           cdm_schema   = cdm_schema,
                           vocab_schema = vocab_schema,
                           person_id    = person_id,
                           dbms         = actual_dbms)
    res <- tryCatch(
      .exec_sql(conn, sql),
      error = function(e) {
        message("[fetch_disease_events] SQL error: ", e$message)
        data.frame(event_date   = as.Date(character(0)),
                   event_label  = character(0),
                   event_detail = character(0),
                   stringsAsFactors = FALSE)
      }
    )
    names(res) <- tolower(gsub("([A-Z])", "_\\1", names(res)))
    names(res) <- sub("^_", "", names(res))
    res
  }

  result <- if (inherits(connector, "trajectory_connector")) {
    with_connector(connector, function(active) {
      actual_dbms <- active$dbms
      if (!length(actual_dbms) || !nzchar(actual_dbms)) actual_dbms <- dbms
      .run(active$conn, actual_dbms)
    })
  } else {
    .run(connector, dbms)
  }

  if (!"event_date" %in% names(result))
    result$event_date <- as.Date(character(nrow(result)))
  if (!"event_label" %in% names(result))
    result$event_label <- character(nrow(result))
  if (!"event_detail" %in% names(result))
    result$event_detail <- character(nrow(result))

  if (!inherits(result$event_date, "Date"))
    result$event_date <- as.Date(result$event_date)

  tibble::as_tibble(result)
}

# ---------------------------------------------------------------------------
# Config-aware resolution helpers (used by server closure)
# ---------------------------------------------------------------------------

#' Resolve a lab key to OMOP concept IDs using a config object
#'
#' @param key Character(1). Key into `config$lab_concepts`.
#' @param config A `dashboard_config` object.
#' @return Integer vector of concept IDs, or `NULL` if not found.
#' @noRd
.resolve_lab_from_config <- function(key, config) {
  k <- tolower(gsub("[- ]", "_", as.character(key)))
  config$lab_concepts[[k]]
}

#' Get upper limit of normal using config, falling back to built-in defaults
#'
#' @param key Character(1). Key into `config$lab_concepts`.
#' @param config A `dashboard_config` object.
#' @return Numeric(1) ULN value, or `NA_real_` if unknown.
#' @noRd
.get_uln_from_config <- function(key, config) {
  k <- tolower(gsub("[- ]", "_", as.character(key)))
  # 1. Config-level override
  v <- config$lab_uln[[k]]
  if (!is.null(v) && length(v) == 1L && !is.na(v)) return(as.numeric(v))
  # 2. Built-in default
  .get_default_uln(k)
}
