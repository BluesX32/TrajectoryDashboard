# TrajectoryDashboard

An interactive Shiny dashboard for longitudinal patient trajectory visualization
from **OMOP CDM** data. Works with any OMOP-compliant database — built for
myositis, adaptable to any rheumatic or chronic disease cohort.

---

## Table of contents

1. [Installation](#installation)
2. [Getting started](#getting-started)
3. [Customizing for your disease](#customizing-for-your-disease)
4. [Dashboard overview](#dashboard-overview)
5. [Connection setup](#connection-setup)
6. [Clinical features reference](#clinical-features-reference)
7. [Key functions](#key-functions)
8. [Package structure](#package-structure)

---

## Installation

```r
# Install from source
devtools::install("path/to/TrajectoryDashboard")

# Or load without installing (development)
devtools::load_all("path/to/TrajectoryDashboard")
```

**Required:** `dplyr`, `lubridate`, `tibble`, `tidyr`, `stringr`, `rlang`, `purrr`  
**Shiny UI:** `shiny`, `shinydashboard`, `plotly`, `DT`  
**Live OMOP:** `DatabaseConnector`, `SqlRender`

---

## Getting started

### Option A — Demo mode (no database needed)

```r
library(TrajectoryDashboard)
launch_trajectory_dashboard()
```

Opens immediately with bundled synthetic patients showing full disease trajectories
(flare → response → stable → relapse → second response). No credentials required.

---

### Option B — Live OMOP database (recommended starting point)

Open **`launch_dashboard.R`**, fill in the four blanks, and run it.

```r
devtools::load_all(".")

# 1. Fill in your site's connection details
connection_details <- DatabaseConnector::createConnectionDetails(
  dbms         = "sql server",     # "postgresql", "spark", "redshift", ...
  server       = "yourserver.edu/OMOP",
  user         = "your_username",
  password     = "your_password",
  port         = 1433,
  pathToDriver = "C:/jdbc"         # folder containing the JDBC .jar
)
cdm_database_schema   <- "dbo"    # schema that holds OMOP CDM tables
vocab_database_schema <- "dbo"    # vocabulary schema (often same as CDM)

# 2. Connect
connection <- DatabaseConnector::connect(connection_details)

# 3. Select your cohort (swap in your own ATLAS JSON)
person_ids <- fetch_cohort_ids(
  connection,
  json_path    = system.file("json", "cohort_VZV_antivirals.json",
                             package = "TrajectoryDashboard"),
  cdm_schema   = cdm_database_schema,
  vocab_schema = vocab_database_schema
)
message(length(person_ids), " patients in cohort.")

# 4. Launch
launch_trajectory_dashboard(
  connector    = connection,
  cdm_schema   = cdm_database_schema,
  vocab_schema = vocab_database_schema,
  person_ids   = as.character(person_ids)
)

DatabaseConnector::disconnect(connection)
```

> **JDBC drivers** are not bundled. Download once with:
> ```r
> DatabaseConnector::downloadJdbcDrivers("sql server", pathToDriver = "C:/jdbc")
> # Other values: "postgresql" | "spark" | "redshift"
> ```

---

## Customizing for your disease

The dashboard engine is fully disease-agnostic. A `dashboard_config` object
controls which lab and drug concepts are queried, what the UI labels say, and
which optional panels appear. Pass it to `launch_trajectory_dashboard()` via
`config = ...`.

### Step 1 — Choose a starting point

| Preset | `disease_name` | Primary biomarker | Workup concepts | Phase `flare_multiplier` |
|---|---|---|---|---|
| `myositis_config()` *(default)* | `"Myositis"` | CK | 8 myositis antibodies | 3× ULN |
| `ra_config()` | `"Rheumatoid Arthritis"` | CRP | RF, Anti-CCP | 2× ULN |
| `sle_config()` | `"Systemic Lupus Erythematosus"` | CRP | Anti-dsDNA, C3, C4 | 2× ULN, 60-day window |

```r
# Use a preset directly — no other changes needed
config <- ra_config()

launch_trajectory_dashboard(
  connector  = connection,
  cdm_schema = "dbo",
  person_ids = as.character(person_ids),
  config     = config
)
```

---

### Step 2 — Customize what you need

Pick any combination of fields. You only need to specify what differs from
the default.

```r
config <- dashboard_config(

  # ── Identity ─────────────────────────────────────────────────────────────
  disease_name = "Rheumatoid Arthritis",   # shown in browser title + header

  # ── Trajectory ──────────────────────────────────────────────────────────
  primary_lab = "crp",

  # ── Lab concepts ─────────────────────────────────────────────────────────
  lab_concepts = list(
    crp         = c(3020460L),
    esr         = c(3009542L),
    rf          = c(3033408L),
    anti_ccp    = c(3010148L),
    wbc         = c(3010813L),
    lymphocytes = c(3004327L),
    creatinine  = c(3051825L)
  ),

  # ── Drug concepts ────────────────────────────────────────────────────────
  drug_concepts = MYOSITIS_DRUG_CONCEPTS,

  # ── Workup decision points ───────────────────────────────────────────────
  workup_concepts = list(
    "Rheumatoid Factor" = c(3033408L),
    "Anti-CCP"          = c(3010148L)
  ),

  # ── Events timeline row ──────────────────────────────────────────────────
  event_row_label = "RA Flares",

  # ── Phase detection ───────────────────────────────────────────────────────
  # CRP does not scale 3× ULN the same way CK does — lower flare threshold
  phase_rules = phase_rules(flare_multiplier = 2.0),

  # ── Decision point detection ─────────────────────────────────────────────
  decision_rules = decision_rules(
    taper_watch_families = c("Corticosteroids"),
    referral_to_specialties = c(
      "Rheumatology" = "rheumatolog",
      "Immunology"   = "immunolog",
      "Cardiology"   = "cardiol"
    )
  ),

  # ── Toxicity monitoring ───────────────────────────────────────────────────
  # Use defaults (hepatotoxicity, lymphopenia, nephrotoxicity)
  # or NULL to disable, or supply a custom list of rules
  toxicity_rules = default_toxicity_rules(),

  # ── Cohort research panel ─────────────────────────────────────────────────
  research_table       = my_cohort_summary_df,
  research_table_title = "RA Cohort Summary"

)

launch_trajectory_dashboard(
  connector  = connection,
  cdm_schema = "dbo",
  person_ids = as.character(person_ids),
  config     = config
)
```

---

### Config field reference

#### Top-level fields

| Field | Type | What it controls |
|---|---|---|
| `disease_name` | `character(1)` | Browser title and dashboard header |
| `primary_lab` | `character(1)` | Default lab driving the trajectory curve |
| `lab_concepts` | named list of `integer` vectors | Which measurements are fetched; each key becomes a picker option |
| `drug_concepts` | named list of `integer` vectors | Which drugs are fetched from `drug_exposure` |
| `drug_families` | named list of `character` vectors | How drug names are grouped into sidebar checkbox rows |
| `lab_uln` | named `numeric` vector | ULN override per lab key (fallback: OMOP `range_high`) |
| `event_row_label` | `character(1)` | Y-axis label for the disease event row |
| `event_json_path` | `character(1)` or `NULL` | Path to event definition JSON; SQL is generated automatically |
| `condition_categories` | named list or `NULL` | Condition badge categories in the patient summary bar |
| `workup_concepts` | named list or `NULL` | Measurements that trigger workup decision-point markers |
| `phase_rules` | `phase_rules()` object | Trajectory thresholds (see below) |
| `decision_rules` | `decision_rules()` object | Decision point detection (see below) |
| `toxicity_rules` | list or `NULL` | Safety monitoring rules (see below); `NULL` = disable |
| `display` | `display_config()` object | Phase colours and label overrides |
| `research_table` | `data.frame` or `NULL` | Cohort-level table in a collapsible panel |
| `research_table_title` | `character(1)` | Heading for the research panel |

> `lab_concepts` also controls the ILD panel: the **FVC/DLCO** checkbox appears automatically
> when `"fvc"` or `"dlco"` are keys in `lab_concepts`.

---

#### `phase_rules()` — trajectory thresholds

```r
phase_rules(
  window_days           = 90L,    # rolling window width
  min_observations      = 2L,     # fewer obs → "sparse"
  slope_threshold_pct   = 0.05,   # rising/falling sensitivity (× ULN/day)
  flare_multiplier      = 3.0,    # mean > N × ULN + rising → "flare"
  worsening_multiplier  = 1.0,    # mean > N × ULN + rising → "worsening"
  response_drop_pct     = 0.30,   # drop ≥ 30% from peak → "response"
  treatment_gap_days    = 30L     # max gap to bridge in treatment episodes
)
```

---

#### `decision_rules()` — decision point detection

```r
decision_rules(
  escalation_lookback_days        = 30L,
  taper_watch_families            = c("Corticosteroids"),  # any drug family
  medication_change_skip_families = NULL,   # NULL = same as taper_watch_families
  admission_visit_types = c("Inpatient Visit", "Emergency Room Visit", ...),
  referral_trigger          = "\\brefer|\\breferral|\\bconsult\\b",
  referral_to_specialties   = c("Pulmonology" = "pulmonolog", ...),  # named vector
  referral_from_specialties = c("Primary Care" = "primary care|pcp", ...),
  show_medication_changes = TRUE,
  show_admissions         = TRUE,
  show_referrals          = TRUE
)
```

Referral labels are direction-aware: `"Referral to Pulmonology"` or
`"Referral from Primary Care to Rheumatology"`.

---

#### `toxicity_rules` — safety monitoring

A list of rule objects, each with this structure:

```r
list(
  name          = "Hepatotoxicity",
  drug_selector = list(
    families     = c("Methotrexate", "Azathioprine"),  # OR logic
    name_pattern = NULL,    # regex on drug_name; NULL = skip
    concept_ids  = NULL     # OMOP drug_concept_id; NULL = skip
    # All NULL → watch ALL drugs
  ),
  lab_keys        = c("alt", "ast"),     # keys in lab_concepts
  threshold_type  = "x_uln",            # "x_uln" | "absolute_low" | "absolute_high"
                                         # | "pct_rise" | "pct_drop"
  threshold_value = 3.0,
  window_days     = 30L,
  severity_levels = list(warning = 3.0, alert = 5.0)
)
```

`default_toxicity_rules()` returns hepatotoxicity + lymphopenia + nephrotoxicity.
Set `toxicity_rules = NULL` to disable all monitoring.

---

#### `display_config()` — phase colours and labels

```r
display_config(
  phase_colors = list(flare="#D32F2F", worsening="#F57C00", stable="#388E3C",
                       response="#0288D1", relapse="#7B1FA2", sparse="#BDBDBD"),
  phase_labels = list(flare="Flare", worsening="Worsening", stable="Stable",
                       response="Response", relapse="Relapse", sparse="Sparse")
)
```

---

### Adding a custom disease-events row

Create a small JSON file that tells the dashboard which OMOP domain and concept
IDs define your events. No SQL required — the dashboard generates
platform-appropriate SQL from the JSON automatically.

**Supported `domain` values:** `condition_occurrence`, `drug_exposure`,
`measurement`, `observation`, `procedure_occurrence`

```json
{
  "name": "RA Flares",
  "domain": "condition_occurrence",
  "concept_ids": [80809, 4189790],
  "include_descendants": true
}
```

Pass the JSON path via `event_json_path`:

```r
config <- dashboard_config(
  primary_lab      = "crp",
  lab_concepts     = list(crp = c(3020460L)),
  event_json_path  = "path/to/my_events.json",
  event_row_label  = "Disease Flares"
)
```

### Bundled event definitions

The following ready-to-use event JSON files are included in `inst/json/`:

| File | Events |
|---|---|
| `event_example.json` | Generic RA flares template — copy and edit for your use case |
| `event_vzv_shingles.json` | VZV / Herpes Zoster (shingles) conditions |
| `event_vzv_organ.json` | VZV organ involvement (encephalitis, pneumonitis, hepatitis, …) |
| `event_phn.json` | Post-herpetic neuralgia |
| `event_pjp.json` | Pneumocystis jirovecii pneumonia (PJP / PCP) |

Reference them with `system.file()`:

```r
config <- dashboard_config(
  primary_lab      = "crp",
  lab_concepts     = list(crp = c(3020460L)),
  event_json_path  = system.file("json", "event_pjp.json",
                                  package = "TrajectoryDashboard"),
  event_row_label  = "PJP Events"
)
```

---

### Cohort definitions

Cohort selection uses ATLAS JSON format. The bundled `cohort_VZV_antivirals.json`
is a complete worked example. Create your own by exporting from the
[OHDSI ATLAS](https://atlas-demo.ohdsi.org/) tool and passing the path to
`fetch_cohort_ids()`.

---

### Backward compatibility

If you omit the `config` argument, the dashboard behaves exactly as before —
myositis defaults with no changes required:

```r
# Still works without any config argument
launch_trajectory_dashboard(connector = con, person_ids = as.character(ids))
```

---

## Dashboard overview

```
┌─ Sidebar ──────────────┐  ┌─ Main content ─────────────────────────────────────┐
│                        │  │                                                      │
│  Patient               │  │  Patient summary bar                                 │
│  ├─ ID selector        │  │  Age · Sex · Dx · Follow-up · Peak CK · Steroids    │
│  ├─ Load Patient       │  │  Last-values row with trend arrows (↗ ↘ →)           │
│  └─ Date range         │  │                                                      │
│                        │  ├─ Layer 1: Macro Trajectory ──────────────────────── │
│  Primary Lab           │  │  Data density strip (quarterly event counts)         │
│  ├─ Lab picker         │  │  Primary lab + LOESS trend + normal range band       │
│  └─ Enzyme panel mode  │  │  Phase shading (flare/worsening/stable/response)     │
│                        │  │  Prednisone overlay on secondary axis                │
│  Medications           │  │                                                      │
│  └─ [checkboxes]       │  ├─ Layer 2: Events & Treatments ──────────────────── │
│                        │  │  Disease event row (config-driven)                   │
│  Display Options       │  │  Medication bars by drug family                      │
│  ├─ Show sparse        │  │  Decision-point markers (▲ escalation, ▽ taper …)   │
│  ├─ Show decision pts  │  │  Drug toxicity flags (orange/red triangles)          │
│  ├─ Show ILD panel*    │  │                                                      │
│  ├─ Episode gap slider │  ├─ Layer 3: Detail Drawer (click any event) ───────── │
│  └─ Phase window       │  │  Selected Event · Labs · Meds · Notes               │
│                        │  │  Conditions · Antibodies · Safety                   │
│  Research Intelligence │  │                                                      │
│  Download Data         │  ├─ ILD Panel* (FVC / DLCO)                            │
└────────────────────────┘  └──────────────────────────────────────────────────────┘
                             * shown only when fvc/dlco are in config lab_concepts
```

---

## Connection setup

### OHDSI-standard (recommended)

The pattern in `launch_dashboard.R` follows the same fill-in-the-blanks
convention as CohortDiagnostics, PatientLevelPrediction, and other HADES studies.

```r
DatabaseConnector::createConnectionDetails(
  dbms         = "sql server",
  server       = "yourserver.edu/OMOP",
  user         = "your_username",
  password     = "your_password",
  port         = 1433,
  pathToDriver = "C:/jdbc"
)
```

Supported `dbms` values: `"sql server"`, `"postgresql"`, `"spark"`,
`"redshift"`, `"oracle"`, `"bigquery"`.

---

### Env-file approach (legacy) — `test_dashboard.R`

For sites that already have a `.env` or `R.env` file, the original helpers
are still fully supported.

**.env** (SQL Server / generic OMOP):
```
SQL_SERVER=yourserver.institution.edu
SQL_DATABASE=OMOP_CDM
SQL_CDM_SCHEMA=dbo
USE_WINDOWS_AUTH=true
SQL_JDBC_PATH=jdbc_drivers/sql
```

```r
con <- create_connection_from_env(".env")
launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
```

---

### JHU SAFER Desktop (Databricks via proxy)

Copy `.env.example` to `R.env` and fill in the `DATABRICKS_*` fields:

```
DATABRICKS_SERVER_HOSTNAME=adb-xxxx.azuredatabricks.net
DATABRICKS_HTTP_PATH=/sql/1.0/warehouses/xxxx
DATABRICKS_TOKEN=dapi...
DATABRICKS_DATA_CATALOG=deid
DATABRICKS_JDBC_JAR=C:/jdbc/databricks-jdbc-2.6.36.jar
```

**Prerequisites:** Java OpenJDK 17 (64-bit), `databricks-jdbc-2.6.36.jar`
([download](https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/)),
R packages `rJava`, `RJDBC`, `DBI`.

```r
con <- create_safer_connection("R.env")
launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
```

---

### JHU Discovery HPC (Databricks direct)

Same `R.env` format, no proxy. Download the jar once:

```bash
mkdir -p ~/jdbc
wget -P ~/jdbc https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/databricks-jdbc-2.6.36.jar
```

```r
con <- create_hpc_connection("R.env")
launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
```

---

### Automatic disconnect

The dashboard disconnects from the database and stops the R process
automatically when the browser window closes (`session$onSessionEnded`).
No manual cleanup is required.

---

## Clinical features reference

### Trajectory phases

| Phase | Color | Definition |
|---|---|---|
| `flare` | Red | Lab > 3× ULN and rising |
| `worsening` | Amber | Lab > ULN and rising |
| `stable` | Green | Lab ≤ ULN or flat slope |
| `response` | Teal | Falling ≥ 30% after flare/worsening |
| `relapse` | Purple | Worsening after a response period |
| `sparse` | Grey hatched | < 2 observations in window — no phase inferred |

### Decision point types

| Type | Trigger |
|---|---|
| `escalation_point` | Flare/worsening with no new medication in prior 30 days |
| `taper_point` | Response phase while a corticosteroid is active |
| `medication_change` | New immunosuppressant or biologic starts |
| `admission` | Inpatient or ER visit |
| `workup_point` | A `workup_concepts` measurement result appears in labs |
| `referral_point` | Referral keywords in clinical notes (low confidence) |

### Drug toxicity flags

Automatically shown as triangles overlaid on medication bars:

| Toxicity | Criterion | Drugs checked |
|---|---|---|
| Hepatotoxicity | ALT or AST > 3× ULN within ±30 days of exposure | MTX, AZA |
| Lymphopenia | Lymphocytes < 0.5 K/µL within ±14 days of exposure | Any IST |
| Creatinine rise | Creatinine > 25% above 90-day baseline | CNIs |

Severity: **warning** (borderline) or **alert** (action required).

### Detail drawer tabs

| Tab | Content |
|---|---|
| Selected Event | Full evidence summary for the clicked decision point |
| Lab Values | Full lab history table (sortable, filterable, downloadable) |
| Medications | Full medication history table |
| Clinical Notes | Note text viewer |
| Conditions | Condition occurrence table |
| Antibodies | Antibody timeline plot + values table |
| Safety | CBC (WBC + lymphocytes) and cardiac biomarkers (Troponin-I, BNP) with danger threshold lines |

### Supported OMOP CDM domains

| Domain | OMOP table | Dashboard use |
|---|---|---|
| Labs | `measurement` | Trajectory computation, lab picker, safety monitoring |
| Medications | `drug_exposure` | Treatment bars, escalation/taper detection, toxicity |
| Diagnoses | `condition_occurrence` | Event markers, detail table, condition categories |
| Visits | `visit_occurrence` | Hospitalization bands, admission events |
| Notes | `note` | Notes viewer, referral keyword detection |
| Observations | `observation` | ECOG / functional status |

---

## Key functions

```r
# ── Connection ──────────────────────────────────────────────────────────────────
DatabaseConnector::createConnectionDetails(...)    # OHDSI-standard (recommended)
create_connection_from_env(".env")                 # Env-file (SQL Server)
create_safer_connection("R.env")                   # JHU SAFER Desktop (Databricks)
create_hpc_connection("R.env")                     # JHU Discovery HPC (Databricks)

# ── Disease config ──────────────────────────────────────────────────────────────
myositis_config()                                  # Myositis preset (default)
ra_config()                                        # RA preset
sle_config()                                       # SLE preset
dashboard_config(primary_lab, lab_concepts, ...)   # Fully custom config

# ── Cohort selection ────────────────────────────────────────────────────────────
fetch_cohort_ids(connector, json_path, ...)        # ATLAS JSON → person_id vector
test_cohort_connection(connector)                  # Diagnose SQL / connection issues

# ── Data extraction ─────────────────────────────────────────────────────────────
fetch_patient_data(connector, person_id, ...)      # All domains for one patient
fetch_disease_events(connector, person_id,         # Generic events row from JSON definition
                     event_json_path)
build_event_sql(json_path, cdm_schema, ...)        # Compile event JSON → dialect SQL
fetch_shingles_events(connector, person_id)        # VZV / herpes zoster events
fetch_shingrix_patients(connector, person_ids)     # Cohort-level Shingrix vaccine flag

# ── Analytics ───────────────────────────────────────────────────────────────────
compute_trajectory_phases(labs_df, ...)            # Phase segmentation
compute_treatment_phases(medications_df)            # Drug interval consolidation
detect_decision_points(patient_data, trajectory,   # Clinical decision markers
                       treatment_phases, config)
detect_toxicity_flags(labs_df, medications_df)     # Safety flags

# ── Launch ──────────────────────────────────────────────────────────────────────
launch_trajectory_dashboard(
  connector  = connection,
  cdm_schema = "dbo",
  person_ids = as.character(person_ids),
  config     = myositis_config()          # omit for myositis default
)
```

---

## Package structure

```
R/
  dashboard_config.R    dashboard_config(), myositis_config(), ra_config(),
                        sle_config(), fetch_disease_events()
  app.R                 launch_trajectory_dashboard()
  app_ui.R              trajectory_ui() — config-driven 3-layer layout
  app_server.R          trajectory_server() — plotly reactive graph
  connector.R           trajectory_connector S3 class; stale-connection retry
  connection.R          create_omop_connection(), env-file helpers
  utils_concepts.R      MYOSITIS_LAB_CONCEPTS, MYOSITIS_DRUG_CONCEPTS,
                        .build_lab_picker_choices()
  cohort.R              fetch_cohort_ids(), fetch_shingles_events(), …
  extract_patient.R     fetch_patient_data()
  extract_labs/meds/    Domain-specific S3 extractors
    conditions/visits/
    notes/observations.R
  trajectory.R          compute_trajectory_phases(), compute_treatment_phases()
  decision_points.R     detect_decision_points(), detect_toxicity_flags()
  report.R              generate_patient_report() — HTML clinical summary

inst/sql/               SqlRender-parameterized SQL (extract + cohort + fetch queries)
inst/json/              Cohort and event JSON definitions
                          cohort_VZV_antivirals.json  — ATLAS cohort (reference)
                          event_example.json          — generic event template
                          event_vzv_shingles.json     — VZV / shingles conditions
                          event_vzv_organ.json        — VZV organ involvement
                          event_phn.json              — post-herpetic neuralgia
                          event_pjp.json              — PJP / PCP
inst/extdata/           synthetic_patient_data.rds (demo)
inst/app/www/           trajectory_styles.css

launch_dashboard.R      ← Start here for live OMOP (OHDSI-standard)
test_dashboard.R        Env-file connection approach (existing deployments)
```

---

## Author

Minqi Xiong — Johns Hopkins University — mxiong5@jhu.edu
