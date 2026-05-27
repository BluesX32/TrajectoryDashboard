# TrajectoryDashboard

An R package for interactive, longitudinal patient trajectory visualization from **OMOP CDM** data.
Built for myositis and other rheumatologic diseases, but designed to work at any OMOP-compliant data site.

## Core idea

Turn messy longitudinal EHR data into a clinically interpretable patient-state view that supports better understanding, monitoring, and future decision support.

The dashboard is:
- **Trajectory-centered** — not just a timeline, but phase-abstracted disease course
- **Sparse-aware** — hatches gaps where data are absent rather than implying continuity
- **Decision-oriented** — highlights escalation points, taper opportunities, and key clinical events
- **Clinically deep** — normal range bands, normalization milestones, cumulative steroid burden, toxicity flags, safety monitoring, and time-in-target metrics

## Installation

```r
# From source (development)
devtools::install("path/to/TrajectoryDashboard")

# Or load without installing
devtools::load_all("path/to/TrajectoryDashboard")
```

**Core dependencies:** `dplyr`, `lubridate`, `tibble`, `tidyr`, `stringr`, `rlang`, `purrr`

**Suggests (Shiny app):** `shiny`, `shinydashboard`, `plotly`, `DT`

**Suggests (live OMOP):** `DatabaseConnector`, `SqlRender`, `rJava`, `RJDBC`, `DBI`

## Quick start

### Demo mode (no database required)

```r
library(TrajectoryDashboard)
launch_trajectory_dashboard()
```

Opens the dashboard with bundled synthetic myositis patients covering full disease courses
(flare → response → stable → relapse → second response).

### Live OMOP database

Create a `.env` file in your working directory (copy `.env.example`):

```
SQL_SERVER=yourserver.institution.edu
SQL_DATABASE=OMOP_CDM
SQL_CDM_SCHEMA=dbo
USE_WINDOWS_AUTH=true
SQL_JDBC_PATH=jdbc_drivers/sql
```

Then launch:

```r
con <- create_connection_from_env(".env")
launch_trajectory_dashboard(con)
```

### Restrict to a cohort with pre-fetched data

```r
# Step 1: identify the cohort (VZV antivirals example)
person_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_VZV_antivirals.json",
                          package = "TrajectoryDashboard")
)

# Step 2: download ALL patient data in ONE connection before launching
#   — avoids repeated JDBC auth round-trips during the session
cache <- prefetch_cohort_data(con, person_ids)

# Optionally persist the cache between R sessions:
# saveRDS(cache, "cohort_cache.rds")
# cache <- readRDS("cohort_cache.rds")

# Step 3: launch — patient loads are served from memory, no DB queries
launch_trajectory_dashboard(con,
  person_ids     = as.character(person_ids),
  preloaded_data = cache)
```

### Ad-hoc patient list (no pre-fetch)

```r
launch_trajectory_dashboard(con, person_ids = c("10001", "10002", "10003"))
```

Each patient load issues live database queries in this mode.

## Dashboard layout

```
+--sidebar-----------+  +--main-content------------------------------------------+
| Patient ID         |  | [Patient summary bar: age, diagnosis, follow-up, CK,   |
| Load Patient       |  |  ferritin, cumulative steroid, last-visit date, TIT %]  |
| Date range         |  +--Layer 1: Macro Trajectory-----------------------------+
| Reset history      |  | [Data density strip — quarterly event counts]           |
|                    |  | [Primary lab + LOESS trend + normal range band + ULN]   |
| Primary Lab        |  | [Phase shading: flare/worsening/stable/response/relapse]|
| [focus lab picker] |  | [Normalization milestone stars after each flare]         |
| Enzyme panel mode  |  | [Prednisone pred-equiv step line (secondary y-axis)]     |
|                    |  +--Layer 2: Events & Treatments--------------------------+
| Medications        |  | [Shingles/VZV events — orange diamonds]                  |
| [checkboxes]       |  | [Medication bars by drug family]                         |
|                    |  | [Decision points: escalation/taper/workup/admission]    |
| Display Options    |  | [Drug toxicity warnings: hepatotox/lymphopenia/CrRise]  |
| Show sparse        |  | [DMARD gap bands >= 30 days (optional)]                  |
| Show decision pts  |  +--Layer 3: Detail Drawer (opens on click)---------------+
| Show admissions    |  | Selected Event | Lab Values | Medications | Notes |      |
| Show ILD panel     |  | Conditions | Antibodies | Safety (CBC + cardiac)         |
| Show DMARD gaps    |  +--[ILD panel: FVC/DLCO (optional)]---------------------+
| Phase window (d)   |  +--[Download Clinical Summary]---------------------------+
|                    |
| Download Data      |
+--------------------+
```

## Clinical features

### Layer 1 — Macro Trajectory

| Feature | Description |
|---|---|
| Focus lab | CK, Aldolase, AST, ALT, LDH, ESR, CRP, ferritin, troponin-I, BNP, antibodies, CBC, creatinine |
| Enzyme panel mode | Overlay all muscle enzymes as % ULN on one plot |
| Normal range band | Green shaded band from `range_low` to `range_high` (or default ULN); grey dashed ULN line |
| LOESS trend | Smoothed trend line (span = 0.4); requires >= 4 observations |
| Normalization milestones | Green star at first value <= ULN after each flare/worsening phase; hover shows days from flare start |
| Phase shading | Colored background bands per disease phase |
| Steroid overlay | Prednisone pred-equiv step function on secondary y-axis (right) |
| Time-in-target (TIT) | % of observations within normal range shown in box header |

### Layer 2 — Events & Treatments

| Feature | Description |
|---|---|
| Shingles row | Herpes zoster events from a dedicated VZV concept-set SQL query (orange diamonds); hover shows DMARDs active ±3 months |
| Medication bars | One row per drug family; gap-bridged episodes with 30-day merge window |
| Decision points | Automatically detected clinical decision moments (see table below) |
| Toxicity flags | Orange/red triangles for hepatotoxicity, lymphopenia, creatinine rise |
| DMARD gap bands | Orange bands highlighting >= 30-day periods without non-steroid DMARD (optional) |

### Layer 3 — Detail Drawer (7 tabs)

| Tab | Content |
|---|---|
| Selected Event | Structured detail for the clicked decision point |
| Lab Values | Full lab history table (DT, sortable/filterable) |
| Medications | Full medication history table |
| Clinical Notes | Note text viewer |
| Conditions | Condition occurrence table |
| Antibodies | Antibody timeline plot + table |
| Safety | CBC (WBC + lymphocytes) and cardiac biomarkers (Troponin-I + BNP) with danger threshold lines |

### Patient summary bar

Tiles shown after patient load: age at first visit, sex, diagnosis, years of follow-up, peak CK (x ULN), ferritin (x ULN), cumulative corticosteroid exposure (grams pred-equiv), last visit date.

A last-values status row shows the most recent value for each lab with trend arrows (↗ ↘ →) and color-coded tiers (normal / elevated / high / critical).

## Trajectory phases

| Phase | Color | Definition |
|---|---|---|
| `flare` | Red | Lab > 3× ULN and rising |
| `worsening` | Amber | Lab > ULN and rising |
| `stable` | Green | Lab ≤ ULN or flat slope |
| `response` | Teal | Falling ≥ 30% after flare/worsening |
| `relapse` | Purple | Worsening after a response period |
| `sparse` | Grey hatched | < 2 observations in window — no phase inferred |

## Decision point types

| Type | Trigger |
|---|---|
| `escalation_point` | Flare/worsening with no new medication in prior 30 days |
| `taper_point` | Response phase while corticosteroid is active |
| `medication_change` | New immunosuppressant or biologic starts |
| `admission` | Shingles / VZV event (sourced from dedicated SQL query) |
| `workup_point` | Myositis antibody result in labs |
| `referral_point` | Referral keywords in clinical notes (low confidence) |

## Drug toxicity detection

Automatically flagged when:

| Toxicity | Criterion | Drugs |
|---|---|---|
| Hepatotoxicity | ALT or AST > 3× ULN within ±30 days of exposure | MTX, AZA |
| Lymphopenia | Lymphocytes < 0.5 K/µL within ±14 days of exposure | Any IST |
| Creatinine rise | Creatinine > 25% above 90-day baseline | CNIs |

Severity: **warning** (borderline) or **alert** (action-required).

## Supported OMOP CDM domains

| Domain | OMOP table | Dashboard use |
|---|---|---|
| Labs | `measurement` | Trajectory computation, focus lab plot, safety monitoring |
| Medications | `drug_exposure` | Treatment interval bars, taper/escalation detection, toxicity |
| Diagnoses | `condition_occurrence` | Event markers in Layer 2, detail table |
| Visits | `visit_occurrence` | Hospitalization bands, admission events |
| Notes | `note` | Notes viewer, referral keyword detection |
| Observations | `observation` | ECOG / functional status |

## OMOP CDM compatibility

All SQL queries use **SqlRender** for cross-DBMS translation. Tested connection targets:

| Platform | Auth | Function |
|---|---|---|
| SQL Server | Windows AD (NTLM) or username/password | `create_omop_connection()` |
| PostgreSQL | Username/password | `create_omop_connection()` |
| Databricks (generic) | PAT token | `create_omop_connection()` |
| Amazon Redshift | Username/password | `create_omop_connection()` |
| JHU SAFER Desktop | Databricks PAT + JHU proxy | `create_safer_connection()` |
| JHU Discovery HPC | Databricks PAT (direct) | `create_hpc_connection()` |

## Connection setup

### OHDSI-standard approach (recommended) — `launch_dashboard.R`

`launch_dashboard.R` follows the OHDSI / HADES convention used by
CohortDiagnostics, PatientLevelPrediction, and other network-study packages.
Credentials are stored in `~/.Renviron`, which R loads automatically at startup
— no project-level env file or custom file-reader is needed.

**One-time setup (run interactively once ever):**

```r
usethis::edit_r_environ()   # opens ~/.Renviron in your editor
```

Add the block for your platform, then restart R (`.rs.restartR()` in RStudio).

**SQL Server — Windows AD / Kerberos (e.g. JHU ESM server):**

```
DB_DBMS=sql server
DB_SERVER=server.esm.johnshopkins.edu
DB_DATABASE=Myositis_OMOP
DB_CDM_SCHEMA=dbo
DB_WINDOWS_AUTH=true
DB_JDBC_PATH=C:/jdbc/sql
```

**Databricks — SAFER Desktop (proxy on):**

```
DB_DBMS=spark
DB_SERVER=adb-1234567890123456.7.azuredatabricks.net
DB_HTTP_PATH=/sql/1.0/warehouses/abcdef1234567890
DB_TOKEN=dapi_xxxxxxxxxxxxxxxxxxxx
DB_CDM_SCHEMA=deid.omop
DB_RESULTS_SCHEMA=reach_users.mxiong5
DB_JDBC_PATH=C:/jdbc/databricks-jdbc-2.6.36.jar
DB_USE_PROXY=true
```

**Databricks — Discovery HPC (no proxy):**

```
DB_DBMS=spark
DB_SERVER=adb-1234567890123456.7.azuredatabricks.net
DB_HTTP_PATH=/sql/1.0/warehouses/abcdef1234567890
DB_TOKEN=dapi_xxxxxxxxxxxxxxxxxxxx
DB_CDM_SCHEMA=deid.omop
DB_RESULTS_SCHEMA=reach_users.mxiong5
DB_JDBC_PATH=~/jdbc/databricks-jdbc-2.6.36.jar
```

**Launch:**

```r
source("launch_dashboard.R")
```

Internally this calls `DatabaseConnector::createConnectionDetails()` directly —
the single OHDSI-blessed function — then wraps it in `create_omop_connector()`.

---

### Legacy env-file approach — `test_dashboard.R`

The original connection helpers (`create_connection_from_env()`,
`create_safer_connection()`, `create_hpc_connection()`) are still fully
supported. They read a project-level `.env` or `R.env` file.

Copy `.env.example` to `.env` and fill in your site's values:

```
# SQL Server + Windows AD (typical at US academic medical centres)
SQL_SERVER=yourserver.institution.edu
SQL_DATABASE=OMOP_CDM
SQL_CDM_SCHEMA=dbo
USE_WINDOWS_AUTH=true
SQL_JDBC_PATH=jdbc_drivers/sql
```

```r
con <- create_connection_from_env(".env")
```

JDBC drivers are not bundled. Download with:

```r
DatabaseConnector::downloadJdbcDrivers("sql server", pathToDriver = "jdbc_drivers/sql")
```

### JHU SAFER Desktop (Databricks via proxy)

Uses RJDBC directly with the JHU proxy (`proxy.jh.edu:3129`), which is required on SAFER Desktop. Copy `.env.example` to `R.env` and fill in the `DATABRICKS_*` section:

```
DATABRICKS_SERVER_HOSTNAME=adb-xxxx.azuredatabricks.net
DATABRICKS_HTTP_PATH=/sql/1.0/warehouses/xxxx
DATABRICKS_TOKEN=dapi...
DATABRICKS_DATA_CATALOG=deid
DATABRICKS_USER_CATALOG=reach_users
DATABRICKS_USERNAME=mxiong5
DATABRICKS_JDBC_JAR=C:/jdbc/databricks-jdbc-2.6.36.jar
```

**Prerequisites:**
- Java OpenJDK 17 (64-bit) installed on SAFER Desktop
- `databricks-jdbc-2.6.36.jar` at `C:/jdbc/` (download from [Maven Central](https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/))
- R packages: `rJava`, `RJDBC`, `DBI`

```r
con <- create_safer_connection("R.env")
launch_trajectory_dashboard(con)
```

### JHU Discovery HPC (Databricks direct)

Same `R.env` format, but no proxy. Default jar location is `~/jdbc/`:

```
DATABRICKS_JDBC_JAR=~/jdbc/databricks-jdbc-2.6.36.jar
```

**Prerequisites:**
- Java configured via `R CMD javareconf` (contact rithhpc-help@jh.edu if needed)
- `databricks-jdbc-2.6.36.jar` in `~/jdbc/`:
  ```bash
  mkdir -p ~/jdbc
  wget -P ~/jdbc https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/databricks-jdbc-2.6.36.jar
  ```

```r
con <- create_hpc_connection("R.env")   # or ~/.env
launch_trajectory_dashboard(con)
```

### Connection lifecycle

The dashboard automatically disconnects when:
- The browser window/tab is closed (`session$onSessionEnded`)
- The R process / `shiny::runApp()` stops (`shiny::onStop`)

Stale JDBC connections are automatically detected and reconnected on the next patient load.

## Key functions

```r
# Connection
create_connection_from_env(".env")             # Generic: load from .env file
create_omop_connection(...)                    # Generic: explicit parameters
create_safer_connection("R.env")               # JHU SAFER Desktop (proxy + RJDBC)
create_hpc_connection("R.env")                 # JHU Discovery HPC (direct + RJDBC)
create_df_connector(patient_data_list)         # In-memory (tests/demos)

# Cohort selection
fetch_cohort_ids(connector, json_path)         # ATLAS JSON → integer vector of person_ids
test_cohort_connection(connector)              # Diagnose connection / SQL issues

# Data extraction
fetch_patient_data(connector, person_id = 12345L)
fetch_shingles_events(connector, person_id = 12345L)  # VZV/herpes zoster events
prefetch_cohort_data(connector, person_ids)    # Batch-fetch all patients (one connection)

# Analytics
compute_trajectory_phases(labs_df, concept_id = 4013722L)
compute_treatment_phases(medications_df)
compute_data_density(patient_data)
detect_decision_points(patient_data, trajectory, treatment_phases)
detect_toxicity_flags(labs_df, medications_df)

# App
launch_trajectory_dashboard(connector = NULL, person_ids = NULL, preloaded_data = NULL)
```

## Myositis-specific concept IDs

The package ships with pre-built OMOP concept ID lists for myositis-relevant labs and drugs:

```r
# Lab concept IDs
# Muscle enzymes: CK, Aldolase, AST, ALT, LDH
# Inflammatory: ESR, CRP
# Myositis antibodies: Anti-Jo-1, Anti-Mi-2, Anti-MDA5, Anti-TIF1-gamma, Anti-HMGCR
# Cardiac & safety: Ferritin, Troponin-I, BNP, WBC, Lymphocytes, Hemoglobin, Creatinine
MYOSITIS_LAB_CONCEPTS

# Drug concept IDs (prednisone, azathioprine, IVIG, rituximab, JAK inhibitors, etc.)
MYOSITIS_DRUG_CONCEPTS

# Fetch only myositis labs
data <- fetch_patient_data(
    con, person_id = 12345L,
    lab_concepts = unlist(MYOSITIS_LAB_CONCEPTS)
)
```

## Package structure

```
R/
    connector.R               S3 trajectory_connector (omop + df variants); stale-connection retry
    connection.R              create_omop_connection(), create_connection_from_env(),
                              create_safer_connection(), create_hpc_connection()
    sql_helpers.R             render_translate_sql(), query_omop() (internal)
    utils_validate.R          assert_required_cols(), safe_as_date(), %||%
    utils_concepts.R          MYOSITIS_LAB_CONCEPTS, MYOSITIS_DRUG_CONCEPTS
    extract_patient.R         fetch_patient_data(), prefetch_cohort_data()
    extract_labs.R            fetch_labs() S3
    extract_medications.R     fetch_medications() S3
    extract_conditions.R      fetch_conditions() S3
    extract_visits.R          fetch_visits() S3
    extract_notes.R           fetch_notes() S3
    extract_observations.R    fetch_observations() S3
    cohort.R                  fetch_cohort_ids(), fetch_shingles_events(),
                              build_cohort_sql(), test_cohort_connection()
    trajectory.R              compute_trajectory_phases(), compute_treatment_phases()
    data_density.R            compute_data_density()
    decision_points.R         detect_decision_points(), detect_toxicity_flags()
    report.R                  generate_patient_report() — HTML clinical summary
    app.R                     launch_trajectory_dashboard()
    app_ui.R                  trajectory_ui() — shinydashboard 3-layer layout
    app_server.R              trajectory_server() — plotly reactive graph

inst/sql/                     SqlRender-parameterized OMOP SQL templates:
                                cohort_VZV_antivirals.sql — full cohort query
                                fetch_shingles_events.sql — per-patient VZV events
                                fetch_phn_events.sql / fetch_vzv_organ_events.sql
                                extract_labs/medications/conditions/visits/notes.sql
inst/json/                    cohort_VZV_antivirals.json — ATLAS cohort definition
preliminary_tables.R          Shingles / VZV analysis (base cohort = rheum + DMARD, no IVIG).
                              SHINGLES_GAP_DAYS (default 90): consecutive VZV condition
                                occurrences within this window are collapsed into one episode.
                              Table 1 (base cohort characteristics by shingles status;
                                       3 columns: Total | No Shingles | Shingles;
                                       race: Asian / Black / White / Other)
                              Table 2 (shingles episode stats; pre/post vaccine columns with
                                       episode-proportion weighting for overlap patients)
                              Step 6 post-vaccine cohort summary — passed to dashboard launch
preliminary_tables_pjp.R      PJP (Pneumocystis jirovecii pneumonia) analysis.
                              Same base cohort as preliminary_tables.R.
                              Table 1 — full base cohort characteristics by PJP status;
                                         3 columns: Total | Without PJP | With PJP;
                                         demographics: age, sex, race (Asian / Black / White / Other);
                                         rheumatic Dx flags, any-ever prophylaxis exposure by regimen
                              Table 2 — PJP patients only; all immunosuppressants + prophylaxis
                                         drugs in 90d window before PJP index date; n (%) per drug
                              Table 3 — prophylaxis regimen outcomes: PJP incidence rate and
                                         ADE rate per 100 person-years (exact Poisson 95% CI)
                                         for TMP-SMX, Dapsone, Atovaquone, Pentamidine
inst/extdata/                 synthetic_patient_data.rds (demo patients)
inst/app/www/                 trajectory_styles.css (responsive 4-breakpoint layout)
```

## Design principles

- **Never falsely imply continuity** — sparse windows render as hatched grey, not colored phases
- **Observed vs inferred** — confidence levels (high/medium/low/none) on every phase and decision point
- **Connector-first** — all functions work with both live OMOP databases and in-memory data frames
- **Cross-platform SQL** — SqlRender translates all queries to the target DBMS dialect
- **Pre-fetch, then serve from memory** — `prefetch_cohort_data()` batches all patients in one JDBC connection before launch; the dashboard never re-queries the database during an interactive session
- **Concept-set consistency** — shingles events use the same VZV ancestor concept IDs as the cohort SQL, so eligibility and visualization are always aligned

## Related packages

- [SteroidDoseR](../DoseCalculation/SteroidDoseR/) — corticosteroid daily dose calculation from OMOP CDM, whose connector architecture this package extends

## Author

Minqi Xiong — Johns Hopkins University — mxiong5@jhu.edu
