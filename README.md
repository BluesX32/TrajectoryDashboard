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

### Restrict to specific patients

```r
launch_trajectory_dashboard(con, person_ids = c("10001", "10002", "10003"))
```

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
| Medications        |  | [Hospitalizations]                                       |
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
| Medication bars | One row per drug family; gap-bridged episodes with 30-day merge window |
| Hospitalizations | Inpatient/ER visits as grey bands |
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
| `admission` | Inpatient or ER visit |
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

### SQL Server / PostgreSQL / generic Databricks

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

# Data extraction
fetch_patient_data(connector, person_id = 12345L)

# Analytics
compute_trajectory_phases(labs_df, concept_id = 4013722L)
compute_treatment_phases(medications_df)
compute_data_density(patient_data)
detect_decision_points(patient_data, trajectory, treatment_phases)
detect_toxicity_flags(labs_df, medications_df)

# App
launch_trajectory_dashboard(connector = NULL, person_ids = NULL)
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
    extract_patient.R         fetch_patient_data() — master 6-domain orchestrator
    extract_labs.R            fetch_labs() S3
    extract_medications.R     fetch_medications() S3
    extract_conditions.R      fetch_conditions() S3
    extract_visits.R          fetch_visits() S3
    extract_notes.R           fetch_notes() S3
    extract_observations.R    fetch_observations() S3
    trajectory.R              compute_trajectory_phases(), compute_treatment_phases()
    data_density.R            compute_data_density()
    decision_points.R         detect_decision_points(), detect_toxicity_flags()
    report.R                  generate_patient_report() — HTML clinical summary
    app.R                     launch_trajectory_dashboard()
    app_ui.R                  trajectory_ui() — shinydashboard 3-layer layout
    app_server.R              trajectory_server() — plotly reactive graph

inst/sql/                     6 SqlRender-parameterized OMOP SQL templates
inst/extdata/                 synthetic_patient_data.rds (demo patients)
inst/app/www/                 trajectory_styles.css (responsive 4-breakpoint layout)
```

## Design principles

- **Never falsely imply continuity** — sparse windows render as hatched grey, not colored phases
- **Observed vs inferred** — confidence levels (high/medium/low/none) on every phase and decision point
- **Connector-first** — all functions work with both live OMOP databases and in-memory data frames
- **Cross-platform SQL** — SqlRender translates all queries to the target DBMS dialect
- **Single connection per patient load** — all 6 domain queries share one JDBC connection

## Related packages

- [SteroidDoseR](../DoseCalculation/SteroidDoseR/) — corticosteroid daily dose calculation from OMOP CDM, whose connector architecture this package extends

## Author

Minqi Xiong — Johns Hopkins University — mxiong5@jhu.edu
