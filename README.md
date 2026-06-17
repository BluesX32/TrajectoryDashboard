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
8. [Preliminary analysis scripts](#preliminary-analysis-scripts)
9. [Package structure](#package-structure)

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

| Preset | Primary biomarker | Workup concepts | Event row |
|---|---|---|---|
| `myositis_config()` *(default)* | CK | Myositis antibodies (8 panels) | Shingles / VZV |
| `ra_config()` | CRP | RF, Anti-CCP | RA Events |
| `sle_config()` | CRP | Anti-dsDNA, C3, C4 | Lupus Events |

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

  # ── Trajectory ──────────────────────────────────────────────────────────
  # The "primary lab" drives the trajectory curve (the main plot).
  # It must be a key that exists in lab_concepts below.
  primary_lab = "crp",

  # ── Lab concepts ─────────────────────────────────────────────────────────
  # Named list: short key → integer vector of OMOP measurement_concept_ids.
  # These are the only labs that will be fetched and shown in the picker.
  # Keys become the lab picker choices; use meaningful short names.
  lab_concepts = list(
    crp         = c(3020460L),          # C-Reactive Protein
    esr         = c(3009542L),          # ESR
    rf          = c(3033408L),          # Rheumatoid Factor
    anti_ccp    = c(3010148L),          # Anti-CCP antibody
    wbc         = c(3010813L),          # WBC
    lymphocytes = c(3004327L),          # Lymphocyte count
    creatinine  = c(3051825L)           # Creatinine
  ),

  # ── Drug concepts ────────────────────────────────────────────────────────
  # Reuse the full myositis DMARD list (covers most rheumatic diseases),
  # or supply your own named list of drug_concept_ids.
  drug_concepts = MYOSITIS_DRUG_CONCEPTS,

  # ── Workup decision points ───────────────────────────────────────────────
  # Measurements in this list trigger a "workup point" marker on the timeline.
  # Label is the display name; value is a vector of measurement_concept_ids.
  # Set to NULL to disable workup markers entirely.
  workup_concepts = list(
    "Rheumatoid Factor" = c(3033408L),
    "Anti-CCP"          = c(3010148L)
  ),

  # ── Events timeline row ──────────────────────────────────────────────────
  # Label shown on the y-axis and in the "episode gap" slider.
  # To add a custom event row from your own SQL, also set event_sql_path.
  event_row_label = "RA Flares",

  # ── Cohort research panel ─────────────────────────────────────────────────
  # Optional: pass any data.frame to display a cohort-level summary table
  # below the per-patient view.  Set to NULL to hide the panel.
  research_table       = my_cohort_summary_df,   # data.frame or NULL
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

| Field | Type | What it changes in the UI |
|---|---|---|
| `primary_lab` | `character(1)` | Default lab selected in the picker; drives the trajectory curve |
| `lab_concepts` | named list of `integer` vectors | Which measurements are fetched; each key becomes a picker option |
| `drug_concepts` | named list of `integer` vectors | Which drugs are fetched from `drug_exposure` |
| `drug_families` | named list of `character` vectors | How individual drug names are grouped into sidebar checkbox rows |
| `lab_uln` | named `numeric` vector | Override upper limit of normal per lab key (fallback: OMOP `range_high`) |
| `event_row_label` | `character(1)` | Y-axis label and "episode gap" slider heading in the event layer |
| `event_sql_path` | `character(1)` or `NULL` | Path to a SqlRender SQL file returning custom disease events; `NULL` = no generic event row |
| `condition_categories` | named list of `integer` vectors or `NULL` | Condition badge categories in the patient summary bar; `NULL` = hide |
| `workup_concepts` | named list of `integer` vectors or `NULL` | Measurements that trigger workup decision-point markers; `NULL` = disable |
| `research_table` | `data.frame` or `NULL` | Cohort-level table shown in a collapsible panel below the patient view |
| `research_table_title` | `character(1)` | Heading for the research table panel |

> **Note:** `lab_concepts` also controls the ILD monitoring panel: the **FVC/DLCO** checkbox
> appears automatically when `"fvc"` or `"dlco"` are keys in `lab_concepts`, and is hidden otherwise.

---

### Adding a custom disease-events row

Any SQL file that returns `event_date`, `event_label`, `event_detail` columns and
accepts `@cdm_schema`, `@vocab_schema`, `@person_id` parameters can power the
events timeline row.

```sql
-- my_events.sql
SELECT
    co.person_id,
    co.condition_start_date  AS event_date,
    c.concept_name           AS event_label,
    co.condition_source_value AS event_detail
FROM @cdm_schema.condition_occurrence co
JOIN @cdm_schema.concept c ON c.concept_id = co.condition_concept_id
WHERE co.person_id        = @person_id
  AND co.condition_concept_id IN (123456, 234567)  -- your concept IDs
ORDER BY co.condition_start_date
;
```

```r
config <- dashboard_config(
  primary_lab     = "crp",
  lab_concepts    = list(crp = c(3020460L)),
  event_sql_path  = "path/to/my_events.sql",
  event_row_label = "Disease Flares"
)
```

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
fetch_disease_events(connector, person_id,         # Generic events row from SQL file
                     event_sql_path)
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

## Preliminary analysis scripts

Four stand-alone R scripts produce the descriptive tables for the shingles/VZV
and PJP infection studies.  Run them interactively in RStudio — each script is
self-contained and follows the same numbered STEP structure.

### Cohort design: prevalent vs. incident

| Term | Base cohort definition |
|---|---|
| **Prevalent** | One rheumatic disease (RD) diagnosis ever + any DMARD. Larger cohort, lower specificity. |
| **Incident** | Two RD diagnoses 30–365 days apart + continuous DMARD (ATLAS-validated). Smaller cohort, higher specificity. |

Prevalent scripts will always produce a larger base cohort than their incident counterparts. Use incident results when you need confirmed RD diagnosis; use prevalent when sample size matters.

---

### Shingles / VZV scripts

#### `preliminary_tables_shingles.R` — prevalent cohort

| STEP | What it does |
|---|---|
| STEP 1 | Builds the **base cohort** — adults with ≥1 RD diagnosis + any DMARD (inline SQL) |
| STEP 2 | Identifies **shingles patients** — base cohort ∩ ATLAS VZV cohort (`cohort_PrevalentRD_VZV_all.json`). No antiviral required — antiviral records are under-documented in most EHR systems |
| STEP 3 | Identifies **vaccinated patients** — shingles patients with a Shingrix/Zostavax record (SQL + ATLAS JSON cross-validation) |
| STEP 3b | Fetches race and vaccine dose count for the base cohort |
| STEP 4 | Fetches disease category flags (SLE / myositis / SSc / GCA / RA / SpA) |
| STEP 5 | Fetches drug exposure flags (any-ever, base cohort) |
| STEP 6 | Assembles the analysis dataset |
| Table 1 | Base cohort demographics: Total \| No Shingles \| Shingles |
| Table 2 | Shingles episode characteristics: Overall \| Pre-vaccine \| Post-vaccine |
| Table 3 | DMARDs used in the 90 days before each shingles episode |
| Table 4 | DMARDs used around the herpes zoster vaccine date |

#### `preliminary_tables_shingles_incident.R` — incident cohort

Same table structure as above, with these differences:

| STEP | Difference from prevalent script |
|---|---|
| STEP 1 | Base cohort requires **two RD diagnoses 30–365 days apart** + continuous DMARD (`json_base_ids` filter from ATLAS). The second diagnosis is the cohort INDEX DATE. |
| STEP 2 | Shingles events must occur **on or after index_date**. SQL retrieves all VZV dates (no antiviral); ATLAS `json_vzv_ids` validates the case set. |
| STEP 3 | Same vaccine logic; only post-index_date episodes are counted. |
| STEP 7 | Extra section: post-vaccine shingles summary (shingles occurring after vaccination). |

---

### PJP (Pneumocystis jirovecii pneumonia) scripts

#### `preliminary_tables_pjp.R` — prevalent cohort

| STEP | What it does |
|---|---|
| STEP 1 | Builds the **base cohort** — adults with ≥1 RD diagnosis + DMARD (no IVIG), inline SQL |
| STEP 1b | Fetches the **first RD diagnosis date** per patient (needed for prophylaxis timing rule) |
| STEP 2 | Identifies **PJP patients** — base cohort patients with SNOMED 438350 (PJP) + descendants. No ATLAS JSON filter: the ATLAS cohort's inclusion rules (IR=2) over-restrict the case count beyond the SQL concept-set match |
| STEP 2b | Fetches race for base cohort |
| STEP 3 | Fetches disease category flags |
| STEP 4 | Fetches **30-day in-hospital mortality** (death within 30 days of PJP index AND overlapping inpatient visit) |
| STEP 5 | Fetches DMARD exposures for PJP patients (90-day window before PJP index) |
| STEP 6 | Fetches **PJP prophylaxis** (TMP-SMX / Dapsone / Atovaquone / Pentamidine) for the full base cohort |
| STEP 7 | Assembles analysis datasets |
| Table 1 | Base cohort demographics: Total \| Without PJP \| With PJP |
| Table 2 | Medications 90 days before PJP (PJP cohort only) |
| Table 3 | Prophylaxis regimen outcomes — PJP incidence rate per 100 person-years (exact Poisson 95% CI) and ADE rate |

#### `preliminary_tables_pjp_incident.R` — incident cohort

Same table structure as above, with these differences:

| STEP | Difference from prevalent script |
|---|---|
| STEP 1 | Base cohort requires **two RD diagnoses 30–365 days apart** + continuous DMARD (`json_base_ids` filter). The second RD date is `rd_index_date`. `first_rd_date` is returned by the same SQL — no separate Step 1b needed. |
| STEP 2 | PJP events must occur **on or after `rd_index_date`**. Same concept set (SNOMED 438350); same no-JSON-filter rationale. |

---

### ATLAS JSON cohort definitions

All JSON files live in `inst/json/` and are parsed at run time by `fetch_cohort_ids()`.

| File | Used in | Purpose |
|---|---|---|
| `cohort_PrevalentRD_VZV_all.json` | Shingles prevalent STEP 2, incident STEP 2 | All herpes zoster in prevalent RD patients (authoritative VZV case definition) |
| `cohort_PrevalentRD_VZV_Morbidity.json` | Shingles Table 2 | VZV with PHN / organ involvement (episode severity classifier) |
| `cohort_PrevalentRD_VZV_vaccine.json` | Shingles STEP 3 | Herpes zoster vaccine recipients (Shingrix / Zostavax) |
| `cohort_PrevalentRD_continuous_DMARDs.json` | Shingles incident STEP 1, PJP incident STEP 1 | Incident base cohort — at-risk population on continuous DMARDs |
| `cohort_PrevalentRD_PJP_infection.json` | PJP (loaded; not applied as filter) | ATLAS-validated PJP cohort — available for cross-validation |
| `cohort_PJP_ppx_infection.json` | PJP STEP 6 / Table 3 | Patients who received PPX before developing PJP |
| `cohort_PJP_infection.json` | (available) | PJP without RD restriction |
| `cohort_PJP_infection_PJP_ppx.json` | (available) | PJP with PPX history |

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

inst/sql/               SqlRender-parameterized SQL templates
inst/json/              ATLAS cohort JSON definitions (see table above)
inst/extdata/           synthetic_patient_data.rds (demo)
inst/app/www/           trajectory_styles.css

launch_dashboard.R                       ← Start here for live OMOP (OHDSI-standard)
test_dashboard.R                         Env-file connection approach (existing deployments)
preliminary_tables_shingles.R            Shingles / VZV analysis tables — prevalent cohort
preliminary_tables_shingles_incident.R   Shingles / VZV analysis tables — incident cohort
preliminary_tables_pjp.R                 PJP analysis tables — prevalent cohort
preliminary_tables_pjp_incident.R        PJP analysis tables — incident cohort
```

---

## Author

Minqi Xiong — Johns Hopkins University — mxiong5@jhu.edu
