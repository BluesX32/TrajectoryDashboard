# SQL Change Log — Shingles / VZV Analysis

Changes to cohort-defining SQL across **SteroidDoseR** and **TrajectoryDashboard**.  
These explain why patient counts and results differ from earlier runs.

---

## 1. Base cohort — definition replaced

**Before:** No formal OMOP phenotype. `preliminary_tables.R` used an ad-hoc query that required:
- Rheumatic disease diagnosis coded by a rheumatology / internal medicine / dermatology specialist (`specialty_concept_id IN (44777791, 38004491, 38003882)`)
- At least one of: prednisone, IVIG, or any DMARD

**After:** Aligns with `inst/sql/templates/rheum-dmard-cohort-omop.sql`. Requirements are:
- Rheumatic disease diagnosis (any provider) using the full OHDSI concept set (codesets 0–7: SLE, RA, inflammatory arthritis, myositis, SSc, GCA, lupus, SpA) — exact match for some codesets, ancestor traversal for others
- DMARD / immunosuppressant exposure only (codeset 8, 24 ancestor IDs) — prednisone and IVIG are **no longer** sufficient on their own
- Age **≥ 18** (was `> 18`, i.e., 18-year-olds were previously excluded)
- No specialist filter

**New file (SteroidDoseR):** `inst/sql/cohort_rheum_dmard.sql`

**Impact:** Likely larger cohort (no specialist filter), but potentially smaller for patients who only had prednisone/IVIG without a DMARD.

---

## 2. Shingles cohort — definition replaced

**Before:** Used `cohort_VZV_antivirals.sql`, which required all three of:
1. VZV / herpes zoster diagnosis
2. Antiviral drug exposure (acyclovir / valacyclovir / famciclovir)
3. Immunosuppressant exposure

**After:** Uses `cohort_shingles_infection.sql` — **VZV diagnosis only**. No antiviral or immunosuppressant required. Any of 38 VZV/herpes zoster concept IDs (plus descendants of 16 ancestor IDs) in `condition_occurrence` qualifies.

**New file (SteroidDoseR):** `inst/sql/cohort_shingles_infection.sql`

**Impact:** Substantially larger shingles cohort. The old definition captured only treated cases; the new one captures all diagnosed cases.

---

## 3. Shingles vaccine cohort — new query

**Before:** Vaccine cohort was pulled from an existing Databricks/Delta Lake SQL (`def_shingrix_vaccine.sql`) that could not run inline via `renderTranslateQuerySql`.

**After:** Portable CTE-based SQL added to both projects. Looks for zoster vaccine records in `drug_exposure` OR `procedure_occurrence` using three ancestor concept IDs (44808679, 21601361, 706103), excluding live-zoster vaccines (40213260, 706104, 40213255, 40213256).

**New file (SteroidDoseR):** `inst/sql/cohort_shingrix_vaccine.sql`

---

## 4. Race query — new (this session)

Added to `preliminary_tables.R` Step 3b. Joins `person.race_concept_id` to `concept.concept_name`. Patients with no race mapping receive `'Unknown'`. Used in Table 1.

---

## 5. Vaccine dose count query — new (this session)

Added to `preliminary_tables.R` Step 3b. Counts distinct vaccine dates per base-cohort patient (combining `drug_exposure` and `procedure_occurrence`). Result is categorised as 0 / 1 / 2+ doses. Used in Table 1.

---

## 6. DMARD episode-window queries — new (this session, Tables 3 & 4)

Two new in-script queries using `concept_ancestor` against the 24 DMARD ancestor IDs:

- **Table 3:** fetches all DMARD exposures for shingles patients, then filters in R to the window `[episode_date − 90 d, episode_date]` for each collapsed shingles episode.
- **Table 4:** same exposure pull for vaccinated shingles patients, filtered to `[vaccine_date − 90 d, vaccine_date + 30 d]` per vaccine dose date.

Episode-level: a patient with multiple shingles episodes or vaccine doses is counted once per episode.

---

## Summary table

| # | What changed | Old definition | New definition | Expected direction of change |
|---|---|---|---|---|
| 1 | Base cohort | Rheum Dx (specialist only) + prednisone / IVIG / DMARD | Rheum Dx (any provider) + DMARD only, age ≥ 18 | Likely **larger** |
| 2 | Shingles cohort | VZV Dx + antiviral + immunosuppressant | VZV Dx only | Substantially **larger** |
| 3 | Vaccine cohort | External Delta Lake SQL | Inline portable CTE | Equivalent |
| 4 | Race | Not collected | Added via `concept` join | New column |
| 5 | Vaccine doses | Not collected | Count of distinct dates (0/1/2+) | New column |
| 6 | DMARD windows (T3/T4) | Not collected | Episode-level 90 d pre-episode / peri-vaccine | New tables |
