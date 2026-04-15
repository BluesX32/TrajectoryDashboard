-- cohort_VZV_antivirals.sql
-- VZV / Herpes Zoster Antivirals cohort
--
-- Selects person_ids of patients who:
--   1. Have a VZV / herpes zoster diagnosis (index event, codeset 0)
--      AND received an antiviral drug on or after that date (codeset 1)
--   2. Age 18+ at the index event
--   3. Have a rheumatic/inflammatory disease diagnosis seen by a relevant
--      specialist (codesets 5-11, specialty concept IDs 44777791/38004491/38003882)
--   4. Have DMARD / immunosuppressant exposure (codeset 12, anytime)
--
-- This is the SqlRender-parameterized version (replaces the Databricks-specific
-- temp-view version). Cross-database dialect translation is handled by
-- SqlRender::translate() at runtime.
--
-- For a JSON-driven version that lets you swap concept sets, use
-- build_cohort_sql() in R/cohort.R.
--
-- Parameters (SqlRender)
-- ----------------------
-- @cdm_schema   : schema containing OMOP CDM clinical tables
-- @vocab_schema : schema containing concept / concept_ancestor tables
--
-- Usage in R
-- ----------
--   sql  <- SqlRender::readSql(system.file("sql", "cohort_VZV_antivirals.sql",
--                                           package = "TrajectoryDashboard"))
--   ids  <- DatabaseConnector::renderTranslateQuerySql(
--             conn, sql,
--             cdm_schema   = "Myositis_OMOP.dbo",
--             vocab_schema = "Myositis_OMOP.dbo",
--             snakeCaseToCamelCase = FALSE
--           )
--   person_ids <- as.integer(ids[[1]])

WITH

-- ---------------------------------------------------------------------------
-- Codeset 0: VZV / herpes zoster conditions (with descendants)
-- ---------------------------------------------------------------------------
vzv_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    4205455, 35205739, 1, 4, 2, 443943, 138682, 45770836, 436336, 440329,
    21, 24, 7, 0, 5, 45590840, 4151978, 192239, 381504, 45542548, 45556927,
    10, 8, 9, 35205737, 35205738, 35205740, 35205741, 443943, 141374, 37165237,
    4221382, 4066727, 37165216, 4080937, 1, 4299673, 2, 37110753, 4064036,
    4067067, 443943, 40175007, 7, 0, 5, 37165342, 4080929, 4063440, 4272156,
    4033204, 4033778, 4206461, 135618, 4033777
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 1, 4, 2, 443943, 138682, 45770836, 436336, 440329,
    21, 24, 7, 0, 5, 45590840, 4151978, 192239, 381504, 45542548, 45556927,
    10, 8, 9, 35205737, 35205738, 35205740, 35205741
  )
    AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Codeset 1: Antiviral drugs (acyclovir, valacyclovir, famciclovir, +desc.)
-- ---------------------------------------------------------------------------
antiviral_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (1703687, 1703603, 1717704)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (1703687, 1703603, 1717704)
    AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Codesets 5-11: Rheumatic / inflammatory disease conditions
--   5=SLE2, 6=DM2(+desc), 7=SSc2, 8=GCA, 9=RA, 10=SpA, 11=vasculitis
-- ---------------------------------------------------------------------------
rheum_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    -- SLE (5)
    37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
    4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
    35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
    35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
    -- DM (7) without descendants
    4270868, 4005037, 80182, 4081250, 4344161,
    -- SSc (7)
    4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
    44811612, 40352976, 4027230,
    -- GCA (8)
    314963, 35208820, 4343935, 35208821,
    -- SpA (10)
    36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
    37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
    45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
    45543577, 45582150, 45567561,
    -- vasculitis (11)
    42535714, 4146124, 4096220, 37166813, 4236160, 37110370, 4137275,
    37110368, 37110369, 37167489
  )
  UNION
  -- DM2 with descendants (codeset 6)
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161)
    AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Codeset 12: DMARD / immunosuppressant drugs (with descendants)
-- ---------------------------------------------------------------------------
dmard_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
  )
    AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Index events: earliest VZV diagnosis within an observation period,
--               with an antiviral drug on or after that date
-- ---------------------------------------------------------------------------
index_events AS (
  SELECT
    co.person_id,
    MIN(co.condition_start_date)              AS index_date,
    MIN(op.observation_period_start_date)     AS op_start_date,
    MAX(op.observation_period_end_date)       AS op_end_date
  FROM @cdm_schema.condition_occurrence co
  JOIN vzv_concepts vc
    ON co.condition_concept_id = vc.concept_id
  JOIN @cdm_schema.observation_period op
    ON  co.person_id = op.person_id
    AND co.condition_start_date
          BETWEEN op.observation_period_start_date
              AND op.observation_period_end_date
  WHERE EXISTS (
    SELECT 1
    FROM @cdm_schema.drug_exposure de
    JOIN antiviral_concepts ac
      ON de.drug_concept_id = ac.concept_id
    WHERE de.person_id = co.person_id
      AND de.drug_exposure_start_date >= co.condition_start_date
      AND de.drug_exposure_start_date
            BETWEEN op.observation_period_start_date
                AND op.observation_period_end_date
  )
  GROUP BY co.person_id
)

-- ---------------------------------------------------------------------------
-- Final cohort: apply all inclusion rules
-- ---------------------------------------------------------------------------
SELECT DISTINCT ie.person_id
FROM index_events ie
JOIN @cdm_schema.person p
  ON p.person_id = ie.person_id

-- Inclusion 0: age >= 18 at index date
WHERE YEAR(ie.index_date) - p.year_of_birth >= 18

-- Inclusion 1: rheumatic / inflammatory disease seen by relevant specialist
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.condition_occurrence co
  JOIN rheum_concepts rc
    ON co.condition_concept_id = rc.concept_id
  LEFT JOIN @cdm_schema.provider pr
    ON co.provider_id = pr.provider_id
  WHERE co.person_id = ie.person_id
    AND co.condition_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
    AND pr.specialty_concept_id IN (44777791, 38004491, 38003882)
)

-- Inclusion 2: DMARD / immunosuppressant exposure (anytime in obs. period)
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  JOIN dmard_concepts dc
    ON de.drug_concept_id = dc.concept_id
  WHERE de.person_id = ie.person_id
    AND de.drug_exposure_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
)
;
