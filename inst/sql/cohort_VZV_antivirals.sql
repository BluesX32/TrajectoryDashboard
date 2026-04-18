-- cohort_VZV_antivirals.sql
-- VZV / Herpes Zoster Antivirals cohort — SqlRender-parameterized version
--
-- Selects DISTINCT person_ids for patients who:
--   1. Have a VZV / herpes zoster diagnosis within an observation period (codeset 0)
--      AND received an antiviral drug on or after that diagnosis date (codeset 1)
--   2. Age >= 18 at index date
--   3. Have a rheumatic/inflammatory disease diagnosis seen by a relevant
--      specialist (codesets 5-11, specialty concept IDs 44777791/38004491/38003882)
--   4. Have DMARD / immunosuppressant exposure anytime in the observation period
--      (codeset 12)
--
-- Gold standard: inst/sql/templates/cohort_VZV_antiviral.sql (ATLAS-generated).
-- Logic mirrors that template exactly; restructured from a single deep CTE chain
-- into sub-selects to avoid Spark SQL CTE mis-optimisation (Spark does not
-- guarantee CTE materialisation, which caused ~105 patients to be lost).
--
-- Parameters (SqlRender)
-- ----------------------
-- @cdm_schema   : schema containing OMOP CDM clinical tables
-- @vocab_schema : schema containing CONCEPT / CONCEPT_ANCESTOR tables
--                 (may differ from @cdm_schema on Databricks)

-- ---------------------------------------------------------------------------
-- Final cohort
-- ---------------------------------------------------------------------------
SELECT DISTINCT ie.person_id

FROM (
  -- -------------------------------------------------------------------------
  -- Index events: earliest qualifying VZV + antiviral event per person
  -- Mirrors template qualified_events + row_number logic.
  -- -------------------------------------------------------------------------
  SELECT
    co.person_id,
    MIN(co.condition_start_date)          AS index_date,
    MIN(op.observation_period_start_date) AS op_start_date,
    MAX(op.observation_period_end_date)   AS op_end_date
  FROM @cdm_schema.condition_occurrence co
  JOIN @cdm_schema.observation_period op
    ON  co.person_id = op.person_id
    AND co.condition_start_date
          BETWEEN op.observation_period_start_date
              AND op.observation_period_end_date
  WHERE
    -- VZV / herpes zoster diagnosis (codeset 0, clean IDs + descendants)
    co.condition_concept_id IN (
      SELECT DISTINCT concept_id
      FROM @vocab_schema.concept
      WHERE concept_id IN (
        4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
        45590840, 4151978, 192239, 381504, 45542548, 45556927,
        35205737, 35205738, 35205740, 35205741, 141374, 37165237,
        4221382, 4066727, 37165216, 4080937, 4299673, 37110753,
        4064036, 4067067, 40175007, 37165342, 4080929, 4063440,
        4272156, 4033204, 4033778, 4206461, 135618, 4033777
      )
      UNION
      SELECT DISTINCT ca.descendant_concept_id
      FROM @vocab_schema.concept_ancestor ca
      JOIN @vocab_schema.concept c
        ON ca.descendant_concept_id = c.concept_id
      WHERE ca.ancestor_concept_id IN (
        4205455, 35205739, 443943, 138682, 45770836, 436336, 440329
      )
        AND c.invalid_reason IS NULL
    )
    -- Antiviral drug on or after index date, within the same obs period
    -- (codeset 1: acyclovir, valacyclovir, famciclovir + descendants)
    AND EXISTS (
      SELECT 1
      FROM @cdm_schema.drug_exposure de
      WHERE de.person_id = co.person_id
        AND de.drug_exposure_start_date >= co.condition_start_date
        AND de.drug_exposure_start_date
              BETWEEN op.observation_period_start_date
                  AND op.observation_period_end_date
        AND de.drug_concept_id IN (
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
        )
    )
  GROUP BY co.person_id
) ie

JOIN @cdm_schema.person p
  ON p.person_id = ie.person_id

-- ---------------------------------------------------------------------------
-- Inclusion 0: age >= 18 at index date
-- ---------------------------------------------------------------------------
WHERE YEAR(ie.index_date) - p.year_of_birth >= 18

-- ---------------------------------------------------------------------------
-- Inclusion 1: rheumatic / inflammatory disease seen by relevant specialist
-- Codesets 5-11 (SLE2, DM2+desc, SSc2, GCA, RA, SpA, vasculitis).
-- Specialty concept IDs: Rheumatology=44777791, Internal Medicine=38004491,
--   Dermatology=38003882 (some myositis/vasculitis patients seen by derm).
-- ---------------------------------------------------------------------------
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.condition_occurrence co2
  LEFT JOIN @cdm_schema.provider pr
    ON co2.provider_id = pr.provider_id
  WHERE co2.person_id = ie.person_id
    AND co2.condition_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
    AND pr.specialty_concept_id IN (44777791, 38004491, 38003882)
    AND co2.condition_concept_id IN (
      -- SLE2_rs (5)
      37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
      4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
      35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
      35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
      -- DM2_rs (6) base — descendants joined below
      4270868, 4005037, 80182, 4081250, 4344161,
      -- SSc2_rs (7)
      4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
      44811612, 40352976, 4027230,
      -- GCA_rs (8)
      314963, 35208820, 4343935, 35208821,
      -- RA_rs (9)
      45548265, 45586838, 45606052, 45543436, 45572339, 45553046, 45591705,
      45562599, 45543443, 45562600, 45567422, 45567423, 45586845, 725373,
      45606064, 45538639, 45606063, 45543442, 45601289, 45572346, 45577117,
      45567425, 45577119, 45548271, 45548270, 45567426, 80809, 4117687,
      4115161, 4116440, 4116150, 4116151, 4117686, 4114439, 4116441, 45591700,
      45572337, 45596437, 45538633, 45548263, 45543435, 45582014, 45553045,
      725370, 45596438, 45548261, 45606051, 45572338, 45548262, 45596436,
      45606050, 45596439, 45562591, 45582015, 45567419, 45533697, 45567418,
      45543434, 45553044, 35208750, 37160562, 45567420, 45577104, 45572340,
      45533702, 45553051, 45533701, 45562593, 45572341, 725372, 45577109,
      45557762, 45606055, 45557763, 45601284, 45606053, 45606054, 45533703,
      45577105, 45577107, 45538635, 45591701, 45596442, 45606056, 35208753,
      45586836, 45557754, 45591686, 45572332, 45538631, 45567415, 45591694,
      45548258, 45548257, 45567413, 45596428, 45596427, 45572327, 4083556,
      37207809, 4035611,
      -- SpA_rs (10)
      36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
      37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
      45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
      45543577, 45582150, 45567561,
      -- vasculitis_rs (11)
      42535714, 4146124, 4096220, 37166813, 4236160, 37110370, 4137275,
      37110368, 37110369, 37167489
    )
)
-- DM2 descendants (codeset 6) handled via a second EXISTS to avoid
-- mixing base-ID and descendant-ID lists in one IN clause
OR EXISTS (
  SELECT 1
  FROM @cdm_schema.condition_occurrence co2
  JOIN @vocab_schema.concept_ancestor ca
    ON co2.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  LEFT JOIN @cdm_schema.provider pr
    ON co2.provider_id = pr.provider_id
  WHERE co2.person_id = ie.person_id
    AND co2.condition_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
    AND ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161)
    AND c.invalid_reason IS NULL
    AND pr.specialty_concept_id IN (44777791, 38004491, 38003882)
)

-- ---------------------------------------------------------------------------
-- Inclusion 2: DMARD / immunosuppressant anytime in observation period
-- (codeset 12, with descendants)
-- ---------------------------------------------------------------------------
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  WHERE de.person_id = ie.person_id
    AND de.drug_exposure_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
    AND de.drug_concept_id IN (
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
    )
)
;
