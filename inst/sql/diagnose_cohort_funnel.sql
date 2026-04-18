-- diagnose_cohort_funnel.sql
-- Step-by-step funnel to find where cohort_VZV_antivirals.sql loses patients
-- vs the ATLAS template (gold standard).
--
-- Run each block separately via DBI::dbGetQuery() or test_cohort_connection().
-- Parameters: @cdm_schema, @vocab_schema
--
-- Expected at each step (template gives 300 total):
--   Block 1  : total persons in DB
--   Block 2  : VZV concept set size (if small → vocab_schema wrong)
--   Block 3  : persons with any VZV condition
--   Block 4  : persons with VZV + antiviral on/after (index events)
--   Block 5  : Block 4 + age >= 18
--   Block 6a : Block 5 + rheumatic disease (ANY, NO specialty filter)
--   Block 6b : Block 5 + rheumatic disease WITH specialty filter   <-- compare 6a vs 6b
--   Block 7  : Block 5 + 6b + DMARD in obs period (final cohort)

-- ============================================================
-- Block 1: Total persons (sanity check)
-- ============================================================
SELECT COUNT(DISTINCT person_id) AS total_persons
FROM @cdm_schema.person
;

-- ============================================================
-- Block 2: VZV concept set size — key vocab_schema diagnostic
-- If this is < 50, vocab_schema is wrong or concept_ancestor is empty
-- ============================================================
SELECT COUNT(DISTINCT concept_id) AS vzv_concept_count
FROM (
  SELECT concept_id
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
  SELECT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329
  )
    AND c.invalid_reason IS NULL
) vzv
;

-- ============================================================
-- Block 3: Persons with ANY VZV condition (no antiviral required)
-- ============================================================
SELECT COUNT(DISTINCT co.person_id) AS vzv_persons
FROM @cdm_schema.condition_occurrence co
WHERE co.condition_concept_id IN (
  SELECT concept_id
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
  SELECT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329
  )
    AND c.invalid_reason IS NULL
)
;

-- ============================================================
-- Block 4: Persons with VZV + antiviral on/after, in same obs period
-- (index events — primary eligibility before any inclusion rule)
-- ============================================================
SELECT COUNT(DISTINCT co.person_id) AS index_event_persons
FROM @cdm_schema.condition_occurrence co
JOIN @cdm_schema.observation_period op
  ON co.person_id = op.person_id
  AND co.condition_start_date
        BETWEEN op.observation_period_start_date
            AND op.observation_period_end_date
WHERE co.condition_concept_id IN (
  SELECT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741, 141374, 37165237,
    4221382, 4066727, 37165216, 4080937, 4299673, 37110753,
    4064036, 4067067, 40175007, 37165342, 4080929, 4063440,
    4272156, 4033204, 4033778, 4206461, 135618, 4033777
  )
  UNION
  SELECT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329
  )
    AND c.invalid_reason IS NULL
)
AND EXISTS (
  SELECT 1 FROM @cdm_schema.drug_exposure de
  WHERE de.person_id = co.person_id
    AND de.drug_concept_id IN (
      SELECT concept_id FROM @vocab_schema.concept
      WHERE concept_id IN (1703687, 1703603, 1717704)
      UNION
      SELECT ca.descendant_concept_id
      FROM @vocab_schema.concept_ancestor ca
      JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
      WHERE ca.ancestor_concept_id IN (1703687, 1703603, 1717704)
        AND c.invalid_reason IS NULL
    )
    AND de.drug_exposure_start_date >= co.condition_start_date
    AND de.drug_exposure_start_date
          BETWEEN op.observation_period_start_date
              AND op.observation_period_end_date
)
;

-- ============================================================
-- Block 5: Block 4 + age >= 18
-- ============================================================
SELECT COUNT(DISTINCT co.person_id) AS after_age_filter
FROM @cdm_schema.condition_occurrence co
JOIN @cdm_schema.observation_period op
  ON co.person_id = op.person_id
  AND co.condition_start_date
        BETWEEN op.observation_period_start_date
            AND op.observation_period_end_date
JOIN @cdm_schema.person p ON p.person_id = co.person_id
WHERE co.condition_concept_id IN (
  SELECT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (
    4205455,35205739,443943,138682,45770836,436336,440329,
    45590840,4151978,192239,381504,45542548,45556927,
    35205737,35205738,35205740,35205741,141374,37165237,
    4221382,4066727,37165216,4080937,4299673,37110753,
    4064036,4067067,40175007,37165342,4080929,4063440,
    4272156,4033204,4033778,4206461,135618,4033777
  )
  UNION
  SELECT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (4205455,35205739,443943,138682,45770836,436336,440329)
    AND c.invalid_reason IS NULL
)
AND EXISTS (
  SELECT 1 FROM @cdm_schema.drug_exposure de
  WHERE de.person_id = co.person_id
    AND de.drug_concept_id IN (
      SELECT concept_id FROM @vocab_schema.concept WHERE concept_id IN (1703687,1703603,1717704)
      UNION
      SELECT ca.descendant_concept_id FROM @vocab_schema.concept_ancestor ca
      JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
      WHERE ca.ancestor_concept_id IN (1703687,1703603,1717704) AND c.invalid_reason IS NULL
    )
    AND de.drug_exposure_start_date >= co.condition_start_date
    AND de.drug_exposure_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
)
AND YEAR(co.condition_start_date) - p.year_of_birth >= 18
;

-- ============================================================
-- Block 6a: Block 5 + rheumatic disease ANY (NO specialty filter)
-- Compare 6a vs 6b to quantify how many patients the specialty filter removes
-- ============================================================
SELECT COUNT(DISTINCT co.person_id) AS rheum_no_specialty
FROM @cdm_schema.condition_occurrence co
JOIN @cdm_schema.observation_period op
  ON co.person_id = op.person_id
  AND co.condition_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
JOIN @cdm_schema.person p ON p.person_id = co.person_id
WHERE co.condition_concept_id IN (
  SELECT concept_id FROM @vocab_schema.concept WHERE concept_id IN (
    4205455,35205739,443943,138682,45770836,436336,440329,
    45590840,4151978,192239,381504,45542548,45556927,
    35205737,35205738,35205740,35205741,141374,37165237,
    4221382,4066727,37165216,4080937,4299673,37110753,
    4064036,4067067,40175007,37165342,4080929,4063440,
    4272156,4033204,4033778,4206461,135618,4033777
  )
  UNION
  SELECT ca.descendant_concept_id FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (4205455,35205739,443943,138682,45770836,436336,440329)
    AND c.invalid_reason IS NULL
)
AND EXISTS (
  SELECT 1 FROM @cdm_schema.drug_exposure de
  WHERE de.person_id = co.person_id
    AND de.drug_concept_id IN (
      SELECT concept_id FROM @vocab_schema.concept WHERE concept_id IN (1703687,1703603,1717704)
      UNION
      SELECT ca.descendant_concept_id FROM @vocab_schema.concept_ancestor ca
      JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
      WHERE ca.ancestor_concept_id IN (1703687,1703603,1717704) AND c.invalid_reason IS NULL
    )
    AND de.drug_exposure_start_date >= co.condition_start_date
    AND de.drug_exposure_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
)
AND YEAR(co.condition_start_date) - p.year_of_birth >= 18
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.condition_occurrence co2
  WHERE co2.person_id = co.person_id
    AND co2.condition_concept_id IN (
      -- all 7 rheumatic disease codesets (5-11), no specialty filter
      37016279,4319305,4300204,4324123,4066824,432919,606388,46273369,
      4055640,35208699,45562709,45567545,257628,606386,255891,46270384,
      35208826,35208701,45606214,3321233,45601434,606430,4145240,4343923,
      35208700,44819941,4344158,4149913,45582126,35208827,45591820,
      4270868,4005037,80182,4081250,4344161,
      4126439,37397763,4337524,4128222,134442,4331739,441928,4105026,
      44811612,40352976,4027230,
      314963,35208820,4343935,35208821,
      45548265,45586838,45606052,45543436,45572339,45553046,45591705,
      45562599,45543443,45562600,45567422,45567423,45586845,725373,
      45606064,45538639,45606063,45543442,45601289,45572346,45577117,
      45567425,45577119,45548271,45548270,45567426,80809,4117687,
      4115161,4116440,4116150,4116151,4117686,4114439,4116441,45591700,
      45572337,45596437,45538633,45548263,45543435,45582014,45553045,
      725370,45596438,45548261,45606051,45572338,45548262,45596436,
      45606050,45596439,45562591,45582015,45567419,45533697,45567418,
      45543434,45553044,35208750,37160562,45567420,45577104,45572340,
      45533702,45553051,45533701,45562593,45572341,725372,45577109,
      45557762,45606055,45557763,45601284,45606053,45606054,45533703,
      45577105,45577107,45538635,45591701,45596442,45606056,35208753,
      45586836,45557754,45591686,45572332,45538631,45567415,45591694,
      45548258,45548257,45567413,45596428,45596427,45572327,4083556,
      37207809,4035611,
      36716891,37017494,1077506,766408,766409,766411,766410,766402,
      37110375,37205058,40319772,45548197,46274123,4064048,437082,
      45548419,45533841,45586969,45601454,45548418,45533840,45553184,
      45543577,45582150,45567561,
      42535714,4146124,4096220,37166813,4236160,37110370,4137275,
      37110368,37110369,37167489
    )
    AND co2.condition_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
)
;

-- ============================================================
-- Block 6b: Block 5 + rheumatic disease WITH specialty filter
-- ============================================================
SELECT COUNT(DISTINCT co.person_id) AS rheum_with_specialty
FROM @cdm_schema.condition_occurrence co
JOIN @cdm_schema.observation_period op
  ON co.person_id = op.person_id
  AND co.condition_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
JOIN @cdm_schema.person p ON p.person_id = co.person_id
WHERE co.condition_concept_id IN (
  SELECT concept_id FROM @vocab_schema.concept WHERE concept_id IN (
    4205455,35205739,443943,138682,45770836,436336,440329,
    45590840,4151978,192239,381504,45542548,45556927,
    35205737,35205738,35205740,35205741,141374,37165237,
    4221382,4066727,37165216,4080937,4299673,37110753,
    4064036,4067067,40175007,37165342,4080929,4063440,
    4272156,4033204,4033778,4206461,135618,4033777
  )
  UNION
  SELECT ca.descendant_concept_id FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (4205455,35205739,443943,138682,45770836,436336,440329)
    AND c.invalid_reason IS NULL
)
AND EXISTS (
  SELECT 1 FROM @cdm_schema.drug_exposure de
  WHERE de.person_id = co.person_id
    AND de.drug_concept_id IN (
      SELECT concept_id FROM @vocab_schema.concept WHERE concept_id IN (1703687,1703603,1717704)
      UNION
      SELECT ca.descendant_concept_id FROM @vocab_schema.concept_ancestor ca
      JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
      WHERE ca.ancestor_concept_id IN (1703687,1703603,1717704) AND c.invalid_reason IS NULL
    )
    AND de.drug_exposure_start_date >= co.condition_start_date
    AND de.drug_exposure_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
)
AND YEAR(co.condition_start_date) - p.year_of_birth >= 18
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.condition_occurrence co2
  LEFT JOIN @cdm_schema.provider pr ON co2.provider_id = pr.provider_id
  WHERE co2.person_id = co.person_id
    AND co2.condition_concept_id IN (
      37016279,4319305,4300204,4324123,4066824,432919,606388,46273369,
      4055640,35208699,45562709,45567545,257628,606386,255891,46270384,
      35208826,35208701,45606214,3321233,45601434,606430,4145240,4343923,
      35208700,44819941,4344158,4149913,45582126,35208827,45591820,
      4270868,4005037,80182,4081250,4344161,
      4126439,37397763,4337524,4128222,134442,4331739,441928,4105026,
      44811612,40352976,4027230,
      314963,35208820,4343935,35208821,
      45548265,45586838,45606052,45543436,45572339,45553046,45591705,
      45562599,45543443,45562600,45567422,45567423,45586845,725373,
      45606064,45538639,45606063,45543442,45601289,45572346,45577117,
      45567425,45577119,45548271,45548270,45567426,80809,4117687,
      4115161,4116440,4116150,4116151,4117686,4114439,4116441,45591700,
      45572337,45596437,45538633,45548263,45543435,45582014,45553045,
      725370,45596438,45548261,45606051,45572338,45548262,45596436,
      45606050,45596439,45562591,45582015,45567419,45533697,45567418,
      45543434,45553044,35208750,37160562,45567420,45577104,45572340,
      45533702,45553051,45533701,45562593,45572341,725372,45577109,
      45557762,45606055,45557763,45601284,45606053,45606054,45533703,
      45577105,45577107,45538635,45591701,45596442,45606056,35208753,
      45586836,45557754,45591686,45572332,45538631,45567415,45591694,
      45548258,45548257,45567413,45596428,45596427,45572327,4083556,
      37207809,4035611,
      36716891,37017494,1077506,766408,766409,766411,766410,766402,
      37110375,37205058,40319772,45548197,46274123,4064048,437082,
      45548419,45533841,45586969,45601454,45548418,45533840,45553184,
      45543577,45582150,45567561,
      42535714,4146124,4096220,37166813,4236160,37110370,4137275,
      37110368,37110369,37167489
    )
    AND co2.condition_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
    AND pr.specialty_concept_id IN (44777791, 38004491, 38003882)
)
;
