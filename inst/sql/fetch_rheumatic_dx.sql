-- fetch_rheumatic_dx.sql
-- Returns each patient's rheumatic disease diagnoses with disease category labels.
-- Uses the same concept sets as cohort_VZV_antivirals.sql codesets 5-11.
-- Returns DISTINCT conditions per disease_category (earliest per category).
--
-- Parameters: @cdm_schema, @vocab_schema, @person_id

SELECT
  disease_category,
  condition_concept_id,
  condition_name,
  condition_start_date,
  condition_source_value
FROM (
  SELECT
    'SLE'        AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND co.condition_concept_id IN (
      37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
      4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
      35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
      35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820
    )

  UNION ALL

  SELECT
    'DM/Myositis' AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  JOIN @vocab_schema.concept_ancestor ca
    ON co.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161)
    AND c.invalid_reason IS NULL

  UNION ALL

  SELECT
    'SSc'         AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND co.condition_concept_id IN (
      4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928,
      4105026, 44811612, 40352976, 4027230
    )

  UNION ALL

  SELECT
    'GCA'         AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND co.condition_concept_id IN (314963, 35208820, 4343935, 35208821)

  UNION ALL

  SELECT
    'RA'          AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND co.condition_concept_id IN (
      45548265, 45586838, 45606052, 45543436, 45572339, 45553046, 45591705,
      45562599, 45543443, 45562600, 45567422, 45567423, 45586845, 725373,
      45606064, 45538639, 45606063, 45543442, 45601289, 45572346, 45577117,
      45567425, 45577119, 45548271, 45548270, 45567426, 80809, 4117687,
      4115161, 4116440, 4116150, 4116151, 4117686, 4114439, 4116441,
      4083556, 37207809, 4035611
    )

  UNION ALL

  SELECT
    'SpA'         AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND co.condition_concept_id IN (
      36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
      37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
      45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
      45543577, 45582150, 45567561
    )

  UNION ALL

  SELECT
    'Vasculitis'  AS disease_category,
    co.condition_concept_id,
    c.concept_name AS condition_name,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    co.condition_source_value,
    ROW_NUMBER() OVER (PARTITION BY co.condition_concept_id
                       ORDER BY co.condition_start_date) AS rn
  FROM @cdm_schema.condition_occurrence co
  LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
  WHERE co.person_id = @person_id
    AND co.condition_concept_id IN (
      42535714, 4146124, 4096220, 37166813, 4236160, 37110370, 4137275,
      37110368, 37110369, 37167489
    )
) all_dx
WHERE rn = 1
ORDER BY disease_category, condition_start_date
