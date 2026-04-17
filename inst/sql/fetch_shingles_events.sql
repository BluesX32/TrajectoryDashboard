-- fetch_shingles_events.sql
-- Returns herpes zoster / shingles condition occurrences for one patient.
-- Uses the same VZV concept set as cohort_VZV_antivirals.sql (codeset 0)
-- so shingles events are exactly consistent with cohort eligibility criteria.
--
-- Parameters
-- ----------
-- @cdm_schema   : OMOP CDM schema
-- @vocab_schema : vocabulary schema
-- @person_id    : integer patient identifier

WITH vzv_concepts AS (
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

SELECT
  co.person_id,
  co.condition_occurrence_id,
  co.condition_start_date,
  co.condition_end_date,
  co.condition_concept_id,
  c.concept_name        AS condition_name,
  co.condition_source_value
FROM @cdm_schema.condition_occurrence co
JOIN vzv_concepts vc
  ON co.condition_concept_id = vc.concept_id
LEFT JOIN @vocab_schema.concept c
  ON co.condition_concept_id = c.concept_id
WHERE co.person_id = @person_id
ORDER BY co.condition_start_date
