-- fetch_phn_events.sql
-- Post-herpetic neuralgia events for one patient.
-- Concept IDs from def_post_herpetic_neuralgia.sql codeset 0:
--   4044396 (post-herpetic neuralgia), 4071164 (postherpetic neuralgia)
--
-- Parameters: @cdm_schema, @vocab_schema, @person_id

SELECT
  co.person_id,
  CAST(co.condition_start_date AS DATE) AS condition_start_date,
  CAST(co.condition_end_date   AS DATE) AS condition_end_date,
  co.condition_concept_id,
  c.concept_name                        AS condition_name,
  co.condition_source_value
FROM @cdm_schema.condition_occurrence co
LEFT JOIN @vocab_schema.concept c ON co.condition_concept_id = c.concept_id
WHERE co.person_id = @person_id
  AND co.condition_concept_id IN (
    SELECT DISTINCT concept_id
    FROM @vocab_schema.concept
    WHERE concept_id IN (4044396, 4071164)
    UNION
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept cv ON ca.descendant_concept_id = cv.concept_id
    WHERE ca.ancestor_concept_id IN (4044396, 4071164)
      AND cv.invalid_reason IS NULL
  )
ORDER BY co.condition_start_date
