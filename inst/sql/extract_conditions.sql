-- extract_conditions.sql
-- Extract condition_occurrence records for a single patient.
--
-- Parameters:
--   @cdm_schema   : schema containing CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @person_id    : integer patient identifier
--   @start_date   : lower bound on condition_start_date
--   @end_date     : upper bound on condition_start_date

SELECT
    co.condition_occurrence_id,
    co.person_id,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    CAST(co.condition_end_date   AS DATE) AS condition_end_date,
    co.condition_concept_id,
    c.concept_name                         AS condition_name,
    co.condition_type_concept_id,
    tc.concept_name                        AS condition_type,
    co.condition_source_value,
    co.stop_reason

FROM @cdm_schema.condition_occurrence co

LEFT JOIN @vocab_schema.concept c
    ON co.condition_concept_id = c.concept_id

LEFT JOIN @vocab_schema.concept tc
    ON co.condition_type_concept_id = tc.concept_id

WHERE co.person_id = @person_id
  AND co.condition_start_date >= CAST('@start_date' AS DATE)
  AND co.condition_start_date <= CAST('@end_date'   AS DATE)

ORDER BY co.condition_start_date, co.condition_concept_id;
