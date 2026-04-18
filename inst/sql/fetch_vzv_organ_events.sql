-- fetch_vzv_organ_events.sql
-- VZV organ involvement events for one patient.
-- Concept IDs from def_VZV_organ.sql codeset 7 (the primary event codeset):
--   encephalitis, meningitis, pneumonitis, hepatitis, disseminated zoster, etc.
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
    4224242, 37209444, 608800, 608996, 37209445, 37209443, 761347,
    4205455, 4171706, 4045976, 4139215, 438961, 45770924, 376028,
    440323, 36712850, 4310159, 45757253, 37108968, 45581141, 45581142,
    35205738, 45561737, 42484196, 45571453
  )
ORDER BY co.condition_start_date
