-- extract_notes.sql
-- Extract note records for a single patient.
--
-- Parameters:
--   @cdm_schema   : schema containing CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @person_id    : integer patient identifier
--   @start_date   : lower bound on note_date
--   @end_date     : upper bound on note_date

SELECT
    n.note_id,
    n.person_id,
    CAST(n.note_date AS DATE)  AS note_date,
    n.note_title,
    n.note_text,
    tc.concept_name             AS note_type,
    nc.concept_name             AS note_class,
    n.visit_occurrence_id

FROM @cdm_schema.note n

LEFT JOIN @vocab_schema.concept tc
    ON n.note_type_concept_id = tc.concept_id

LEFT JOIN @vocab_schema.concept nc
    ON n.note_class_concept_id = nc.concept_id

WHERE n.person_id = @person_id
  AND n.note_date >= CAST('@start_date' AS DATE)
  AND n.note_date <= CAST('@end_date'   AS DATE)

ORDER BY n.note_date;
