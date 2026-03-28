-- extract_visits.sql
-- Extract visit_occurrence records for a single patient.
--
-- Parameters:
--   @cdm_schema   : schema containing CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @person_id    : integer patient identifier
--   @start_date   : lower bound on visit_start_date
--   @end_date     : upper bound on visit_start_date

SELECT
    vo.visit_occurrence_id,
    vo.person_id,
    CAST(vo.visit_start_date AS DATE) AS visit_start_date,
    CAST(vo.visit_end_date   AS DATE) AS visit_end_date,
    vo.visit_concept_id,
    c.concept_name                     AS visit_type,
    vo.visit_source_value,
    dc.concept_name                    AS discharge_disposition,
    ac.concept_name                    AS admitted_from,
    vo.care_site_id

FROM @cdm_schema.visit_occurrence vo

LEFT JOIN @vocab_schema.concept c
    ON vo.visit_concept_id = c.concept_id

LEFT JOIN @vocab_schema.concept dc
    ON vo.discharged_to_concept_id = dc.concept_id

LEFT JOIN @vocab_schema.concept ac
    ON vo.admitted_from_concept_id = ac.concept_id

WHERE vo.person_id = @person_id
  AND vo.visit_start_date >= CAST('@start_date' AS DATE)
  AND vo.visit_start_date <= CAST('@end_date'   AS DATE)

ORDER BY vo.visit_start_date;
