-- extract_observations.sql
-- Extract observation records for a single patient.
--
-- Parameters:
--   @cdm_schema      : schema containing CDM tables
--   @vocab_schema    : schema containing vocabulary tables
--   @person_id       : integer patient identifier
--   @start_date      : lower bound on observation_date
--   @end_date        : upper bound on observation_date
--   @concept_filter  : comma-separated observation_concept_ids; empty = all

SELECT
    ob.observation_id,
    ob.person_id,
    CAST(ob.observation_date AS DATE) AS observation_date,
    ob.observation_concept_id,
    c.concept_name                     AS observation_name,
    ob.value_as_number,
    ob.value_as_string,
    vc.concept_name                    AS value_as_concept_name,
    ob.observation_source_value,
    uc.concept_name                    AS unit_name

FROM @cdm_schema.observation ob

LEFT JOIN @vocab_schema.concept c
    ON ob.observation_concept_id = c.concept_id

LEFT JOIN @vocab_schema.concept vc
    ON ob.value_as_concept_id = vc.concept_id

LEFT JOIN @vocab_schema.concept uc
    ON ob.unit_concept_id = uc.concept_id

WHERE ob.person_id = @person_id
  AND ob.observation_date >= CAST('@start_date' AS DATE)
  AND ob.observation_date <= CAST('@end_date'   AS DATE)
  {@concept_filter != ''} ? {AND ob.observation_concept_id IN (@concept_filter)}

ORDER BY ob.observation_date, ob.observation_concept_id;
