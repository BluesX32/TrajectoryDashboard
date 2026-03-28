-- extract_labs.sql
-- Extract measurement (lab) records for a single patient.
--
-- Parameters:
--   @cdm_schema      : schema containing CDM tables (e.g. "MyDB.dbo")
--   @vocab_schema    : schema containing vocabulary tables
--   @person_id       : integer patient identifier
--   @start_date      : lower date bound (inclusive)
--   @end_date        : upper date bound (inclusive)
--   @concept_filter  : comma-separated concept IDs; leave empty for all labs

SELECT
    m.measurement_id,
    m.person_id,
    CAST(m.measurement_date AS DATE)      AS measurement_date,
    m.measurement_concept_id,
    c.concept_name                         AS measurement_name,
    m.value_as_number,
    m.range_low,
    m.range_high,
    uc.concept_name                        AS unit_name,
    m.measurement_source_value,
    m.value_source_value

FROM @cdm_schema.measurement m

LEFT JOIN @vocab_schema.concept c
    ON m.measurement_concept_id = c.concept_id

LEFT JOIN @vocab_schema.concept uc
    ON m.unit_concept_id = uc.concept_id

WHERE m.person_id = @person_id
  AND m.measurement_date >= CAST('@start_date' AS DATE)
  AND m.measurement_date <= CAST('@end_date'   AS DATE)
  {@concept_filter != ''} ? {AND m.measurement_concept_id IN (@concept_filter)}

ORDER BY m.measurement_date, m.measurement_concept_id;
