-- extract_medications.sql
-- Extract drug_exposure records for a single patient.
--
-- Parameters:
--   @cdm_schema      : schema containing CDM tables
--   @vocab_schema    : schema containing vocabulary tables
--   @person_id       : integer patient identifier
--   @start_date      : lower bound on drug_exposure_start_date
--   @end_date        : upper bound on drug_exposure_start_date
--   @concept_filter  : comma-separated drug_concept_ids; empty = all

SELECT
    de.drug_exposure_id,
    de.person_id,
    CAST(de.drug_exposure_start_date AS DATE) AS drug_exposure_start_date,
    CAST(de.drug_exposure_end_date   AS DATE) AS drug_exposure_end_date,
    de.drug_concept_id,
    c.concept_name                             AS drug_name,
    de.quantity,
    de.days_supply,
    de.sig,
    de.drug_source_value,
    rc.concept_name                            AS route_name,
    ds.amount_value,
    de.stop_reason

FROM @cdm_schema.drug_exposure de

LEFT JOIN @vocab_schema.concept c
    ON de.drug_concept_id = c.concept_id

LEFT JOIN @vocab_schema.concept rc
    ON de.route_concept_id = rc.concept_id

LEFT JOIN (
    SELECT
        drug_concept_id,
        MAX(amount_value) AS amount_value
    FROM @vocab_schema.drug_strength
    WHERE amount_value IS NOT NULL
    GROUP BY drug_concept_id
) ds ON de.drug_concept_id = ds.drug_concept_id

WHERE de.person_id = @person_id
  AND de.drug_exposure_start_date >= CAST('@start_date' AS DATE)
  AND de.drug_exposure_start_date <= CAST('@end_date'   AS DATE)
  {@concept_filter != ''} ? {AND de.drug_concept_id IN (@concept_filter)}

ORDER BY de.drug_exposure_start_date, de.drug_concept_id;
