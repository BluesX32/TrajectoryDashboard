-- fetch_shingrix.sql
-- Shingrix (recombinant zoster vaccine) events for one patient.
-- Searches PROCEDURE_OCCURRENCE and DRUG_EXPOSURE.
-- Concept set mirrors def_shingrix_vaccine.sql (codeset 0):
--   ancestors 44808679, 21601361, 706103 with exclusions 40213260, 706104,
--   40213255, 40213256 and their descendants.
--
-- Parameters: @cdm_schema, @vocab_schema, @person_id

WITH shingrix_included AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
),
shingrix_excluded AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (40213260, 706104, 40213255, 40213256)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (40213260, 706104)
    AND c.invalid_reason IS NULL
),
shingrix_concepts AS (
  SELECT i.concept_id
  FROM shingrix_included i
  LEFT JOIN shingrix_excluded e ON i.concept_id = e.concept_id
  WHERE e.concept_id IS NULL
)

SELECT
  po.person_id,
  CAST(po.procedure_date AS DATE)  AS event_date,
  'procedure'                       AS event_type,
  c.concept_name                    AS event_name,
  po.procedure_source_value         AS source_value
FROM @cdm_schema.procedure_occurrence po
JOIN shingrix_concepts sc ON po.procedure_concept_id = sc.concept_id
LEFT JOIN @vocab_schema.concept c ON po.procedure_concept_id = c.concept_id
WHERE po.person_id = @person_id

UNION ALL

SELECT
  de.person_id,
  CAST(de.drug_exposure_start_date AS DATE) AS event_date,
  'drug'                                     AS event_type,
  c.concept_name                             AS event_name,
  de.drug_source_value                       AS source_value
FROM @cdm_schema.drug_exposure de
JOIN shingrix_concepts sc ON de.drug_concept_id = sc.concept_id
LEFT JOIN @vocab_schema.concept c ON de.drug_concept_id = c.concept_id
WHERE de.person_id = @person_id

ORDER BY event_date
