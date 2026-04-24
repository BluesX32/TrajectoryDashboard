-- fetch_shingrix_cohort.sql
-- Returns person_ids from a cohort who have Shingrix vaccination records.
-- Uses the same concept logic as fetch_shingrix.sql (codeset 0 from
-- def_shingrix_vaccine.sql): included ancestors 44808679, 21601361, 706103
-- minus excluded 40213260, 706104, 40213255, 40213256.
--
-- Parameters: @cdm_schema, @vocab_schema, @person_ids

-- Concept-set logic mirrors def_shingrix_vaccine.sql exactly:
--   Included  : root ancestors + their valid descendants
--   Excluded  : 40213260/706104 + their descendants; 40213255/40213256 directly only
--              (40213255 and 40213256 are NOT used as ancestor IDs for expansion —
--               this was the previous bug that caused over-exclusion of valid concepts)
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
),
vaccinated AS (
  SELECT DISTINCT person_id
  FROM @cdm_schema.procedure_occurrence
  WHERE procedure_concept_id IN (SELECT concept_id FROM shingrix_concepts)
    AND person_id IN (@person_ids)

  UNION

  SELECT DISTINCT person_id
  FROM @cdm_schema.drug_exposure
  WHERE drug_concept_id IN (SELECT concept_id FROM shingrix_concepts)
    AND person_id IN (@person_ids)
)
SELECT person_id FROM vaccinated
ORDER BY person_id
