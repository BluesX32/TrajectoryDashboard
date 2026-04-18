-- fetch_shingrix_cohort.sql
-- Returns person_ids from a cohort who have Shingrix vaccination records.
-- Uses the same concept logic as fetch_shingrix.sql (codeset 0 from
-- def_shingrix_vaccine.sql): included ancestors 44808679, 21601361, 706103
-- minus excluded 40213260, 706104, 40213255, 40213256.
--
-- Parameters: @cdm_schema, @vocab_schema, @person_ids

WITH shingrix_included AS (
  SELECT DISTINCT ca.descendant_concept_id AS concept_id
  FROM @vocab_schema.concept_ancestor ca
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
),
shingrix_excluded AS (
  SELECT DISTINCT ca.descendant_concept_id AS concept_id
  FROM @vocab_schema.concept_ancestor ca
  WHERE ca.ancestor_concept_id IN (40213260, 706104, 40213255, 40213256)
),
shingrix_concepts AS (
  SELECT concept_id FROM shingrix_included
  EXCEPT
  SELECT concept_id FROM shingrix_excluded
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
