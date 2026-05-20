DROP TABLE IF EXISTS @temp_database_schema.laolk54sCodesets;
CREATE TABLE @temp_database_schema.laolk54sCodesets  
USING DELTA
 AS
SELECT
CAST(NULL AS int) AS codeset_id,
	CAST(NULL AS bigint) AS concept_id  WHERE 1 = 0;
INSERT INTO @temp_database_schema.laolk54sCodesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (37016279,4319305,4300204,4324123,4066824,432919,606388,46273369,4055640,4324123,35208699,4324123,4066824,35208699,45562709,45567545,257628,4324123,4066824,35208699,606386,255891,46270384,35208826,35208701,45606214,3321233,45601434,606430,4145240,4343923,35208700,44819941,257628,4344158,4149913,45582126,35208827,45591820))
) I
) C UNION ALL 
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4270868,4005037,80182,4081250,4344161))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4270868,4005037,80182,4081250,4344161))
) I
) C UNION ALL 
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (42535714,4146124,4096220,37166813,4236160,37110370,4137275,37110368,37110369,37167489))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (42535714))
) I
) C UNION ALL 
SELECT 3 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (45548265,45586838,45606052,45543436,45572339,45553046,45591705,45562599,45543443,45562600,45567422,45567423,45586845,725373,45606064,45538639,45606063,45543442,45601289,45572346,45577117,45567425,45577119,45548271,45548270,45567426,80809,4117687,4115161,4116440,4116150,4116151,4117686,4114439,4116441,45591700,45572337,45596437,45538633,45548263,45543435,45582014,45553045,725370,45596438,45548261,45606051,45572338,45548262,45596436,45606050,45596439,45562591,45582015,45567419,45533697,45567418,45543434,45553044,35208750,37160562,45567420,45577104,45572340,45533702,45553051,45533701,45562593,45572341,725372,45577109,45557762,45606055,45557763,45601284,45606053,45606054,45533703,45577105,45577107,45538635,45591701,45596442,45606056,35208753,45586836,45557754,45591686,45572332,45538631,45567415,45591694,45548258,45548257,45567413,45596428,45596427,45572327,4083556,37207809,4035611))
) I
) C UNION ALL 
SELECT 4 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (36716891,37017494,1077506,766408,766409,766411,766410,766402,37110375,37205058,40319772,45548197,46274123,4064048,437082,45548419,45533841,45586969,45601454,45548418,45533840,45553184,45543577,45582150,45567561))
) I
) C UNION ALL 
SELECT 5 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4126439,37397763,4337524,4128222,134442,4331739,441928,4105026,44811612,40352976,4027230))
) I
) C UNION ALL 
SELECT 6 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (314963,35208820,4343935,35208821))
) I
) C UNION ALL 
SELECT 7 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4305666,313223,4344493,606328,320749))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4305666,313223,4344493,606328,320749))
) I
) C;
DROP TABLE IF EXISTS @temp_database_schema.laolk54squalified_events;
CREATE TABLE @temp_database_schema.laolk54squalified_events
USING DELTA
AS
SELECT
event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id
FROM
(
 select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal, cast(pe.visit_occurrence_id as bigint) as visit_occurrence_id
 FROM (-- Begin Primary Events
select P.ordinal as event_id, P.person_id, P.start_date, P.end_date, op_start_date, op_end_date, cast(P.visit_occurrence_id as bigint) as visit_occurrence_id
FROM
(
 select E.person_id, E.start_date, E.end_date,
 row_number() OVER (PARTITION BY E.person_id ORDER BY E.sort_date ASC, E.event_id) ordinal,
 OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date, cast(E.visit_occurrence_id as bigint) as visit_occurrence_id
 FROM 
 (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 0)
) C
-- End Condition Occurrence Criteria
UNION ALL
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 1)
) C
-- End Condition Occurrence Criteria
UNION ALL
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 3)
) C
-- End Condition Occurrence Criteria
UNION ALL
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 4)
) C
-- End Condition Occurrence Criteria
UNION ALL
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 5)
) C
-- End Condition Occurrence Criteria
UNION ALL
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 6)
) C
-- End Condition Occurrence Criteria
UNION ALL
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.laolk54sCodesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 7)
) C
-- End Condition Occurrence Criteria
 ) E
 JOIN @cdm_database_schema.observation_period OP on E.person_id = OP.person_id and E.start_date >= OP.observation_period_start_date and E.start_date <= op.observation_period_end_date
 WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE
) P
WHERE P.ordinal = 1
-- End Primary Events
) pe
) QE
;
DROP TABLE IF EXISTS @temp_database_schema.laolk54sInclusion_0;
CREATE TABLE @temp_database_schema.laolk54sInclusion_0
USING DELTA
AS
SELECT
0 as inclusion_rule_id, person_id, event_id
FROM
(
 select pe.person_id, pe.event_id
 FROM @temp_database_schema.laolk54squalified_events pe
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM @temp_database_schema.laolk54squalified_events E
 INNER JOIN
 (
 -- Begin Demographic Criteria
SELECT 0 as index_id, e.person_id, e.event_id
FROM @temp_database_schema.laolk54squalified_events E
JOIN @cdm_database_schema.PERSON P ON P.PERSON_ID = E.PERSON_ID
WHERE YEAR(E.start_date) - P.year_of_birth >= 18
GROUP BY e.person_id, e.event_id
-- End Demographic Criteria
 ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
 GROUP BY E.person_id, E.event_id
 HAVING COUNT(index_id) = 1
) G
-- End Criteria Group
) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
) Results
;
DROP TABLE IF EXISTS @temp_database_schema.laolk54sinclusion_events;
CREATE TABLE @temp_database_schema.laolk54sinclusion_events
USING DELTA
AS
SELECT
inclusion_rule_id, person_id, event_id
FROM
(select inclusion_rule_id, person_id, event_id from @temp_database_schema.laolk54sInclusion_0) I;
TRUNCATE TABLE @temp_database_schema.laolk54sInclusion_0;
DROP TABLE @temp_database_schema.laolk54sInclusion_0;
DROP TABLE IF EXISTS @temp_database_schema.laolk54sincluded_events;
CREATE TABLE @temp_database_schema.laolk54sincluded_events
USING DELTA
AS
SELECT
event_id, person_id, start_date, end_date, op_start_date, op_end_date
FROM
(
 SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
 from
 (
 select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
 from @temp_database_schema.laolk54squalified_events Q
 LEFT JOIN @temp_database_schema.laolk54sinclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
 GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
 ) MG -- matching groups
 -- the matching group with all bits set ( POWER(2,# of inclusion rules) - 1 = inclusion_rule_mask
 WHERE (MG.inclusion_rule_mask = POWER(cast(2 as bigint),1)-1)
) Results
WHERE Results.ordinal = 1
;
DROP TABLE IF EXISTS @temp_database_schema.laolk54scohort_rows;
CREATE TABLE @temp_database_schema.laolk54scohort_rows
USING DELTA
AS
SELECT
person_id, start_date, end_date
FROM
( -- first_ends
 select F.person_id, F.start_date, F.end_date
 FROM (
 select I.event_id, I.person_id, I.start_date, CE.end_date, row_number() over (partition by I.person_id, I.event_id order by CE.end_date) as ordinal
 from @temp_database_schema.laolk54sincluded_events I
 join ( -- cohort_ends
-- cohort exit dates
-- By default, cohort exit at the event's op end date
select event_id, person_id, op_end_date as end_date from @temp_database_schema.laolk54sincluded_events
 ) CE on I.event_id = CE.event_id and I.person_id = CE.person_id and CE.end_date >= I.start_date
 ) F
 WHERE F.ordinal = 1
) FE;
DROP TABLE IF EXISTS @temp_database_schema.laolk54sfinal_cohort;
CREATE TABLE @temp_database_schema.laolk54sfinal_cohort
USING DELTA
AS
SELECT
person_id, min(start_date) as start_date, DATEADD(day,-1 * 0, max(end_date)) as end_date
FROM
(
 select person_id, start_date, end_date, sum(is_start) over (partition by person_id order by start_date, is_start desc rows unbounded preceding) group_idx
 from (
 select person_id, start_date, end_date, 
 case when max(end_date) over (partition by person_id order by start_date rows between unbounded preceding and 1 preceding) >= start_date then 0 else 1 end is_start
 from (
 select person_id, start_date, DATEADD(day,0,end_date) as end_date
 from @temp_database_schema.laolk54scohort_rows
 ) CR
 ) ST
) GR
group by person_id, group_idx;
DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM @temp_database_schema.laolk54sfinal_cohort CO
;
TRUNCATE TABLE @temp_database_schema.laolk54scohort_rows;
DROP TABLE @temp_database_schema.laolk54scohort_rows;
TRUNCATE TABLE @temp_database_schema.laolk54sfinal_cohort;
DROP TABLE @temp_database_schema.laolk54sfinal_cohort;
TRUNCATE TABLE @temp_database_schema.laolk54sinclusion_events;
DROP TABLE @temp_database_schema.laolk54sinclusion_events;
TRUNCATE TABLE @temp_database_schema.laolk54squalified_events;
DROP TABLE @temp_database_schema.laolk54squalified_events;
TRUNCATE TABLE @temp_database_schema.laolk54sincluded_events;
DROP TABLE @temp_database_schema.laolk54sincluded_events;
TRUNCATE TABLE @temp_database_schema.laolk54sCodesets;
DROP TABLE @temp_database_schema.laolk54sCodesets;