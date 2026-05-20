DROP TABLE IF EXISTS @temp_database_schema.focv1wh3Codesets;
CREATE TABLE @temp_database_schema.focv1wh3Codesets  
USING DELTA
 AS
SELECT
CAST(NULL AS int) AS codeset_id,
	CAST(NULL AS bigint) AS concept_id  WHERE 1 = 0;
INSERT INTO @temp_database_schema.focv1wh3Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (438350))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (438350))
) I
) C UNION ALL 
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (1705674,21602929,1751310,1730370,997881,1711759,1781733))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (1705674,21602929,1751310,1730370,997881,1711759,1781733))
) I
) C UNION ALL 
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4187217,3048512,37071933,3009595))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4187217,3048512,37071933,3009595))
) I
) C UNION ALL 
SELECT 3 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (2314027))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (2314027))
) I
) C UNION ALL 
SELECT 4 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (9201))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (9201))
) I
) C UNION ALL 
SELECT 5 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4305666,313223,4344493,606328,4218161))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4305666,313223,4344493,606328))
) I
) C UNION ALL 
SELECT 6 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (314963,4347064,4343935))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (314963,4347064,4343935))
) I
) C UNION ALL 
SELECT 7 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (36716891,37017494,37110375,37205058,40319772))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (36716891,37017494,37110375,40319772))
) I
) C UNION ALL 
SELECT 8 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (80809,4083556,4035611))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (80809,4083556,4035611))
) I
) C UNION ALL 
SELECT 9 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (37016279,4319305,4300204,4324123,4066824,432919,606388,46273369,4055640,4324123,4324123,4066824,257628,4324123,4066824,606386,255891,46270384,606430,4145240,4343923,257628,4344158,4149913))
) I
) C UNION ALL 
SELECT 10 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4270868,4005037,80182,4081250,4344161))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4270868,4005037,80182,4081250,4344161))
) I
) C UNION ALL 
SELECT 11 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4126439,37397763,4337524,4128222,134442,4331739,441928,4105026,44811612,40352976,4027230))
) I
) C UNION ALL 
SELECT 12 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (1711759,1705674,1836430,1781733,1730370))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (1711759,1705674,1836430,1781733))
) I
) C;
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3qualified_events;
CREATE TABLE @temp_database_schema.focv1wh3qualified_events
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
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 3)
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
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 12)
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
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3Inclusion_0;
CREATE TABLE @temp_database_schema.focv1wh3Inclusion_0
USING DELTA
AS
SELECT
0 as inclusion_rule_id, person_id, event_id
FROM
(
 select pe.person_id, pe.event_id
 FROM @temp_database_schema.focv1wh3qualified_events pe
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM @temp_database_schema.focv1wh3qualified_events E
 INNER JOIN
 (
 -- Begin Demographic Criteria
SELECT 0 as index_id, e.person_id, e.event_id
FROM @temp_database_schema.focv1wh3qualified_events E
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
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3Inclusion_1;
CREATE TABLE @temp_database_schema.focv1wh3Inclusion_1
USING DELTA
AS
SELECT
1 as inclusion_rule_id, person_id, event_id
FROM
(
 select pe.person_id, pe.event_id
 FROM @temp_database_schema.focv1wh3qualified_events pe
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM @temp_database_schema.focv1wh3qualified_events E
 INNER JOIN
 (
 -- Begin Correlated Criteria
select 0 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 5)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 1 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 6)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 2 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 7)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 3 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 8)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 4 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 9)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 5 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 10)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 6 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.focv1wh3qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.focv1wh3Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 11)
) C
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
 ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
 GROUP BY E.person_id, E.event_id
 HAVING COUNT(index_id) > 0
) G
-- End Criteria Group
) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
) Results
;
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3inclusion_events;
CREATE TABLE @temp_database_schema.focv1wh3inclusion_events
USING DELTA
AS
SELECT
inclusion_rule_id, person_id, event_id
FROM
(select inclusion_rule_id, person_id, event_id from @temp_database_schema.focv1wh3Inclusion_0
UNION ALL
select inclusion_rule_id, person_id, event_id from @temp_database_schema.focv1wh3Inclusion_1) I;
TRUNCATE TABLE @temp_database_schema.focv1wh3Inclusion_0;
DROP TABLE @temp_database_schema.focv1wh3Inclusion_0;
TRUNCATE TABLE @temp_database_schema.focv1wh3Inclusion_1;
DROP TABLE @temp_database_schema.focv1wh3Inclusion_1;
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3included_events;
CREATE TABLE @temp_database_schema.focv1wh3included_events
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
 from @temp_database_schema.focv1wh3qualified_events Q
 LEFT JOIN @temp_database_schema.focv1wh3inclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
 GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
 ) MG -- matching groups
 -- the matching group with all bits set ( POWER(2,# of inclusion rules) - 1 = inclusion_rule_mask
 WHERE (MG.inclusion_rule_mask = POWER(cast(2 as bigint),2)-1)
) Results
WHERE Results.ordinal = 1
;
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3cohort_rows;
CREATE TABLE @temp_database_schema.focv1wh3cohort_rows
USING DELTA
AS
SELECT
person_id, start_date, end_date
FROM
( -- first_ends
 select F.person_id, F.start_date, F.end_date
 FROM (
 select I.event_id, I.person_id, I.start_date, CE.end_date, row_number() over (partition by I.person_id, I.event_id order by CE.end_date) as ordinal
 from @temp_database_schema.focv1wh3included_events I
 join ( -- cohort_ends
-- cohort exit dates
-- By default, cohort exit at the event's op end date
select event_id, person_id, op_end_date as end_date from @temp_database_schema.focv1wh3included_events
 ) CE on I.event_id = CE.event_id and I.person_id = CE.person_id and CE.end_date >= I.start_date
 ) F
 WHERE F.ordinal = 1
) FE;
DROP TABLE IF EXISTS @temp_database_schema.focv1wh3final_cohort;
CREATE TABLE @temp_database_schema.focv1wh3final_cohort
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
 from @temp_database_schema.focv1wh3cohort_rows
 ) CR
 ) ST
) GR
group by person_id, group_idx;
DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM @temp_database_schema.focv1wh3final_cohort CO
;
TRUNCATE TABLE @temp_database_schema.focv1wh3cohort_rows;
DROP TABLE @temp_database_schema.focv1wh3cohort_rows;
TRUNCATE TABLE @temp_database_schema.focv1wh3final_cohort;
DROP TABLE @temp_database_schema.focv1wh3final_cohort;
TRUNCATE TABLE @temp_database_schema.focv1wh3inclusion_events;
DROP TABLE @temp_database_schema.focv1wh3inclusion_events;
TRUNCATE TABLE @temp_database_schema.focv1wh3qualified_events;
DROP TABLE @temp_database_schema.focv1wh3qualified_events;
TRUNCATE TABLE @temp_database_schema.focv1wh3included_events;
DROP TABLE @temp_database_schema.focv1wh3included_events;
TRUNCATE TABLE @temp_database_schema.focv1wh3Codesets;
DROP TABLE @temp_database_schema.focv1wh3Codesets;