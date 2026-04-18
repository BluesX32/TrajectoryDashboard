DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63Codesets;
CREATE TABLE @temp_database_schema.bxj4cs63Codesets  
USING DELTA
 AS
SELECT
CAST(NULL AS int) AS codeset_id,
	CAST(NULL AS bigint) AS concept_id  WHERE 1 = 0;
INSERT INTO @temp_database_schema.bxj4cs63Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4205455,35205739,1,4,2,443943,138682,45770836,436336,440329,21,24,7,0,5,45590840,4151978,192239,381504,45542548,45556927,10,8,9,35205737,35205738,35205740,35205741,443943,141374,37165237,4221382,4066727,37165216,4080937,1,4299673,2,37110753,4064036,4067067,443943,40175007,7,0,5,37165342,4080929,4063440,4272156,4033204,4033778,4206461,135618,4033777))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4205455,35205739,1,4,2,443943,138682,45770836,436336,440329,21,24,7,0,5,45590840,4151978,192239,381504,45542548,45556927,10,8,9,35205737,35205738,35205740,35205741))
) I
) C UNION ALL 
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (1703687,1703603,1717704,1703687,1703603,1717704))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (1703687,1703603,1717704,1703687,1703603,1717704))
) I
) C UNION ALL 
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4029319,4029319,3401482,35205732,4048187,37169438,3409105,765280,3349573,761784,3270243,761562,3312710,3185127,761563,4141045,3411324,3297633,761366,761365,765220,3662327,765072,3373839,761339,3219561,3377334,761363,761362,3300309,3388062,4296064,3246489,42538551,3401482,4029319,4322568,3265685,3655328,4345802,40479787,764428,3222557,3266478,3414782,45757127,45757066,3425144,3119769,3539436,4092535,3304219,3528336,3580206,3580172,4235380,4172956,3528343,3398373,3528458,3528543,37018291,3294914,44822822,40319594,3145051,3571276,3104782,4113635,37164620,4253626,3231467,4140602,3286830,46273148,44830928,40560009,3175884,373127,44822823,3119773,40384309,37160818,40344469,40389694,3123023,3141448,436904,37160805,3381573,3402379,37018885,40484151,3408179,3409284,765831,3259065,3251427,44835617,440634,4072340,4311484,4218443,3214381,4265441,3409184,441787,40384314,3119778,44832076,44830930,45585942,4265440,3383430,3399584,380324,3119779,44836782,40384316,3320168,3259602,4051338,3119780,40384318,3398576,4312115,37160817,135471,4265859,3276730,606423,3394395,4234499,606425,3267697,4251448,3293655,40795737,37169819,3119782,4087450,3536096,3085890,40276882,44828682,440021,3326463,44823976,1567350,3349790,36715971,36685125,3180166,42537647,3374123,37311936,3280554,37108970,37209449,3387505,3296820,761319,761318,3171880,3180429,761322,761323,3233759,761368,761367,3330756,765905,3244509,37169419,3316880,3353707,380941,3281070,3655659,3655657,443453,37164506,3283765,3261711,3104780,3119774,40384310,40319592,373404,3532317,4092537,3119775,3315751,4165265,3224708,3243433,4294437,4294436,3309207,4298850,3230788,3234954,4300208,4122757,3355652,4298849,3227154,37160069,3528337,3341820,4291597,4235833,3263730,3567564,3580207,3580173,3580174,3580208,4235834,3187665,3172156,4291634,37016140,3371371,37163417,3177111,4300209,4298851,3287082,3291535,4300210,4300211,3245742,760175,3173211,36685194,760174,3246418,3385404,4122758,761768,3254911,761767,3415927,3290732,761769,36685192,36685193,37169420,3308596,40491523,3265812,3563067,3107139,761364,3379603,4306509,3372299,44835619,3239373,436630))
) I
) C UNION ALL 
SELECT 3 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4306655,18,1,14,46273369,2,37016279,255891,365,0,5,4319305,31,4145240,4300204))
) I
) C UNION ALL 
SELECT 4 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4270868,4005037,4306655,80182,4081250,4344161,18,1,2,606434,37395588,606385,365,36674477,0,5,31,1212005,16))
) I
) C UNION ALL 
SELECT 5 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (37016279,4319305,4300204,4324123,4066824,432919,606388,46273369,4055640,4324123,35208699,4324123,4066824,35208699,45562709,45567545,257628,4324123,4066824,35208699,606386,255891,46270384,35208826,35208701,45606214,3321233,45601434,606430,4145240,4343923,35208700,44819941,257628,4344158,4149913,45582126,35208827,45591820))
) I
) C UNION ALL 
SELECT 6 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4270868,4005037,80182,4081250,4344161))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (4270868,4005037,80182,4081250,4344161))
) I
) C UNION ALL 
SELECT 7 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4126439,37397763,4337524,4128222,134442,4331739,441928,4105026,44811612,40352976,4027230))
) I
) C UNION ALL 
SELECT 8 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (314963,35208820,4343935,35208821))
) I
) C UNION ALL 
SELECT 9 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (45548265,45586838,45606052,45543436,45572339,45553046,45591705,45562599,45543443,45562600,45567422,45567423,45586845,725373,45606064,45538639,45606063,45543442,45601289,45572346,45577117,45567425,45577119,45548271,45548270,45567426,80809,4117687,4115161,4116440,4116150,4116151,4117686,4114439,4116441,45591700,45572337,45596437,45538633,45548263,45543435,45582014,45553045,725370,45596438,45548261,45606051,45572338,45548262,45596436,45606050,45596439,45562591,45582015,45567419,45533697,45567418,45543434,45553044,35208750,37160562,45567420,45577104,45572340,45533702,45553051,45533701,45562593,45572341,725372,45577109,45557762,45606055,45557763,45601284,45606053,45606054,45533703,45577105,45577107,45538635,45591701,45596442,45606056,35208753,45586836,45557754,45591686,45572332,45538631,45567415,45591694,45548258,45548257,45567413,45596428,45596427,45572327,4083556,37207809,4035611))
) I
) C UNION ALL 
SELECT 10 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (36716891,37017494,1077506,766408,766409,766411,766410,766402,37110375,37205058,40319772,45548197,46274123,4064048,437082,45548419,45533841,45586969,45601454,45548418,45533840,45553184,45543577,45582150,45567561))
) I
) C UNION ALL 
SELECT 11 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (42535714,4146124,4096220,37166813,4236160,37110370,4137275,37110368,37110369,37167489))
) I
) C UNION ALL 
SELECT 12 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (19014878,19068900,19003999,1361580,42904205,40171288,1305058,1101898,1594587,1310317,1314273,701470,40236987,45892883,746895,1119119,937368,1151789,1593700,40161532,1511348,1186087,1777087))
UNION select c.concept_id
 from @vocabulary_database_schema.CONCEPT c
 join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
 WHERE c.invalid_reason is null
 and (ca.ancestor_concept_id in (19014878,19068900,19003999,1361580,42904205,40171288,1305058,1101898,1594587,1310317,1314273,701470,40236987,45892883,746895,1119119,937368,1151789,1593700,40161532,1511348,1186087,1777087))
) I
) C UNION ALL 
SELECT 13 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
 select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (4,44808679,7,0,5,36,40213255,40213256,21601361,706103,40213260,706104))
) I
) C;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63qualified_events;
CREATE TABLE @temp_database_schema.bxj4cs63qualified_events
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
 select PE.person_id, PE.event_id, PE.start_date, PE.end_date, PE.visit_occurrence_id, PE.sort_date FROM (
-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 0)
) C
-- End Condition Occurrence Criteria
) PE
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM (SELECT Q.person_id, Q.event_id, Q.start_date, Q.end_date, Q.visit_occurrence_id, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
FROM (-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 0)
) C
-- End Condition Occurrence Criteria
) Q
JOIN @cdm_database_schema.OBSERVATION_PERIOD OP on Q.person_id = OP.person_id 
 and OP.observation_period_start_date <= Q.start_date and OP.observation_period_end_date >= Q.start_date
) E
 INNER JOIN
 (
 -- Begin Correlated Criteria
select 0 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM (SELECT Q.person_id, Q.event_id, Q.start_date, Q.end_date, Q.visit_occurrence_id, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
FROM (-- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 0)
) C
-- End Condition Occurrence Criteria
) Q
JOIN @cdm_database_schema.OBSERVATION_PERIOD OP on Q.person_id = OP.person_id 
 and OP.observation_period_start_date <= Q.start_date and OP.observation_period_end_date >= Q.start_date
) P
JOIN (
 -- Begin Drug Exposure Criteria
select C.person_id, C.drug_exposure_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id,C.start_date as sort_date
from 
(
 select de.person_id,de.drug_exposure_id,de.drug_concept_id,de.visit_occurrence_id,days_supply,quantity,refills,de.drug_exposure_start_date as start_date, COALESCE(de.drug_exposure_end_date, DATEADD(day,de.days_supply,de.drug_exposure_start_date), DATEADD(day,1,de.drug_exposure_start_date)) as end_date 
 FROM @cdm_database_schema.DRUG_EXPOSURE de
JOIN @temp_database_schema.bxj4cs63Codesets cs on (de.drug_concept_id = cs.concept_id and cs.codeset_id = 1)
) C
-- End Drug Exposure Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,0,P.START_DATE) AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
 ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
 GROUP BY E.person_id, E.event_id
 HAVING COUNT(index_id) = 1
) G
-- End Criteria Group
) AC on AC.person_id = pe.person_id and AC.event_id = pe.event_id
 ) E
 JOIN @cdm_database_schema.observation_period OP on E.person_id = OP.person_id and E.start_date >= OP.observation_period_start_date and E.start_date <= op.observation_period_end_date
 WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE
) P
-- End Primary Events
) pe
) QE
;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63Inclusion_0;
CREATE TABLE @temp_database_schema.bxj4cs63Inclusion_0
USING DELTA
AS
SELECT
0 as inclusion_rule_id, person_id, event_id
FROM
(
 select pe.person_id, pe.event_id
 FROM @temp_database_schema.bxj4cs63qualified_events pe
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM @temp_database_schema.bxj4cs63qualified_events E
 INNER JOIN
 (
 -- Begin Demographic Criteria
SELECT 0 as index_id, e.person_id, e.event_id
FROM @temp_database_schema.bxj4cs63qualified_events E
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
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63Inclusion_1;
CREATE TABLE @temp_database_schema.bxj4cs63Inclusion_1
USING DELTA
AS
SELECT
1 as inclusion_rule_id, person_id, event_id
FROM
(
 select pe.person_id, pe.event_id
 FROM @temp_database_schema.bxj4cs63qualified_events pe
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM @temp_database_schema.bxj4cs63qualified_events E
 INNER JOIN
 (
 -- Begin Correlated Criteria
select 0 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 5)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (44777791,38004491,38003882)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 1 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 6)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (44777791,38004491,38003882)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 2 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 7)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (38003882,44777791,38004491)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 3 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 8)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (38003882,44777791,38004491)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 4 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 9)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (38003882,44777791,38004491)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 5 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 10)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (38003882,44777791,38004491)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
UNION ALL
-- Begin Correlated Criteria
select 6 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
 SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.provider_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
 FROM @cdm_database_schema.CONDITION_OCCURRENCE co
 JOIN @temp_database_schema.bxj4cs63Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 11)
) C
LEFT JOIN @cdm_database_schema.PROVIDER PR on C.provider_id = PR.provider_id
WHERE PR.specialty_concept_id in (38003882,44777791,38004491)
-- End Condition Occurrence Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
 ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
 GROUP BY E.person_id, E.event_id
 HAVING COUNT(index_id) >= 1
) G
-- End Criteria Group
) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
) Results
;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63Inclusion_2;
CREATE TABLE @temp_database_schema.bxj4cs63Inclusion_2
USING DELTA
AS
SELECT
2 as inclusion_rule_id, person_id, event_id
FROM
(
 select pe.person_id, pe.event_id
 FROM @temp_database_schema.bxj4cs63qualified_events pe
JOIN (
-- Begin Criteria Group
select 0 as index_id, person_id, event_id
FROM
(
 select E.person_id, E.event_id 
 FROM @temp_database_schema.bxj4cs63qualified_events E
 INNER JOIN
 (
 -- Begin Correlated Criteria
select 0 as index_id, cc.person_id, cc.event_id
from (SELECT p.person_id, p.event_id 
FROM @temp_database_schema.bxj4cs63qualified_events P
JOIN (
 -- Begin Drug Exposure Criteria
select C.person_id, C.drug_exposure_id as event_id, C.start_date, C.end_date,
 C.visit_occurrence_id,C.start_date as sort_date
from 
(
 select de.person_id,de.drug_exposure_id,de.drug_concept_id,de.visit_occurrence_id,days_supply,quantity,refills,de.drug_exposure_start_date as start_date, COALESCE(de.drug_exposure_end_date, DATEADD(day,de.days_supply,de.drug_exposure_start_date), DATEADD(day,1,de.drug_exposure_start_date)) as end_date 
 FROM @cdm_database_schema.DRUG_EXPOSURE de
JOIN @temp_database_schema.bxj4cs63Codesets cs on (de.drug_concept_id = cs.concept_id and cs.codeset_id = 12)
) C
-- End Drug Exposure Criteria
) A on A.person_id = P.person_id AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE ) cc 
GROUP BY cc.person_id, cc.event_id
HAVING COUNT(cc.event_id) >= 1
-- End Correlated Criteria
 ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
 GROUP BY E.person_id, E.event_id
 HAVING COUNT(index_id) = 1
) G
-- End Criteria Group
) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
) Results
;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63inclusion_events;
CREATE TABLE @temp_database_schema.bxj4cs63inclusion_events
USING DELTA
AS
SELECT
inclusion_rule_id, person_id, event_id
FROM
(select inclusion_rule_id, person_id, event_id from @temp_database_schema.bxj4cs63Inclusion_0
UNION ALL
select inclusion_rule_id, person_id, event_id from @temp_database_schema.bxj4cs63Inclusion_1
UNION ALL
select inclusion_rule_id, person_id, event_id from @temp_database_schema.bxj4cs63Inclusion_2) I;
TRUNCATE TABLE @temp_database_schema.bxj4cs63Inclusion_0;
DROP TABLE @temp_database_schema.bxj4cs63Inclusion_0;
TRUNCATE TABLE @temp_database_schema.bxj4cs63Inclusion_1;
DROP TABLE @temp_database_schema.bxj4cs63Inclusion_1;
TRUNCATE TABLE @temp_database_schema.bxj4cs63Inclusion_2;
DROP TABLE @temp_database_schema.bxj4cs63Inclusion_2;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63included_events;
CREATE TABLE @temp_database_schema.bxj4cs63included_events
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
 from @temp_database_schema.bxj4cs63qualified_events Q
 LEFT JOIN @temp_database_schema.bxj4cs63inclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
 GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
 ) MG -- matching groups
 -- the matching group with all bits set ( POWER(2,# of inclusion rules) - 1 = inclusion_rule_mask
 WHERE (MG.inclusion_rule_mask = POWER(cast(2 as bigint),3)-1)
) Results
;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63cohort_rows;
CREATE TABLE @temp_database_schema.bxj4cs63cohort_rows
USING DELTA
AS
SELECT
person_id, start_date, end_date
FROM
( -- first_ends
 select F.person_id, F.start_date, F.end_date
 FROM (
 select I.event_id, I.person_id, I.start_date, CE.end_date, row_number() over (partition by I.person_id, I.event_id order by CE.end_date) as ordinal
 from @temp_database_schema.bxj4cs63included_events I
 join ( -- cohort_ends
-- cohort exit dates
-- By default, cohort exit at the event's op end date
select event_id, person_id, op_end_date as end_date from @temp_database_schema.bxj4cs63included_events
 ) CE on I.event_id = CE.event_id and I.person_id = CE.person_id and CE.end_date >= I.start_date
 ) F
 WHERE F.ordinal = 1
) FE;
DROP TABLE IF EXISTS @temp_database_schema.bxj4cs63final_cohort;
CREATE TABLE @temp_database_schema.bxj4cs63final_cohort
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
 from @temp_database_schema.bxj4cs63cohort_rows
 ) CR
 ) ST
) GR
group by person_id, group_idx;
DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM @temp_database_schema.bxj4cs63final_cohort CO
;
TRUNCATE TABLE @temp_database_schema.bxj4cs63cohort_rows;
DROP TABLE @temp_database_schema.bxj4cs63cohort_rows;
TRUNCATE TABLE @temp_database_schema.bxj4cs63final_cohort;
DROP TABLE @temp_database_schema.bxj4cs63final_cohort;
TRUNCATE TABLE @temp_database_schema.bxj4cs63inclusion_events;
DROP TABLE @temp_database_schema.bxj4cs63inclusion_events;
TRUNCATE TABLE @temp_database_schema.bxj4cs63qualified_events;
DROP TABLE @temp_database_schema.bxj4cs63qualified_events;
TRUNCATE TABLE @temp_database_schema.bxj4cs63included_events;
DROP TABLE @temp_database_schema.bxj4cs63included_events;
TRUNCATE TABLE @temp_database_schema.bxj4cs63Codesets;
DROP TABLE @temp_database_schema.bxj4cs63Codesets;