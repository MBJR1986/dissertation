--Author: Mark Burghart

-- SQLite file for creating several tables in dissertation database
-- Tables to create include:
--      -Conditions (historical) --DONE
--      -Conditions (Concussion only) --DONE
--      -Encounter (concussion only) --DONE (basically is concussion table)
--      -Results (concussion only) --DONE
--      -Demographics --DONE (But subset from concussion_dx patient_nums)
--      -Text notes --DONE
--      -Procedures (pt/ot/st,etc) --DONE


--Edits:
--		1. 2017-10-08: After beginning manual note reviews, I see a need to adjust the cohort (potentially).
--			Prior to doing this, I want to review ALL notes from a person's history that may be around a concussion date.
--			Doing so will allow me to see visit progression within a episode of care (ed -> clinic, etc). Well, hopefully...
--			To do this, I need to re-join the text notes table with the patient_num, not the encounter_num. Changes have been made.



-- Rename 'data' table to change variable types to dates/integer
ALTER TABLE 'Mark_dissertation_20170718-data' RENAME TO tmp;

CREATE TABLE master_long(
    patient_num INT
    ,encounter_num INT
    ,valtype TEXT
    ,tval TEXT
    ,nval NUMERIC
    ,units TEXT
    ,code TEXT
    ,modifier TEXT
    ,instance TEXT
    ,start_date NUMERIC
    ,end_date NUMERIC
    ,sourcesystem_cd TEXT
    ,sub_encounter TEXT
    ,variable TEXT
    ,variable_index NUMERIC
    ,code_label TEXT
    ,modifier_label TEXT
);

INSERT INTO master_long(patient_num
                        ,encounter_num
                       ,valtype
                       ,tval
                       ,nval
                       ,units
                       ,code
                       ,modifier
                       ,instance
                       ,start_date
                       ,end_date
                       ,sourcesystem_cd
                       ,sub_encounter
                       ,variable
                       ,variable_index
                       ,code_label
                       ,modifier_label
)
select patient_num
        ,encounter_num
        ,valtype
        ,tval
        ,nval
        ,units
        ,code
        ,modifier
        ,instance
        ,start_date
        ,end_date
        ,sourcesystem_cd
        ,sub_encounter
        ,variable
        ,variable_index
        ,code_label
        ,modifier_label
from tmp;

DROP TABLE IF EXISTS tmp;

--------------------------------------------------------------------
-- Encounter location table (to join to actual encounters if needed)
--------------------------------------------------------------------
DROP TABLE IF EXISTS encounter_location;

CREATE TABLE encounter_location as
SELECT patient_num
    ,encounter_num
    ,start_date
    ,sub_encounter
    ,variable_index
    ,CASE WHEN code LIKE '%VISITDETAIL%' THEN variable END AS encounter_loc
FROM master_long
where encounter_loc IS NOT NULL
ORDER BY patient_num
    ,encounter_num
    ,start_date
;

------------------------------
-- Create concussion_dx table
------------------------------
DROP TABLE IF EXISTS concussion_dx;

CREATE TABLE concussion_dx as
select distinct * from (
select x.*
    ,y.encounter_loc
from(
    select distinct * from (
            select master_long.patient_num
                ,master_long.encounter_num
                ,master_long.start_date
	            ,'Mark_dissertation_20170718-code'.variable_index
                ,'Mark_dissertation_20170718-code'.variable
            from master_long
            left join 'Mark_dissertation_20170718-code'
            on master_long.variable_index = 'Mark_dissertation_20170718-code'.variable_index
            where upper(master_long.variable) LIKE '%CONCUSSION%'
            AND master_long.modifier = 'DiagObs:PRIMARY_DX_YN'
            order by master_long.patient_num
                ,master_long.encounter_num
                ,master_long.start_date
    ) as a
) as x 
left join encounter_location as y
on x.encounter_num = y.encounter_num
)as tmp
--where encounter_loc is not null
;

--------------------------
-- Create results table --
--------------------------
DROP TABLE IF EXISTS results;

CREATE TABLE results as
select x.patient_num
	,x.encounter_num
	,x.start_date
	,x.tval
	,x.nval
	,x.valtype
	,x.variable
	,x.variable_index
	,x.code_label
	,y.variable_path
	,y.code_path
from master_long as x
inner join 'Mark_dissertation_20170718-code' as y
ON x.variable_index = y.variable_index
AND x.code = y.code
where x.code LIKE '%MEAS_ID%'
;

----------------------------------------
-- Create Historical Conditions table --
----------------------------------------
DROP TABLE IF EXISTS med_history;

CREATE TABLE med_history as
SELECT patient_num
    ,encounter_num
    ,start_date
    ,code_label
    ,variable
    ,variable_index
FROM master_long
where modifier_label = 'Medical History diagnosis'
order by patient_num, encounter_num, start_date
;
    
------------------
-- Notes table  --
------------------
-- There appear to be many notes that aren't associated with the encouter_nums
-- from a person's concussion visits. I'll need to innerjoin all notes found
-- with the encounter_nums from concussion_dx;

-- Edit: not inner joining with patient_num, to show visit progression across settings. 

DROP TABLE IF EXISTS text_notes;

CREATE TABLE text_notes as 
SELECT DISTINCT * 
FROM (
        SELECT x.patient_num
            ,x.encounter_num
            ,x.start_date as note_date
			,MIN(y.start_date) as conc_dx_date
            ,x.code_label
            ,x.variable
            ,x.variable_index
            ,x.tval
        FROM master_long as x
        INNER JOIN concussion_dx as y
        ON x.patient_num = y.patient_num 
        --AND x.encounter_num = y.encounter_num
        WHERE x.valtype = 'T'
        AND x.code_label IN ('Ambulatory Procedure Notes (17)', 'Ambulatory Progress Notes (15)', 'Care Plan (600008)', 'Discharge Summaries (5)', 'ED Notes (6)', 'Progress Notes (1)')
        AND x.variable_index != 265
        AND x.tval IS NOT NULL
        AND x.tval != '  '
		AND x.tval != '@'
		group by x.patient_num, x.encounter_num, note_date, x.code_label, x.variable, x.variable_index, x.tval
        order by x.patient_num, note_date
)as tmp
;
--convert note_date and conc_dx_date to date format
ALTER TABLE text_notes RENAME TO tmp;

CREATE TABLE text_notes (
	patient_num INT
	,encounter_num INT
	,note_date DATETIME
	,conc_dx_date DATETIME
	,code_label TEXT
	,variable TEXT
	,variable_index NUM
	,tval TEXT
	);
INSERT INTO text_notes(patient_num, encounter_num, note_date,conc_dx_date,code_label,variable,variable_index,tval)
SELECT patient_num, encounter_num, note_date,conc_dx_date,code_label,variable,variable_index,tval
from tmp
order by patient_num, note_date;
DROP TABLE tmp;
----------------------
-- Procedures table --
----------------------
DROP TABLE IF EXISTS procedures
CREATE TABLE procedures as
SELECT DISTINCT * 
FROM (
        SELECT patient_num
                ,encounter_num
                ,start_date                
                ,code
                ,variable
                ,variable_index
                ,code_label
        FROM master_long
        where code LIKE '%CPT%'
        order by patient_num, encounter_num, start_date
) as tmp
;


