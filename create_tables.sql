-- SQLite file for creating several tables in dissertation database
-- Tables to create include:
--      -Conditions (historical) --DONE
--      -Conditions (Concussion only) --DONE
--      -Encounter (concussion only) --DONE (basically is concussion table)
--      -Results (concussion only) --DONE
--      -Demographics --DONE (But subset from concussion_dx patient_nums)
--      -Text notes
--      -Procedures (pt/ot/st,etc)

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

DROP TABLE IF EXISTS text_notes;

CREATE TABLE text_notes as 
SELECT DISTINCT * 
FROM (
        SELECT x.patient_num
            ,x.encounter_num
            ,x.start_date
            ,x.code_label
            ,x.variable
            ,x.variable_index
            ,x.tval
        FROM master_long as x
        INNER JOIN concussion_dx as y
        ON x.patient_num = y.patient_num 
        AND x.encounter_num = y.encounter_num
        WHERE x.valtype = 'T'
        AND x.code_label IN ('Ambulatory Procedure Notes (17)', 'Ambulatory                       Progress Notes (15)', 'Care Plan (600008)',                             'Discharge Summaries (5)', 'ED Notes (6)',                              'Progress Notes (1)')
        AND x.variable_index != 265
        AND x.tval IS NOT NULL
        AND x.tval != '  '
        order by x.patient_num, x.encounter_num, x.start_date
)as tmp
;