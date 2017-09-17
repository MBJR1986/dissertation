# Dissertation Code
#Data Processing Script for Logistic Regression model

#Data from HERON has already been structured for processing with
#create_tables.sql script.

#This script creates or converts data into variables for regression analysis.
# This script will also create the final training/testing dataframe PRIOR to data splitting.
# -     i.e. one row per person, with all variables.

#load packages
library(lubridate)
library(sqldf)


#load data
setwd("C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/Dissertation/data/Mark_dissertation_20170718/")
master <- read.csv('master.csv', stringsAsFactors = FALSE)
concussion_dx <- read.csv('concussion_dx_cohort.csv', stringsAsFactors = FALSE)
demo <- read.csv('Mark_dissertation_20170718-patient.csv', stringsAsFactors = FALSE)
enc_loc <- read.csv('encounter_location.csv', stringsAsFactors = FALSE)
med_hx <- read.csv('medical_history.csv', stringsAsFactors = FALSE)
results <- read.csv('results.csv', stringsAsFactors = FALSE)
notes <- read.csv('text_notes.csv', stringsAsFactors = FALSE)

#Change Variable types
#concussion dx table
concussion_dx$start_date <- mdy_hm(concussion_dx$start_date)
concussion_dx$patient_num <- as.factor(concussion_dx$patient_num)
concussion_dx$encounter_num <- as.factor(concussion_dx$encounter_num)
concussion_dx$variable_index <- as.factor(concussion_dx$variable_index)
concussion_dx$encounter_loc <- as.factor(concussion_dx$encounter_loc)

###################
####    LOR     ###
###################

#PRIMARY OUTCOME VARIABLE

#Determine length of a person's recovery, from  the difference between 1st visit and last visit 
#(assuming last visit = return to normal functioning)

#create variable for eval_date and dc_date
min = aggregate(concussion_dx$start_date,by=list(concussion_dx$patient_num),min)
max = aggregate(concussion_dx$start_date,by=list(concussion_dx$patient_num),max)
concussion_dx = merge(concussion_dx,min,by.x=1,by.y=1) #merge min date (eval)
concussion_dx = merge(concussion_dx,max,by.x=1,by.y=1) #merge max date (dc)
colnames(concussion_dx) = c("patient_num", "encounter_num", "start_date", "variable_index", 
                            "variable", "encounter_loc", "eval_date", "dc_date") #adjust column names

#date difference between eval and dc to represent length of treatment/recovery in DAYS (unit)
concussion_dx$LOR <- as.numeric(difftime(concussion_dx$dc_date, concussion_dx$eval_date, units = "days"))
#convert concussion_dx$dc_date to date
concussion_dx$dc_date <- as.Date(concussion_dx$dc_date, "%m/%d/%y %H:%M")

#####################
# historical dxs    #
#####################
med_hx$code_label <- as.factor(med_hx$code_label)
contrasts(med_hx$code_label) #see how glm function will convert to binaries

#####################
#    demographics   #
#####################
demo$vital_status <- as.factor(demo$vital_status)
demo$sex <- as.factor(demo$sex)
demo$language <- as.factor(demo$language)
demo$race <- as.factor(demo$race)
demo$marital_status <- as.factor(demo$marital_status)
demo$religion <- as.factor(demo$religion)

#################################################
#   create log_reg_full dataset for analysis    #
#################################################
log_reg <- as.data.frame(sqldf("select distinct conc.patient_num
                                        ,conc.LOR
                                        ,demo.age
                                        ,demo.sex
                                        ,med.code_label as historical_condition
                                        ,demo.language
                                        ,demo.race
                                        ,demo.marital_status
                                        ,demo.religion
                                        ,enc.encounter_loc
                                    from concussion_dx as conc
                                    left outer join med_hx as med
                                    on conc.patient_num = med.patient_num
                                    left outer join demo 
                                    on conc.patient_num = demo.patient_num
                                    left outer join (
                                            select patient_num
                                                ,MIN(start_date) as first_date
                                                ,encounter_loc
                                            from enc_loc
                                            group by patient_num) as enc
                                    on conc.patient_num = enc.patient_num
                                    order by conc.patient_num"))
#convert log_reg vars to non-factor/character for dummy creation
log_reg$patient_num <- as.numeric(log_reg$patient_num)
log_reg$encounter_loc <- as.factor(log_reg$encounter_loc)
library(caret) #package converts historical_conditions to binary dummy variables
dmys <- dummyVars(" ~ .", data = log_reg, fullRank = T) #still need to join back to patient_num
log_reg_full <- unique(data.frame(predict(dmys, newdata = log_reg))) #still producing multiple rows per person...

#combine rows where multiple conditions are present
library(dplyr)
log_reg_full <- log_reg_full %>% group_by(patient_num) %>% summarise_all(funs(max))
#log_reg_full now has one row per person.

#join in earliest encounter date per row
enc_loc$start_date <- as.Date(enc_loc$start_date, "%m/%d/%y %H:%M")
#enc_loc$start_date <- floor_date(enc_loc$start_date, "day")
results$start_date <- as.Date(results$start_date, "%m/%d/%y %H:%M")
#results$start_date <- floor_date(results$start_date, "day")
log_reg_full <-sqldf("select x.*
                            ,MIN(y.start_date) as start_date
                     from log_reg_full as x
                     inner join enc_loc as y
                     on x.patient_num = y.patient_num
                     group by x.patient_num")

###################
###   RESULTS   ###
###################

# Parse out results by person, type, and value (by person). Ideally, I will need to take the earliest 
# value per person, to represent the evaluation results...

#parse out results that have '@' for valtype variable, as there is no values for rows.
results <- subset(results, subset = (results$valtype != '@'))

#valtype is going to be confusing...
# when eval = concussion score / symptoms: use nval variable
# when eval = ImPACT: use tval (will need to convert text to integer...)

#First, start by parsing out results into descriptive categorical variable

#inner join results table to log_reg_full on patient_num and start_date

subset_res <- sqldf("select res.*
                    from results as res
                    inner join log_reg_full as log
                    on res.patient_num = log.patient_num
                    AND res.start_date = log.start_date") 
#remove duplicate rows
subset_res <- subset(subset_res, subset = (subset_res$variable != '002- #95014789 Composite Score:'))

#write.csv(subset_res, file = 'result_subset.csv')

# TODO: Continue adding categories. START WITH SYMPTOMS
# Interesting and may need to look into this, but there were no patient_nums with symptoms...just total score.
#casting variable paths to an exploded view of 'result type'
results_xpld <- sqldf("select log.patient_num
                        ,res.encounter_num
                        ,res.start_date
                        ,res.tval
                        ,res.nval
                        ,CASE 
                            WHEN variable_path LIKE '%IMPACT COMMENT%' THEN 'IMPACT_COMMENT'
                            WHEN variable_path LIKE '%CONCUSSION IMPACT IMPULSE CONTRO%' THEN 'IMPACT_IMPULSE_CONTROL'
                            WHEN variable_path LIKE '%CONCUSSION IMPACT MEMORY COMPOSI%' THEN 'IMPACT_MEMORY_COMPOSITE'
                            WHEN variable_path LIKE '%CONCUSSION IMPACT REACTION TIME%' THEN 'IMPACT_REACTION_TIME'
                            WHEN variable_path LIKE '%CONCUSSION IMPACT TOTAL SYMPTOM%' THEN 'IMPACT_TOTAL_SYMPTOM'
                            WHEN variable_path LIKE '%CONCUSSION IMPACT VISUAL MOTOR%' THEN 'IMPACT_VISUAL_MOTOR'
                            WHEN variable_path LIKE '%ROW BALANCE ERRORS TOTAL%' THEN 'CONCUSSION_SCORE_TOTAL_BALANCE_ERRORS'
                            WHEN variable_path LIKE '%ROW CONCENTRATION SCORE%' THEN 'CONCUSSION_SCORE_CONCENTRATION_TOTAL'
                            WHEN variable_path LIKE '%ROW DELAYED RECALL SCORE%' THEN 'CONCUSSION_SCORE_DELAYED_RECALL'
                            WHEN variable_path LIKE '%ROW IMMEDIATE MEMORY SCORE%' THEN 'CONCUSSION_SCORE_IMMEDIATE_MEMORY'
                            WHEN variable_path LIKE '%ROW TOTAL COGNITION SCORE%' THEN 'CONCUSSION_SCORE_TOTAL_COGNITION'
                            WHEN variable_path LIKE '%DELAYED RECALL  SCORE%' THEN 'CONCUSSION_SCORE_DELAYED_RECALL'
                            WHEN variable_path LIKE '%SYMPTOM CHECKLIST TOTAL NUMBER OF%' THEN 'CONCUSSION_SYMPTOMS_TOTAL_NUMBER'
                            WHEN variable_path LIKE '%SYMPTOM CHECKLIST TOTAL SCORE%' THEN 'CONCUSSION_SYMPTOMS_TOTAL_SCORE'
                        ELSE 0
                        END result_test
                        FROM subset_res as res
                        INNER JOIN log_reg_full as log
                        ON res.patient_num = log.patient_num")
 #convert result_test to factor
results_xpld$result_test<- as.factor(results_xpld$result_test)
results_xpld$tval <- as.character(results_xpld$tval)
results_xpld$nval <- as.numeric(results_xpld$nval)
#reshape result_test column into multiple columns and display result
res_final <- sqldf("select patient_num
                  ,encounter_num
                   ,start_date
                   ,MAX(CASE WHEN result_test = 'IMPACT_MEMORY_COMPOSITE' THEN tval end) as IMPACT_MEMORY_COMPOSITE
                   ,MAX(CASE WHEN result_test = 'IMPACT_COMMENT' THEN tval end) as IMPACT_COMMENT
                   ,MAX(CASE WHEN result_test = 'IMPACT_IMPULSE_CONTROL' THEN tval end) as IMPACT_IMPULSE_CONTROL
                   ,MAX(CASE WHEN result_test = 'IMPACT_REACTION_TIME' THEN tval end) as IMPACT_REACTION_TIME
                   ,MAX(CASE WHEN result_test = 'IMPACT_TOTAL_SYMPTOM' THEN tval end) as IMPACT_TOTAL_SYMPTOM
                   ,MAX(CASE WHEN result_test = 'IMPACT_VISUAL_MOTOR' THEN tval end) as IMPACT_VISUAL_MOTOR
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SCORE_TOTAL_BALANCE_ERRORS' THEN nval end) as CONCUSSION_SCORE_TOTAL_BALANCE_ERRORS
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SCORE_CONCENTRATION_TOTAL' THEN nval end) as CONCUSSION_SCORE_CONCENTRATION_TOTAL
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SCORE_DELAYED_RECALL' THEN nval END) as CONCUSSION_SCORE_DELAYED_RECALL
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SCORE_IMMEDIATE_MEMORY' THEN nval end) as CONCUSSION_SCORE_IMMEDIATE_MEMORY
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SCORE_TOTAL_COGNITION' THEN nval end) as CONCUSSION_SCORE_TOTAL_COGNITION
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SYMPTOMS_TOTAL_NUMBER' THEN nval end) as CONCUSSION_SYMPTOMS_TOTAL_NUMBER
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SYMPTOMS_TOTAL_SCORE' THEN nval end) as CONCUSSION_SYMPTOMS_TOTAL_SCORE
                    FROM results_xpld
                    group by patient_num
                   order by patient_num") #have to use max function to populate columns for some reason

#test for same # of patients 
print("Do patient counts match for all results subsets?")
length(unique(res_final$patient_num)) == length(unique(subset_res$patient_num))

#clean up soon-to-be numerics
#edited via edit()
#write out res_final and reload for storage purposes
#write.csv(res_final, 'results_final_concussion_pop.csv')
res_final <- read.csv('results_final_concussion_pop.csv')

#convert variables to numeric (ImPACT variables)
res_final$IMPACT_COMMENT <- as.numeric(res_final$IMPACT_COMMENT)
res_final$IMPACT_IMPULSE_CONTROL <- as.numeric(res_final$IMPACT_IMPULSE_CONTROL)
res_final$IMPACT_MEMORY_COMPOSITE <- as.numeric(res_final$IMPACT_MEMORY_COMPOSITE)
res_final$IMPACT_REACTION_TIME <- as.numeric(res_final$IMPACT_REACTION_TIME)
res_final$IMPACT_TOTAL_SYMPTOM <- as.numeric(res_final$IMPACT_TOTAL_SYMPTOM)
res_final$IMPACT_VISUAL_MOTOR <- as.numeric(res_final$IMPACT_VISUAL_MOTOR)


#Join res_final back into log_reg_full table
log_reg_full <- sqldf("select x.*
                      ,x.start_date as eval_date__Date
                      ,y.IMPACT_MEMORY_COMPOSITE
                      ,y.IMPACT_COMMENT
                      ,y.IMPACT_IMPULSE_CONTROL
                      ,y.IMPACT_REACTION_TIME
                      ,y.IMPACT_TOTAL_SYMPTOM
                      ,y.IMPACT_VISUAL_MOTOR
                      ,y.CONCUSSION_SCORE_TOTAL_BALANCE_ERRORS
                      ,y.CONCUSSION_SCORE_CONCENTRATION_TOTAL
                      ,y.CONCUSSION_SCORE_DELAYED_RECALL
                      ,y.CONCUSSION_SCORE_IMMEDIATE_MEMORY
                      ,y.CONCUSSION_SCORE_TOTAL_COGNITION
                      ,y.CONCUSSION_SYMPTOMS_TOTAL_NUMBER
                      ,y.CONCUSSION_SYMPTOMS_TOTAL_SCORE
                      FROM log_reg_full as x
                      left outer join res_final as y
                      ON x.patient_num = y.patient_num
                      order by x.patient_num",method = "name__class") #TODO: start_date keeps getting converted to integer here

#write out file for logistic regression analysis
#write.csv(log_reg_full, "logistic_regression_data_cleaned.csv")


###############
### NOTES   ###
###############
#TODO: Really need to look at these and make sure they are matching up... ptnum 1060 should be included in both and aren't
#   -person in concussion_dx, and definitely has notes...


notes$start_date <- as.Date(notes$start_date, "%m/%d/%y %H:%M") #convert to date for join
notes_patient <- unique(as.character(notes$patient_num)) #generate list of unique patient_num
#join in start_date from log_reg_full to verify start vs injury date (manual review)

min_note_table<- data.frame(matrix(nrow=0, ncol=7)) #create main result table (all fired alerts)
cnames<- c("patient_num", "encounter_num", "start_date", "code_label", "variable", "variable_index", "tval")
colnames(min_note_table)<-cnames

#for loop to extract rows == min date per person
for (i in 1:length(notes_patient)) {
  pt_id <- as.factor(as.character(notes_patient[i]))
  print(pt_id)
  pt_notes <- notes[as.character(notes[,"patient_num"])==pt_id,]
  min <- as.Date(min(pt_notes$start_date))
  min_notes <- as.data.frame(subset(pt_notes, subset = (pt_notes$start_date == min)))
  min_note_table <- merge(min_note_table, min_notes, all = TRUE)
}


#write out notes_eval for manual chart review for dates
#write.csv(min_note_table, "notes_eval_date.csv")

#Discharge notes
max_note_table<- data.frame(matrix(nrow=0, ncol=7)) #create main result table (all fired alerts)
cnames<- c("patient_num", "encounter_num", "start_date", "code_label", "variable", "variable_index", "tval")
colnames(max_note_table)<-cnames

#for loop to extract rows == min date per person
for (i in 1:length(notes_patient)) {
  pt_id <- as.factor(as.character(notes_patient[i]))
  print(pt_id)
  pt_notes <- notes[as.character(notes[,"patient_num"])==pt_id,]
  max <- as.Date(max(pt_notes$start_date))
  max_notes <- as.data.frame(subset(pt_notes, subset = (pt_notes$start_date == max)))
  max_note_table <- merge(max_note_table, max_notes, all = TRUE)
}
#write out file
#write.csv(max_note_table, "notes_dc_date.csv")
