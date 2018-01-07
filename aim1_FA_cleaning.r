# Dissertation Code

#Aim 1: Factor analysis for Long Treatment time
# Research Question: Which factors influence long tx time?
#Data Processing Script 

#Data from HERON has already been structured for processing with
#create_tables.sql script.

#This script creates or converts data into variables for mixed variable factor analysis.
# This script will also create the final training/testing dataframe PRIOR to data splitting.
# -     i.e. one row per person, with all variables.

#General Notes:
#   - Data will include only Concussion Clinic Patients
#   - Subjects will not need to verify injury date (doing so reduces data from 500 to 150 people)
#   - Subject to Variable Ratio = 10 to 1
#   - Likely need to reduce number of variables down to 50
#   - No nominal variables can be included (i.e. Race/Ethnicity) without dummifying
#   - May also need to bin some variables (i.e. # of historical neurological disorders vs. explicit dx)

#load packages
library(lubridate)
library(sqldf)
library(ggplot2)

#load data
setwd("C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/Dissertation/data/Mark_dissertation_20170718/")
master <- read.csv('master.csv', stringsAsFactors = FALSE)
concussion_dx <- read.csv('concussion_dx_cohort.csv', stringsAsFactors = FALSE)
demo <- read.csv('Mark_dissertation_20170718-patient.csv', stringsAsFactors = FALSE)
enc_loc <- read.csv('encounter_location.csv', stringsAsFactors = FALSE)
med_hx <- read.csv('medical_history.csv', stringsAsFactors = FALSE)
results <- read.csv('results.csv', stringsAsFactors = FALSE)
notes <- read.csv('text_notes.csv', stringsAsFactors = FALSE)
notes_clean <- read.csv('cc_text_notes.csv', stringsAsFactors = FALSE) #concussion clinic notes, contains concussion dx date, and manual injury date

################
### TODO    ####
################
# 1. Dummify demographic variables




### (1). Parse out Sports Medicine Clinic Patients (with concussion dx) and length of treatment window (i.e. dates)
# pull out rows where concussion_dx$encounter_loc == ICC SPORTS MEDICINE CL, ZZARW SPORTS REHAB CL, ARW SPORTS MEDICINE CL, 
#                                             ICC SPN REHAB MED CL

#Change Variable types
#concussion dx table
concussion_dx$start_date <- as.Date(concussion_dx$start_date, format = '%m/%d/%y %H:%M')
concussion_dx$patient_num <- as.factor(concussion_dx$patient_num)
concussion_dx$encounter_num <- as.factor(concussion_dx$encounter_num)
concussion_dx$variable_index <- as.factor(concussion_dx$variable_index)
concussion_dx$encounter_loc <- as.factor(concussion_dx$encounter_loc)

sports_med <- sqldf("select *
                   from concussion_dx
                   where encounter_loc IN ('ICC SPORTS MEDICINE CL', 'ZZARW SPORTS REHAB CL', 
                                            'ARW SPORTS MEDICINE CL', 'ICC SPN REHAB MED CL')
                    order by patient_num, start_date") #9633 rows, 665 people


### (2). Calculate length of tx for each person 


# calculate max date as last_visit_dt, min date as eval_dt, diff = length_of_treat
temp <- sqldf("select distinct patient_num
                    ,MAX(start_date) as last_visit_dt__Date
                    ,MIN(start_date) as eval_dt__Date
                    from sports_med
                    group by patient_num", method = "name__class")
temp$length_of_treat <- temp$last_visit_dt - temp$eval_dt

#change variable types
temp$patient_num <- as.factor(temp$patient_num)
temp$length_of_treat <- as.numeric(temp$length_of_treat)

#inner join temp to sports_med (removing unnecessary columns) and drop temp from memory
sports_med <- sqldf("select distinct * from (
                      select sm.patient_num
                    ,sm.encounter_num
                    ,sm.start_date
                    ,sm.encounter_loc
                    ,t.last_visit_dt
                    ,t.eval_dt
                    ,t.length_of_treat
                    from sports_med as sm
                    inner join temp as t
                    on sm.patient_num = t.patient_num
                    ) as tmp") # each row isn't unique person. Left encounters and visit dates on here still for checking.
rm(temp) #removes temp from memory (plus, recalculated below)
#summary of LOT
summary(sports_med$length_of_treat) #range 0 - 776, median 22, mean 63.91

######plot distribution of LOT
#create df where each row = 1 person
plot <- sqldf("select distinct patient_num, MAX(length_of_treat) as LOT
              from sports_med
              group by patient_num")
lot_dist = ggplot(data = plot, aes(x = LOT)) # data & aesthetics
lot_dist = lot_dist + geom_histogram()# add histogram
lot_dist + xlab("Length Of Treatment (days)") +
  ylab("Patient Count (n=665)") +
  ggtitle("Length of treatment for Concussion Clinic patients")


###(3). Remove encounters with a gap > 100 days (group by person) (this is needed to reduce skewing of LOT)


#order data
sports_med <- sports_med[order(sports_med$patient_num,sports_med$start_date),]
#create variable representing time between visits (days)
sports_med$visit_gap <- do.call(c,by(sports_med$start_date,sports_med$patient_num,function(x) c(NA,diff(x))))
#convert NAs to 0 (i.e. Keep in dataframe)
sports_med$visit_gap[is.na(sports_med$visit_gap)] <- 0
#create new variable (flag) to represent row where visit_gap >= 180 days (6 months)
sports_med$flag <- ifelse(sports_med$visit_gap >= 180, 1, 0)

#filter each person iteratively by the first occurence of the flag by rows (i.e. See flag, drop rest of rows per person)
library(dplyr)
sports_med <- group_by(sports_med, patient_num) %>%
  mutate(first2 = min(which(flag == 1 | row_number() == n()))) %>%
  filter(row_number() <= first2) %>%
  select(-first2)
#still has flag, now remove row where flag == 1 and done!
sports_med <- filter(sports_med, flag != 1)




#lets try plotting again to see how distribution changes
#need to recalc length_of_treat variable without extra rows per person
temp <- sqldf("select distinct patient_num
                    ,MAX(start_date) as last_visit_dt__Date
              ,MIN(start_date) as eval_dt__Date
              from sports_med
              group by patient_num", method = "name__class")
temp$length_of_treat <- temp$last_visit_dt - temp$eval_dt

#change variable types
temp$patient_num <- as.factor(temp$patient_num)
temp$length_of_treat <- as.numeric(temp$length_of_treat)

#inner join temp to sports_med (removing unnecessary columns) and drop temp from memory
sports_med <- sqldf("select distinct * from (
                    select sm.patient_num
                    ,sm.encounter_num
                    ,sm.start_date
                    ,sm.encounter_loc
                    ,t.last_visit_dt
                    ,t.eval_dt
                    ,t.length_of_treat
                    from sports_med as sm
                    inner join temp as t
                    on sm.patient_num = t.patient_num
) as tmp")
rm(temp)

summary(sports_med$length_of_treat) #median = 20.5, mean = 38.7, range 0-381 (MUCH better)
#plot
#create df
plot_2 <- sqldf("select distinct patient_num, MAX(length_of_treat) as LOT
              from sports_med
              group by patient_num")
lot_dist = ggplot(data = plot_2, aes(x = LOT)) # data & aesthetics
lot_dist = lot_dist + geom_histogram()# add histogram
lot_dist + xlab("Length Of Treatment (days)") +
  ylab("Patient Count (n = 665)") +
  ggtitle("Length of treatment for Concussion Clinic patients (outliers removed)")


#general summary of population breakdown: Count of patients with treatment >= 28 days: 148 (22%)
#                                         Count of patients with treatment <= 28 days: 525 (78%)
#                                         Count of patients with Tx >= 28 days and age >= 18: 47
#                                         Count of patients with Tx >= 28 days and age >= 14: 138 (age cutoff = 14)


#################################
#
### Cleaning other variables  ###
#
#################################

#####################
# historical dxs    #
#####################
#subset med_hx with just patient_nums in sports_med table
med_hx <- sqldf("select distinct * from (
                select x.*
                from med_hx as x
                inner join sports_med as y
                on x.patient_num = y.patient_num) as tmp")
med_hx$code_label <- as.factor(med_hx$code_label)
contrasts(med_hx$code_label) #see how glm function will convert to binaries

#####################
#    demographics   #
#####################
#all these need to be dummified.... ALL NOMINAL AND CAN"T HAVE THIS

#subset demo with patient_nums in sports_med
demo <- sqldf("select distinct * from (
                select x.*
                from demo as x
                inner join sports_med as y
                on x.patient_num = y.patient_num) as tmp")

demo$vital_status <- as.factor(demo$vital_status)
demo$sex <- as.factor(demo$sex)
demo$language <- as.factor(demo$language)
demo$race <- as.factor(demo$race)
demo$marital_status <- as.factor(demo$marital_status)
demo$religion <- as.factor(demo$religion)
#plot age
age_dist = ggplot(data = demo, aes(x = age)) # data & aesthetics
age_dist = age_dist + geom_histogram()# add histogram
age_dist + xlab("Age (yrs)") +
  ylab("Patient Count (n = 665)") +
  ggtitle("Age Distribution for Concussion Clinic patients (outliers removed)")



###################
###   RESULTS   ###
###################
# Parse out results by person, type, and value (by person). Ideally, I will need to take the earliest 
# value per person, to represent the evaluation results...

#subset by patient_num in sports_med
results <- sqldf("select distinct * from (
                select x.*
                 from results as x
                 inner join sports_med as y
                 on x.patient_num = y.patient_num) as tmp")

#parse out results that have '@' for valtype variable, as there is no values for rows.
results <- subset(results, subset = (results$valtype != '@'))

#valtype is going to be confusing...
# when eval = concussion score / symptoms: use nval variable
# when eval = ImPACT: use tval (will need to convert text to integer...

#First, start by parsing out results into descriptive categorical variable
#convert results$start_date & log_reg_full$start_date to date:
results$start_date <- as.Date(mdy_hm(results$start_date))
#remove duplicate rows
results <- subset(results, subset = (results$variable != '002- #95014789 Composite Score:'))

#casting variable paths to an exploded view of 'result type'
results_xpld <- sqldf("select log.patient_num
                      ,res.encounter_num
                      ,res.start_date
                      ,res.tval
                      ,res.nval
                      ,CASE 
                      WHEN variable_path LIKE '%IMPACT COMMENT%' THEN 'IMPACT_COMMENT'
                      WHEN variable_path LIKE '%CONCUSSION IMPACT IMPULSE CONTRO%' THEN 'IMPACT_IMPULSE_CONTROL'
                      WHEN variable LIKE '%Memory Composite (Verbal)%' THEN 'IMPACT_VERBAL_MEMORY'
                      WHEN variable LIKE '%Memory Composite (Visual)%' THEN 'IMPACT_VISUAL_MEMORY'
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
                      FROM results as res
                      INNER JOIN sports_med as log
                      ON res.patient_num = log.patient_num")
#results_xpld lengthens dataframe into longer form, but less columns. Creates result_test as a factor
#convert result_test to factor
results_xpld$result_test<- as.factor(results_xpld$result_test)
results_xpld$tval <- as.character(results_xpld$tval)
results_xpld$nval <- as.numeric(results_xpld$nval)

#reshape result_test column into multiple columns and display result
res_final <- sqldf("select patient_num
                   ,encounter_num
                   ,start_date
                   ,MAX(CASE WHEN result_test = 'IMPACT_VERBAL_MEMORY' THEN tval end) as impact_verbal_memory
                    ,MAX(CASE WHEN result_test = 'IMPACT_VISUAL_MEMORY' THEN tval end) as impact_visual_memory
                   ,MAX(CASE WHEN result_test = 'IMPACT_COMMENT' THEN tval end) as impact_comment
                   ,MAX(CASE WHEN result_test = 'IMPACT_IMPULSE_CONTROL' THEN tval end) as impact_impulse_control
                   ,MAX(CASE WHEN result_test = 'IMPACT_REACTION_TIME' THEN tval end) as impact_reaction_time
                   ,MAX(CASE WHEN result_test = 'IMPACT_TOTAL_SYMPTOM' THEN tval end) as impact_total_symptom
                   ,MAX(CASE WHEN result_test = 'IMPACT_VISUAL_MOTOR' THEN tval end) as impact_visual_motor
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SYMPTOMS_TOTAL_NUMBER' THEN nval end) as concussion_symptoms_total_count
                   ,MAX(CASE WHEN result_test = 'CONCUSSION_SYMPTOMS_TOTAL_SCORE' THEN nval end) as concussion_symptoms_total_severity
                   FROM results_xpld
                   group by patient_num
                   order by patient_num") #have to use max function to populate columns for some reason

###Need to clean up some columns in res_final before loading into df_fa
#IMPACT VERBAL MEMORY COMPOSITE
res_final$impact_verbal_memory <- substr(res_final$impact_verbal_memory, 0, 2) #slice at two digits
res_final$impact_verbal_memory <- gsub("<", "", paste(res_final$impact_verbal_memory)) #remove < symbol
res_final$impact_verbal_memory <- as.numeric(res_final$impact_verbal_memory, na.rm = TRUE) #convert to number, with NAs

#IMPACT VISUAL MEMORY COMPOSITE
res_final$impact_visual_memory <- substr(res_final$impact_visual_memory, 0, 2) #slice at two digits
res_final$impact_visual_memory <- gsub("<", "", paste(res_final$impact_visual_memory)) #remove < symbol
res_final$impact_visual_memory <- as.numeric(res_final$impact_visual_memory, na.rm = TRUE) 

#IMPACT COMMENT
res_final$impact_comment <- as.numeric(gsub("CEI", "", paste(res_final$impact_comment)), na.rm = TRUE) #remove CEI, convert to int

#IMPACT IMPULSE CONTROL
res_final$impact_impulse_control <- as.numeric(res_final$impact_impulse_control, na.rm = TRUE)

#IMPACT REACTION TIME
res_final$impact_reaction_time[res_final$impact_reaction_time > 1.5] <- NA #chop values > 1.5
res_final$impact_reaction_time <- substr(res_final$impact_reaction_time, 0, 4) #slice at 4 digits
res_final$impact_reaction_time <- as.numeric(res_final$impact_reaction_time, na.rm = TRUE) 

#IMPACT_VISUAL_MOTOR
res_final$impact_visual_motor <- gsub("<", "", paste(res_final$impact_visual_motor))
res_final$impact_visual_motor <- as.numeric(res_final$impact_visual_motor, na.rm = TRUE)

#ImPact total symptom to numeric
res_final$impact_total_symptom <- as.numeric(res_final$impact_total_symptom)

#########################
### Create DF for FA  ###
#########################

#create dummy vars/binning for df_fa (1 = TRUE, 0 = FALSE)
#need to collapse several variables into dichotomous bins, or counts (i.e. historical diagnoses)
#may need to move this above code block at line 225

#sex (1 = TRUE (male))
demo$sex_male <- ifelse(demo$sex == "m", 1, 0)
#religion (1 = TRUE (religious))
demo$religious <- ifelse(demo$religion == "none" | demo$religion == "unknown", 0,1)
#race (1 = White)
demo$race_white <- ifelse(demo$race == "white", 1, 0)

#historical concussion (1 = yes)
#subset with patient_nums in sports_med
med_hx <- sqldf("select x.* from med_hx as x
                inner join sports_med as y
                on x.patient_num = y.patient_num")
med_hx$concussion_hxdx <- ifelse(med_hx$code_label == "Concussion", 1, 0)

#count of neurological diagnoses (excluding concussion)
#create new df with one row per person/dx
med_hx_temp <- sqldf('select distinct * from (
                     select patient_num
                     ,code_label
                     from med_hx
                     ) as tmp')
med_hx_count <- sqldf('select distinct patient_num
                      ,COUNT(code_label) as historical_dx_count
                      from med_hx_temp
                      where code_label != "Concussion"
                      group by patient_num') #Final DF, count represent all dxs NOT INCLUDING concussion





#Create One Master Dataframe from all objects for analysis
df_fa <- as.data.frame(sqldf("select distinct conc.patient_num
                              ,conc.length_of_treat
                             ,demo.age
                             ,demo.sex_male
                             ,demo.race_white
                             ,demo.religious
                              ,MAX(med.concussion_hxdx) as concussion_hxdx
                              ,MAX(med_hx.historical_dx_count) as historical_dx_count
                              ,res.impact_verbal_memory
                              ,res.impact_visual_memory
                              ,res.impact_comment
                              ,res.impact_impulse_control
                              ,res.impact_reaction_time
                              ,res.impact_total_symptom
                              ,res.impact_visual_motor
                              ,res.concussion_symptoms_total_count
                              ,res.concussion_symptoms_total_severity
                             from sports_med as conc
                             left outer join med_hx as med
                             on conc.patient_num = med.patient_num
                             left outer join demo 
                             on conc.patient_num = demo.patient_num
                              left outer join med_hx_count as med_hx
                              on conc.patient_num = med_hx.patient_num
                              left outer join res_final as res
                              on conc.patient_num = res.patient_num
                              group by conc.patient_num
                             order by conc.patient_num")) #some multiple rows per person, if multiple dxs... will control later.

#convert NAs to 0 when applicable for count variables 
df_fa$concussion_hxdx[is.na(df_fa$concussion_hxdx)] <- 0 
df_fa$historical_dx_count[is.na(df_fa$historical_dx_count)] <- 0




#### IMPUTING MISSING DATA (testing)#############
#Resolve missing data. Either remove patients, or impute? 
#   - Best solution is to remove individuals without all variables
#   - alternative could be imputation, but I would need to simulate and test this. Maybe do both? More power...
#   Imputation shouldn't be done at this point. I think my only option is to move forward with 83 people with long recovery and full data

#filter out rows with NAs
df_fa_no_nas <- df_fa[complete.cases(df_fa), ] #312 rows, so have of population have full data... 80 people with long recovery. 148 in full dataset...
#subset long recovery patients
df_fa_no_nas_long <- subset(df_fa_no_nas, subset = (df_fa_no_nas$length_of_treat >= 28))

#test correlation between impact symptoms and concussion scale to see if dropping is needed
cor(df_fa_no_nas$impact_total_symptom, df_fa_no_nas$concussion_symptoms_total_severity) #0.83.. drop
cor(df_fa_no_nas$impact_total_symptom, df_fa_no_nas$concussion_symptoms_total_count) #.35 meh...

#drop concussion symptoms severity and total and see how many don't have NAs
drops <- c("concussion_symptoms_total_count","concussion_symptoms_total_severity")
df_fa_droppedcols <- df_fa[ , !(names(df_fa) %in% drops)]
#filter out NAs
df_fa_droppedcols_no_nas <- df_fa_droppedcols[complete.cases(df_fa_droppedcols), ]
#long recovery
df_fa_droppedcols_long <- subset(df_fa_droppedcols_no_nas, subset = (df_fa_droppedcols_no_nas$length_of_treat >= 28)) #83, lets use this as final df

######OKAY LET's DO THIS##########
#drop other columns not in analysis
final_drop <- c("patient_num", "length_of_treat")
final <- df_fa_droppedcols_long[ , !(names(df_fa_droppedcols_long) %in% final_drop)]



###########################################
##########  Factor Analysis ###############
###########################################
#standardized variables (rescaled to mean of 0 and SD of 1). Makes it easier for comparison across units.
final_stan = as.data.frame(scale(final))

#ss loadings in outputs = eigenvalues (i.e. variance in all variables which is accounted for by that factor. 
#                         the eigenvalue/# of variables = proportion variance)
# As a rule-of-thumb a factor is important if its eigenvalue is greater than 1 (i.e., the average); 
#             (this is also referred to as the Kaiser Rule)

library(psych)
library(GPArotation)

#parallel analysis for # of factors
parallel <- fa.parallel(final_stan, fm = 'minres', fa = 'fa')
#suggests 3

threefactor <- fa(final_stan,nfactors = 3,rotate = "oblimin",fm="minres") #oblique rotation, as there appears to be some correlations
print(threefactor)
print(threefactor$loadings,cutoff = 0.3)#prints factors with a loading >= 0.3

fa.diagram(threefactor) #plots loadings
#interpretations: 
#   RMSR = 0.06 (meh, want close 0)
#     RMSEA index =  0.077 (good fit = < 0.05)
#   Tucker Lewis Index of factoring reliability =  0.776 (want over 0.9)

#Not great fit....


#four factors
fourfactor <- fa(final_stan, nfactors = 4, rotate = "oblimin", fm="minres")
print(fourfactor)
print(fourfactor$loadings,cutoff = 0.3) #better fit, but still not great

#polychoric correlation structure
library(polycor)
threefactor_pc <- hetcor(final_stan, ML = TRUE)
fa_3factor_pc <- fa(r=threefactor_pc$correlations, nfactors=3, rotate="oblimin", fm = "minres")
print(fa_3factor_pc)
print(fa_3factor_pc$loadings,cutoff = 0.3)
#Fit: RMSR = 0.06 (less than 0.08 is good fit)
fa.diagram(fa_3factor_pc)

