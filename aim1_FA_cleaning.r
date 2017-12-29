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



#########################
### Create DF for FA  ###
#########################

df_fa <- as.data.frame(sqldf("select distinct conc.patient_num
                                        ,conc.length_of_treat
                               ,demo.age
                               ,demo.sex
                               ,med.code_label as historical_condition
                               ,demo.language
                               ,demo.race
                               ,demo.religion
                               from sports_med as conc
                               left outer join med_hx as med
                               on conc.patient_num = med.patient_num
                               left outer join demo 
                               on conc.patient_num = demo.patient_num
                               left outer join (
                               ,MIN(start_date) as first_date
                               ,encounter_loc
                               from enc_loc
                               group by patient_num) as enc
                               on conc.patient_num = enc.patient_num
                               order by conc.patient_num")) #some multiple rows per person, if multiple dxs... will control later.

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

