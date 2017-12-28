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
# 1. still need to figure out how I can control for people with multiple concussions... Can't treat them as same person.
#   - Going to be some work... could always do manual (pass). But likely need to come up with some logic to state that if
#       person doesn't come back to clinic within 3-6 months, but then comes in again for concussion, this is now a second concussion
#       (or something else?). Doing this will likely need to be a for loop or function... going to take some time and thought.




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
#create new variable (flag) to represent row where visit_gap >= 100 days
sports_med$flag <- ifelse(sports_med$visit_gap >= 100, 1, 0)

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









