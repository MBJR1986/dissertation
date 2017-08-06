# Dissertation Code
#Data Processing Script for Logistic Regression model

#load packages
library(sqldf)

#load data
setwd("/Users/Rachel/Documents/mark/dissertation/data/Mark_dissertation_20170718/")
master <- read.csv('Mark_dissertation_20170718-data.csv', stringsAsFactors = FALSE)

#start separating master out into different tables
#TODO: Person (demographics), Conditions, notes, results (similar to SJS_v15 structure)
#   - really need to understand what is in each table I received first...


# for logistic regression model, I might just be able to use the person table, as it contains
#the first and last values, which is all I'm concerned with, right???
#   - Yeah no, that won't work... but all demographic info is in this table.

###############################
### Concussion Population   ###
###############################
#generate list of patient-ids with a concussion, along with dates and encounter
pt_list <- unique(sqldf("select patient_num
                    ,encounter_num
                    ,start_date
                    ,variable
                    ,variable_index
                    ,modifier_label
                 from master
                 where modifier = 'DiagObs:PRIMARY_DX_YN'"))#1626 unique people...
#adding visit location
sites <- c("ARW SPORTS MEDICINE CL", "EMERGENCY", "EMH EMERGENCY", "ICC SPECCARE NEURO", "ICC SPN REHAB MED CL",
           "ICC SPORTS MEDICINE CL", "KUCC OVPK PT", "UKP-FAIRWAY NEUROLOGY", "UKP-KCK FAM MED", "UKP-KCK PED AMB",
           "UKP-KU FAM MED", "UKP-KU PED AMB", "UKP-KU PED NEUROLOGY", "UKP-KU REHAB MED CL", "UKP-KUMW REHAB MED CL",
           "UKP-LCOA NEUROLOGY", "UKP-LWR SPORTS MED CL", "ZZ30F REHAB MEDICINE", "ZZARW SPORTS REHAB CL", 
           "ZZUKP-PV PED URGENT CA")

###########################
####    CONDITIONS      ###
###########################
# Include: PID, EID, modifier label = Primary Dx

#
