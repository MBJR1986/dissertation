# Dissertation Code
#Data Processing Script for Logistic Regression model

#Data from HERON has already been structured for processing with
#create_tables.sql script.

#This script creates or converts data into variables for regression analysis.

#load packages
library(lubridate)
library(sqldf)

#load data
setwd("/Users/Rachel/Documents/mark/dissertation/data/Mark_dissertation_20170718/")
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

#LOR
#Determine length of a person's recovery, from  the difference between 1st visit and last visit 
#(assuming last visit = return to normal functioning)

#create variable for eval_dt
min = aggregate(concussion_dx$start_date,by=list(concussion_dx$patient_num),min)
max = aggregate(concussion_dx$start_date,by=list(concussion_dx$patient_num),max)
#TODO make column names same before merging
concussion_dx = merge(concussion_dx,min,by.x=1,by.y=1)


