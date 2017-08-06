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





###########################
####    CONDITIONS      ###
###########################
# Include: PID, EID, modifier label = Primary Dx

#
