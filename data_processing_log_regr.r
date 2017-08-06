# Dissertation Code
#Data Processing Script for Logistic Regression model

#load data
setwd("/Users/Rachel/Documents/mark/dissertation/data/Mark_dissertation_20170718/")
master <- read.csv('Mark_dissertation_20170718-data.csv')

#start separating master out into different tables
#TODO: Person (demographics), Conditions, notes, results (similar to SJS_v15 structure)

###########################
####    CONDITIONS      ###
###########################
# Include: PID, EID, modifier label = Primary Dx

#
