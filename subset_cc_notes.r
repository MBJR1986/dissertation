#Dissertation: Subset notes from Concussion Clinic

#load data
setwd("C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/Dissertation/data/Mark_dissertation_20170718/")
notes <- read.csv('text_notes_full.csv', stringsAsFactors = FALSE)
dx <- read.csv('concussion_dx_cohort.csv', stringsAsFactors = FALSE)

#load library
library(sqldf)

#plan is to subset all patient_nums with concussion clinic note, then inner join list with notes file

cc_pts <- sqldf("select distinct patient_num from notes where tval LIKE '%ORTHOPEDICS%'")

#SUBSET NOTES
cc_notes <- sqldf("select x.* from notes as x
                  inner join cc_pts as y
                  ON x.patient_num = y.patient_num")

#write out file
write.csv(cc_notes, "cc_text_notes.csv")
