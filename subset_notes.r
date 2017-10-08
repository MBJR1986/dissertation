# Dissertation script
# clean text notes (remove fields with @ sign only)

#load data
setwd("C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/Dissertation/data/Mark_dissertation_20170718/")
notes <- read.csv('text_notes_full.csv', stringsAsFactors = FALSE)
dx <- read.csv('concussion_dx_cohort.csv', stringsAsFactors = FALSE)

#convert to factors
notes$patient_num <- as.factor(notes$patient_num)

#convert to dates
notes$note_date <- as.Date(notes$note_date, "%m/%d/%y %H:%M")
notes$conc_dx_date <- as.Date(notes$conc_dx_date, "%m/%d/%y %H:%M")

#remove rows with @ sign only in tval field
notes <- subset(notes, subset = (tval != '@'))

#parse out notes that occur before concussion_dx date for each patient_num
notes$counter <- notes$note_date - notes$conc_dx_date #create counter comparing difference of dates between dx and note
notes <- subset(notes, subset = (notes$counter >= 0)) #populate if counter is positive, meaning occurs after dx!

#order by patient_num, note_date
notes <-notes[with(notes,order(patient_num,note_date)),]

#write out file
write.csv(notes,'text_notes_subset.csv')
