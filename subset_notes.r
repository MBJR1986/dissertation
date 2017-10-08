# Dissertation script
# clean text notes (remove fields with @ sign only)

#load data
setwd("C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/Dissertation/data/Mark_dissertation_20170718/")
notes <- read.csv('text_notes.csv', stringsAsFactors = FALSE)

#remove rows with @ sign only in tval field
notes <- subset(notes, subset = (tval != '@'))

#write out file
write.csv(notes,'text_notes_subset.csv')
