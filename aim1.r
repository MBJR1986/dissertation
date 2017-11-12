#Aim 1: which personal factor indepedently influence concussion treatment time?
  #a. assess the amount of time between the injury event and the date of evaluation 
  #   to determine if there is a significant portion of time between injury date and evaluation date.
  #b. explore clinical notes from each person’s last clinical visit to investigate the assumption that
  #   a person is no longer clinically followed because they have returned to their previous levels of functioning.


# Population: All individuals seen at the KUMC Concussion Clinic between the dates of January 1, 2011 and Aug 1, 2017?
#   This analysis will only include individuals with TEXT NOTES, as the manual chart reviews of these individuals is necessary
#   for the completion of this aim.

library(lubridate)
library(sqldf)

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

#####################
###     Aim 1.a   ###
#####################

#compare eval and injury dates from notes_clean assess difference between 2 dates by patient_num
num_pts <- as.numeric(length(unique(notes_clean$patient_num))) #137

#convert to dates
notes_clean$conc_dx_date <-as.Date(mdy(notes_clean$conc_dx_date))
notes_clean$note_date <- as.Date(mdy(notes_clean$note_date))
notes_clean$injury_date <- as.Date(mdy(notes_clean$injury_date))
notes_clean$last_visit_date <- as.Date(mdy(notes_clean$last_visit_date))

#create dataframe with 1 row per person with min/max dates
aim1_df <- sqldf("select distinct patient_num
                 , MIN(note_date) as first_visit_date__Date
                 ,MIN(injury_date) as injury_date__Date
                  ,MAX(last_visit_date) as last_visit_date__Date
                 from notes_clean
                 group by patient_num", method = "name__class")

#Create new variable for the difference between injury date and eval date, indicating wait period for appt:
aim1_df$wait_days <- as.numeric(aim1_df$first_visit_date - aim1_df$injury_date)

#summary:
summary(aim1_df)

#plot
library(ggplot2)
wait_hist = ggplot(data = aim1_df, aes(x = wait_days)) # data & aesthetics
wait_hist + geom_histogram() # add histogram

# summary: For aim 1.a, I wanted to look at the difference in injury date vs. eval date. On average, nearly 18 days passed 
# between injury date and evaluation date. This data was skewed to outliers on the high end, as the median was 6 days wait. 
# Range = 0 days to 132 days. That being said, I think it would be the best choice to use the date of evaluation, as opposed to 
# injury date for the work moving forward, as this event is clearly documented and referenced. I was able to verify the injury date in 
# roughly 1/3rd of the encounters at the concussion clinic.




#################
### Aim 1.b   ###
#################
#little stastical analysis here, but generally speaking, there were not many formal discharges.
#From notes, the common discharge procedure was 'patient will follow up if there are any concerns'\
#which clinically makes sense, but from a data perspective, can't verify a discretely recorded event.

#alternatively, here is a plot of Length of Treatment, comparing the first evaluation date with
# the last date seen clinically:

aim1_df$LOT <- as.numeric(aim1_df$last_visit_date - aim1_df$first_visit_date)
summary(aim1_df$LOT)

lot_hist = ggplot(data = aim1_df, aes(x = LOT))+
  geom_histogram()
lot_hist
