#survival analysis (dissertation) data cleaning and structuring for analysis

#This R script takes all the HERON extracts and prepares them for a survival analysis (done in juypter notebook with R kernel)
# using individuals with diagnosed concussion and were seen at KUMC Concussion Clinic. Total population = 665.

#load libraries
library(lubridate)
library(sqldf)
library(ggplot2)
library(survival)
library(ranger)
library(ggfortify)


#load data
setwd("D:/KUMC/Dissertation/data/Mark_dissertation_20170718/")
master <- read.csv('master.csv', stringsAsFactors = FALSE)
concussion_dx <- read.csv('concussion_dx_cohort.csv', stringsAsFactors = FALSE)
demo <- read.csv('full_demographic.csv', stringsAsFactors = FALSE) #includes person-wise SVI data
enc_loc <- read.csv('encounter_location.csv', stringsAsFactors = FALSE)
med_hx <- read.csv('medical_history.csv', stringsAsFactors = FALSE)
#results <- read.csv('results.csv', stringsAsFactors = FALSE)
#notes <- read.csv('text_notes.csv', stringsAsFactors = FALSE)
#notes_clean <- read.csv('cc_text_notes.csv', stringsAsFactors = FALSE) #concussion clinic notes, contains concussion dx date, and manual injury date
svi <- read.csv('patient_svi_countylevel.csv', stringsAsFactors = FALSE) #SVI data with MRN info


#Some general TODOs for data structure in R's survival analysis:
#   - One row per person
#   - Variables can be cont, dich, ordinal
#   - Need to use 'LOT' variable as the time component
#   - Need to create a 'Recovered' flag for Event of interest (1,0); all will be 1.
#   - No ImPACT right now.



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


#general summary of population breakdown: Count of patients with treatment >= 28 days: 148 (22%)
#                                         Count of patients with treatment <= 28 days: 525 (78%)


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

#combining some categories into one 
levels(med_hx$code_label) <- list(Concussion = c("Concussion", "Concussion without loss of consciousness"), 
                                  ADHD = c("ADD (attention deficit disorder)", "ADHD (attention deficit hyperactivity disorder)",
                                           "Attention deficit disorder with hyperactivity", "Attention deficit hyperactivity disorder (ADHD)"),
                                  Anxiety = c("Anxiety", "Anxiety disorder"),
                                  Depression = c("Depressed", "Depression"),
                                  Dyslexia = c("Dyslexia"),
                                  Migraine = c("Migraine", "Migraines"),
                                  Psychiatric_illness = c("Psychiatric illness"),
                                  PTSD = c("PTSD (post-traumatic stress disorder"))
#convert med_hx$code_label to dummy variables
library(caret)
dummy_df <- sqldf("Select patient_num
                        ,code_label
                  from med_hx")
dummy_df$patient_num <- as.numeric(dummy_df$patient_num)
dmys <- dummyVars(" ~ .", data = dummy_df)
med_hx_xplode <- unique(data.frame(predict(dmys, newdata = dummy_df)))
#change column names
colnames(med_hx_xplode) <- c("patient_num", "ADHD", "Anxiety", "Concussion", "Depression", "Dyslexia", "Migraine", "Psychiatric_illness")
med_hx_xplode$patient_num <- as.factor(med_hx_xplode$patient_num)
#med_hx_xplode now ready to be joined into dataframe for survival analysis.


### Demographics cleaning
#convert to factors
demo$patient_num <- as.factor(demo$patient_num)
demo$sex <- as.factor(demo$sex)
demo$language <- as.factor(demo$language)
demo$race <- as.factor(demo$race)
demo$religion <- as.factor(demo$religion)
#religion (1 = TRUE (religious))
demo$religious <- ifelse(demo$religion == "none" | demo$religion == "unknown", 0,1)
#enlgish speaking (1 = Yes, 0 = no)
demo$english_speaking <- ifelse(demo$language == "english", 1,0)



############################
####    final dataframe ####
############################
surv_df <- sqldf("select distinct * from (
                  select sm.patient_num
                 ,sm.length_of_treat
                 ,demo.sex
                 ,demo.english_speaking
                 ,demo.religious
                 ,demo.age
                 ,demo.race
                ,demo.RPL_THEME1 as perc_rank_socioeconomic
                ,demo.RPL_THEME2 as perc_rank_householdcomp
                ,demo.RPL_THEME3 as perc_rank_minoritystatus
                ,demo.RPL_THEME4 as perc_rank_housingtransport
                 ,MAX(mh.ADHD) as ADHD
                 ,MAX(mh.Anxiety) as Anxiety
                 ,MAX(mh.Concussion) as Concussion
                 ,MAX(mh.Depression) as Depression
                 ,MAX(mh.Dyslexia) as Dyslexia
                 ,MAX(mh.Migraine) as Migraine
                 ,MAX(mh.Psychiatric_illness) as Psychiatric_illness
                 from sports_med as sm
                 left join demo as demo
                 on sm.patient_num = demo.patient_num
                 left join med_hx_xplode as mh
                 on sm.patient_num = mh.patient_num
                group by sm.patient_num
                 ) as tmp")

#convert NAs to 0
surv_df$ADHD[is.na(surv_df$ADHD)] <- 0 
surv_df$Anxiety[is.na(surv_df$Anxiety)] <- 0 
surv_df$Concussion[is.na(surv_df$Concussion)] <- 0 
surv_df$Depression[is.na(surv_df$Depression)] <- 0 
surv_df$Dyslexia[is.na(surv_df$Dyslexia)] <- 0 
surv_df$Migraine[is.na(surv_df$Migraine)] <- 0 
surv_df$Psychiatric_illness[is.na(surv_df$Psychiatric_illness)] <- 0 

#add 'dc' event flag
surv_df$dc <- 1




#########################
### Write DF to disk  ###
#########################
#write.csv(surv_df, file = "survival_analysis_df.csv")




#######################################
### Kaplan Meier Survival Analysis  ###
#######################################
#665 cases used for analysis. Seen at concussion clinic, have all variables. for Binary variables (i.e. Religious? and English-speaking)
# codes = '1' represent positive classification.


#surv() to build survival object
km <- with(surv_df, Surv(length_of_treat, dc))

#fit survival probability estimates
km_fit <- survfit(Surv(length_of_treat, dc)~1, data = surv_df)
summary(km_fit, times = c(1,5,10,15,20*(1:10))) #prints estimates for 1,5,10 etc days, then every 20 after

#plot all population survival curve (time = days)
all_pop <- autoplot(km_fit, xlim = c(0, 200), xlab = "Length of Care (days)", ylab = "Survival Probability")
all_pop + theme_minimal()

#plot survival curve by sex
km_sex_fit <- survfit(Surv(length_of_treat,dc) ~ sex, data = surv_df)
sex_curve <- autoplot(km_sex_fit, xlim = c(0,200), xlab = "Length of Care (days)", ylab = "Survival Probability")
sex_curve <- sex_curve + theme_bw()
sex_curve <- sex_curve + scale_fill_discrete(name = "Sex", labels = c("Female", "Male"))
sex_curve

#todo: adjust colors. Otherwise looks good and can just delete second legend out of R

#https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_surv.html
#survival plotting examples

#plot concussion history (significant diff)
km_conc_hist_fit <- survfit(Surv(length_of_treat,dc) ~ Concussion, data = surv_df)
conc_hist_curve <- autoplot(km_conc_hist_fit, xlim= c(0,200), xlab = "Length of Care (days)", ylab = "Survival Probability")
conc_hist_curve <- conc_hist_curve + theme_bw()
conc_hist_curve <- conc_hist_curve + scale_fill_discrete(name = "Previous Concussion?", labels = c("No", "Yes"))
conc_hist_curve

######################
### Log rank tests for differences in survival rates between groups
######################

#sex
surv_diff_sex = survdiff(Surv(length_of_treat,dc)~sex, data = surv_df)
surv_diff_sex
#Interpretation: P-value less than 0.05 (0.0002) means significant diff. 

#Race 
surv_df$race_white <- ifelse(surv_df$race == "white", 1, 0) #white =1, minority = 0
survdiff(Surv(length_of_treat,dc) ~race_white, data = surv_df)
#p = 0.9


#Prior Concussions
surv_diff_conc <- survdiff(Surv(length_of_treat,dc)~Concussion, data = surv_df)
surv_diff_conc
# p = 0.02 ***

#religious
surv_diff_relig <- survdiff(Surv(length_of_treat,dc)~religious, data = surv_df)
surv_diff_relig
#nope. P = 0.9

#English-speaking
surv_diff_eng <- survdiff(Surv(length_of_treat,dc)~english_speaking, data = surv_df)
surv_diff_eng
#close: p-value = 0.1

#ADHD
surv_diff_adhd <- survdiff(Surv(length_of_treat,dc)~ADHD, data = surv_df)
surv_diff_adhd
#nope: p = 1

#anxiety
surv_diff_anx <- survdiff(Surv(length_of_treat,dc)~Anxiety, data = surv_df)
surv_diff_anx
# p = 0.3

#Dyslexia
surv_diff_dys <- survdiff(Surv(length_of_treat,dc)~Dyslexia, data = surv_df)
surv_diff_dys
#p = 0.2

#Migraines
survdiff(Surv(length_of_treat,dc)~Migraine, data = surv_df)
#p = 0.8

#depression
survdiff(Surv(length_of_treat,dc)~Depression, data = surv_df)
#p = 0.7

#psychiatric illness (defined...?)
survdiff(Surv(length_of_treat,dc)~Psychiatric_illness, data = surv_df)
# p = 0.2



##############################
### Cox PH regression   ######
##############################
#univariate cox regression
res.cox <- coxph(Surv(length_of_treat,dc)~ sex, data = surv_df)
summary(res.cox)
#interpret: With sex as covariate, this is significant (p = 0.0003). Beta coefficient indicates that females have higher risk of treatment
#when compared to males. Hazards ratio give the effect size of covariate. Here, being female increases the hazard by a factor of 1.34.
# note: interpretation here is inverse of traditional survival models. Here, we want shorter 'survival', which represent length of care.

#mulitvariate cox regression
cox <- coxph(Surv(length_of_treat, dc) ~ sex + english_speaking + religious + age + race_white + ADHD + Anxiety + Concussion +
                 Depression + Dyslexia + Migraine + Psychiatric_illness + perc_rank_socioeconomic +
                 perc_rank_householdcomp + perc_rank_minoritystatus + perc_rank_housingtransport, data = surv_df)
summary(cox)
#interpretation:
# Significant model, as Likelihood ratio test, Wald test, and Logrank test are all significant p-values.
# Here, sex and history of psychiatric illness significant, with several close to significance (P < .1)
# p-value for sex had a hazard ratio of 1.36, indicating strong relationship to longer recoveries.
#Concordance statistic isn't great, and neither is Rsquare... 

#Socioeconominc had a hazard of 2.3, the highest hazard in variable list.

#might need to consider removing some diagnoses. Psychiatric illness had one instance... likely need to get out of here. 

#mulitvariate cox regression
cox_v2 <- coxph(Surv(length_of_treat, dc) ~ sex + english_speaking + religious + age + race_white + ADHD + Anxiety + Concussion +
                 Depression + Dyslexia + Migraine + perc_rank_socioeconomic +
                 perc_rank_householdcomp + perc_rank_minoritystatus + perc_rank_housingtransport, data = surv_df)
summary(cox_v2)

#Interpret:
# Still significant model, as Likelihood, Wald, and logrank tests were all highly signficant. Here, with psychiatric history
# removed, sex was highly significant, along with history of concussion (p = .04). Several variable were close to significance (p < .10).
# Hazard ratio: same for sex 1.36, indicating stronger relationship to long recoveries. SocioEconomic had a high rate as well, but
#interpretation of the continuous variable I will need to think about. This might be representing that individuals with higher socio-
#economic status have higher risk for longer recoveries. alternative: they may be more likely to seek out services.
#concordance stat is pretty shitty...


