id<-c(123,123,123,123,124,124,124,124)
date<-as.Date(c('2010-01-15','2010-01-01','2013-01-01','2013-01-13','2010-03-05','2010-01-05','2010-03-16','2010-05-05'))
score<-c(10,15,20,30)
data<-data.frame(id,date,score)

data <- data[order(data$id,data$date),]
data$dayssincelast<-do.call(c,by(data$date,data$id,function(x) c(NA,diff(x))))
# Or, even more concisely
data$dayssincelast<-unlist(by(data$date,data$id,function(x) c(NA,diff(x))))

# okay, now lets figure out how to remove any values after the date in which dayssincelast >= 100 days
#convert nas to 0 (i.e. keep)
data$dayssincelast[is.na(data$dayssincelast)] <- 0

#create new variable with the date when dayssincelast >= 100
data$flag <- ifelse(data$dayssincelast >= 100, 1,0)

#filter each person by the first occurrence of flag
library(dplyr)
data_test <- group_by(data, id) %>%
  mutate(first2 = min(which(flag == 1 | row_number() == n()))) %>%
  filter(row_number() <= first2) %>%
  select(-first2)
#still has flag row, now just remove when flag == 1
data_test <- filter(data_test, flag != 1)

#YES IT WORKS!!!