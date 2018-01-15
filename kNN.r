#kNN for imputing length of treatment (LOT)

# according to power analysis, I need ~300 data points to detect significance at ~80% power
# I currently have approximately 140 people with valid injury dates and d/c dates.
# I'd like to see if I can use kNN clustering to 'predict' the injury and d/c times.

# ideally, I'd match on individuals really close, return the two dates, run a residual analysis (or something)
#similar to gauge accuracy, then include them in model. I don't think this will work.

#KNN machine learning algorithm, not statistically-based model.
# Instance-based learner: distance between the stored data and the new instance
# is calculated by means of some kind of distance/similarity measure. (i.e. Euclidean/Manhattan distance)

#Since I intend on assigning a value, this KNN implementation will be regression-based.


#load data
# dataframe with log regression setup, but with a training/test split in half
#   #one row per person





