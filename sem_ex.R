# SEM Example

#Path analysis

#load packages
library(lavaan)
library(semPlot)
library(foreign) #load SPSS .sav file

#load data
setwd('C:/Users/MB047320/OneDrive - Cerner Corporation/KUMC/example_datasets/')
data <- read.spss('2 path faculty eval.sav', to.data.frame = TRUE)

#Data explained
# A = q12 (a course I wanted to take)
# B = q2 (clear and organized)
# C = q4 (grading was fair)
# D = q1 (overall evaluation)

#specify path model:
model <- '
#c ~ a + b
q4 ~ q12 + q2
#d ~ c + a
q1 ~ q4 + q12
'
#run model
path = sem(model, data = data)

#view outputs
summary(path, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE, modindices = TRUE)

#build SEM plot
semPaths(path, whatLabels = "par", layout = "spring") #labels = parameter estimates, layout = spring/tree/circle

#######################
#OTHER example...

regression.cor = lav_matrix_lower2full(c(1.00,
                                         0.20,1.00,
                                         0.24,0.30,1.00,
                                         0.70, 0.80, 0.30, 1.00)) #correlation matrix for example, all the data you need...

#name variables in matrix (gives column and row names)
colnames(regression.cor) =
  rownames(regression.cor) =
  c("X1", "X2", "X3", "Y")

#simple example. use x1, x2, x3 as exogenous to predict y (endogenous)

regression.model = 'Y ~ X1 + X2 + X3'

#estimating without full dataset
#sem(model, sample.cov=correlation table, sample.nobs = number of people)

#fit the model
regression.fit = sem(regression.model,
                     sample.cov = regression.cor,
                     sample.nobs = 1000)

summary(regression.fit,
        rsquare = TRUE)

semPaths(regression.fit,
         whatLabels = "par",
         layout = "tree")



#################
### Another ex, mediation

beaujean.cov = lav_matrix_lower2full(c(648.07,
                                       30.05, 8.64,
                                       140.18, 25.57, 233.21)) #covariance matrix
#name matrix
colnames(beaujean.cov) = 
  rownames(beaujean.cov) = 
  c("salary", "school", "iq")

#this example looks at how the endogenous variable 'school' (years of schooling) reflects exo variables
# 'salary' and 'iq'. Intuitively, one would assume greater salary and IQ as schooling increases.

model = '
  iq ~ b*school
  salary ~ c*iq + a*school
  ind := b*c #indirect effect
'
#fit model
beaujean.fit = sem(model,
                   sample.cov = beaujean.cov,
                   sample.nobs = 300)
summary(beaujean.fit, rsquare = TRUE)

#plot
semPaths(beaujean.fit, 
         whatLabels = "par",
         layout = "tree")