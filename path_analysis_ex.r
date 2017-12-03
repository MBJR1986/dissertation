#Path Analysis homework (examples)
#   - contains info on loading data from corr and cov matrix.
#   -later half of script walks through examples with fit indices intrepretations


#load packages
library(lavaan)
library(semPlot)

#ex 2.2 in some textbook...

#build correlation table

data.cor = lav_matrix_lower2full(c(1.000,
                                   0.178, 1.000,
                                   0.230, 0.327, 1.000,
                                   0.106, 0.245, 0.183, 1.000,
                                   0.195, 0.356, 0.721, 0.178, 1.000))

#create column and rownames for matrix
colnames(data.cor) =
  rownames(data.cor) =
  c('race', 'ses','cog', 'school', 'acad')


#create path model

model = '
acad ~ cog + race + ses + school
cog ~ race + ses
school ~ cog + race + ses
ses ~ race
'
#fit model
model.fit = sem(model,
                sample.cov = data.cor,
                sample.nobs = 18058)
#summary
summary(model.fit, standardized = TRUE, rsquare = TRUE)

#interpretation:
# all variables are significant. What we would think about is that academic achievement seems to be
#related to cognitive ability the most (0.671), and ses (0.128) seems heavily related, too (as long as same scale).
#BC data is from correlation table, all scales are standardized and direct comparison is possible.
#School model is also heavily predicted by ses, which could indicate your ses determines which school you go to.

#plot
semPaths(model.fit, whatLabels = 'par', layout = "spring")
               


#####Another example
#Testing for indirect effect (mediation model)

data.cov = lav_matrix_lower2full(c(84.85,
                                   71.28, 140.34,
                                   18.83, -6.25, 72.92,
                                   60.05, 84.54, 37.18, 139.48)) #covariance matrix for data
colnames(data.cov) = 
  rownames(data.cov) = 
  c('teacher', 'social', 'material', 'achieve')

#build model
model = '
achieve ~ b1*social + b2*material + c*teacher
material ~ a2*teacher
social ~ a1*teacher
indirect1 := a1*b1
indirect2 := a2*b2
'
#indirect effects look at the path to the outcome together (I think...)

#fit model
model.fit2 = sem(model, sample.cov = data.cov, sample.nobs = 40)

summary(model.fit2, standardized = TRUE)
#intrepretation:
#Unstandardized solution (using covariances). all paths are sign, 
#except teacher (which is good bc you want direct path to be non-significant in mediation model).
#First indirect was sign.

#plot
semPaths(model.fit2, whatLabels = 'par', layout = 'spring')


##################################
### Examples with Fit indices   ##
##################################
#focus is building and fitting models, and intrepreting outputs.

#data (correlation table)

data.cor = lav_matrix_lower2full(c(1,
                                   .19, 1,
                                   -.16, -.20, 1,
                                   -.37, -.06, .36, 1,
                                   -.06, -.05, -.03, -.25, 1,
                                   .13, -.06, -.09, -.28, .41, 1))
#add column/row names
rownames(data.cor) = 
  colnames(data.cor) = 
  c('agg', 'with', 'edu', 'age', 'emotion', 'conduct')

#build model
model = '
conduct ~ agg + age + edu
emotion ~ age + edu + with
edu ~ with
age ~ agg
age ~~ edu #correlation in model
'
model.fit3 = sem(model, sample.cov = data.cor, sample.nobs = 200)
summary(model.fit3, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
#intrepretation:
# min function test stat = chi square. p value is sign.
# model test baseline = null model where nothing is related. You want this worse than your model.
# this example is worse, which indicates that our model's paths are good.
#CFI and TLI are pretty high, which is pretty good. you want > .95
# AIC/BIC used when comparing different versions of model (like latent). You want these lowest.
# RMSEA = scaled on low end, want close to 0. this one is pretty small (0.089). must be less than .1.
#    - P-value RMSEAS <= 0.05 you want really high, this one was kinda small (.134)
# SRMR good 0.05
# So we have a mix of good and okay stats.
fitMeasures(model.fit3)
#same info, just in clean format. tons of stuff. 

#plot
semPaths(model.fit3, whatLabels = 'par', layout = 'spring')





### Another version of the model. Let's say we try to improve the previous model

# let's try removing education and age

#build model
model2 = '
conduct ~ agg 
emotion ~ with
'
model.fit4 = sem(model2, sample.cov = data.cor, sample.nobs = 200)
summary(model.fit4, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
fitMeasures(model.fit4)

#plot
semPaths(model.fit4, whatLabels = 'par', layout = 'spring')

#Int
