# Regression and Decision Tree examples.

#using this data to mimic modeling and predicting injury date when missing from dataset
# for dissertation (KUMC Concussion Clinic data)

#############
### Plan: ###
#############

#1. Assemble and explore data
#1. Clean variables. build what is needed
#3. Three models: Linear, regression tree, random forest, (maybe xGBoost)
#4. Choose best model, test on test set

### Load packages
library(data.table)
library(FeatureHashing)
library(Matrix)
library(xgboost)
require(randomForest)
require(caret)
require(dplyr)
require(ggplot2)
library(pROC)
library(stringr)
library(dummies)
library(Metrics)
library(kernlab)
library(mlbench)

#################
### Clean data ###
##################

