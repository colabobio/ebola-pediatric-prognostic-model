rm(list=ls())

library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(Hmisc)
library(randomForest)
library(leaps)
library(purrr)
library(glinternet)
library(readr)
library(caret)
library(glmnet)
library(rpart)
library(hydroGOF)
library(ordinalNet)
library(rpartScore)
library(ordinalForest)
library(rms)
library(foreach)
library(doParallel)
library(rpart.plot)
library(rattle)
library(MASS)
library(MKmisc)
library(rtf)
library(gmodels)
library(dplyr)

## Preprocess Data
setwd('~/Ebola/Ebola')
load('complete_100_imputations.RData')

#R source for big function
source('BinaryModels_cv_lglk_kq_2020_10_23.R')

####Options for the function are:
#K.forward,response,names,splines,include.interaction, train.data,test.data,eval.method='orc'

cont.vars <- c("PatientAge", "CT")

cat.vars <- c("PatientSex", 
              "Malaria",
              "AbdominalPain", 
              "Anorexia", 
              "AnyBleeding", 
              "JointPain", 
              "AstheniaWeakness", 
              "BoneMusclePain",
              "Vomit",
              "Diarrhoea",
              "Breathlessness",
              "Headache",
              "SwallowingProblems",
              "Fever",
              "Hiccups",
              "Nausea",
              "GI_Symptoms",
              "Conjunctivitis")

all.vars <- c(cat.vars, cont.vars)

########################## TRY A LOOP ##################################

imp_list_df2 <- map(1:length(imp_list_df), 
                    function(x) dplyr::select(imp_list_df[[x]], c('Death', all.vars)))

### create folds in each data frame
foldfunc <- function(data, x) {
  folds <- createFolds(1:nrow(data[[x]]), k=10,list = F)
  cbind(data[[x]], folds)
}

listdf <- map(1:length(imp_list_df2), function(x) foldfunc(imp_list_df2, x))

K = 10
names  = all.vars
splines = F
include.interaction = F

bigfunc <- function(train.data) {
  a <- forward.selection.modelBinary(K,'Death',
                                     names,
                                     splines,
                                     include.interaction,
                                     train.data,
                                     train.data,
                                     eval.method='lglk')
  return(a)
}

q <- map(1:length(listdf), function(x) bigfunc(listdf[[x]]))

save(q, file = 'imputed-model-output.RData')

selected_vars <- unlist(map(1:length(q), function(x) names(q[[x]]$model$coefficients)[-1]))

l <- table(selected_vars)

write.csv(l, "fwd_selection_results.csv")
