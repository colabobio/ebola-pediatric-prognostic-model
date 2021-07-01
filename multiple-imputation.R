rm(list=ls())
library(mice)
library(purrr)
library(varhandle)
setwd('~/Ebola/Ebola')
#####################################  Process data #####################################
data <- read.csv('~/cleaned-data-june29.csv')

cont.vars <- c("PatientAge", "CT")

cat.vars <- c("PatientSex", 
              "Death", 
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
              "GI_Symptoms")

all.vars <- c(cat.vars, cont.vars)

data[cat.vars] <- lapply(data[cat.vars], factor)
data <- data[, c(cont.vars, cat.vars)]

# Review missingness
round(colSums(is.na(data))/nrow(data), 2)

##################################### IMPUTE DATA #########################################
m = 100
imp <- mice(data, seed = 123, m, print = F) #impute data
save(imp, file = paste0('mids_',m,'.RData'))

#stack <- mice::complete(imp, "long") #stack imputations into one long data frame
imp_list_df <- map(1:m, function(x) complete(imp, x))

# png("imputed-CT-density.png", width = 600, height = 600)
# densityplot(imp, ~CT)
# dev.off()

##################################### ADD INTERACTIONS ######################################

cont.vars <- c("PatientAge", "CT")

cat.vars <- c("PatientSex", 
              "Death", 
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
              "GI_Symptoms")

all.vars <- c(cat.vars, cont.vars)

imp_list_df <- map(imp_list_df, unfactor) # Recode factors as numeric to allow calculation of interactions

imp_list_df <- map(1:length(imp_list_df), function(x) {
  imp_list_df[[x]] %>%
    mutate(Age_x_Bleeding = AnyBleeding*PatientAge) %>%
    mutate(CT_x_Bleeding = AnyBleeding*CT) %>%
    mutate(CT_cat = ifelse(imp_list_df[[x]]$CT <= 20, 0, 1))
})

save(imp_list_df, file = paste0('complete_',m,'_imputations.RData'))

