
rm(list=ls())

library(data.table)
library(mice)

###############################################################
################ Train models and pool results ################
###############################################################
setwd('~/Desktop/Ebola')
vars <- data.table::fread("fwd_selection_results.csv", drop = "V1")  
load('mids_100.RData')

fifty <- (subset(vars, Freq >= 50))$Var1
forty <- (subset(vars, Freq >= 40))$Var1
thirty <- (subset(vars, Freq >=30))$Var1 

fit1 <- with(imp, glm(Death ~ AstheniaWeakness + Breathlessness + CT + PatientAge, family = "binomial"))
est1 <- pool(fit1)

fit2 <- with(imp, glm(Death ~ AstheniaWeakness + Breathlessness + CT + Malaria + PatientAge, family = "binomial"))
est2 <- pool(fit2)

fit3 <- with(imp, glm(Death ~ AnyBleeding + AstheniaWeakness + Breathlessness + CT + 
                        Diarrhoea + Headache + Malaria + PatientAge, family = "binomial"))
est3 <- pool(fit3)
