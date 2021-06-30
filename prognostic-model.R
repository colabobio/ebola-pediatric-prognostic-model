rm(list=ls())
library(data.table)

vars <- data.table::fread("fwd_selection_results.csv", drop = "V1")  
load('mids_100.RData')

fifty <- (subset(vars, Freq >= 50))$Var1
forty <- (subset(vars, Freq >= 40))$Var1
thirty <- (subset(vars, Freq >=30))$Var1 

fit50 <- with(imp, glm(reformulate(fifty, response = 'Death'), family = "binomial"))
est50 <- pool(fit50)

fit40 <- with(imp, glm(reformulate(forty, response = 'Death'), family = "binomial"))
est40 <- pool(fit40)

fit30 <- with(imp, glm(reformulate(thirty, response = 'Death'), family = "binomial"))
est30 <- pool(fit30)

save(est50, file = 'pooledmodel50.RData')

est50$pooled
class(est50)
summary(est50, conf.int = T)
