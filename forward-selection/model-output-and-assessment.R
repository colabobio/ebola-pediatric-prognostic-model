
source('Code/prognostic-model.R')
###############################################################
######################## Model summary ########################
###############################################################

n = 1
p = 50
est <- get(paste0('est', n))
fit <- get(paste0('fit', n))
vars <- get(paste0('model', n, 'vars'))
name <- paste0('Model_',n)

coef <- tibble(summary(est, exponentiate = F, conf.int = TRUE))
coef %>%
  mutate(odds = exp(estimate)) %>%
  kbl(caption = paste0(name, ' (Variables selected in >= ',p,'% of models')) %>%
  kable_paper('hover') %>%
  save_kable(paste0(name,'_summary.pdf'))


### ROC 
data <- read.csv('cleaned-data-june29.csv')[,-1]
data <- data %>%
  select(Death, vars) 

pooled_fit = fit$analyses[[1]]
pooled_fit$coefficients = summary(est)$estimate
pred.prob <- predict(pooled_fit, data = data)

ROC01 = roc(data$Death, pred.prob,ci=TRUE)

ROC01_table <- data.frame(se = ROC01$sensitivities,
                          sp = ROC01$specificities,
                          threshold = ROC01$thresholds)

#Compute The Confidence Interval Of Sensitivities At Given Specificities
selected.seq = seq(0,length(ROC01$thresholds),l=100)
ROC01.ci = ci.se(ROC01, specificities=ROC01$specificities[selected.seq],progress='none')

dat.ci <- data.frame(x = as.numeric(rownames(ROC01.ci)),
                     lower = ROC01.ci[, 1],
                     upper = ROC01.ci[, 3])


r capture.output(ROC01$auc)`
`r capture.output(ROC01$ci) `


```{r}
roc.plot = ggroc(ROC01) + theme_minimal() + geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() + 
  geom_ribbon(data = dat.ci, aes(x = x, ymin = lower, ymax = upper), fill = "steelblue", alpha= 0.2) + ggtitle(capture.output(ROC01$ci))


roc.plot = ggroc(ROC01) + theme_minimal() + geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() + 
  geom_ribbon(data = dat.ci, aes(x = x, ymin = lower, ymax = upper), fill = "steelblue", alpha= 0.2) + ggtitle('') + ylab('Sensitivity') + xlab('Specificity
')

```
