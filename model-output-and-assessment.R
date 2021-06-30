
library(dplyr)
library(kableExtra)
library(broom)
library(knitr)
library(tidyr)

## Model without interaction and splines

### odds ratio and coefficients 
```{r}
getwd()
a1 <- get(load("pooledmodel50.RData"))
predictors <- c('Death', levels(a1$pooled$term)[-1])
model.formula.upd = reformulate(levels(a1$pooled$term)[-1], response = 'Death')
output.coef = tibble(summary(est, conf.int = TRUE))
output.oddsratio = tibble(summary(est, exponentiate = T, conf.int = TRUE))

t <- output.coef %>%
  kbl(caption = 'Model 1: Variables selected in >= 50% of models') %>%
  kable_paper('hover')
save_kable(t, 'model1-output.pdf')





fit50 <- with(imp, glm(reformulate(fifty, response = 'Death'), family = "binomial"))
est50 <- pool(fit50)
reformulate(fifty, response = 'Death')
library(mice)
t <- with(imp, glm(Death ~ AstheniaWeakness + Breathlessness + CT + PatientAge, family = binomial))
est <- pool(t)

