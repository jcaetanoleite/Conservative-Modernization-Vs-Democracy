library(ggplot2)
library(plm)
library(plotly)
library(ggthemes)
library(tidyverse)
library(readxl)


#Fixed effects models
##Basic data
DDCGdata <- haven::read_dta("C:/Users/Joao Caetano/Desktop/DDCGdata_final.dta")

df <- DDCGdata %>%
  dplyr::select(1:30)

rm(DDCGdata)
head(df)
### Creating Lags of GDP
df <- df %>%
  dplyr::group_by(country_name) %>%
  dplyr::mutate(
    lag1 = lag(y, 1),
    lag2 = lag(y, 2),
    lag3 = lag(y, 3),
    lag4 = lag(y, 4),
    lag5 = lag(y, 5),
    lag6 = lag(y, 6),
    lag7 = lag(y, 7),
    lag8 = lag(y, 8)
  )

###Making it a Panel Dataset
pdf=pdata.frame(df,index=c("country_name","year"))

## Model 1 -> 1 Lag

model1 <- plm(
  y ~ dem + lag1, #the equation with country fixed effects
  model="within", #The fixed effects within estimator
  data = pdf, 
  effect = "twoways"
)
summary(model1)

##Model 2 -> 2 Lags

model2 <- plm(
  y ~ dem + lag1 + lag2 , #the equation with country fixed effects
  model="within", #The fixed effects within estimator
  data = pdf, 
  effect = "twoways"
)
summary(model2)

##Model 3-> 4 Lags

model3 <- plm(
  y ~ dem + lag1 + lag2 + lag3 + lag4, #the equation with country fixed effects
  model="within", #The fixed effects within estimator
  data = pdf, 
  effect = "twoways"
)
summary(model3)

model4 <- plm(
  y ~ dem + lag1 + lag2 + lag3 +lag4 +lag5 +lag6+ lag7 +lag8, #the equation with country fixed effects
  model="within", #The fixed effects within estimator
  data = pdf, 
  effect = "twoways"
)
summary(model4)

## Long Run Effects

beta_hat <- coef(model1)["dem"]
gamma_hat <- coef(model1)["lag1"]

long_run_effect_1 <- beta_hat / (1 - sum(gamma_hat))

beta_hat <- coef(model2)["dem"]
gamma_hat <- (coef(model2)[2:3])

long_run_effect_2 <- beta_hat / (1 - sum(gamma_hat))

beta_hat <- coef(model3)["dem"]
gamma_hat <- (coef(model3)[2:5])

long_run_effect_3 <- beta_hat / (1 - sum(gamma_hat))

beta_hat <- coef(model4)["dem"]
gamma_hat <- (coef(model4)[2:9])

long_run_effect_4 <- beta_hat / (1 - sum(gamma_hat))

lre <- c(long_run_effect_1, long_run_effect_2, long_run_effect_3, long_run_effect_4)
lre <- round(lre, 3)
lre

##Persistence

pers1 <- sum(coef(model1)[2])
pers2 <- sum(coef(model2)[2:3])
pers3 <- sum(coef(model3)[2:5])
pers4 <- sum(coef(model4)[2:9])

pers <- c(pers1, pers2, pers3, pers4)
pers <- round(pers, 3)
pers

##Effect after 25 years
###Model 1
dem_shortrun <- coef(model1)["dem"]
lag1 <- (coef(model1)[2])

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun

effects <- c(effect1, effect2)

for (i in 3:30) {
  eff <- (effects[i-1] * lag1) + effects[1]
  effects <- c(effects, eff)
}

eff_25_1 <- effects[25]
eff_25_1

###Model 2

dem_shortrun <- coef(model2)["dem"]
lag1 <- (coef(model2)[2])
lag2 <- (coef(model2)[3])

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun

effects <- c(effect1, effect2, effect3)

### Model 3

dem_shortrun <- coef(model3)["dem"]
lag1 <- (coef(model3)[2])
lag2 <- (coef(model3)[3])
lag3 <- (coef(model3)[4])
lag4 <- (coef(model3)[5])

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun
effect4 <- (effect3 * lag1) + (effect2 * lag2) + (effect1 * lag3) + dem_shortrun

effects <- c(effect1,effect2, effect3, effect4)

for (i in 5:30) {
  eff <- (effects[i-1] * lag1) + (effects[i-2] * lag2) + (effects[i-3] * lag3) + (effects[i-4] * lag4) + dem_shortrun
  effects <- c(effects, eff)
}

eff_25_3 <- effects[25]
eff_25_3

### Model 4

dem_shortrun <- coef(model4)["dem"]
lag1 <- (coef(model4)[2])
lag2 <- (coef(model4)[3])
lag3 <- (coef(model4)[4])
lag4 <- (coef(model4)[5])
lag5 <- (coef(model4)[6])
lag6 <- (coef(model4)[7])
lag7 <- (coef(model4)[8])
lag8 <- (coef(model4)[9])

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun
effect4 <- (effect3 * lag1) + (effect2 * lag2) + (effect1 * lag3) + dem_shortrun
effect5 <- (effect4 * lag1) + (effect3 * lag2) + (effect2 * lag3) + (effect1 * lag4) + dem_shortrun
effect6 <- (effect5 * lag1) + (effect4 * lag2) + (effect3 * lag3) + (effect2 * lag4) + (effect1 * lag5) + dem_shortrun
effect7 <- (effect6 * lag1) + (effect5 * lag2) + (effect4 * lag3) + (effect3 * lag4) + (effect2 * lag5) + (effect1 * lag6) + dem_shortrun
effect8 <- (effect7 * lag1) + (effect6 * lag2) + (effect5 * lag3) + (effect4 * lag4) + (effect3 * lag5) + (effect2 * lag6) + (effect1 * lag7) + dem_shortrun

effects <- c(effect1, effect2, effect3, effect4, effect5, effect6, effect7, effect8)

for (i in 9:30) {
  eff <- (effects[i-1] * lag1) + (effects[i-2] * lag2) + (effects[i-3] * lag3) + (effects[i-4] * lag4) + (effects[i-5] * lag5) + (effects[i-6] * lag6) + (effects[i-7] * lag7) + (effects[i-8] * lag8) + dem_shortrun
  effects <- c(effects, eff)
}

eff_25_4 <- effects[25]
eff_25_4

eff_25 <- c(eff_25_1, eff_25_2, eff_25_3, eff_25_4)
eff_25 <- round(eff_25, 3)
eff_25

## Regression Tables

models <- list(model1, model2, model3, model4)

stargazer(models, 
          type = "latex", #change to "text" for easy viewing in console
          out = "output/table_fe.tex",
          title = "Effect of Democracy on (Log) GDP per Capita", # title
          dep.var.labels = "Log GDP per Capita",  
          covariate.labels = c("Democracy"),
          omit.stat = c("ser", "f","adj.rsq"), 
          keep.stat = c("n", "N"), #keep observations,
          omit = c("lag6", "lag7", "lag8"),
          model.numbers = FALSE, # remove model numbers (are the same as column labels)
          column.labels = c("(1)", "(2)", "(3)", "(4)"), # add column labels
          add.lines = list( # add lines for persistence, long run effect and effect after 25 years
            c("Persistence: ", pers), 
            c("Long run effect: ", lre),
            c("Effect after 25 years: ", eff_25)
          ),
          notes = "The reported coefficient on democracy is multiplied by 100. Replication of Table 2 in ANRR (2019)",
          notes.append = FALSE # add notes to the end of the table
)




