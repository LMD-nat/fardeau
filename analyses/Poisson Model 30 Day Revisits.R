### Thirty day revisits: Poisson regression model ###

# Clear the environment
rm(list = ls())

# Load all the packages in one shot
x <- c(
  "tidyverse",
  "aod",
  "lubridate",
  "readr",
  "readxl",
  "table1",
  "ggplot2",
  "MASS",
  "lmtest",
  "magrittr",
  "energy",
  "epiDisplay",
  "AER"
)
lapply(
  x,
  FUN = function(X) {
    do.call("require", list(X))
  }
)
options(scipen = 999)

# Import data
r <- read_rds("~/Desktop/ready.RData")
adm <- read_excel("~/Desktop/admission_data.xlsx")

# Organize
r2 <- left_join(r, adm, by = 'id_record') #ED30Count is Y
r2$triage_delay[is.na(r2$triage_delay)] <- 0

# Select variables
r1 <- r2 %>% dplyr::select(
  ED30Count,
  zbi_score,
  pt_sex ,
  pt_age,
  arrival_method,
  triage,
  hospital,
  time_stretcher_hrs,
  triage_delay,
  provenance,
  Visits365,
  soutien_social,
  period,
  charlson_score_adj,
  cg_sex,
  med_fam,
  rv_med_fam,
  transport,
  pt_education,
  cg_education,
  cg_pt_relation,
  pt_revenue,
  cg_revenue,
  pt_residence,
  cg_residence
)

### --- Model 1
poisson.model <- glm(ED30Count ~ ., family = 'poisson', data = r1)
summary(poisson.model) # hospital, triage, ED visits, ZBI and provenance

# Model fit of model 1
# 1 - pchisq(deviance(poisson.model1), df = poisson.model1$df.residual)

### --- Model 2
poisson.model2 <-
  glm(
    ED30Count ~ zbi_score + provenance + hospital + triage + Visits365,
    family = 'poisson',
    data = r1
  )
summary(poisson.model2)

# Model fit of model 2
1 - pchisq(deviance(poisson.model2), df = poisson.model2$df.residual)

### --- Model 3
stepAIC(poisson.model, direction = 'backward', k = log(dim(r1)[1])) ### Stepwise selection poisson model
# glm(formula = ED30Count ~ zbi_score + Visits365, family = "poisson", data = r1)

poisson.model3 <-
  glm(
    formula = ED30Count ~ zbi_score + Visits365,
    family = "poisson",
    data = r1
  )
summary(poisson.model3)

# Model fit of model 3
pchisq(poisson.model3$deviance,
       df = poisson.model3$df.residual,
       lower.tail = FALSE)

### --- Assumption of dispersion
# In GLM, model misfits often induce over-dispersion: the variance is greater than what the model predicts.
# Model 1
dispersiontest(poisson.model)
# Model 2
poisson.model2 <-
  glm(
    formula = ED30Count ~ zbi_score + provenance + hospital +
      triage + Visits365,
    family = "poisson",
    data = r1
  )
dispersiontest(poisson.model2)
# Model 3
poisson.model3 <-
  glm(
    formula = ED30Count ~ zbi_score + Visits365,
    family = "poisson",
    data = r1
  )
dispersiontest(poisson.model3)

# Check data fit for model 2
Pearson <- sum((r1$ED30Count - poisson.model2$fitted.values) ^ 2
               / poisson.model2$fitted.values)
1 - pchisq(Pearson, df = poisson.model2$df.residual)

# Check data fit for model 3
Pearson <- sum((r1$ED30Count - poisson.model3$fitted.values) ^ 2
               / poisson.model3$fitted.values)
1 - pchisq(Pearson, df = poisson.model3$df.residual)

# Note: Quasipoisson models might be preferred when there's evidence of overdispersion in the data.

### --- Negative binomial models

# Note: NB models accommodate variability in the data better than the Poisson model
# by allowing for different mean and variance parameters.
# And they help accounf for many zounts of zero within the dataset

# Model 1
poisson.modelnb1 <-
  MASS::glm.nb(formula = ED30Count ~ .,
               init.theta = 1.176937,
               data = r1)
summary(poisson.modelnb1)

# Model 2
poisson.modelnb2 <-
  MASS::glm.nb(
    formula = ED30Count ~ zbi_score + provenance + hospital +
      triage + rv_med_fam + Visits365,
    init.theta = 1.199046,
    data = r1
  )
summary(poisson.modelnb2)

# Model 3
poisson.modelnb3 <-
  MASS::glm.nb(
    formula = ED30Count ~ zbi_score +  Visits365,
    init.theta = 1.236356,
    data = r1
  )
summary(poisson.modelnb3)

# Model Fits

# M2
1 - pchisq(942.37, 1396)
# M3
1 - pchisq(933.76, 1406)
# Both fit the data fine

# When the ratio of residual deviance to
# the df for a model is equal to or approximately 1.00
# then the model fit is acceptable.*
# Allison PD, Waterman RP (2002) Fixed effects negative binomial
# regression models. Sociol Methodol 32, 247-265.

# Goodness of fit
# RateRatioZBI = exp(0.019493)
# RateRatioED365 = exp(0.115412)
poisgof(poisson.modelnb2)
poisgof(poisson.modelnb3)

# We need to use modelnb2
lrtest(poisson.modelnb3, poisson.modelnb2)

### Final Model: Negative Binomial Model 2 ###
summary(poisson.modelnb2)
