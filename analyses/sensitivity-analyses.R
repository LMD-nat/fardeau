### Sensitivity Analyses

library(readr)
library(readxl)
library(dplyr)
library(survival)

r <- read_rds("~/Desktop/ready.RData")
adm <- read_excel("admission_data.xlsx")
r2 <- left_join(r, adm, by = 'id_record')
cgptdates <- read_csv("~/Downloads/cgptdates.csv")
r3 <- left_join(r2, cgptdates, by = 'id_record')

r_before <- subset(r3, result == FALSE)
r_after <- subset(r3, result == TRUE)

m_standard <- glm(EDrevisit30 ~ zbi_score +
                  Visits365 + period + zbi_score*period + Visits365*period, 
                family = "binomial", data = r3)

m_before <- glm(EDrevisit30 ~ zbi_score +
                Visits365 + period + zbi_score*period + Visits365*period, 
                family = "binomial", data = r_before)

m_after <- glm(EDrevisit30 ~ zbi_score +
                  Visits365 + period + zbi_score*period + Visits365*period, 
                family = "binomial", data = r_after)

summary(m_standard)
summary(m_before)
summary(m_after)

survival::concordance(m_standard)
survival::concordance(m_before)
survival::concordance(m_after)
