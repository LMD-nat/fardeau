# Revisions Round 2 #

# 1. Import
a <- analysis
triage <- read_csv("triage.csv")
adm <- read_excel("admission_data.xlsx")
a <- left_join(a, adm, by = 'id_record')
a <- left_join(a, triage, by = 'id_record')

# 2. Results section: demographics
# Count types of revisits
table(a$EDrevisit30)
table(a$EDrevisit7)
table(a$EDrevisit3)
table(a$admission)

# Count sexes
table(a$pt_sex)
table(a$cg_Sexe) #check
table(a$cg_sex)

# Count relationships
table(a$`cg_Relation avec le patient (participant)`)
table(a$cg_pt_relation)

# mean and sd ages
# pts
mean(a$pt_age)
sd(a$pt_age)
# pas
mean(a$cg_age)
sd(a$cg_age)

# 3. Check and set reference levels
a$triage_delay[is.na(a$triage_delay)] <- 0
a$triage.x <- as.factor(a$triage.x)
levels(a$triage.x) <- c(5, 4, 3, 2)
a$cg_residence.x <- relevel(factor(a$cg_residence.x), ref = "Home")
a$pt_residence.x <- relevel(factor(a$pt_residence.x), ref = "Home")
a$cg_revenue <- relevel(factor(a$cg_revenue), ref = "No response")
a$pt_sex_edit <- relevel(factor(a$pt_sex), ref = "Woman")
a$cg_revenue_edit <- relevel(factor(a$cg_revenue), ref = "> or equal to 50,000$")


# 4. Results section: table 1 checking

library(table1)
a$revisit30 <- factor(a$EDrevisit30)

render_cont_two_cells <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  
  c(
    "Mean (SD)" = sprintf("%.2f ± %.2f", m, s),
    "Median range)" = sprintf("%.2f (%.2f–%.2f)", med, minx, maxx)
  )
}


render_all_categorical <- function(x) {
  tbl <- table(x)
  pct <- prop.table(tbl) * 100
  out <- sprintf("%d (%.2f%%)", as.integer(tbl), pct)
  names(out) <- names(tbl)
  
  return(out)
}


table1(~ pt_sex + pt_age + arrival_method + triage.x + hospital.x + time_stretcher_hrs + triage_delay + Visits365 +
         soutien_social + period + charlson_score + cg_sex + cg_age + med_fam + rv_med_fam + transport + 
         pt_education + cg_education + cg_pt_relation + zbi_score + cg_revenue + pt_revenue + pt_residence.x +
         cg_residence.x | revisit30, data=a, overall = F, 
       render.continuous = render_all_continuous,
       render.categorical = render_all_categorical)


# 5. Results section: models

# Function to summarize final models for the paper
summarize_logistic_model <- function(model, use_profile_CI = FALSE) {
  model_summary <- summary(model)
  
  coefs <- coef(model)[-1]
  se <- coef(model_summary)[-1, "Std. Error"]
  pvals <- coef(model_summary)[-1, "Pr(>|z|)"]
  odds_ratios <- exp(coefs)
  
  if (use_profile_CI) {
    confint_logit <- confint(model)[-1, ]
    ci_lower <- exp(confint_logit[, 1])
    ci_upper <- exp(confint_logit[, 2])
  } else {
    z <- qnorm(0.975)
    ci_lower <- exp(coefs - z * se)
    ci_upper <- exp(coefs + z * se)
  }
  
  format2 <- function(x) formatC(x, format = "f", digits = 2)
  format3 <- function(x) formatC(x, format = "f", digits = 3)
  
  df <- data.frame(
    Variable = names(coefs),
    OR = format2(odds_ratios),
    CI = paste0("(", format2(ci_lower), ", ", format2(ci_upper), ")"),
    p_value = format3(pvals),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# Sets of final miltivariate models (Output)
# 3 days
summarize_logistic_model(m_ED3 <- glm(EDrevisit3 ~ triage.x + time_stretcher_hrs + zbi_score + Visits365 + cg_residence.x, data = a, family = "binomial"))
# 7 days
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ pt_sex + zbi_score + time_stretcher_hrs + Visits365 + pt_residence.x + cg_residence.x + triage.x, family = "binomial", data = a))
# 30 days
summarize_logistic_model(m_ED30 <- glm(EDrevisit30 ~ zbi_score + Visits365 + period + zbi_score*period + Visits365*period, family = "binomial", data = a))
# 30 day admissions
summarize_logistic_model(m_a30 <- glm(admission ~ arrival_method + zbi_score + charlson_score_adj + cg_revenue, family = "binomial", data = a))


# Sets of univariate models (Output), 3 days
summarize_logistic_model(m_ED3 <- glm(EDrevisit3 ~ zbi_score, data = a, family = "binomial"))
summarize_logistic_model(m_ED3 <- glm(EDrevisit3 ~ Visits365, data = a, family = "binomial"))
summarize_logistic_model(m_ED3 <- glm(EDrevisit3 ~ cg_residence.x, data = a, family = "binomial"))
summarize_logistic_model(m_ED3 <- glm(EDrevisit3 ~ triage.x, data = a, family = "binomial"))
summarize_logistic_model(m_ED3 <- glm(EDrevisit3 ~ time_stretcher_hrs, data = a, family = "binomial"))

# Sets of univariate models (Output), 7 days
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ zbi_score, data = a, family = "binomial"))
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ Visits365, data = a, family = "binomial"))
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ time_stretcher_hrs, data = a, family = "binomial"))
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ pt_sex, data = a, family = "binomial"))
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ pt_residence.x, data = a, family = "binomial"))
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ cg_residence.x, data = a, family = "binomial"))
summarize_logistic_model(m_ED7 <- glm(EDrevisit7 ~ triage.x, data = a, family = "binomial"))

# Sets of univariate models (Output), admisisons
summarize_logistic_model(m_a30 <- glm(admission ~ zbi_score, data = a, family = "binomial"))
summarize_logistic_model(m_a30 <- glm(admission ~ charlson_score_adj, data = a, family = "binomial"))
summarize_logistic_model(m_a30 <- glm(admission ~ arrival_method, data = a, family = "binomial"))
summarize_logistic_model(m_a30 <- glm(admission ~ cg_revenue, data = a, family = "binomial"))



