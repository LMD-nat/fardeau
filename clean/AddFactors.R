### Prepare analysis file ###

##### Import #####
library(readr)
library(readxl)
library(dplyr)
library(psych)
options(scipen=999)

triage <- read_csv("triage.csv")
analysis <- read_excel("analysis.xlsx")
analysis <- dplyr::left_join(analysis, triage, by = 'id_record')

##### Program the COVID-19 Wave Period #####

w0 <- seq.Date(
  from = as.Date("2018-03-01"),
  to   = as.Date("2020-02-24"),
  by   = "days"
)  


w1 <- seq.Date(
  from = as.Date("2020-02-25"),
  to   = as.Date("2020-07-11"),
  by   = "days"
)

between_w1_w2 <- seq.Date(
  from = as.Date("2020-07-12"),
  to   = as.Date("2020-08-22"),
  by   = "days"
)

w2 <- seq.Date(
  from = as.Date("2020-08-23"),
  to   = as.Date("2021-03-20"),
  by   = "days"
)

w3 <- seq.Date(
  from = as.Date("2021-03-21"),
  to   = as.Date("2021-07-17"),
  by   = "days"
)

w4 <- seq.Date(
  from = as.Date("2021-07-18"),
  to   = as.Date("2021-12-21"),
  by   = "days"
)

w0 <- data.frame(w0)
w0$period <- "0"
names(w0)[1] ="date"

w1 <- data.frame(w1)
w1$period <- "1"
names(w1)[1] ="date"

w1w2 <- data.frame(between_w1_w2)
w1w2$period <- "2"
names(w1w2)[1] ="date"

w2 <- data.frame(w2)
w2$period <- "3"
names(w2)[1] ="date"

w3 <- data.frame(w3)
w3$period <- "4"
names(w3)[1] ="date"

w4 <- data.frame(w4)
w4$period <- "5"
names(w4)[1] ="date"

time_periods <- do.call("rbind", list(w0, w1, w1w2, w2, w3, w4))
time_periods$covid_date <- time_periods$date
analysis$dds <- as.character(analysis$dds)

# turn dates into strings and then match by the dates
time_periods$covid_date <- as.character(time_periods$covid_date)
analysis <- inner_join(analysis, time_periods, by = c("dds" = "covid_date"))

##### Numerics #####

analysis$charlson_score <- analysis$age_comorb + analysis$infarctus + analysis$congestive_heart_failure + analysis$peripheral_vascular +
  analysis$cva_or_tia + analysis$dementia + analysis$mpoc + analysis$connective_tissue_disease + analysis$peptic_ulcer_disease +
  analysis$liver_disease + analysis$leukemia + analysis$diabetes_mellitus + analysis$hemiplegia + analysis$chronic_kidney_disease + 
  analysis$solid_tumor + analysis$lymphoma + analysis$aids

analysis$charlson_score_adj <- analysis$infarctus + analysis$congestive_heart_failure + analysis$peripheral_vascular +
  analysis$cva_or_tia + analysis$dementia + analysis$mpoc + analysis$connective_tissue_disease + analysis$peptic_ulcer_disease +
  analysis$liver_disease + analysis$leukemia + analysis$diabetes_mellitus + analysis$hemiplegia + analysis$chronic_kidney_disease + 
  analysis$solid_tumor + analysis$lymphoma + analysis$aids

##### Recategorize #####

analysis$pt_sex <- ifelse(analysis$pt_sex == "Homme", "Man", "Woman")
analysis$cg_sex <- ifelse(analysis$cg_Sexe == "Masculin", "Man", "Woman")
analysis$med_fam <- ifelse(analysis$medfam == 1, "Yes", "No")
analysis$rv_med_fam <- ifelse(analysis$`Pouvez-vous avoir un RDV rapidement en cas de besoin ?` == 1, "Yes", "No")
analysis$transport <- ifelse(analysis$`Avez-vous accès à un transport pour vos déplacements et rendez-vous?`  == 1, "Yes", "No")
#period ok
analysis$provenance <- ifelse(analysis$provenance %in% c("Clinique médicale", "CLSC", "Contrôle-Follow-up", "Info-santé", "Groupe de médecine familiale", "Transfert d'hôpital"), "Another medical institution",
                                       ifelse(analysis$provenance %in% c("Domicile", "Maison"), "Home",
                                              ifelse(analysis$provenance %in% c("Résidence personne agées-RI-RTF", "Résidence personnes agées- RI -RTF"), "Retirement home", "Unknown")))
#triage missing
analysis$pt_education <- ifelse(analysis$`Plus haut degré de scolarité complété:` %in% c("Inconnu / manquant", "Ne préfère pas répondre", "Études primaires"), "Primary school",
                                                   ifelse(analysis$`Plus haut degré de scolarité complété:` %in% c("Études secondaires (DES)", "Formation professionnelle (DEP, ASP)", "Études collégiales (DEC)"), "Secondary school",
                                                          ifelse(analysis$`Plus haut degré de scolarité complété:` %in% c("Baccalauréat", "Études de cycles supérieurs (2e ou 3e cycle)"), "University", NA)))
analysis$cg_education <- ifelse(analysis$`cg_Plus haut degré de scolarité complété` %in% c("Inconnu / manquant", "Ne préfère pas répondre", "Études primaires"), "Primary school",
                                                      ifelse(analysis$`cg_Plus haut degré de scolarité complété` %in% c("Études secondaires (DES)", "Formation professionnelle (DEP, ASP)", "Études collégiales (DEC)"), "Secondary school",
                                                             ifelse(analysis$`cg_Plus haut degré de scolarité complété` %in% c("Baccalauréat", "Études de cycles supérieurs (2e ou 3e cycle)"), "University", NA)))
analysis$cg_pt_relation <- ifelse(analysis$`cg_Relation avec le patient (participant)` == "Conjoint/Conjointe", "Spouse",
                                  ifelse(analysis$`cg_Relation avec le patient (participant)` == "Fils/Fille", "Parent-Child", "Other family"))


analysis$pt_revenue <- ifelse(grepl("Moins de 10 000\\$|10 000 à 19 999\\$|20 000 à 29 999\\$", analysis$pt_revenue), "< 30,000$",
                              ifelse(grepl("30 000 à 39 999\\$|40 000 à 49 999\\$|50 000 à 59 999\\$|60 000 à 69 999\\$|70 000 à 79 999\\$|80 000 à 89 999\\$|90 000 à 99 999\\$|Plus de 100 000\\$", analysis$pt_revenue), "> or equal to 30,000$", "No response"))

analysis$cg_revenue <- ifelse(grepl("Moins de 10 000\\$|10 000 à 19 999\\$|20 000 à 29 999\\$|30 000 à 39 999\\$|40 000 à 49 999\\$", analysis$cg_revenue), "< 50,000$",
                              ifelse(grepl("50 000 à 59 999\\$|60 000 à 69 999\\$|70 000 à 79 999\\$|80 000 à 89 999\\$|90 000 à 99 999\\$|Plus de 100 000\\$", analysis$cg_revenue), "> or equal to 50,000$", "No response"))

analysis$pt_residence <- ifelse(analysis$pt_residence %in% c("Centre hospitalier de soins longue durée (CHSLD)", "Résidence privée pour personnes âgées avec présence infirmière 24h/24", "Résidence privée pour personnes âgées sans infirmière sur place"), "Care home",
                              ifelse(analysis$pt_residence %in% c("Domicile, partagé"), "Home",
                                     ifelse(analysis$pt_residence %in% c("Domicile, seul", "Habitation à loyer modique (HLM)", "Ressources intermédiaires ou de type familial (RI ou RTF)"), "Home, alone", "Unknown")))

analysis$cg_residence <- ifelse(analysis$cg_residence %in% c("Centre hospitalier de soins longue durée (CHSLD)", "Résidence privée pour personnes âgées avec présence infirmière 24h/24", "Résidence privée pour personnes âgées sans infirmière sur place"), "Care home",
                                ifelse(analysis$cg_residence %in% c("Domicile, partagé"), "Home",
                                       ifelse(analysis$cg_residence %in% c("Domicile, seul", "Habitation à loyer modique (HLM)", "Ressources intermédiaires ou de type familial (RI ou RTF)"), "Home, alone", "Home, alone")))



r <- analysis %>% dplyr::select(id_record, "zbi_score", "pt_sex", "pt_age", "arrival_method", "triage", "hospital", "time_stretcher_hrs", 
                         "triage_delay", "provenance", "Visits365", "Visits183", "EDrevisit30", "EDrevisit14", 
                         "EDrevisit7", "EDrevisit3", "soutien_social", "period", "charlson_score", 
                         "charlson_score_adj", "cg_sex", "cg_age", "med_fam", "rv_med_fam", "transport", "pt_education",                                                        
                         "cg_education", "cg_pt_relation", "pt_revenue", "cg_revenue", "pt_residence", "cg_residence") 

r$triage_delay[is.na(r$triage_delay)] <- 0
r$triage_delay <- as.numeric(r$triage_delay)

##### Re-level Factor Variables #####
# Choose reference categories (1. normative), (2. largest category), (3. mean in the middle or extremes)
r$pt_sex <- as.factor(r$pt_sex)
levels(r$pt_sex) <- c("Woman", "Man")

r$cg_sex <- as.factor(r$cg_sex)
levels(r$cg_sex) <- c("Woman", "Man")

r$hospital <- as.factor(r$hospital)
levels(r$hospital) <- c("HDL", "Montmagny", "Beauce (St-Georges)", "Thetford")

r$med_fam <- as.factor(r$med_fam)
levels(r$med_fam) <- c("Yes", "No")

r$rv_med_fam <- as.factor(r$rv_med_fam)
levels(r$rv_med_fam) <- c("Yes", "No")

r$transport <- as.factor(r$transport)
levels(r$transport) <- c("Yes", "No")

r$period <- as.factor(r$period)
levels(r$period) <- c(0, 1, 2, 3, 4, 5)

r$provenance <- as.factor(r$provenance)
levels(r$provenance) <- c("Unknown", "Home", "Retirement home", "Another medical institution")

r$arrival_method <- as.factor(r$arrival_method)
levels(r$arrival_method) <- c("Ambulance", "Ambulant")

r$triage <- as.factor(r$triage)
levels(r$triage) <- c(5, 4, 3, 2)

r$pt_education <- as.factor(r$pt_education)
levels(r$pt_education) <- c("Primary school", "Secondary school", "University")

r$cg_education <- as.factor(r$cg_education)
levels(r$cg_education) <- c("Primary school", "Secondary school", "University")

r$cg_pt_relation <- as.factor(r$cg_pt_relation)
levels(r$cg_pt_relation) <- c("Other family", "Spouse", "Parent-Child")

r$pt_revenue <- as.factor(r$pt_revenue)
levels(r$pt_revenue) <- c("No response", "< 30,000$", "> or equal to 30,000$")

r$cg_revenue <- as.factor(r$cg_revenue)
levels(r$cg_revenue) <- c("No response", "< 50,000$", "> or equal to 50,000$")

r$provenance <- as.factor(r$provenance)
levels(r$provenance) <- c("Unknown", "Another medical institution", "Home", "Retirement home")

r$pt_residence <- as.factor(r$pt_residence)
levels(r$pt_residence) <- c("Home", "Home, alone", "Care home")

r$cg_residence <- as.factor(r$cg_residence)
levels(r$cg_residence) <- c("Home", "Home, alone", "Care home")

r$provenance <- as.factor(r$provenance)
levels(r$provenance) <- c("Unknown", "Another medical institution", "Home", "Retirement home")

# check types
sapply(r, class)

##### Save #####
write_rds(r, "~/Desktop/ready.RData")
