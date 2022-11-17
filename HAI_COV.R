#############################################################################################################################
## HAI_COV.R                                                                                                               ##
## Detection of healthcare associated infections(HAI) of SARS-CoV-2 in Norwegian hospitals                                 ##
#############################################################################################################################
## Author: Håvard Skagseth                                                                                                 ##
## Idea by: Petter Elstrøm, Oliver Kacelnik and Hanne-Merete Eriksen-Volle originally created in Stata                     ##
#############################################################################################################################

#Libraries
library(RODBC)
library(tidyverse)
library(lubridate)
library(haven)
library(tsibble)

# max No. of days from discharge to positive test we're interested in keeping
inc_time = 7
# Last date we are looking at for hospital stays and positive tests
sluttdato = as.Date("2022-09-18") ##Sys.Date()

####################################
## Data Collection and washing    ##
####################################

#Making SQL handle
dbhandle <- # *Create SQL-handle*

# SQL code for getting the hospital stays and positive tests
text = # *SQL Query that gets data on all hospital stays for people that has tested positive for SARS-CoV-2* including admission date, discharge date, positive test and details on the hospital stay
# Admission date: innDatoTid
# Getting hospital stays combined with positive tests
npr_msis <- as_tibble(sqlQuery(dbhandle, text, as.is=TRUE))%>%
  filter(!is.na(innDatoTid),
         omsorgsniva == 1)%>%
  mutate(rhf = case_when(helseforetakOrgNr %in% c("983974880","983974899","983974910","983974929","983974937","918177833") ~ 1,
                         helseforetakOrgNr %in% c("883974832","997005562","983974791","983974805") ~ 2,
                         helseforetakOrgNr %in% c("983974732","983974724","983974694","983974678","983974716","987601787", "984027737") ~ 3, 
                         helseforetakOrgNr %in% c("983971636","993467049","883971752","992281618","983975259","983971709","983975267","983971768","914637651","983975240","894166762", "965985166","982791952") ~ 4,
                         TRUE ~ 5))%>%
  mutate(aldermsis = year(Prøvedato) - year(yearmonth(Prøvedato)-as.integer(AlderMåneder)))%>%
  mutate(alderkat_msis = case_when(aldermsis>= 0 & aldermsis < 10 ~ "0-9",aldermsis>= 10 & aldermsis < 20 ~ "10-19",
                                   aldermsis>= 20 & aldermsis < 30 ~ "20-29",aldermsis>= 30 & aldermsis < 40 ~ "30-39",
                                   aldermsis>= 40 & aldermsis < 50 ~ "40-49",aldermsis>= 50 & aldermsis < 60 ~ "50-59",
                                   aldermsis>= 60 & aldermsis < 70 ~ "60-69",aldermsis>= 70 & aldermsis < 80 ~ "70-79",
                                   aldermsis>= 80 & aldermsis < 90 ~ "80-89",aldermsis>= 90 ~ "90+"))%>%
  group_by(persId_hash, Prøvedato)%>%
  arrange(innDatoTid)%>%
  mutate(utDatoTid = case_when(is.na(utDatoTid) & as.Date(innDatoTid) %in% seq(as.Date("2020-12-24"), by = "day", length.out = 8) ~ "2021-01-01",
                               is.na(utDatoTid) & as.Date(innDatoTid) %in% seq(as.Date("2021-12-24"), by = "day", length.out = 8) ~ "2022-01-01",
                               is.na(utDatoTid) ~ lead(innDatoTid),
                               TRUE ~ utDatoTid))%>%
  mutate(utDatoTid = case_when(is.na(utDatoTid) ~ as.character(Sys.Date()), TRUE ~ utDatoTid))%>%
  ungroup()

# Data to get which hospital, ward and specialty

npr_RefEnhet_sted <- as_tibble(sqlQuery(dbhandle, # *SQL Query to get which hospital the stay is at*
                                        as.is=TRUE))%>%
  filter(nprEpisodeid %in% npr_msis$nprEpisodeid)
npr_RefEnhet_avdeling <- as_tibble(sqlQuery(dbhandle,# *SQL Query to get which ward the stay is at*
                                            as.is=TRUE))%>%
  filter(nprEpisodeid %in% npr_msis$nprEpisodeid)

enhetsektor <- as_tibble(sqlQuery(dbhandle, # *SQL Query to get mapping to get which hospital and ward each stay is at* 
                                  ))%>%
  distinct()
npr_msis <- npr_msis%>%
  left_join(npr_RefEnhet_sted)%>%
  left_join(npr_RefEnhet_avdeling)%>%
  left_join(enhetsektor)

# Somatic hospital stays
npr_msis_somatikk <- npr_msis%>%
  filter(is.na(sektor) | sektor == "som/annet" & !str_detect(avdeling_enhetlokal,regex("østmarka spesialpost", ignore_case = TRUE)))

# Stays at drug dependency and psychiatric institutions
npr_msis_rus_psy <- npr_msis %>%
  filter(!is.na(sektor) & (sektor %in% c("rus", "psy") | str_detect(avdeling_enhetlokal,regex("østmarka spesialpost", ignore_case = TRUE))))

npr_msis <- npr_msis_somatikk


npr_msis <- npr_msis%>%
  distinct(nprEpisodeid, Prøvedato, .keep_all = T)%>%
  # Making a variable to say which stays to keep
  mutate(keep = 1)

# Filtering stays within stays and combining stays one calendar day or less apart
npr_msis <- npr_msis%>%
  arrange(persId_hash, Prøvedato,innDatoTid)%>%
  group_by(persId_hash,Prøvedato)%>%
  mutate(keep = case_when(as.Date(utDatoTid) <= as.Date(lag(utDatoTid)) ~ 0,
                          TRUE ~ 1))%>%
  filter(keep == 1)%>%
  mutate(keep = case_when(as.Date(innDatoTid)-1 <= as.Date(lag(utDatoTid)) ~ 0, 
                          TRUE ~ 1))%>%
  ungroup()%>%
  mutate(opphold = cumsum(keep))%>% 
  group_by(opphold)%>%
  mutate(utDatoTid = last(utDatoTid))%>%
  ungroup()%>%
  filter(keep == 1)%>%
  select(-opphold)

# Age and gender
npr_msis <- npr_msis%>%
  mutate(age = year(Prøvedato)-fodselsar,
         Kjønn = case_when(kjonn == 1 ~ "Male", kjonn == 2 ~ "Female"))

##################################################################################################################################
## ANALYSES                                                                                                                     ##
##################################################################################################################################
## At this point we have a dataset (npr_msis) that contains all the necessary information to do the analyses                    ##
##                                                                                                                              ##
## Datastructure                                                                                                                ##
## Person ID            : persid_hash                                                                                           ##
## Admission date       : innDatoTid                                                                                            ##
## Discharge date       : utDatoTid                                                                                             ##
## Positive test date   : Prøvedato                                                                                             ##
## Age                  : age                                                                                                   ##
## Sex                  : Kjønn                                                                                                 ##
## Hospital             : behandlingssted_enhetlokal                                                                            ##
## Hospital ward        : avdeling_enhetlokal                                                                                   ##
##################################################################################################################################


# Classifying Defnite HAI(4, 9), Probable HAI (3, 7), Indeterminate HAI (2, 6, 8) and community acquired (1,5), change test_time_in to modify the definition of Definite, probable, indeterminate and not HAI
npr_msis2 <- npr_msis %>%
  #removing stays for people that didn't have positive tests
  filter(!is.na(Prøvedato))%>%
  # number of days hospitalized
  mutate(days_hosp = as.Date(utDatoTid)-as.Date(innDatoTid)) %>%
  # removing stays where day of admission and discharge are the same
  filter(days_hosp != 0)%>%
  # keep every stay where positive test date is from one day after admission to 7 days after discharge
  filter(as.Date(Prøvedato) > as.Date(innDatoTid) & as.Date(Prøvedato) - inc_time <= as.Date(utDatoTid))%>%
  # number of days testing positive after admission and number of days before or after discharge
  mutate(test_time_in = as.Date(Prøvedato)-as.Date(innDatoTid)) %>%
  mutate(test_time_out = as.Date(Prøvedato)-as.Date(utDatoTid)) %>%
  # whether the patient tested positive before (1) or after (2) discharge
  mutate(hai_code = ifelse(test_time_out>0,2,1)) %>%
  # Classifying Defnite HAI(4, 9), Probable HAI (3, 7), Indeterminate HAI (2, 6, 8) and community acquired (1,5)
  mutate(hai_pred = case_when(hai_code == 1 & test_time_in <= 1 ~ 1,
                              hai_code == 1 & test_time_in >= 2 & test_time_in <= 4 ~ 2,
                              hai_code == 1 & test_time_in >= 5 & test_time_in <= 7 ~ 3,
                              hai_code == 1 & test_time_in >= 8 ~ 4,
                              hai_code == 2 & test_time_out == 1 & test_time_in <= 2 ~ 5,
                              hai_code == 2 & test_time_out == 2 & test_time_in <= 3 ~ 5,
                              hai_code == 2 & test_time_out == 1 & test_time_in >= 3 & test_time_in <= 5 ~ 6, 
                              hai_code == 2 & test_time_out == 2 & test_time_in >= 4 & test_time_in <= 6 ~ 6,
                              hai_code == 2 & test_time_out == 1 & test_time_in >= 6 & test_time_in <= 7 ~ 7,
                              hai_code == 2 & test_time_out == 2 & test_time_in >= 7 ~ 7,
                              hai_code == 2 & test_time_out >= 3 ~ 8,
                              hai_code == 2 & test_time_out == 1 & test_time_in >= 8 ~ 9)) %>%
  mutate(hai_pred = factor(hai_pred,levels = c(4,9,3,7,2,6,8,1,5))) %>%
  # hai_pred = 1 During stay, no hospital onset, 2 during stay indeterminate hospital onset, 3 during stay probable/definate hospital onset,
  # 5 < 48h after stay no hospital onset, 6 <48h after stay indeterminate hospital onset, 7<48h after stay probable hospital onset
  # 8 3+days after stay indeterminate hospital onset
  arrange(hai_pred)%>%
  # remove multiple HAI predictions if there are more than one per positive test, keeping the one with highest likelyhood of being a HAI
  distinct(persId_hash, Prøvedato, .keep_all = TRUE)%>%
  # Age, age category and gender
  mutate(age = year(Prøvedato) - fodselsar,
         Kjønn = case_when(kjonn == 1 ~ "Male",
                           kjonn == 2 ~ "Female"),
         age_kat = case_when(age >= 0 & age < 10 ~ "0-9", age >= 10 & age < 20 ~ "10-19", age >= 20 & age < 30 ~ "20-29", age >= 30 & age < 40 ~ "30-39",
                             age >= 40 & age < 50 ~ "40-49", age >= 50 & age < 60 ~ "50-59", age >= 60 & age < 70 ~ "60-69", age >= 70 & age < 80 ~ "70-79",
                             age >= 80 & age < 90 ~ "80-89", age >= 90 ~ "90+"))


# Datasets with:
# Probable HAI
prob <- npr_msis2 %>%
  filter(hai_pred %in% c(3,7))
# Probable and Definite HAI
prob_def <- npr_msis2 %>%
  filter(hai_pred %in% c(3,4,7,9))
# Probable, Definite and Indeterminate HAI
prob_def_indet <- npr_msis2 %>%
  filter(hai_pred %in% c(2,3,4,6,7,8,9))
# Definite HAI
definite <- npr_msis2%>%
  filter(hai_pred %in% c(4,9))
# Community AI
nohai <- npr_msis2%>%
  filter(hai_pred %in% c(1,5))
# Indeterminate HAI
indet <- npr_msis2 %>%
  filter(hai_pred %in% c(2,6,8))


# A plot of all Probable and Definite HAI per week
ukeplot <- prob_def%>%
  ggplot(aes(x = yearweek(Prøvedato)))+
  geom_bar(stat = "count")+
  labs(x = "Week", y = "Cases")
ukeplot
