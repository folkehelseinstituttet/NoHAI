#############################################################################################################################
## Cluster analyses of outbreaks of SARS-CoV-2 in long-term care facilities                                                ##
#############################################################################################################################
## Author: Petter Sunde Nymark                                                                                             ##
## Code reviewed and edited by: Anders Skyrud Danielsen                                                                    ##
#############################################################################################################################


library(tidyverse)
library(RODBC)
library(lubridate)
library(readxl)
library(openxlsx)

source("../sykehjem_populasjon_generering/scripts/IPLOS_populasjon_gen.R", encoding = "UTF-8")

# This script detects outbreaks occurring in the current long-term care facility population
# The population is based on registered users in the dataset "NAV Institution"

# An omitted code chunk accessed the data, through the following steps:
# 1: Create a database handle and access the database
# 2: Log the access
# 3: Load positive tests by person ID from MSIS (object: msis)
# 4: Load all laboratory tests by person ID from the laboratory database (object: lab_results)
# 5: Load sequencing results by person ID (object: virsus_variant)
# 6: Load vaccine doses by person ID (object: sysvak)
# 7: Load the LTCF population (object: mydata)

## Vaccine doses
# Use SYSVAK data linked with population data to decide first dose, second dose, third dose etc for each person
# Days allowed between first and second dose and days between second and third dose can be set on the top of the script

# --- Code chunk by Håvard Mikal Skagseth ---

dager_mellom_dose_1_2 = 19
dager_mellom_dose_2_3 = 150


all_vaccines <- sysvak %>%
  filter(persId_hash != "",
         persId_hash %in% mydata$persId_hash
         #Konsultasjonsdato >= "2020-01-01"
  ) %>%
  mutate(vaksinenr = case_when(VaksineKode == "BNT03" ~ 1, VaksineKode == "MOD03" ~ 2,VaksineKode == "ASZ03" ~ 3,
                               VaksineKode == "JAN03" ~ 4, VaksineKode == "Ukjent" ~ 5)) %>%
  arrange(Konsultasjonsdato)

unique_ppl <- all_vaccines %>%
  filter(!duplicated(persId_hash))

unique_ppl$dose1 = all_vaccines$Konsultasjonsdato[match(unique_ppl$persId_hash,all_vaccines$persId_hash)]
unique_ppl$vaksinetype1 = all_vaccines$vaksinenr[match(unique_ppl$persId_hash,all_vaccines$persId_hash)]
dose1posi = match(unique_ppl$persId_hash,all_vaccines$persId_hash)

mellomlagring_vaksine <- all_vaccines %>%
  slice(-dose1posi)
unique_ppl$dose2 = mellomlagring_vaksine$Konsultasjonsdato[match(unique_ppl$persId_hash,mellomlagring_vaksine$persId_hash)]
unique_ppl$vaksinetype2 = mellomlagring_vaksine$vaksinenr[match(unique_ppl$persId_hash,mellomlagring_vaksine$persId_hash)]
dose2posi = match(mellomlagring_vaksine$persId_hash,mellomlagring_vaksine$persId_hash)
mellomlagring_vaksine <- mellomlagring_vaksine %>%
  slice(-dose2posi)
unique_ppl$dose3 = mellomlagring_vaksine$Konsultasjonsdato[match(unique_ppl$persId_hash,mellomlagring_vaksine$persId_hash)]
unique_ppl$vaksinetype3 = mellomlagring_vaksine$vaksinenr[match(unique_ppl$persId_hash,mellomlagring_vaksine$persId_hash)]
dose3posi = match(mellomlagring_vaksine$persId_hash,mellomlagring_vaksine$persId_hash)
mellomlagring_vaksine <- mellomlagring_vaksine %>%
  slice(-dose3posi)
unique_ppl$dose4 = mellomlagring_vaksine$Konsultasjonsdato[match(unique_ppl$persId_hash,mellomlagring_vaksine$persId_hash)]
unique_ppl$vaksinetype4 = mellomlagring_vaksine$vaksinenr[match(unique_ppl$persId_hash,mellomlagring_vaksine$persId_hash)]

unique_ppl <- unique_ppl %>%
  mutate(forstedose = dose1) %>%
  mutate(vaksine_1 = vaksinetype1) %>%
  mutate(andredose = case_when(!is.na(dose2) & !is.na(forstedose) & as.numeric(as.Date(dose2)) - as.numeric(as.Date(forstedose)) >= dager_mellom_dose_1_2 ~ dose2,
                               !is.na(dose3) & !is.na(forstedose) & as.numeric(as.Date(dose3) - as.Date(forstedose)) >= dager_mellom_dose_1_2 ~ dose3,
                               !is.na(dose4) & !is.na(forstedose) & as.numeric(as.Date(dose4) - as.Date(forstedose)) >= dager_mellom_dose_1_2 ~ dose4)) %>%
  mutate(vaksine_2 = case_when(!is.na(dose2) & !is.na(forstedose) & as.numeric(as.Date(dose2)) - as.numeric(as.Date(forstedose)) >= dager_mellom_dose_1_2 ~ vaksinetype2,
                               !is.na(dose3) & !is.na(forstedose) & as.numeric(as.Date(dose3) - as.Date(forstedose)) >= dager_mellom_dose_1_2 ~ vaksinetype3,
                               !is.na(dose4) & !is.na(forstedose) & as.numeric(as.Date(dose4) - as.Date(forstedose)) >= dager_mellom_dose_1_2 ~ vaksinetype4)) %>%
  mutate(tredjedose = case_when(!is.na(dose3) & !is.na(andredose) #& as.numeric(as.Date(dose3)>earliestd3)
                                & as.numeric(as.Date(dose3) - as.Date(andredose)) >= dager_mellom_dose_2_3 ~ dose3,
                                is.na(dose4) & !is.na(andredose) #& as.numeric(as.Date(dose4)>earliestd3)
                                & as.numeric(as.Date(dose4) - as.Date(andredose)) >= dager_mellom_dose_2_3 ~ dose4)) %>%
  mutate(vaksine_3 = case_when(!is.na(dose3) & !is.na(andredose) #& as.numeric(as.Date(dose3)>earliestd3)
                               & as.numeric(as.Date(dose3) - as.Date(andredose)) >= dager_mellom_dose_2_3 ~ vaksinetype3,
                               is.na(dose4) & !is.na(andredose) #& as.numeric(as.Date(dose4)>earliestd3)
                               & as.numeric(as.Date(dose4) - as.Date(andredose)) >= dager_mellom_dose_2_3 ~ vaksinetype4)) %>%

  mutate(fjerdedose = case_when(!is.na(dose4) & !is.na(tredjedose) & as.numeric(as.Date(dose4)- as.Date(tredjedose)) >= dager_mellom_dose_1_2 ~ dose4)) %>%
  mutate(vaksine_4 = case_when(!is.na(dose4) & !is.na(tredjedose) & as.numeric(as.Date(dose4)- as.Date(tredjedose)) >= dager_mellom_dose_1_2 ~ vaksinetype4))

mydata <- left_join(mydata, unique_ppl)

# --- end code chunk ---

population <- mydata %>%
  left_join(unique_ppl) %>%
  mutate_at(vars(kommune_nummer), as.double)

populasjon <- population %>%
  mutate(FraUke = isoweek(Inndato),
         TilUke = isoweek(now()))

pop_med_msis <- populasjon %>%
  left_join(msis)

positive_tester <- pop_med_msis %>%
  mutate_at(vars(Prøvedato, Inndato, Utdato, DDATO, andredose), date) %>%
  filter(!is.na(Orgnummer)) %>%
  mutate(dod_14 = case_when(DDATO - Prøvedato <= 14 & DDATO - Prøvedato >= 0 ~ TRUE,
                            is.na(DDATO) ~ NA,
                            TRUE ~ FALSE)) %>%
  mutate(dod_register = case_when((str_detect(ALLE_KODER, "U071") | str_detect(ALLE_KODER, "U072")) ~ TRUE,
                            is.na(DDATO) ~ NA,
                            TRUE ~ FALSE)) %>%
  select(persId_hash, yrkesnavn_08, PrøvedatoUke, Prøvedato, DDATO, ALLE_KODER, dod_14, dod_register, Orgnummer, Inndato, Utdato, er_beboer, kommune_nummer,
         forstedose, andredose, tredjedose) %>% # Removed organisation number
  mutate(Utdato = case_when((Utdato > DDATO) | is.na(Utdato) ~ DDATO,
                             (Utdato <= DDATO ) | is.na(DDATO) ~ Utdato,)) %>%
  arrange(Orgnummer, Prøvedato) %>%
  filter(Prøvedato < date("2022-01-01")) # Limits dates possible for positive tests


# General info --------------------------------------------------

# general_info is an overview of the population

general_info <- pop_med_msis %>%
  mutate_at(vars(Inndato, Utdato, DDATO), date) %>%
  filter(er_beboer == 1,
         isoyear(Inndato) >= isoyear("1900-01-01"),
         (Inndato < Utdato) | is.na(Utdato),
         Inndato < DDATO | is.na(DDATO)) %>%
  mutate(stay_length = case_when(is.na(DDATO) ~ Utdato - Inndato,
                                 !is.na(DDATO) ~ DDATO - Inndato)) %>%
  summarise(death_rate_2020_2021 = paste(100 * round(sum(!is.na(DDATO)) / n(), digits = 3), "%"),
            mean_stay = mean(stay_length, na.rm = TRUE),
            shortest_stay = min(stay_length, na.rm = TRUE),
            longest_stay = max(stay_length, na.rm = TRUE),
            n_full_period = n(),
            n_stay_length = sum(!is.na(stay_length)),
            n_no_stay_length = sum(is.na(stay_length)),
            n_no_utdato = sum(is.na(Utdato) & is.na(DDATO)),
            n_org = length(unique(Orgnummer)))

# We use the in and out dates to see if the individual is present in a given period which consists of four months, to count the population.
# We use the first and second dose variables to see how many are vaccinated in each of the periods.
# We remove whoever has mistakes in their registration. The datasets are transformed to piece together the clusters later.

pop_size <- pop_med_msis %>%
  mutate_at(vars(Inndato, Utdato), date) %>%
  group_by(er_beboer) %>%
  summarise("2020, tertial 1" = sum(((Inndato < date("2020-01-01") & (Utdato > date("2020-01-01") | is.na(Utdato)) &
                                                                       (DDATO > date("2020-01-01") | is.na(DDATO)))), na.rm = TRUE),

            "2020, tertial 2" = sum(((Inndato < date("2020-05-01") & (Utdato > date("2020-05-01") | is.na(Utdato)) &
                                                                       (DDATO > date("2020-05-01") | is.na(DDATO)))), na.rm = TRUE),

            "2020, tertial 3" = sum(((Inndato < date("2020-09-01") & (Utdato > date("2020-09-01") | is.na(Utdato)) &
                                                                       (DDATO > date("2020-09-01") | is.na(DDATO)))), na.rm = TRUE),

            "2021, tertial 1" = sum(((Inndato < date("2021-01-01") & (Utdato > date("2021-01-01") | is.na(Utdato)) &
                                                                       (DDATO > date("2021-01-01") | is.na(DDATO)))), na.rm = TRUE),

            "2021, tertial 2" = sum(((Inndato < date("2021-05-01") & (Utdato > date("2021-05-01") | is.na(Utdato)) &
                                                                       (DDATO > date("2021-05-01") | is.na(DDATO)))), na.rm = TRUE),

            "2021, tertial 3" = sum(((Inndato < date("2021-09-01") & (Utdato > date("2021-09-01") | is.na(Utdato)) &
                                                                       (DDATO > date("2021-09-01") | is.na(DDATO)))), na.rm = TRUE)) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column("tertial") %>%
  rename("n_ansatt" = V1, "n_beboer" = V2) %>%
  filter(row_number() != 1) %>%
  as_tibble() %>%
  mutate_at(vars(n_ansatt, n_beboer), as.character) %>%
  mutate_at(vars(n_ansatt, n_beboer), as.integer)

vacc_info_forstedose <- pop_med_msis %>%
  mutate_at(vars(Inndato, Utdato, forstedose), date) %>%
  filter(Inndato < Utdato | is.na(Utdato)) %>%
  group_by(er_beboer) %>%
  summarise("2020, tertial 1" = sum((forstedose >= date("2020-01-01") & forstedose < date("2020-05-01")) &
                                      ((Inndato < date("2020-01-01") & (Utdato > date("2020-01-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2020-01-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2020, tertial 2" = sum((forstedose >= date("2020-01-01") & forstedose < date("2020-09-01")) &
                                      ((Inndato < date("2020-05-01") & (Utdato > date("2020-05-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2020-05-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2020, tertial 3" = sum((forstedose >= date("2020-01-01") & forstedose < date("2021-01-01")) &
                                      ((Inndato < date("2020-09-01") & (Utdato > date("2020-09-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2020-09-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2021, tertial 1" = sum((forstedose >= date("2020-01-01") & forstedose < date("2021-05-01")) &
                                      ((Inndato < date("2021-01-01") & (Utdato > date("2021-01-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2021-01-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2021, tertial 2" = sum((forstedose >= date("2020-01-01") & forstedose < date("2021-09-01")) &
                                      ((Inndato < date("2021-05-01") & (Utdato > date("2021-05-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2021-05-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2021, tertial 3" = sum((forstedose >= date("2020-01-01") & forstedose < date("2022-01-01")) &
                                      ((Inndato < date("2021-09-01") & (Utdato > date("2021-09-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2021-09-01") | is.na(DDATO)))),
                                    na.rm = TRUE)) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column("tertial") %>%
  rename("ansatt_forste" = V1, "beboer_forste" = V2) %>%
  filter(row_number() != 1) %>%
  as_tibble() %>%
  mutate_at(vars(ansatt_forste, beboer_forste), as.character) %>%
  mutate_at(vars(ansatt_forste, beboer_forste), as.integer)

vacc_info_andredose <- pop_med_msis %>%
  mutate_at(vars(Inndato, Utdato, andredose), date) %>%
  filter(Inndato < Utdato | is.na(Utdato)) %>%
  group_by(er_beboer) %>%
  summarise("2020, tertial 1" = sum((andredose >= date("2020-01-01") & andredose < date("2020-05-01")) &
                                      ((Inndato < date("2020-01-01") & (Utdato > date("2020-01-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2020-01-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2020, tertial 2" = sum((andredose >= date("2020-01-01") & andredose < date("2020-09-01")) &
                                      ((Inndato < date("2020-05-01") & (Utdato > date("2020-05-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2020-05-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2020, tertial 3" = sum((andredose >= date("2020-01-01") & andredose < date("2021-01-01")) &
                                      ((Inndato < date("2020-09-01") & (Utdato > date("2020-09-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2020-09-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2021, tertial 1" = sum((andredose >= date("2020-01-01") & andredose < date("2021-05-01")) &
                                      ((Inndato < date("2021-01-01") & (Utdato > date("2021-01-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2021-01-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2021, tertial 2" = sum((andredose >= date("2020-01-01") & andredose < date("2021-09-01")) &
                                      ((Inndato < date("2021-05-01") & (Utdato > date("2021-05-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2021-05-01") | is.na(DDATO)))),
                                    na.rm = TRUE),
            "2021, tertial 3" = sum((andredose >= date("2020-01-01") & andredose < date("2022-01-01")) &
                                      ((Inndato < date("2021-09-01") & (Utdato > date("2021-09-01") | is.na(Utdato)) &
                                                                         (DDATO > date("2021-09-01") | is.na(DDATO)))),
                                    na.rm = TRUE)) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column("tertial") %>%
  rename("ansatt_andre" = V1, "beboer_andre" = V2) %>%
  filter(row_number() != 1) %>%
  as_tibble() %>%
  mutate_at(vars(ansatt_andre, beboer_andre), as.character) %>%
  mutate_at(vars(ansatt_andre, beboer_andre), as.integer)

pop_vac_info <- pop_size %>%
  left_join(vacc_info_forstedose, by = "tertial") %>%
  left_join(vacc_info_andredose, by = "tertial") %>%
  select(tertial, n_beboer, beboer_forste, beboer_andre, n_ansatt, ansatt_forste, ansatt_andre)


# Clustering ---------------------------------------------------

# Variable that determines the cluster size
min_cluster_size <- 3 # Could it be interesting to look at only one resident?
days_inclusion <- 14

# Clustering positive tests together based on the same institution number and that the difference in sample dates are less than 14 days.
# Creating variables with summarize with more information about each cluster.

clusters <-  positive_tester %>%
  filter(!is.na(Prøvedato),
         Prøvedato > Inndato,
         Prøvedato < Utdato | is.na(Utdato),
         Prøvedato < DDATO | is.na(DDATO)) %>%
  arrange(Prøvedato) %>%
  group_by(Orgnummer) %>%
  group_by(cluster_number = 1 + cumsum(c(0, diff(Prøvedato) > days_inclusion))) %>%
  ungroup() %>%
  group_by(Orgnummer, cluster_number) %>%
  mutate(er_ansatt = case_when(er_beboer == 1 ~ "No",
                               er_beboer == 0 ~ "Yes")) %>%
  left_join(virsus_variant, by = "persId_hash") %>%
  filter((abs(date(Prøvedato) - date(Prøvedato_variant)) <= 3) | is.na(Prøvedato_variant)) %>% # Allow 14 days difference on test and sequencing
  summarise(cases = n(),
            cases_resident = sum(er_beboer == 1),
            cases_employees = sum(er_beboer == 0),
            infected_after_vac_res = sum(er_beboer == 1 & (Prøvedato > andredose + 7), na.rm = TRUE), # Do we want 7 days between vaccine and positive test?
            infected_after_vac_emp = sum(er_beboer == 0 & (Prøvedato > andredose + 7), na.rm = TRUE), # Change variable name (protected)
            index_case_emp = first(er_ansatt),
            død_innen_14 = sum(dod_14 == TRUE & er_beboer == 1, na.rm = TRUE), # Check death within 14 days after positive test for residents
            død_register = sum(dod_register == TRUE & er_beboer == 1, na.rm = TRUE),
            død_all_cause = sum(!is.na(DDATO) & er_beboer == 1),
            n_alpha = sum(Overall_result == "Alpha" | Overall_result == "sanns_Alpha", na.rm = TRUE),
            n_delta = sum(Overall_result == "Delta"| Overall_result == "sanns_Delta", na.rm = TRUE),
            n_omnikron = sum(Overall_result == "Omikron" | Overall_result == "sanns_Omikron", na.rm = TRUE),
            start = first(Prøvedato),
            slutt = last(Prøvedato)) %>%
  filter(cases >= min_cluster_size,
         start < date('2022-01-01')) # Remove cluster that started after 31 Dec 2021

# Change the cluster overview to study periods
cluster_tertial <- clusters %>%
  group_by(tertial = case_when(start >= date("2020-01-01") & start < date("2020-05-01") ~ "2020, tertial 1",
                               start >= date("2020-05-01") & start < date("2020-09-01") ~ "2020, tertial 2",
                               start >= date("2020-09-01") & start < date("2021-01-01") ~ "2020, tertial 3",
                               start >= date("2021-01-01") & start < date("2021-05-01") ~ "2021, tertial 1",
                               start >= date("2021-05-01") & start < date("2021-09-01") ~ "2021, tertial 2",
                               start >= date("2021-09-01") & start < date("2022-01-01") ~ "2021, tertial 3")) %>%
  summarise(n_outbreaks = n(),
            n_index_employee = sum(index_case_emp == "Yes"),
            total_cases = sum(cases),
            avg_size = round(mean(cases), digits = 1),
            median_size = median(cases),
            IQR_cases = IQR(cases),
            cases_residents = sum(cases_resident),
            cases_employees = sum(cases_employees),
            death_14_days = sum(død_innen_14),
            death_reg_days = sum(død_register),
            death_all_cause = sum(død_all_cause),
            death_rate_14 = round(death_14_days/cases_residents, digits = 3), # Show who dies less than 14 days after positive test (residents)
            death_rate_reg = round(death_reg_days/cases_residents, digits = 3),
            death_all_cause_rate = round(death_all_cause/cases_residents, digits = 3),
            n_breakthrough_res = sum(infected_after_vac_res),
            n_breakthrough_emp = sum(infected_after_vac_emp),
            ratio_alpha = paste(100 * round(sum(n_alpha, na.rm = TRUE) / sum(n_alpha + n_delta + n_omnikron, na.rm = TRUE), digits = 3), "%"),
            ratio_delta = paste(100 * round(sum(n_delta, na.rm = TRUE) / sum(n_alpha + n_delta + n_omnikron, na.rm = TRUE), digits = 3), "%"),
            ratio_omikron = paste(100 * round(sum(n_omnikron, na.rm = TRUE) / sum(n_alpha + n_delta + n_omnikron, na.rm = TRUE), digits = 3), "%")
            ) %>%
  filter(!is.na(tertial)) %>%
  left_join(pop_vac_info, by = "tertial") %>%
  mutate(case_rate_res = paste(100 * round(cases_residents / n_beboer, digits = 3), "%"),
         case_rate_emp = paste(100 * round(cases_employees / n_ansatt, digits = 3), "%")) %>%
  mutate(perc_vac_res = paste(100 * round(beboer_andre / n_beboer, digits = 3), "%"),
         perc_vac_emp = paste(100 * round(ansatt_andre / n_ansatt, digits = 3), "%"),
         # AR_residents = paste(100 * round(cases_residents / n_beboer, digits = 3), "%"),
         # AR_employees = paste(100 * round(cases_employees / n_ansatt, digits = 3), "%"),
         # AR_residents_vac = paste(100 * (round(n_breakthrough_res / beboer_andre, digits = 4)), "%"),
         # AR_emplyees_vac = paste(100 * round(n_breakthrough_emp / ansatt_andre, digits = 4), "%"),
         # AR_residents_unvac = paste(100 * round((cases_residents - n_breakthrough_res) / (n_beboer - beboer_andre), digits = 4), "%"),
         # AR_emplyees_unvac = paste(100 * round((cases_employees - n_breakthrough_emp) / (n_ansatt - ansatt_andre), digits = 4), "%")
  )


# Look at clusters by months

cluster_month <- clusters %>%
  group_by(year_month = format(start, format = "%Y - %m")) %>%
  summarise(n_outbreaks = n(),
            n_index_employee = sum(index_case_emp == "Yes"),
            total_cases = sum(cases),
            avg_size = round(mean(cases), digits = 1),
            median_size = median(cases),
            IQR_cases = IQR(cases),
            cases_residents = sum(cases_resident),
            cases_employees = sum(cases_employees),
            death_14_days = sum(død_innen_14),
            death_rate = round(death_14_days/cases_residents, digits = 3), # Show who dies less than 14 days after positive test (residents)
            n_breakthrough_res = sum(infected_after_vac_res),
            n_breakthrough_emp = sum(infected_after_vac_emp),
            ratio_alpha = (100 * round(sum(n_alpha, na.rm = TRUE) / sum(n_alpha + n_delta + n_omnikron, na.rm = TRUE), digits = 4)),
            ratio_delta = (100 * round(sum(n_delta, na.rm = TRUE) / sum(n_alpha + n_delta + n_omnikron, na.rm = TRUE), digits = 4)),
            ratio_omikron = (100 * round(sum(n_omnikron, na.rm = TRUE) / sum(n_alpha + n_delta + n_omnikron, na.rm = TRUE), digits = 4))
  ) #%>%
  # filter(!is.na(year_month)) %>%
  # left_join(pop_vac_info, by = "tertial") %>%
  # mutate(case_rate_res = paste(100 * round(cases_residents / n_beboer, digits = 3), "%"),
  #        case_rate_emp = paste(100 * round(cases_employees / n_ansatt, digits = 3), "%")) %>%
  # mutate(perc_vac_res = paste(100 * round(beboer_andre / n_beboer, digits = 3), "%"),
  #        perc_vac_emp = paste(100 * round(ansatt_andre / n_ansatt, digits = 3), "%"),
  #        # AR_residents = paste(100 * round(cases_residents / n_beboer, digits = 3), "%"),
  #        # AR_employees = paste(100 * round(cases_employees / n_ansatt, digits = 3), "%"),
  #        # AR_residents_vac = paste(100 * (round(n_breakthrough_res / beboer_andre, digits = 4)), "%"),
  #        # AR_emplyees_vac = paste(100 * round(n_breakthrough_emp / ansatt_andre, digits = 4), "%"),
  #        # AR_residents_unvac = paste(100 * round((cases_residents - n_breakthrough_res) / (n_beboer - beboer_andre), digits = 4), "%"),
  #        # AR_emplyees_unvac = paste(100 * round((cases_employees - n_breakthrough_emp) / (n_ansatt - ansatt_andre), digits = 4), "%")
  # )


# Look at cluster by month
cluster_month_info <- clusters %>%
  mutate(c_month = month(start), c_year = year(start)) %>%
  group_by(c_year, c_month) %>%
  summarise(outbreak_month = n())


# Dominant variant -----------------------------------------------------------------------------------------------------------

# Overview of what variant dominated

alpha_dominant <- clusters %>%
  filter(n_alpha > n_delta & n_alpha > n_omnikron) %>%
  ungroup() %>%
  summarise(snitt = mean(cases),
            death_14_days = mean(død_innen_14))

delta_dominant <- clusters %>%
  filter(n_alpha < n_delta & n_delta > n_omnikron) %>%
  ungroup() %>%
  summarise(snitt = mean(cases),
            death_14_days = mean(død_innen_14))

omnikron_dominant <- clusters %>%
  filter(n_omnikron > n_delta & n_alpha < n_omnikron) %>%
  ungroup() %>%
  summarise(snitt = mean(cases),
            death_14_days = mean(død_innen_14))

dominant_virus <- tibble(variant = c("mean cases", "CFR 14 days (mean)"),
                         alpha = c(round(alpha_dominant$snitt, digits = 3), round(alpha_dominant$death_14_days, digits = 3)),
                         delta = c(round(delta_dominant$snitt, digits = 3), round(delta_dominant$death_14_days, digits = 3)),
                         omnikron = c(round(omnikron_dominant$snitt, digits = 3), round(omnikron_dominant$death_14_days, digits = 3))
                         )

# Testing activity ----------------------------------------------------------------------------------------------------------------

# Look at testing activity for each study period

# testing_activity <- lab_results %>%
#   group_by(tertial = case_when(Prøvedato >= date("2020-01-01") & Prøvedato < date("2020-05-01") ~ "2020, tertial 1",
#                                Prøvedato >= date("2020-05-01") & Prøvedato < date("2020-09-01") ~ "2020, tertial 2",
#                                Prøvedato >= date("2020-09-01") & Prøvedato < date("2021-01-01") ~ "2020, tertial 3",
#                                Prøvedato >= date("2021-01-01") & Prøvedato < date("2021-05-01") ~ "2021, tertial 1",
#                                Prøvedato >= date("2021-05-01") & Prøvedato < date("2021-09-01") ~ "2021, tertial 2",
#                                Prøvedato >= date("2021-09-01") & Prøvedato < date("2022-01-01") ~ "2021, tertial 3")) %>%
#   summarise(n_tests = n())
#
#
#
# positive_activity <- msis %>%
#   group_by(tertial = case_when(Prøvedato >= date("2020-01-01") & Prøvedato < date("2020-05-01") ~ "2020, tertial 1",
#                                Prøvedato >= date("2020-05-01") & Prøvedato < date("2020-09-01") ~ "2020, tertial 2",
#                                Prøvedato >= date("2020-09-01") & Prøvedato < date("2021-01-01") ~ "2020, tertial 3",
#                                Prøvedato >= date("2021-01-01") & Prøvedato < date("2021-05-01") ~ "2021, tertial 1",
#                                Prøvedato >= date("2021-05-01") & Prøvedato < date("2021-09-01") ~ "2021, tertial 2",
#                                Prøvedato >= date("2021-09-01") & Prøvedato < date("2022-01-01") ~ "2021, tertial 3")) %>%
#   summarise(n_positive = n())
#
#
#
# testing_versus_positive <- positive_activity %>%
#   left_join(testing_activity, by = "tertial") %>%
#   mutate(ratio = round(n_positive / n_tests, digits = 3))

# Store data --------------------------------------------------------------------------------------------

lagres <- cluster_tertial%>%
  select(-n_ansatt,-ansatt_forste, -ansatt_andre, -n_beboer, -beboer_forste,-beboer_andre)
write.xlsx(lagres, "output/Data/clustertabell.xlsx")

# Plotting ---------------------------------------------------------------------------------------------------

ggplot(data = cluster_tertial, aes(x = tertial, group = 1)) +
  geom_line(aes(y = cases_residents, color = 'Cases residents')) +
  geom_line(aes(y = death_rate * 750, color = 'Death rate')) +
  scale_y_continuous(name = "Avarage size", sec.axis = sec_axis(~./750, name = "Death rate")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45))

ggplot(data = cluster_tertial, aes(x = tertial, group = 1)) +
  geom_line(aes(y = beboer_forste, color = 'First dose residents')) +
  geom_line(aes(y = beboer_andre, color = 'Second dose employees')) +
  geom_line(aes(y = n_beboer , color = 'Total residents')) +
  labs(y = "Number of residents") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45))

ggplot(data = cluster_month, aes(x = year_month, group = 1)) +
  geom_line(aes(y = cases_residents, color = 'cases residents')) +
  geom_line(aes(y = cases_employees, color = 'cases employees')) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45))


cluster_month %>%
  filter(ratio_alpha != 'NaN') %>%
  ggplot(aes(x = year_month, group = 1)) +
  geom_line(aes(y = ratio_alpha, color = "Alpha")) +
  geom_line(aes(y = ratio_delta, color = "Delta")) +
  geom_line(aes(y = ratio_omikron, color = "Omikron")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45))

# Variant plotting testing ---------------------------------------------------

test <-  positive_tester %>%
  filter(!is.na(Prøvedato),
         Prøvedato > Inndato,
         Prøvedato < Utdato | is.na(Utdato),
         Prøvedato < DDATO | is.na(DDATO)) %>%
  arrange(Prøvedato) %>%
  mutate(er_ansatt = case_when(er_beboer == 1 ~ "No",
                               er_beboer == 0 ~ "Yes")) %>%
  left_join(virsus_variant, by = "persId_hash") %>%
  mutate(PDATO = floor_date(date(Prøvedato_variant), "week", week_start = 1)) %>%
  filter(PDATO > date("2020-01-01"), PDATO < date("2022-01-01"))# %>%
  #group_by(PDATO, Overall_result) %>%
  #summarise(per_week = n())

ggplot(test) +
geom_histogram(aes(x=date(PDATO)), stat = "count", na.rm = TRUE) +
geom_line(data = subset(test, Overall_result %in% c("Omikron", "Alpha", "Delta")), aes(x = PDATO, color = Overall_result),
                 stat = "count", na.rm = TRUE) +
#scale_x_date(date_breaks = "4 week", date_labels = "%Y, uke %W") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.45))

