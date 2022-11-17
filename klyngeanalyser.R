#############################################################################################################################
## klyngeanalyser.R                                                                                                        ##
## Cluster analyses of healthcare associated infections of SARS-CoV-2 based on the detected cases from HAI_COV.R           ##
#############################################################################################################################
## Author: Håvard Skagseth                                                                                                 ##
## Idea by: Håvard Skagseth inspired by similar work by Petter Nymark on long-term care facilities                         ##
#############################################################################################################################
## Run HAI_COV.R before running this script                                                                                ##
#############################################################################################################################

days_inclusion_dep = 14
days_inclusion_hosp = 14
# From HAI_COV.R we have the datasets prob_def (all probable and definite HAIs), prob_def_indet(all indeterminate, probable and definite HAIs) and npr_msis_somatikk(all hospital stays throughout the study period for these people)

#Large hospitals
store_sykehus <- c("Tromsø Somatikk",
                   "Haukeland universitetssjukehus Somatikk",
                   "Sykehuset, Våland Somatikk",
                   "Nordbyhagen Somatikk",
                   "Ullevål Somatikk",
                   "Radiumhospitalet Somatikk", 
                   "Rikshospitalet Somatikk", 
                   "St. Olavs hospital, Somatikk Øya",
                   "Drammen sykehus Somatikk")

#Select the important variables from Prob_def
prob_def_cluster <- prob_def%>%
  select(c(persId_hash,Prøvedato, hai_code))

#Dataset with all hospital stays for the probable and definite HAIs
clustersett <- npr_msis_somatikk%>%
  inner_join(prob_def_cluster, by = c("persId_hash", "Prøvedato"))

# Change inn/utdatotid to date and keep the stay where they were 4 days before positive test, if multiple keep the one with the earliest admission date
clustersett <- clustersett%>%
  mutate(innDatoTid = as.Date(innDatoTid),
         utDatoTid = as.Date(utDatoTid))%>%
  filter(innDatoTid <= as.Date(Prøvedato)-4,
         utDatoTid >= as.Date(Prøvedato)-4)%>%
  
  arrange(innDatoTid)%>%
  distinct(persId_hash, Prøvedato, .keep_all = TRUE)

# Dataset for large hospitals
clustersett_Store_Sykehus <- clustersett%>%
  filter(behandlingssted_enhetlokal %in% store_sykehus)

# Dataset for smaller hospitals
clustersett_Andre_Sykehus <- clustersett%>%
  filter(!behandlingssted_enhetlokal %in% store_sykehus)

# Cluster probable and definite cases for large hospitals based on wards
cluster_avdeling <- clustersett_Store_Sykehus%>%
  filter(!is.na(avdeling_enhetlokal))%>%
  arrange(behandlingssted_enhetlokal,avdeling_enhetlokal, Prøvedato)%>%
  group_by(helseforetakOrgNr,behandlingssted_enhetlokal,avdeling_enhetlokal)%>%
  mutate(cluster_number = 1 + cumsum(c(0, diff(as.Date(Prøvedato)) > days_inclusion_dep)))%>%
  ungroup()%>%
  group_by(helseforetakOrgNr,behandlingssted_enhetlokal,avdeling_enhetlokal,cluster_number)%>%
  summarise(antall = n(),start = min(Prøvedato), slutt = max(Prøvedato))%>%
  filter(antall >= 1#,
         #start <= "2021-10-01"
         )%>%
  ungroup()%>%
  arrange(start)

# Cluster probable and definite cases for smaller hospitals based on hospital 
cluster_sykehus <- clustersett_Andre_Sykehus%>%
  filter(!is.na(avdeling_enhetlokal))%>%
  arrange(behandlingssted_enhetlokal, Prøvedato)%>%
  group_by(helseforetakOrgNr,behandlingssted_enhetlokal)%>%
  mutate(cluster_number = 1 + cumsum(c(0, diff(as.Date(Prøvedato)) > days_inclusion_hosp)))%>%
  ungroup()%>%
  group_by(helseforetakOrgNr,behandlingssted_enhetlokal, cluster_number)%>%
  summarise(antall = n(),start = min(Prøvedato), slutt = max(Prøvedato))%>%
  filter(antall >= 1)%>%
  ungroup()%>%
  mutate(avdeling_enhetlokal = "")

#Dataset with where indeterminate cases were if they were hospitalized 4 days before positive test
indet_cluster4 <- indet%>%
  select(c(persId_hash,Prøvedato, hai_code))
clustersett_indet4 <- npr_msis_somatikk%>%
  inner_join(indet_cluster4, by = c("persId_hash", "Prøvedato"))

clustersett_indet4 <- clustersett_indet4%>%
  mutate(innDatoTid = as.Date(innDatoTid),
         utDatoTid = as.Date(utDatoTid))%>%
  filter(innDatoTid <= as.Date(Prøvedato)-4,
         utDatoTid >= as.Date(Prøvedato)-4)%>%
  
  arrange(innDatoTid)%>%
  distinct(persId_hash, Prøvedato, .keep_all = TRUE)

# Dataset containing where indeterminate cases were hospitalized if they were not hospitalized 4 days before testm but 2 days before
indet_cluster2 <- indet%>%
  select(c(persId_hash,Prøvedato, hai_code))%>%
  filter(!(persId_hash %in% clustersett_indet4$persId_hash & Prøvedato %in% clustersett_indet4$Prøvedato))
clustersett_indet2 <- npr_msis_somatikk%>%
  inner_join(indet_cluster2, by = c("persId_hash", "Prøvedato"))

clustersett_indet2 <- clustersett_indet2%>%
  mutate(innDatoTid = as.Date(innDatoTid),
         utDatoTid = as.Date(utDatoTid))%>%
  filter(innDatoTid <= as.Date(Prøvedato)-2,
         utDatoTid >= as.Date(Prøvedato)-2)%>%
  
  arrange(innDatoTid)%>%
  distinct(persId_hash, Prøvedato, .keep_all = TRUE)

# dataset if indeterminates hospitalized 6 days before postive test, but not 2 or 4 days
indet_cluster6 <- indet%>%
  select(c(persId_hash,Prøvedato, hai_code))%>%
  filter(!(persId_hash %in% clustersett_indet4$persId_hash & Prøvedato %in% clustersett_indet4$Prøvedato),
         !(persId_hash %in% clustersett_indet2$persId_hash & Prøvedato %in% clustersett_indet2$Prøvedato))
clustersett_indet6 <- npr_msis_somatikk%>%
  inner_join(indet_cluster6, by = c("persId_hash", "Prøvedato"))

clustersett_indet6 <- clustersett_indet6%>%
  mutate(innDatoTid = as.Date(innDatoTid),
         utDatoTid = as.Date(utDatoTid))%>%
  filter(innDatoTid <= as.Date(Prøvedato)-6,
         utDatoTid >= as.Date(Prøvedato)-6)%>%
  
  arrange(innDatoTid)%>%
  distinct(persId_hash, Prøvedato, .keep_all = TRUE)

# Dataset indeterminates hospitlaized 7 days before positive test, but not 2, 4 or 6 days
indet_cluster7 <- indet%>%
  select(c(persId_hash,Prøvedato, hai_code))%>%
  filter(!(persId_hash %in% clustersett_indet4$persId_hash & Prøvedato %in% clustersett_indet4$Prøvedato),
         !(persId_hash %in% clustersett_indet2$persId_hash & Prøvedato %in% clustersett_indet2$Prøvedato),
         !(persId_hash %in% clustersett_indet6$persId_hash & Prøvedato %in% clustersett_indet6$Prøvedato))
clustersett_indet7 <- npr_msis_somatikk%>%
  inner_join(indet_cluster7, by = c("persId_hash", "Prøvedato"))

clustersett_indet7 <- clustersett_indet7%>%
  mutate(innDatoTid = as.Date(innDatoTid),
         utDatoTid = as.Date(utDatoTid))%>%
  filter(innDatoTid <= as.Date(Prøvedato)-7,
         utDatoTid >= as.Date(Prøvedato)-7)%>%
  
  arrange(innDatoTid)%>%
  distinct(persId_hash, Prøvedato, .keep_all = TRUE)

# Dataset containing where indeterminate cases are assumed to be infected if they were infected at a hospital
clustersett_indet <- rbind(clustersett_indet4,clustersett_indet2,clustersett_indet6, clustersett_indet7)

cluster_avdeling_w_indet <- cluster_avdeling%>%
  mutate(associated_indets = 0,
         PogDHAI_etter_utskrivelse = 0,
         IHAI_etter_utskrivelse = 0)

cluster_sykehus_w_indet <- cluster_sykehus%>%
  mutate(associated_indets = 0,
         PogDHAI_etter_utskrivelse = 0,
         IHAI_etter_utskrivelse = 0)

# Adding indeterminate cases to the clusters discovered at large hospitals
for(i in 1:length(cluster_avdeling_w_indet$behandlingssted_enhetlokal)){
  temp_clusterindet <- clustersett_indet%>%
    filter(behandlingssted_enhetlokal == cluster_avdeling_w_indet$behandlingssted_enhetlokal[i],
           avdeling_enhetlokal == cluster_avdeling_w_indet$avdeling_enhetlokal[i])
  temp_probdef <- clustersett%>%
    filter(behandlingssted_enhetlokal == cluster_avdeling_w_indet$behandlingssted_enhetlokal[i],
           avdeling_enhetlokal == cluster_avdeling_w_indet$avdeling_enhetlokal[i])
  cluster_avdeling_w_indet$associated_indets[i] = sum(as.Date(temp_clusterindet$Prøvedato) %in% seq.Date(as.Date(cluster_avdeling_w_indet$start[i])-7,as.Date(cluster_avdeling_w_indet$slutt[i])+7,by = "day"))
  cluster_avdeling_w_indet$PogDHAI_etter_utskrivelse[i] = sum(temp_probdef$hai_code == 2 & as.Date(temp_probdef$Prøvedato) %in% seq.Date(as.Date(cluster_avdeling_w_indet$start[i]),as.Date(cluster_avdeling_w_indet$slutt[i]),by = "day"))
  cluster_avdeling_w_indet$IHAI_etter_utskrivelse[i] = sum(temp_clusterindet$hai_code == 2 & as.Date(temp_clusterindet$Prøvedato) %in% seq.Date(as.Date(cluster_avdeling_w_indet$start[i])-7,as.Date(cluster_avdeling_w_indet$slutt[i])+7,by = "day"))
}

# Adding indeterminate cases to the clusters discovered at smaller hospitals
for(i in 1:length(cluster_sykehus_w_indet$behandlingssted_enhetlokal)){
  temp_clusterindet <- clustersett_indet%>%
    filter(behandlingssted_enhetlokal == cluster_sykehus_w_indet$behandlingssted_enhetlokal[i])
  temp_probdef <- clustersett%>%
    filter(behandlingssted_enhetlokal == cluster_avdeling_w_indet$behandlingssted_enhetlokal[i])
  cluster_sykehus_w_indet$associated_indets[i] = sum(as.Date(temp_clusterindet$Prøvedato) %in% seq.Date(as.Date(cluster_sykehus_w_indet$start[i])-7,as.Date(cluster_sykehus_w_indet$slutt[i])+7,by = "day"))
  cluster_sykehus_w_indet$PogDHAI_etter_utskrivelse[i] = sum(temp_probdef$hai_code == 2 & as.Date(temp_probdef$Prøvedato) %in% seq.Date(as.Date(cluster_sykehus_w_indet$start[i]),as.Date(cluster_sykehus_w_indet$slutt[i]),by = "day"))
  cluster_sykehus_w_indet$IHAI_etter_utskrivelse[i] = sum(temp_clusterindet$hai_code == 2 & as.Date(temp_clusterindet$Prøvedato) %in% seq.Date(as.Date(cluster_sykehus_w_indet$start[i])-7,as.Date(cluster_sykehus_w_indet$slutt[i])+7,by = "day"))
}

# Combining the clusters for small and large hospitals to get one dataset of all possible outbreaks detected
Clustre <- rbind(cluster_sykehus_w_indet, cluster_avdeling_w_indet)%>%
  filter(start <= as.Date("2022-09-04"))

# all clusters with at least 3 probable and definite cases or 5 indeterminate, probable or definite cases 
cluster_final_3_5 <- Clustre%>%
  mutate(min_størrelse = antall,
         max_størrelse = antall+associated_indets)%>%
  select(behandlingssted_enhetlokal, avdeling_enhetlokal, min_størrelse, max_størrelse, start, slutt, PogDHAI_etter_utskrivelse, IHAI_etter_utskrivelse)%>%
  mutate(Andel_etter_utskrivelse = (PogDHAI_etter_utskrivelse + IHAI_etter_utskrivelse) / max_størrelse)%>%
  filter(min_størrelse >= 3 | max_størrelse >= 5)%>%
  mutate(Metode = "Definisjon 2")

# all clusters with at least 2 probable and definite cases or 4 indeterminate, probable or definite cases
cluster_final_2_4 <- Clustre%>%
  mutate(min_størrelse = antall,
         max_størrelse = antall+associated_indets)%>%
  select(behandlingssted_enhetlokal, avdeling_enhetlokal, min_størrelse, max_størrelse, start, slutt, PogDHAI_etter_utskrivelse, IHAI_etter_utskrivelse)%>%
  mutate(Andel_etter_utskrivelse = (PogDHAI_etter_utskrivelse + IHAI_etter_utskrivelse) / max_størrelse)%>%
  filter(min_størrelse >= 2 | max_størrelse >= 4)%>%
  mutate(Metode = "Definisjon 1")

# all clusters with at least 1 probable and definite case
cluster_final_1_1 <- Clustre%>%
  mutate(min_størrelse = antall,
         max_størrelse = antall+associated_indets)%>%
  select(behandlingssted_enhetlokal, avdeling_enhetlokal, min_størrelse, max_størrelse, start, slutt, PogDHAI_etter_utskrivelse, IHAI_etter_utskrivelse)%>%
  mutate(Andel_etter_utskrivelse = (PogDHAI_etter_utskrivelse + IHAI_etter_utskrivelse) / max_størrelse)%>%
  mutate(Metode = "Definisjon 1")

# Different information on the clusters, like mean size, median size, range, 25 and 75% quantiles
mean(cluster_final_2_4$min_størrelse)
mean(cluster_final_3_5$min_størrelse)
mean(cluster_final_2_4$max_størrelse)
mean(cluster_final_3_5$max_størrelse)
median(cluster_final_2_4$min_størrelse)
median(cluster_final_3_5$min_størrelse)
median(cluster_final_2_4$max_størrelse)
median(cluster_final_3_5$max_størrelse)
range(cluster_final_2_4$min_størrelse)
range(cluster_final_3_5$min_størrelse)
range(cluster_final_2_4$max_størrelse)
range(cluster_final_3_5$max_størrelse)

quantile(cluster_final_2_4$min_størrelse,c(0.25,0.5,0.75))
quantile(cluster_final_3_5$min_størrelse,c(0.25,0.5,0.75))
quantile(cluster_final_2_4$max_størrelse,c(0.25,0.5,0.75))
quantile(cluster_final_3_5$max_størrelse,c(0.25,0.5,0.75))


