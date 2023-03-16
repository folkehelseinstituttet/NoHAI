# NoHAI

NoHAI is the automated surveillance system of healthcare-associated infections in Norway. In this repository, scripts used in the development of the system may be found. Scripts contain data cleaning and structuring, and implementation of different definitions and algorithms to detect healthcare-associated infections and clusters of transmission within routinely collected health data. 

Both clusters of healthcare-associated infections in hospitals and clusters in long-term care facilities are created in R, using data from the emergency preparedness register Beredt C19. 

### Publications

Gravningen, K., Nymark, P., Wyller, T., & Kacelnik, O. (2022). A new automated national register-based surveillance system for outbreaks in long-term care facilities in Norway detected three times more severe acute respiratory coronavirus virus 2 (SARS-CoV-2) clusters than traditional methods. Infection Control & Hospital Epidemiology, 1-7. doi:10.1017/ice.2022.297

Skagseth, H., Danielsen, A. S., Kacelnik, O., Trondsen, U. T., Berg, T. C., Sorknes, N. K., Eriksen-Volle, H-M. (2023)
Clusters of healthcare-associated SARS-CoV-2 infections in Norwegian hospitals detected by a fully automatic register-based surveillance system,
Journal of Hospital Infection. doi: 10.1016/j.jhin.2023.02.014.

## Healthcare-associated infections (HAI) in hospitals

### Discovering HAIs

The HAI_COV.R file creates a dataset with info on hospital stays and positive test days, and classify each positive test as definite, probable, indeterminate HAI or community associated infection if they were in hospital at least one of 1-7 days before positive test.

### Cluster analysis of HAIs

Klyngeanalyser.R uses the data on hospital stays and categories to create clusters of healthcare associated infections in hospitals, either based on hospital for smaller hospitals and based on ward for larger hospitals.

## Clusters in long-term care facilities

The cluster_analysis_LTCF.R script uses data on individuals currently residing in long-term care facilities in Norway, tests, vaccinations, and sequencing results, to group positive cases into clusters, thus detecting possible outbreaks. 
