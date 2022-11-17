# NoHAI


## Healthcare associated infections (HAI) in hospitals

The project is created in R in Beredt C19.

There is currently no published articles on this topic, but we're working on publishing articles.

### Discovering HAIs

The HAI_COV.R file creates a dataset with info on hospital stays and positive test days, and classify each positive test as definite, probable, indeterminate HAI or community associated infection if they were in hospital at least one of 1-7 days before positive test.

### Cluster analysis of HAIs

Klyngeanalyser.R uses the data on hospital stays and categories to create clusters of healthcare associated infections in hospitals, either based on hospital for smaller hospitals and based on ward for larger hospitals.




## Healthcare associated infections in long-term care facilities

Add text