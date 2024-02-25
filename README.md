# A comparison of five epidemiological models for transmission of SARS-CoV-2 in India

> Please use the following citation: Purkayastha, Soumik, Rupam Bhattacharyya, Ritwik Bhaduri, Ritoban Kundu, Xuelin Gu, Maxwell Salvatore, Debashree Ray, Swapnil Mishra, and Bhramar Mukherjee. "A comparison of five epidemiological models for transmission of SARS-CoV-2 in India." BMC Infectious Diseases 21, no. 1 (2021): 1-23.

The paper may be found [here](https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-021-06077-9). 

## Abstract

### Background
Many popular disease transmission models have helped nations respond to the COVID-19 pandemic by informing decisions about pandemic planning, resource allocation, implementation of social distancing measures, lockdowns, and other non-pharmaceutical interventions. We study how five epidemiological models forecast and assess the course of the pandemic in India: a baseline curve-fitting model, an extended SIR (eSIR) model, two extended SEIR (SAPHIRE and SEIR-fansy) models, and a semi-mechanistic Bayesian hierarchical model (ICM).

### Methods
Using COVID-19 case-recovery-death count data reported in India from March 15 to October 15 to train the models, we generate predictions from each of the five models from October 16 to December 31. To compare prediction accuracy with respect to reported cumulative and active case counts and reported cumulative death counts, we compute the symmetric mean absolute prediction error (SMAPE) for each of the five models. For reported cumulative cases and deaths, we compute Pearson’s and Lin’s correlation coefficients to investigate how well the projected and observed reported counts agree. We also present underreporting factors when available, and comment on uncertainty of projections from each model.

### Results
For active case counts, SMAPE values are 35.14% (SEIR-fansy) and 37.96% (eSIR). For cumulative case counts, SMAPE values are 6.89% (baseline), 6.59% (eSIR), 2.25% (SAPHIRE) and 2.29% (SEIR-fansy). For cumulative death counts, the SMAPE values are 4.74% (SEIR-fansy), 8.94% (eSIR) and 0.77% (ICM). Three models (SAPHIRE, SEIR-fansy and ICM) return total (sum of reported and unreported) cumulative case counts as well. We compute underreporting factors as of October 31 and note that for cumulative cases, the SEIR-fansy model yields an underreporting factor of 7.25 and ICM model yields 4.54 for the same quantity. For total (sum of reported and unreported) cumulative deaths the SEIR-fansy model reports an underreporting factor of 2.97. On October 31, we observe 8.18 million cumulative reported cases, while the projections (in millions) from the baseline model are 8.71 (95% credible interval: 8.63–8.80), while eSIR yields 8.35 (7.19–9.60), SAPHIRE returns 8.17 (7.90–8.52) and SEIR-fansy projects 8.51 (8.18–8.85) million cases. Cumulative case projections from the eSIR model have the highest uncertainty in terms of width of 95% credible intervals, followed by those from SAPHIRE, the baseline model and finally SEIR-fansy.

### Conclusions
In this comparative paper, we describe five different models used to study the transmission dynamics of the SARS-Cov-2 virus in India. While simulation studies are the only gold standard way to compare the accuracy of the models, here we were uniquely poised to compare the projected case-counts against observed data on a test period. The largest variability across models is observed in predicting the “total” number of infections including reported and unreported cases (on which we have no validation data). The degree of under-reporting has been a major concern in India and is characterized in this report. Overall, the SEIR-fansy model appeared to be a good choice with publicly available R-package and desired flexibility plus accuracy.

