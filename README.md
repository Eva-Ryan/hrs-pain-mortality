# hrs_pain_mortality
Causal analysis of the relationship between pain and mortality in American older adults using data from the Health and Retirement Study (HRS).

**Note:** HRS data can be accessed by creating an account on the HRS website: https://hrsdata.isr.umich.edu/. This analysis is carried out using data from the RAND HRS Fatfiles for study waves 1992-2018 inclusive, the RAND HRS Longitudinal File 2020, and the Cross-Wave Census Region/Division and Mobility File.  

## Description of files

1.  The file [1 Select variables and merge waves.R](https://github.com/Eva-Ryan/hrs_pain_mortality/blob/main/1%20Select%20variables%20and%20merge%20waves.R) contains code to load data from each data source (Fatfiles, Longitudinal File, and Census Region File), select only the variables of interest, and merge them into one dataframe.
2.  The file [2 Clean variables.R](https://github.com/Eva-Ryan/hrs_pain_mortality/blob/main/2%20Clean%20variables.R) contains code to prepare the required variables for modelling.
3.  The file [3 Imputation propensity scores and survival modelling.R](https://github.com/Eva-Ryan/hrs_pain_mortality/blob/main/3%20Imputation%20propensity%20scores%20and%20survival%20modelling.R) contains code to impute missing data using multiple imputation, apply propensity score matching and IPW methods within each of the 20 imputations, then pool and summarize the results.
