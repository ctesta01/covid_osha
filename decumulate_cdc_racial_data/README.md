# Decumulating the CDC Race/Ethnicity COVID-19 Deaths Data

The CDC dataset [*Deaths involving coronavirus disease 2019 (COVID-19) by race
and Hispanic origin group and age, by state*](https://data.cdc.gov/NCHS/Deaths-involving-coronavirus-disease-2019-COVID-19/ks3g-spdg) available from the National Center
for Health Statistics is only available in a cumulative format -- however,
the dataset is updated weekly and as such as have been downloading it each time it 
is updated. 

Those downloads are stored within this R package 
(in `inst/deaths-covid-19-by-race-age-state-cdc/`). 

This R package contains code for: 

  1. loading the data,
  2. decumulating it on a weekly basis, and 
  3. decumulating it on a monthly basis.

The decumulated national data on a monthly basis was used to create the dataset
`covid19_deaths_us 093020.csv` used for the `monthly_racial_ethnic_disparities`
analysis.


