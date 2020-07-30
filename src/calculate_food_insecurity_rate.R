library(data.table)
library(here)

fir_coef <- fread(here("data/food_insecurity_coefficients.csv"))
acs <- fread(here("data/acs/combined_acs.csv")) %>% 
  filter(NAME == "South Wasco County School District 1, Oregon") %>% 
  select(NAME, year, unemployment_over_16, below_poverty, median_household_income, disability, race_black, race_hispanic, owner_occupied_housing_perc)

#first 2018
fir <- c()
fir[1] <- fir_coef$poverty_rate*acs$below_poverty[1] + fir_coef$unemployment_rate*acs$unemployment_over_16[1] + 
  fir_coef$median_income*acs$median_household_income[1] + fir_coef$percent_hispanic*acs$race_hispanic[1] +
  fir_coef$percent_african_american*acs$race_black[1] + fir_coef$percent_homeownership*acs$owner_occupied_housing_perc[1] +
  fir_coef$fixed_effect_2018 + fir_coef$constant
fir[2] <- fir_coef$poverty_rate*acs$below_poverty[2] + fir_coef$unemployment_rate*acs$unemployment_over_16[2] + 
  fir_coef$median_income*acs$median_household_income[2] + fir_coef$percent_hispanic*acs$race_hispanic[2] +
  fir_coef$percent_african_american*acs$race_black[2] + fir_coef$percent_homeownership*acs$owner_occupied_housing_perc[2] +
  fir_coef$fixed_effect_2017 + fir_coef$constant
fir[3] <- fir_coef$poverty_rate*acs$below_poverty[3] + fir_coef$unemployment_rate*acs$unemployment_over_16[3] + 
  fir_coef$median_income*acs$median_household_income[3] + fir_coef$percent_hispanic*acs$race_hispanic[3] +
  fir_coef$percent_african_american*acs$race_black[3] + fir_coef$percent_homeownership*acs$owner_occupied_housing_perc[3] +
  fir_coef$fixed_effect_2016 + fir_coef$constant
fir[4] <- fir_coef$poverty_rate*acs$below_poverty[4] + fir_coef$unemployment_rate*acs$unemployment_over_16[4] + 
  fir_coef$median_income*acs$median_household_income[4] + fir_coef$percent_hispanic*acs$race_hispanic[4] +
  fir_coef$percent_african_american*acs$race_black[4] + fir_coef$percent_homeownership*acs$owner_occupied_housing_perc[4] +
  fir_coef$fixed_effect_2015 + fir_coef$constant

food_insecurity_calculations <- data.frame(Food.Insecurity.Rate = fir, year = c(2018, 2017, 2016, 2015))
fwrite(food_insecurity_calculations, here("data/food_insecurity_calculations.csv"))
#### Very unsuccessful negative values :::(((