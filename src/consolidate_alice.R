library(data.table)
library(tidyverse)
library(here)
library(readxl)

### Alice data is from United for ALICE
### Asset Limited Income Constrained Employed

### Data for state of Oregon and Washington
### Sheet 1: Metadata
### Sheet 2: County estimates for 2010-2018 every 2 years
### Sheet 3: Congressional District
### Sheet 4: Place
### Sheet 5: Sub-county (CCD)
### Sheet 6: Zip-code


### read in county level data for oregon
alice_or_county <- read_xlsx(here("data/alice", "alice_or.xlsx"), sheet = 2) %>% 
  filter(County == "Wasco" | County == "Hood River" | County == "Sherman" | County == "Jefferson" |
           County == "Multnomah" | County == "Clackamas" | County == "Marion" | County == "Washington" | 
           County == "Deschutes" | County == "Lane" | County == "Umatilla")
alice_or_county$Percent_ALICE_Households <- round(alice_or_county$`ALICE Household`/ alice_or_county$Household * 100, 2)
alice_or_county$Percent_Poverty_Households <- round(alice_or_county$`Poverty Household`/ alice_or_county$Household * 100,2)
### county level data for washington
alice_wa_county <- read_xlsx(here("data/alice", "alice_wa.xlsx"), sheet = 2) %>% 
  filter(County == "Skamania" | County == "Klickitat")
alice_wa_county$Percent_ALICE_Households <- round(alice_wa_county$`ALICE Household`/ alice_wa_county$Household * 100, 2)
alice_wa_county$Percent_Poverty_Households <- round(alice_wa_county$`Poverty Household`/ alice_wa_county$Household * 100,2)

### Combine the county level data
alice_counties <- rbind(alice_or_county, alice_wa_county) %>% mutate
colnames(alice_counties) <- c("Year", "GEO.id", "GEO.display_label", "County", "State",
                              "State_Abbr", "Households", "Poverty_Household",  "ALICE_Household",
                              "Above_ALICE_Household","ALICE_Threshold_HH_under_65",
                              "ALICE_Threshold_HH_65_and_over", "Source_American_Community_Survey",
                              "Percent_ALICE_Households", "Percent_Poverty_Households")
fwrite(alice_counties, here("data/alice", "alice_counties.csv"))



### Get estimates for just wasco county cities. | 5 year estimate only for 2018
alice_wasco_place <- read_xlsx(here("data/alice", "alice_or.xlsx"), sheet = 4) %>% 
  filter(Name == "The Dalles city" | Name == "Mosier city"|Name == "Dufur city" | Name == "Maupin city" | 
           Name == "Pine Hollow CDP" | Name == "Tygh Valley CDP" | Name == "Warm Springs CDP") %>%
  select(-c("US_States...3", "US_States...4", "Geography type"))
colnames(alice_wasco_place) <- c("Year", "GEO.id", "GEO.display_label", "Name", "Households",
                                 "Poverty_Household", "ALICE_Household","Above_ALICE_Household",
                                 "Source_American_Community_Survey")
alice_wasco_place$Percent_ALICE_Households <- round(alice_wasco_place$ALICE_Household/ alice_wasco_place$Households * 100, 2)
alice_wasco_place$Percent_Poverty_Households <- round(alice_wasco_place$Poverty_Household/ alice_wasco_place$Households * 100, 2)

fwrite(alice_wasco_place, here("data/alice", "alice_wasco_place.csv"))