library(tidycensus)
library(tidyverse)
library(data.table)

#census_api_key("adb7bddbd043e99c415c4475d151b00c8cd4971a", install= TRUE)

#retrieve poverty status in past 12 months table
#id: s1701

#by tract
pr_ <- get_acs(geography = "tract", state= "OR" , county = "Wasco",
               year = 2018, survey = "acs5", variables = "DP03")

#Total population | by 5 year estimates.
#id: B01003
total_pop <- rbind(get_acs(geography = "school district (unified)", state= "OR",
                           year = 2018, survey = "acs5", variables = "B01003_001", output = "wide") %>% 
                     filter(NAME == "South Wasco County School District 1, Oregon"),
                   get_acs(geography = "county", state= "OR", county = "Wasco",
                           year = 2018, survey = "acs5", variables = "B01003_001", output = "wide"),
                   get_acs(geography = "state", state = "OR", 
                           year = 2018, survey = "acs5", variables= "B01003_001", output = "wide")) %>% 
  rename(total_pop = B01003_001E, total_pop_moe= B01003_001M)

#### Total Population and Racial Diversity | Percentages
# racial_diversity <- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
#                             variables = c("B02001_001", "B02001_002","B02001_003", "B02001_004"), output = "wide")
# demographics <- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
#                         table = "DP05")

pop_race <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5", 
                          c("DP05_0001", "DP05_0037P","DP05_0038P", "DP05_0039P","DP05_0044P", "DP05_0052P", "DP05_0071P"), output = "wide", cache = TRUE) %>% 
                    filter(NAME == "South Wasco County School District 1, Oregon"),
                  get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5", 
                          c("DP05_0001", "DP05_0037P","DP05_0038P", "DP05_0039P","DP05_0044P", "DP05_0052P", "DP05_0071P"), output = "wide", cache = TRUE),
                  get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
                    variables = c("DP05_0001", "DP05_0037P","DP05_0038P", "DP05_0039P","DP05_0044P", "DP05_0052P", "DP05_0071P"), output = "wide", cache = TRUE)) %>%
  rename(total_pop = DP05_0001E, total_pop_moe = DP05_0001M, white = DP05_0037PE, white_moe=DP05_0037PM, 
         black = DP05_0038PE, black_moe = DP05_0038PM, american_indian = DP05_0039PE, american_indian_moe = DP05_0039PM,
         asian = DP05_0044PE, asian_moe =DP05_0044PM, native_hawaiian = DP05_0052PE, native_hawaiian_moe = DP05_0052PM,
         hispanic = DP05_0071PE, hispanic_moe = DP05_0071PM)

fwrite(pop_race,"~/git/dspg20wasco/data/acs/demographics.csv", sep = ",")

#### family stability for households with children under 18 | counts
family_stability <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5", 
                                  c("B09002_001", "B09002_002","B09002_008", "B09002_009","B09002_015"), output = "wide") %>% 
                            filter(NAME == "South Wasco County School District 1, Oregon"),
                          get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5", 
                                  c("B09002_001", "B09002_002","B09002_008", "B09002_009","B09002_015"), output = "wide", cache = TRUE),
                          get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
                                  variables = c("B09002_001", "B09002_002","B09002_008", "B09002_009","B09002_015"), output = "wide", cache = TRUE)) %>%
  rename(total_family = B09002_001E, total_family_moe = B09002_001M, 
         married = B09002_002E, married_moe=B09002_002M, 
         other_family = B09002_008E, other_family_moe = B09002_008M, 
         male_no_spouse = B09002_009E, male_no_spouse_moe = B09002_009M,
         female_no_spouse = B09002_015E, female_no_spouse_moe =B09002_015M)%>%
  mutate(married_perc = married/total_family*100,other_family_perc = other_family/total_family*100,
         male_no_spouse_perc = male_no_spouse/total_family*100, female_no_spouse_perc = female_no_spouse/total_family*100)


fwrite(family_stability,"~/git/dspg20wasco/data/acs/family_stability.csv", sep = ",")
######### Economic and financial info ########################
econ_fin <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5", 
                          variables = c("S2301_C03_001","S2301_C04_001","S2301_C03_021","S2301_C04_021","S1701_C03_001"), output = "wide")%>% 
                    filter(NAME == "South Wasco County School District 1, Oregon"),
                  get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
                    variables = c("S2301_C03_001","S2301_C04_001","S2301_C03_021","S2301_C04_021","S1701_C03_001"), output = "wide"),
                  get_acs(geography = "tract", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
                          variables = c("S2301_C03_001","S2301_C04_001","S2301_C03_021","S2301_C04_021","S1701_C03_001"), output = "wide"))%>%
  rename(employment_over_16 = S2301_C03_001E, employment_over_16moe = S2301_C03_001M,
         unemployment_over_16 = S2301_C04_001E, unemployment_over_16moe = S2301_C04_001M,
         employment_20_to_60 = S2301_C03_021E, employment_20_to_60moe = S2301_C03_021M,
         unemployment_20_to_60 = S2301_C04_021E, unemployment_20_to_60moe = S2301_C04_021M,
         below_poverty_20_to_60 = S1701_C03_001E, below_poverty_20_to_60moe = S1701_C03_001M)



fwrite(econ_fin,"~/git/dspg20wasco/data/acs/employment.csv", sep = ",")




#poverty
poverty_county <- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
                   variables = "S1701_C03_001", output = "wide", cache = TRUE, geometry=TRUE, keep_geo_vars=TRUE)
poverty_school <-get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5", 
                         variables = "S1701_C03_001", output = "wide", cache = TRUE, geometry=TRUE, keep_geo_vars=TRUE)



#Employment status
employment_status <- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5", 
                             table = "S2301")

#social characteristics 
social_char <- get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5", 
                             table = "DP02")







### examples of correct use of API
orange <- get_acs(state = "CA", county = "Orange", geography = "tract",
                  variables = "B19013_001", geometry = TRUE)

head(orange)

v18 <- load_variables(2018, "acs5", cache = TRUE)