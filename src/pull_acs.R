library(tidycensus)
library(tidyverse)
library(data.table)

#census_api_key("adb7bddbd043e99c415c4475d151b00c8cd4971a", overwrite = TRUE, install= TRUE)

######## CREATE COMBINED DATASET FOR YEARS 2015-2018 FROM 5 YEAR ESTIMATES #########
#single years are 5 year weighted estimates for tract and county level,
#school year estimates don't change over 5 year period.
#this is wide format... try long format?
years <- c(2018, 2017, 2016, 2015)
acsvars <- c(
  #Employment and unemployment to population ratio for adults 20-64:
  "S2301_C03_021","S2301_C04_021",
  #Employment and unemployment to population ratio for adults 16 and older:
  "S2301_C03_001","S2301_C04_001",


  # Total Households
  "S1901_C01_001",
  # Median Household income in the past 12 months ($$):
  "S1901_C01_012",
  #Household income brackets (in %):
  "S1901_C01_002", # % less than 10,000
  "S1901_C01_003", # % between 10,000-14,999
  "S1901_C01_004", # % between 15,000-24,999
  "S1901_C01_005", # % between 25,000-34,999
  "S1901_C01_006", # % between 35,000-49,999
  "S1901_C01_007", # % between 50,000-74,999
  "S1901_C01_008", # % between 75,000-99,999
  "S1901_C01_009", # % between 100,000-149,999
  "S1901_C01_010", # % between 150,000-199,999
  "S1901_C01_011", # % above 200,000

  # % percent of entire population for whom poverty status is determined in the past 12 months
  "S1701_C03_001",

  #Total Population Estimate
  "DP05_0001",
  #Racial Diversity Percentages: Race alone
  "DP05_0037P", # % Whte
  "DP05_0038P", # % Black or African American
  "DP05_0039P", # % American Indian or Alaskan Native
  "DP05_0044P", # % Asian
  "DP05_0052P", # % Native Hawaiian or Other Pacific Islander
  "DP05_0057P", # % Some other race
  "DP05_0071P", # % Hispanic or Latino of any race
  "DP05_0035P", # % two or more races

  # Total Family with own children under 18
  "B09002_001",
  # Family Stability for own children under 18 | totals
  "B09002_002", # Married Couple Families
  "B09002_008", # Other families (assumed to be single parents)
  "B09002_009", # Male Householder no wife present
  "B09002_015", # Female Householder no husband present

  # Educational Attainment for Population 25 years and older: Total | Percent
  "S1501_C01_006", #Total population of adults 25 yrs and older
  "S1501_C01_007", "S1501_C02_007", # Less than 9th grade
  "S1501_C01_008", "S1501_C02_008", # 9-12th no diploma
  "S1501_C01_009", "S1501_C02_009", # High School diploma or equivalent
  "S1501_C01_010", "S1501_C02_010", # Some college, no degree
  "S1501_C01_011", "S1501_C02_011", # Associates degree
  "S1501_C01_012", "S1501_C02_012", # Bachelor's degree
  "S1501_C01_013", "S1501_C02_013", # Graduate or Professional degree
  "S1501_C02_014", "S1501_C02_014", # High School or Higher
  "S1501_C02_015", "S1501_C02_015", # Bachelor's or higher

  # % of civilian noninstitutionalized population with disability
  "S1810_C03_001",

  # Homeowners in occupied housing units
  "B25003_001", #Total Occupied Housing Units
  "B25003_002", #Owner Occupied
  "B25003_003" #Renter Occupied
  )

#call variables for tract
wasco_tract_acs<- get_acs(geography = "tract", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                          variables = acsvars, output = "wide", geometry=TRUE, cache_table = TRUE) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp<- get_acs(geography = "tract", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                            variables = acsvars, output = "wide", geometry=TRUE, cache_table=TRUE) %>% mutate(year = years[i])
  wasco_tract_acs <- rbind(wasco_tract_acs, temp)
}

#Download data by school district
south_wasco_acs <- get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
        variables = acsvars, output = "wide", cache_table = TRUE, geometry = TRUE) %>%
  filter(NAME == "South Wasco County School District 1, Oregon") %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp<- get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                 variables = acsvars, output = "wide", cache_table = TRUE, geometry=TRUE) %>%
    filter(NAME == "South Wasco County School District 1, Oregon") %>% mutate(year =years[i])
  south_wasco_acs <- rbind(south_wasco_acs, temp) %>% mutate(NAME= "South Wasco")
}


#call variables for county: Can't collect 2017 for some reason???
wasco_county_acs<- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                          variables = acsvars, output = "wide", geometry=TRUE) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp<- get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                 variables = acsvars, output = "wide", geometry=TRUE) %>% mutate(year =years[i])
  wasco_county_acs <- rbind(wasco_county_acs, temp) %>% mutate(NAME = "Wasco")
}

#call variables for oregon state
oregon_acs<- get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
                           variables = acsvars, output = "wide", geometry=TRUE) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp<- get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
                 variables = acsvars, output = "wide", geometry=TRUE) %>% mutate(year =years[i])
  oregon_acs <- rbind(oregon_acs, temp)
}



#bind all together
combined_acs <- rbind(south_wasco_acs, wasco_tract_acs, wasco_county_acs, oregon_acs) %>%
  transmute(GEOID = GEOID, NAME = NAME, year = year,

          #employment | unemployment for adults, poverty rate for whole population
          employment_over_16 = S2301_C03_001E, employment_over_16_moe = S2301_C03_001M,
          unemployment_over_16 = S2301_C04_001E, unemployment_over_16_moe = S2301_C04_001M,
          employment_20_to_64 = S2301_C03_021E, employment_20_to_64_moe = S2301_C03_021M,
          unemployment_20_to_64 = S2301_C04_021E, unemployment_20_to_64_moe = S2301_C04_021M,
          below_poverty = S1701_C03_001E, below_poverty_moe = S1701_C03_001M,

          #Household income (in %)
          median_household_income = S1901_C01_012E, median_household_income_moe =  S1901_C01_012M,
          income_less_than_10k = S1901_C01_002E, income_less_than_10k_moe = S1901_C01_002M,
          income_10k_14999 = S1901_C01_003E, income_10k_14999_moe = S1901_C01_003M,
          income_15k_24999 = S1901_C01_004E, income_15k_24999_moe = S1901_C01_004M,
          income_25k_34999 = S1901_C01_005E, income_25k_34999_moe = S1901_C01_005M,
          income_35K_49999 = S1901_C01_006E, income_35K_49999_moe = S1901_C01_006M,
          income_50K_74999 = S1901_C01_007E, income_50K_74999_moe = S1901_C01_007M,
          income_75K_99999 = S1901_C01_008E, income_75K_99999_moe = S1901_C01_008M,
          income_100K_149999 = S1901_C01_009E, income_100K_149999_moe = S1901_C01_009M,
          income_150K_199999 = S1901_C01_010E, income_150K_199999_moe = S1901_C01_010M,
          income_200K_more = S1901_C01_011E, income_200K_more_moe = S1901_C01_011M,
          # Population demographics: race and family
          total_pop = DP05_0001E, total_pop_moe = DP05_0001M, race_white = DP05_0037PE, race_white_moe=DP05_0037PM,
          race_black = DP05_0038PE, race_black_moe = DP05_0038PM, race_american_indian = DP05_0039PE, race_american_indian_moe = DP05_0039PM,
          race_asian = DP05_0044PE, race_asian_moe =DP05_0044PM, race_native_hawaiian = DP05_0052PE, race_native_hawaiian_moe = DP05_0052PM,
          race_hispanic = DP05_0071PE, race_hispanic_moe = DP05_0071PM, race_other = DP05_0057PE, race_other_moe = DP05_0057PM,
          race_two_more = DP05_0035PE, race_two_more_moe = DP05_0035PM,

          total_family = B09002_001E, total_family_moe = B09002_001M,
          married_perc = B09002_002E/B09002_001E*100, other_family_perc = B09002_008E/B09002_001E*100,
          male_no_spouse_perc = B09002_009E/B09002_001E*100, female_no_spouse_perc = B09002_015E/B09002_001E*100,

          #Education Attainment
          less_hs = (S1501_C01_007E + S1501_C01_008E) / S1501_C01_006E * 100,
          hs_grad = S1501_C01_009E/ S1501_C01_006E * 100,
          assoc_some_college = (S1501_C01_010E + S1501_C01_011E) / S1501_C01_006E * 100,
          bachelors_or_higher = (S1501_C01_012E + S1501_C01_013E) / S1501_C01_006E * 100,

          # Percent with a disability
          disability = S1810_C03_001E, disability_moe = S1810_C03_001M,

          # Homeowners
          owner_occupied = B25003_002E/B25003_001E,
          renter_occupied = B25003_003E/B25003_001E
)

#remove geometry variable since list cannot be exported
combined_acs <- data.table(combined_acs)[,!"geometry"]

fwrite(combined_acs,"~/git/dspg20wasco/data/acs/combined_acs.csv", sep = ",")





#### Total Population and Racial Diversity | Percentages ###########
# racial_diversity <- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
#                             variables = c("B02001_001", "B02001_002","B02001_003", "B02001_004"), output = "wide")
# demographics <- get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
#                         table = "DP05")

race_vars <- c(#Total Population Estimate
  "DP05_0001",
  #Racial Diversity Percentages: alone , alone or in combination
  "DP05_0037P", "DP05_0064P", # % Whte
  "DP05_0038P", "DP05_0065P", # % Black or African American
  "DP05_0039P", "DP05_0066P", # % American Indian or Alaskan Native
  "DP05_0044P", "DP05_0067P", # % Asian
  "DP05_0052P", "DP05_0068P", # % Native Hawaiian or Other Pacific Islander
  "DP05_0057P", "DP05_0069P", # % Some other race
  "DP05_0071P", # % Hispanic or Latino of any race
  "DP05_0035P" # % two or more races
)

pop_race <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                          variables = race_vars, output = "wide", cache = TRUE) %>%
                    filter(NAME == "South Wasco County School District 1, Oregon"),
                  get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
                          variables = race_vars, output = "wide", cache = TRUE),
                  get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                          variables = race_vars, output = "wide", cache = TRUE)) %>%
  rename(total_pop = DP05_0001E, total_pop_moe = DP05_0001M, white = DP05_0037PE, white_moe=DP05_0037PM,
         black = DP05_0038PE, black_moe = DP05_0038PM, american_indian = DP05_0039PE, american_indian_moe = DP05_0039PM,
         asian = DP05_0044PE, asian_moe =DP05_0044PM, native_hawaiian = DP05_0052PE, native_hawaiian_moe = DP05_0052PM,
         hispanic = DP05_0071PE, hispanic_moe = DP05_0071PM, other = DP05_0057PE, other_moe = DP05_0057PM,
         two_races = DP05_0035PE, two_races_moe = DP05_0035PM,

         white2 = DP05_0064PE, white_moe2=DP05_0064PM,
         black2 = DP05_0065PE, black_moe2 = DP05_0065PM, american_indian2 = DP05_0066PE, american_indian_moe2 = DP05_0066PM,
         asian2 = DP05_0067PE, asian_moe2 =DP05_0067PM, native_hawaiian2 = DP05_0068PE, native_hawaiian_moe2 = DP05_0068PM,
         hispanic2 = DP05_0071PE, hispanic_moe2 = DP05_0071PM, other2 = DP05_0069PE, other_moe2 = DP05_0069PM)

fwrite(pop_race,"~/git/dspg20wasco/data/acs/demographics.csv", sep = ",")

#### family stability for households with children under 18 | counts ########

#gathered from own children under 18 household...
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


#### Household characteristics of childre under 18: only available from 2016 as the latest ##########
# family_stability2 <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
#                                   c("B09005_001", "B09005_002","B09005_003", "B09005_004","B09005_005","B09005_006"), output = "wide") %>%
#                             filter(NAME == "South Wasco County School District 1, Oregon"),
#                           get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
#                                   c("B09005_001", "B09005_002","B09005_003", "B09005_004","B09005_005","B09005_006"), output = "wide", cache = TRUE),
#                           get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
#                                   variables = c("B09005_001", "B09005_002","B09005_003", "B09005_004","B09005_005","B09005_006"), output = "wide", cache = TRUE)) %>%
#   rename(total_household_children= B09005_001E, total_household_children_moe= B09005_001M,
#          total_family_children = B09005_002E, total_family_children_moe = B09005_002M,
#          married = B09005_003E, married_moe=B09005_003M,
#          male_no_spouse = B09005_004E, male_no_spouse_moe = B09005_004M,
#          female_no_spouse = B09005_005E, female_no_spouse_moe =B09005_005M,
#          non_family = B09005_006E, non_family_moe = B09005_006E) %>%
#   mutate(with_family_perc = total_family_children/total_household_children *100 ,married_perc = married/total_household_children*100,
#          single_parent_family_perc = (male_no_spouse+female_no_spouse)/total_household_children*100,
#          male_no_spouse_perc = male_no_spouse/total_household_children*100, female_no_spouse_perc = female_no_spouse/total_household_children*100,
#          non_family_perc = non_family / total_household_children *100)

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
         employment_20_to_64 = S2301_C03_021E, employment_20_to_64moe = S2301_C03_021M,
         unemployment_20_to_64 = S2301_C04_021E, unemployment_20_to_64moe = S2301_C04_021M,
         below_poverty = S1701_C03_001E, below_poverty_moe = S1701_C03_001M)



fwrite(econ_fin,"~/git/dspg20wasco/data/acs/employment.csv", sep = ",")
