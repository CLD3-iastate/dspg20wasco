library(tidycensus)
library(tidyverse)
library(data.table)
#library(dplyr)

#census_api_key("adb7bddbd043e99c415c4475d151b00c8cd4971a", overwrite = TRUE, install= TRUE)

######## CREATE COMBINED DATASET FOR YEARS 2015-2018 FROM 5 YEAR ESTIMATES #########
#Oregon state level collected from ACS 1 year estimates
#single years are 5 year weighted estimates for tract and county level,
#school year estimates don't change over 5 year period.
#this is wide format
years <- c(2018, 2017, 2016, 2015)
acsvars <- c(
  #Labor Force Participation, Employment and unemployment to population ratio for adults 20-64:
  "S2301_C02_021", "S2301_C03_021","S2301_C04_021",
  #Labor Force Participation, Employment and unemployment to population ratio for adults 16 and older:
  "S2301_C03_001", "S2301_C03_001","S2301_C04_001",

  ### median earnings
  # population 16 years and older with earnings
  # Total | Median Earning $$
  "S2001_C01_001", "S2001_C01_002", 
  
  # Median by education attainment | population 25 years and older with earnings
  "S2001_C01_015", #total 
  "S2001_C01_016", # less than HS
  "S2001_C01_017", #high school grad
  "S2001_C01_018", #Some college or associates
  "S2001_C01_019", #Bachelor's degree
  "S2001_C01_020", #graduate or professional
  
  

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
  "B02001_001",
  #Racial Diversity Percentages: Race alone
  "B02001_002", # % Whte
  "B02001_003", # % Black or African American
  "B02001_004", # % American Indian or Alaskan Native
  "B02001_005", # % Asian
  "B02001_006", # % Native Hawaiian or Other Pacific Islander
  "B02001_007", # % Some other race
  "B03003_003", # % Hispanic or Latino of any race
  "B02001_008", # % two or more races

  # Total children under 18 in households
  "B09005_001",
  # Family Stability children under 18 in households | totals
  "B09005_003", # Married Couple Families
  "B09005_004", # Single parents - male
  "B09005_005", # Single parents - female
  "B09005_006", # Nonfamily household with under 18 children
  
  "B09019_002", #Total Households (block group available)
  "B09019_024",#non_family_households |living alone or with non-family
  
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
  "B25003_003", #Renter Occupied
  
  #Housing affordability-
  
  # total of owned-occupied housing units who's monthly housing costs is less than 30% 
  # Total households in income bracket | less than 20% | 20-29% 
  "B25106_003", "B25106_004", "B25106_005", #less than 30% for income less than $20,000 
  "B25106_007", "B25106_008", "B25106_009", #less than 30% for income $20,000-$34999
  "B25106_011", "B25106_012", "B25106_013", #less than 30% for income $35,000-$49999
  "B25106_015", "B25106_016", "B25106_017", #less than 30% for income $50,000-$74999
  "B25106_019", "B25106_020", "B25106_021", #less than 30% for income $75,000 or more
  
  #%all rented housing units who's monthly housing costs is less than 30% (share of entire population of rented housing units)
  # Total households in income bracket | less than 20% | 20-29% 
  "B25106_025", "B25106_026", "B25106_027",#less than 30% for income less than $20,000 
  "B25106_029", "B25106_030", "B25106_031",#less than 30% for income $20,000-$34999
  "B25106_033", "B25106_034", "B25106_035",#less than 30% for income $35,000-$49999
  "B25106_037", "B25106_038", "B25106_039",#less than 30% for income $50,000-$74999
  "B25106_041", "B25106_042", "B25106_043" #30% or less for income $75,000 or more
  
  )

combined_acs <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE) %>%
                      filter(NAME == "South Wasco County School District 1, Oregon"),
                    #wasco county
                    get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #hood river county
                    get_acs(geography = "tract", state= "OR", county="Hood River", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Hood River", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #sherman county
                    get_acs(geography = "tract", state= "OR", county="Sherman", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Sherman", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #jefferson county
                    get_acs(geography = "tract", state= "OR", county="Jefferson", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Jefferson", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Multnomah County
                    # get_acs(geography = "tract", state= "OR", county="Multnomah", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Multnomah", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Clackamas County
                    # get_acs(geography = "tract", state= "OR", county="Clackamas", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Clackamas", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Marion County
                    # get_acs(geography = "tract", state= "OR", county="Marion", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Marion", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Washington County
                    # get_acs(geography = "tract", state= "OR", county="Washington", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Washington", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Deschutes County
                    # get_acs(geography = "tract", state= "OR", county="Deschutes", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Deschutes", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Lane County
                    # get_acs(geography = "tract", state= "OR", county="Lane", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Lane", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #Umatilla County
                    # get_acs(geography = "tract", state= "OR", county="Umatilla", year = 2018, survey = "acs5",
                    #         variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "OR", county = "Umatilla", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #skamania county
                    get_acs(geography = "tract", state= "WA", county="Skamania", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "WA", county = "Skamania", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #klickitat county
                    get_acs(geography = "tract", state= "WA", county="Klickitat", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    get_acs(geography = "county", state= "WA", county = "Klickitat", year = 2018, survey = "acs5",
                            variables = acsvars, output = "wide", cache = TRUE),
                    #state
                    get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
                            variables = acsvars, output = "wide", cache = TRUE)) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE) %>%
                  filter(NAME == "South Wasco County School District 1, Oregon"),
                #wasco county
                get_acs(geography = "tract", state= "OR", county="Wasco", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #hood river county
                get_acs(geography = "tract", state= "OR", county="Hood River", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Hood River", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #sherman county
                get_acs(geography = "tract", state= "OR", county="Sherman", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Sherman", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #jefferson county
                get_acs(geography = "tract", state= "OR", county="Jefferson", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Jefferson", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #Multnomah County
                # get_acs(geography = "tract", state= "OR", county="Multnomah", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Multnomah", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                # #Clackamas County
                # get_acs(geography = "tract", state= "OR", county="Clackamas", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Clackamas", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                # #Marion County
                # get_acs(geography = "tract", state= "OR", county="Marion", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Marion", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                # #Washington County
                # get_acs(geography = "tract", state= "OR", county="Washington", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Washington", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                # #Deschutes County
                # get_acs(geography = "tract", state= "OR", county="Deschutes", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Deschutes", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                # #Lane County
                # get_acs(geography = "tract", state= "OR", county="Lane", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Lane", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                # #Umatilla County
                # get_acs(geography = "tract", state= "OR", county="Umatilla", year = years[i], survey = "acs5",
                #         variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Umatilla", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #skamania county
                get_acs(geography = "tract", state= "WA", county="Skamania", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "WA", county = "Skamania", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #klickitat county
                get_acs(geography = "tract", state= "WA", county="Klickitat", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "WA", county = "Klickitat", year = years[i], survey = "acs5",
                        variables = acsvars, output = "wide", cache = TRUE),
                #oregon state
                get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
                        variables = acsvars, output = "wide", cache = TRUE)) %>% mutate(year =years[i]) 
  combined_acs <- rbind(combined_acs, temp)
}


#bind all together
combined_acs <- combined_acs %>%
  transmute(GEOID = GEOID, NAME = NAME, year = year,

          #employment | unemployment for adults, poverty rate for whole population
          labor_force_over_16 = S2301_C03_001E, labor_force_over_16_moe = S2301_C03_001M,
          employment_over_16 = S2301_C03_001E, employment_over_16_moe = S2301_C03_001M,
          unemployment_over_16 = S2301_C04_001E, unemployment_over_16_moe = S2301_C04_001M,
          labor_force_20_to_64 = S2301_C02_021E,labor_force_20_to_64_moe =S2301_C02_021M,
          employment_20_to_64 = S2301_C03_021E, employment_20_to_64_moe = S2301_C03_021M,
          unemployment_20_to_64 = S2301_C04_021E, unemployment_20_to_64_moe = S2301_C04_021M,
          below_poverty = S1701_C03_001E, below_poverty_moe = S1701_C03_001M,

          earnings_16_older_total = S2001_C01_001E, earnings_16_older_total_moe = S2001_C01_001M,
          earnings_16_older_median = S2001_C01_002E, earnings_16_older_median_moe = S2001_C01_002M,
          earnings_25_older_total = S2001_C01_015E, earnings_25_older_total_moe = S2001_C01_015M,
          earnings_25_older_less_hs = S2001_C01_016E,earnings_25_older_less_hs_moe = S2001_C01_016M,
          earnings_25_older_hs_grad = S2001_C01_017E,earnings_25_older_hs_grad_moe = S2001_C01_017M,
          earnings_25_older_some_college = S2001_C01_018E,earnings_25_older_some_college_moe = S2001_C01_018M,
          earnings_25_older_bachelors = S2001_C01_019E,earnings_25_older_bachelors_moe = S2001_C01_019M,
          earnings_25_older_graduate = S2001_C01_020E,earnings_25_older_graduate_moe = S2001_C01_020M,
          
          
          
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
          total_pop = B02001_001E, total_pop_moe = B02001_001M, 
          race_white = B02001_002E/B02001_001E * 100, race_white_moe=B02001_002M/B02001_001E *100,
          race_black = B02001_003E/B02001_001E * 100, race_black_moe = B02001_003M/B02001_001E *100, 
          race_american_indian = B02001_004E/B02001_001E * 100, race_american_indian_moe =B02001_004M/B02001_001E *100,
          race_asian = B02001_005E/B02001_001E * 100, race_asian_moe =B02001_005M/B02001_001E *100, 
          race_native_hawaiian = B02001_006E/B02001_001E * 100, race_native_hawaiian_moe = B02001_006M/B02001_001E *100,
          race_hispanic = B03003_003E/B02001_001E * 100, race_hispanic_moe = B03003_003M/B02001_001E * 100, 
          race_other = B02001_007E/B02001_001E * 100, race_other_moe = B02001_007M/B02001_001E *100,
          race_two_more = B02001_008E/B02001_001E * 100, race_two_more_moe = B02001_008M/B02001_001E *100,

          family_children_total = B09005_001E, family_children_total_moe = B09005_001M,
          family_married_parent_perc = B09005_003E/B09005_001E*100, family_married_parent_perc_moe = B09005_003M/B09005_001E*100,
          family_single_parent_male_perc = B09005_004E/B09005_001E*100, family_single_parent_male_perc_moe = B09005_004M/B09005_001E*100,
          family_single_parent_female_perc = B09005_005E/B09005_001E*100, family_single_parent_female_perc_moe = B09005_005M/B09005_001E*100,
          family_single_parent_perc = (B09005_004E + B09005_005E)/B09005_001E*100, family_single_parent_perc_moe = (B09005_004M + B09005_005M)/B09005_001E*100,
          family_children_nonfamily_perc = B09005_006E/B09005_001E*100,family_children_nonfamily_perc_moe = B09005_006M/B09005_001E*100,
          household_single_or_nonfamily_perc = B09019_024E/B09019_002E*100, household_single_or_nonfamily_perc_moe = B09019_024M/B09019_002E*100,
          
          #Education Attainment (for population over 25)
          education_less_hs = (S1501_C01_007E + S1501_C01_008E) / S1501_C01_006E * 100, education_less_hs_moe = (S1501_C01_007M + S1501_C01_008M) / S1501_C01_006E * 100,
          education_hs_grad = S1501_C01_009E/ S1501_C01_006E * 100,education_hs_grad_moe = S1501_C01_009M/ S1501_C01_006E * 100,
          education_assoc_some_college = (S1501_C01_010E + S1501_C01_011E) / S1501_C01_006E * 100,education_assoc_some_college_moe = (S1501_C01_010M + S1501_C01_011M) / S1501_C01_006E * 100,
          education_bachelors_or_higher = (S1501_C01_012E + S1501_C01_013E) / S1501_C01_006E * 100,education_bachelors_or_higher_moe = (S1501_C01_012M + S1501_C01_013M) / S1501_C01_006E * 100,

          # Percent with a disability
          disability = S1810_C03_001E, disability_moe = S1810_C03_001M,

          # Homeowners
          housing_occupied_total = B25003_001E, housing_occupied_total_moe = B25003_001M,
          owner_occupied_housing_total = B25003_002E, owner_occupied_housing_total_moe = B25003_002M,
          renter_occupied_housing_total = B25003_003E, renter_occupied_housing_total_moe = B25003_003M,### B25074 for renters: HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
          
          owner_occupied_housing_perc = B25003_002E/B25003_001E *100, owner_occupied_housing_perc_moe = B25003_002M/B25003_001E *100,
          renter_occupied_housing_perc = B25003_003E/B25003_001E *100, renter_occupied_housing_perc_moe = B25003_003M/B25003_001E *100,
          
          # Percent of households who have affordable housing costs
          affordable_housing_own_total = B25106_004E + B25106_005E + B25106_008E + B25106_009E + B25106_012E + 
            B25106_013E + B25106_016E + B25106_017E + B25106_020E + B25106_021E,
          affordable_housing_own_total_moe = B25106_004M + B25106_005M + B25106_008M + B25106_009M + B25106_012M + 
            B25106_013M + B25106_016M + B25106_017M + B25106_020M + B25106_021M,
          
          affordable_housing_own_perc = affordable_housing_own_total / B25003_002E * 100,
          affordable_housing_own_perc_moe = affordable_housing_own_total_moe / B25003_002E * 100,
          
          affordable_housing_rent_total = B25106_026E + B25106_027E + B25106_030E + B25106_031E + B25106_034E + 
            B25106_035E + B25106_038E + B25106_039E +  B25106_042E + B25106_043E,
          affordable_housing_rent_total_moe = B25106_026M + B25106_027M + B25106_030M + B25106_031M + B25106_034M + 
            B25106_035M + B25106_038M + B25106_039M +  B25106_042M + B25106_043M,
          
          affordable_housing_rent_perc = affordable_housing_rent_total / B25003_003E * 100,
          affordable_housing_rent_perc_moe = affordable_housing_rent_total_moe / B25003_003E * 100,
          
          affordable_housing_all_perc = (affordable_housing_own_total + affordable_housing_rent_total) / B25003_001E *100,
          affordable_housing_all_perc_moe = (affordable_housing_own_total_moe + affordable_housing_rent_total_moe) / B25003_001E *100,
          
          affordable_housing_less_50k = (B25106_004E + B25106_005E + B25106_026E + B25106_027E + B25106_008E + 
                                           B25106_009E + B25106_030E + B25106_031E + B25106_034E + B25106_035E +
                                           B25106_012E + B25106_013E) / (B25106_003E + B25106_025E + B25106_007E + 
                                                                           B25106_029E + B25106_033E +B25106_011E) * 100,
          affordable_housing_less_50k_moe = (B25106_004M + B25106_005M + B25106_026M + B25106_027M + B25106_008M + 
                                           B25106_009M + B25106_030M + B25106_031M + B25106_034M + B25106_035M +
                                             B25106_012M + B25106_013M) / (B25106_003E + B25106_025E + B25106_007E + 
                                                                             B25106_029E + B25106_033E +B25106_011E) * 100
          
  )
combined_acs$NAME <- gsub("County, Washington", "County, WA", 
                          gsub("District 1, Oregon","District 1, OR",
                               gsub("County, Oregon","County, OR",combined_acs$NAME))) 
#remove geometry variable since list cannot be exported
library(here)
#fwrite(combined_acs,here("/data/acs/combined_acs.csv"), sep = ",")

library(tigris)
#write files into separate county and tracts .Rds files for easier shiny app handling
acs_data <- fread(here("/data/acs/combined_acs.csv")) 
acs_data$GEOID <- as.character(acs_data$GEOID)
acs_counties <- filter(acs_data, NAME == "South Wasco County School District 1, OR" | 
                         NAME == "Wasco County, OR"| NAME == "Hood River County, OR" |
                         NAME == "Sherman County, OR" | NAME == "Jefferson County, OR" |
                         NAME == "Multnomah County, OR" | NAME == "Clackamas County, OR" |
                         NAME == "Marion County, OR" | NAME == "Washington County, OR" |
                         NAME == "Deschutes County, OR" | NAME == "Lane County, OR" | 
                         NAME == "Umatilla County, OR" |
                         NAME == "Skamania County, WA" | NAME == "Klickitat County, WA" | 
                         NAME == "Oregon")
#saveRDS(acs_counties, file = here("/data/acs_counties.Rds"))


or_tracts <- tracts(state = "OR", county = c("Wasco", "Hood River", "Sherman", "Jefferson"),
                    cb = TRUE)
wa_tracts <- tracts(state = "WA", county = c("Skamania", "Klickitat"),
                    cb = TRUE)
tract_geo <- rbind(or_tracts, wa_tracts)
acs_tracts <- acs_data %>% filter(grepl("Tract",NAME)) 
acs_tracts <- inner_join(tract_geo, acs_tracts, by = "GEOID") %>% rename("NAME" = "NAME.y")
#saveRDS(acs_tracts, file = here("/data/acs_tracts.Rds"))

####### PRACTICE CODE #######
# The code below is to prepare for combined pull and troubleshoot issues with the api.
#### Social Data ###########
social_vars <- c(  #Total Population Estimate
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
  
  # Total children under 18 in households
  "B09005_001",
  # Family Stability children under 18 in households | totals
  "B09005_003", # Married Couple Families
  "B09005_004", # Single parents - male
  "B09005_005", # Single parents - female
  "B09005_006", # Nonfamily household with under 18 children

  "B09019_002", #Total Households (block group available)
  "B09019_024",#non_family_households |living alone or with non-family
  
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
  "S1810_C03_001"
) 

social_acs <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                          variables = social_vars, output = "wide", cache = TRUE) %>%
                    filter(NAME == "South Wasco County School District 1, Oregon"),
                  get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
                          variables = social_vars, output = "wide", cache = TRUE),
                  get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                          variables = social_vars, output = "wide", cache = TRUE),
                  get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
                          variables = social_vars, output = "wide", cache = TRUE)) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = years[i], survey = "acs5",
                        variables = social_vars, output = "wide", cache = TRUE) %>%
                  filter(NAME == "South Wasco County School District 1, Oregon"),
                get_acs(geography = "tract", state= "OR", county="Wasco", year = years[i], survey = "acs5",
                        variables = social_vars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                        variables = social_vars, output = "wide", cache = TRUE),
                get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
                        variables = social_vars, output = "wide", cache = TRUE)) %>% mutate(year =years[i]) 
  social_acs <- rbind(social_acs, temp)
}
social_acs <- social_acs %>% transmute(GEOID = GEOID, NAME = NAME, year = year,
    total_pop = DP05_0001E, total_pop_moe = DP05_0001M, race_white = DP05_0037PE, race_white_moe=DP05_0037PM,
    race_black = DP05_0038PE, race_black_moe = DP05_0038PM, race_american_indian = DP05_0039PE, race_american_indian_moe = DP05_0039PM,
    race_asian = DP05_0044PE, race_asian_moe =DP05_0044PM, race_native_hawaiian = DP05_0052PE, race_native_hawaiian_moe = DP05_0052PM,
    race_hispanic = DP05_0071PE, race_hispanic_moe = DP05_0071PM, race_other = DP05_0057PE, race_other_moe = DP05_0057PM,
    race_two_more = DP05_0035PE, race_two_more_moe = DP05_0035PM,
    
    family_children_total = B09005_001E, family_children_total_moe = B09005_001M,
    family_married_parent_perc = B09005_003E/B09005_001E*100, 
    family_single_parent_perc = (B09005_004E + B09005_005E)/B09005_001E*100, 
    family_children_nonfamily_perc = B09005_006E/B09005_001E*100,
    
    #Education Attainment (for population over 25)
    education_less_hs = (S1501_C01_007E + S1501_C01_008E) / S1501_C01_006E * 100,
    education_hs_grad = S1501_C01_009E/ S1501_C01_006E * 100,
    education_assoc_some_college = (S1501_C01_010E + S1501_C01_011E) / S1501_C01_006E * 100,
    education_bachelors_or_higher = (S1501_C01_012E + S1501_C01_013E) / S1501_C01_006E * 100,
    
    # Percent with a disability
    disability = S1810_C03_001E, disability_moe = S1810_C03_001M
  )

#fwrite(social_acs,"~/git/dspg20wasco/data/acs/social.csv", sep = ",")



##### Housing ##########
housing_vars <- c(
  # Homeowners in occupied housing units
  "B25003_001", #Total Occupied Housing Units
  "B25003_002", #Owner Occupied
  "B25003_003", #Renter Occupied
  
  #Housing affordability-
  
  # total of owned-occupied housing units who's monthly housing costs is less than 30% 
  # Total households in income bracket | less than 20% | 20-29% 
  "B25106_003", "B25106_004", "B25106_005", #less than 30% for income less than $20,000 
  "B25106_007", "B25106_008", "B25106_009", #less than 30% for income $20,000-$34999
  "B25106_011", "B25106_012", "B25106_013", #less than 30% for income $35,000-$49999
  "B25106_015", "B25106_016", "B25106_017", #less than 30% for income $50,000-$74999
  "B25106_019", "B25106_020", "B25106_021", #less than 30% for income $75,000 or more
  
  #%all rented housing units who's monthly housing costs is less than 30% (share of entire population of rented housing units)
  # Total households in income bracket | less than 20% | 20-29% 
  "B25106_025", "B25106_026", "B25106_027",#less than 30% for income less than $20,000 
  "B25106_029", "B25106_030", "B25106_031",#less than 30% for income $20,000-$34999
  "B25106_033", "B25106_034", "B25106_035",#less than 30% for income $35,000-$49999
  "B25106_037", "B25106_038", "B25106_039",#less than 30% for income $50,000-$74999
  "B25106_041", "B25106_042", "B25106_043" #30% or less for income $75,000 or more
)

housing_acs2 <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                 variables = housing_vars, output = "wide", cache = TRUE) %>%
           filter(NAME == "South Wasco County School District 1, Oregon"),
         get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
                 variables = housing_vars, output = "wide", cache = TRUE),
         get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                 variables = housing_vars, output = "wide", cache = TRUE),
         get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
                 variables = housing_vars, output = "wide", cache = TRUE)) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = years[i], survey = "acs5",
                        variables = housing_vars, output = "wide", cache = TRUE) %>%
                  filter(NAME == "South Wasco County School District 1, Oregon"),
                get_acs(geography = "tract", state= "OR", county="Wasco", year = years[i], survey = "acs5",
                        variables = housing_vars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                        variables = housing_vars, output = "wide", cache = TRUE),
                get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
                        variables = housing_vars, output = "wide", cache = TRUE)) %>% mutate(year =years[i]) 
  housing_acs2 <- rbind(housing_acs2, temp)
}
housing_acs2 <- housing_acs2 %>% transmute(GEOID = GEOID, NAME = NAME, year = year, 
                                         # Homeowners
                                         housing_occupied_total = B25003_001E,
                                         owner_occupied_housing_total = B25003_002E,
                                         renter_occupied_housing_total = B25003_003E, ### B25074 for renters: HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
                                         
                                         owner_occupied_housing_perc = B25003_002E/B25003_001E *100 ,
                                         renter_occupied_housing_perc = B25003_003E/B25003_001E *100 ,
                                         
                                         # Percent of households who have affordable housing costs
                                         affordable_housing_own_total = B25106_004E + B25106_005E + B25106_008E + B25106_009E + B25106_012E + 
                                           B25106_013E + B25106_016E + B25106_017E + B25106_020E + B25106_021E,
                                         affordable_housing_own_perc = affordable_housing_own_total / B25003_002E * 100,
                                         affordable_housing_rent_total = B25106_026E + B25106_027E + B25106_030E + B25106_031E + B25106_034E + 
                                           B25106_035E + B25106_038E + B25106_039E +  B25106_042E + B25106_043E,
                                         affordable_housing_rent_perc = affordable_housing_rent_total / B25003_003E * 100,
                                         affordable_housing_all_perc = (affordable_housing_own_total + affordable_housing_rent_total) / B25003_001E *100,
                                         
                                         affordable_housing_less_20k = (B25106_004E + B25106_005E + B25106_026E + B25106_027E) / (B25106_003E + B25106_025E) * 100,
                                         affordable_housing_20k_34k = (B25106_008E + B25106_009E + B25106_030E + B25106_031E)/ (B25106_007E + B25106_029E) * 100,
                                         affordable_housing_35k_49k = (B25106_012E + B25106_013E + B25106_034E + B25106_035E)/ (B25106_011E + B25106_033E)* 100,
                                         affordable_housing_50k_74k = (B25106_016E + B25106_017E + B25106_038E + B25106_039E)/ (B25106_015E + B25106_037E) * 100,
                                         affordable_housing_more_75k = (B25106_020E + B25106_021E + B25106_042E + B25106_043E)/ (B25106_019E + B25106_041E) * 100
)

#fwrite(housing_acs2,"~/git/dspg20wasco/data/acs/housing.csv", sep = ",")


##### Housing First try! (DOESNT go earlier than 2016) ##########
# housing_vars <- c(
#   # Homeowners in occupied housing units
#   "B25003_001", #Total Occupied Housing Units
#   "B25003_002", #Owner Occupied
#   "B25003_003", #Renter Occupied
#   
#   #Housing affordability-
#   #counts: Total houses | total with less than 20% | total 20%-29%
#   "S2503_C01_025","S2503_C01_026", "S2503_C01_027", #30% or less for income less than $20,000 
#   "S2503_C01_029","S2503_C01_030", "S2503_C01_031", #30% or less for income $20,000-$34999
#   "S2503_C01_033", "S2503_C01_034", "S2503_C01_035", #30% or less for income $35,000-$49999
#   "S2503_C01_037", "S2503_C01_038", "S2503_C01_039", #30% or less for income $50,000-$74999
#   "S2503_C01_041", "S2503_C01_042", "S2503_C01_043", #30% or less for income $75,000 or more
#   
#   #%all occupied housing units who's monthly housing costs is less than 30% (share of entire population of occupied housing units)
#   "S2503_C02_026", "S2503_C02_027", #30% or less for income less than $20,000 
#   "S2503_C02_030", "S2503_C02_031", #30% or less for income $20,000-$34999
#   "S2503_C02_034", "S2503_C02_035", #30% or less for income $35,000-$49999
#   "S2503_C02_038", "S2503_C02_039", #30% or less for income $50,000-$74999
#   "S2503_C02_042", "S2503_C02_043", #30% or less for income $75,000 or more
#   
#   #%all owned housing units who's monthly housing costs is less than 30% (share of entire population of owned housing units)
#   "S2503_C04_026", "S2503_C04_027", #30% or less for income less than $20,000 
#   "S2503_C04_030", "S2503_C04_031", #30% or less for income $20,000-$34999
#   "S2503_C04_034", "S2503_C04_035", #30% or less for income $35,000-$49999
#   "S2503_C04_038", "S2503_C04_039", #30% or less for income $50,000-$74999
#   "S2503_C04_042", "S2503_C04_043", #30% or less for income $75,000 or more
#   
#   #%all rented housing units who's monthly housing costs is less than 30% (share of entire population of rented housing units)
#   "S2503_C06_026", "S2503_C06_027", #30% or less for income less than $20,000 
#   "S2503_C06_030", "S2503_C06_031", #30% or less for income $20,000-$34999
#   "S2503_C06_034", "S2503_C06_035", #30% or less for income $35,000-$49999
#   "S2503_C06_038", "S2503_C06_039", #30% or less for income $50,000-$74999
#   "S2503_C06_042", "S2503_C06_043" #30% or less for income $75,000 or more
# )
# 
# housing_acs <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
#                              variables = housing_vars, output = "wide", cache = TRUE) %>%
#                        filter(NAME == "South Wasco County School District 1, Oregon"),
#                      get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
#                              variables = housing_vars, output = "wide", cache = TRUE),
#                      get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
#                              variables = housing_vars, output = "wide", cache = TRUE),
#                      get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
#                              variables = housing_vars, output = "wide", cache = TRUE)) %>% mutate(year = 2018)
# for(i in 2:length(years)){
#   temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = years[i], survey = "acs5",
#                         variables = housing_vars, output = "wide", cache = TRUE) %>%
#                   filter(NAME == "South Wasco County School District 1, Oregon"),
#                 get_acs(geography = "tract", state= "OR", county="Wasco", year = years[i], survey = "acs5",
#                         variables = housing_vars, output = "wide", cache = TRUE),
#                 get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
#                         variables = housing_vars, output = "wide", cache = TRUE),
#                 get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
#                         variables = housing_vars, output = "wide", cache = TRUE)) %>% mutate(year =years[i]) 
#   housing_acs <- rbind(housing_acs, temp)
# }
# housing_acs <- housing_acs %>% transmute(GEOID = GEOID, NAME = NAME, year = year, 
#                                          # Homeowners
#                                          housing_occupied_total = B25003_001E,
#                                          owner_occupied_housing_total = B25003_002E,
#                                          renter_occupied_housing_total = B25003_003E, ### B25074 for renters: HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
#                                          
#                                          owner_occupied_housing_perc = B25003_002E/B25003_001E *100 ,
#                                          renter_occupied_housing_perc = B25003_003E/B25003_001E *100 ,
#                                          
#                                          # Percent of households who have affordable housing costs
#                                          affordable_housing_all = S2503_C02_026E+S2503_C02_027E+S2503_C02_030E+S2503_C02_031E+S2503_C02_034E+S2503_C02_035E+S2503_C02_038E+S2503_C02_039E+S2503_C02_042E+S2503_C02_043E,
#                                          affordable_housing_own = S2503_C04_026E+S2503_C04_027E+S2503_C04_030E+S2503_C02_031E+S2503_C04_034E+S2503_C04_035E+S2503_C04_038E+S2503_C04_039E+S2503_C04_042E+S2503_C04_043E,
#                                          affordable_housing_rent = S2503_C06_026E+S2503_C02_027E+S2503_C06_030E+S2503_C06_031E+S2503_C06_034E+S2503_C06_035E+S2503_C06_038E+S2503_C06_039E+S2503_C06_042E+S2503_C06_043E,
#                                          
#                                          affordable_housing_less_20k = (S2503_C01_026E + S2503_C01_027E) / S2503_C01_025E * 100,
#                                          affordable_housing_20k_34k = (S2503_C01_030E + S2503_C01_031E)/S2503_C01_029E * 100,
#                                          affordable_housing_35k_49k = (S2503_C01_034E + S2503_C01_035E)/S2503_C01_033E * 100,
#                                          affordable_housing_50k_74k = (S2503_C01_038E + S2503_C01_039E)/S2503_C01_037E * 100,
#                                          affordable_housing_more_75k = (S2503_C01_042E+S2503_C01_043E)/S2503_C01_041E * 100
# )





######### Financial ########################
financial_vars <-  c( # Total Households
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
"S1701_C03_001"
)

financial_acs <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                              variables = financial_vars, output = "wide", cache = TRUE) %>%
                        filter(NAME == "South Wasco County School District 1, Oregon"),
                      get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
                              variables = financial_vars, output = "wide", cache = TRUE),
                      get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                              variables = financial_vars, output = "wide", cache = TRUE),
                      get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
                              variables = financial_vars, output = "wide", cache = TRUE)) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = years[i], survey = "acs5",
                        variables = financial_vars, output = "wide", cache = TRUE) %>%
                  filter(NAME == "South Wasco County School District 1, Oregon"),
                get_acs(geography = "tract", state= "OR", county="Wasco", year = years[i], survey = "acs5",
                        variables = financial_vars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                        variables = financial_vars, output = "wide", cache = TRUE),
                get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
                        variables = financial_vars, output = "wide", cache = TRUE)) %>% mutate(year =years[i]) 
  financial_acs <- rbind(financial_acs, temp)
}

financial_acs <- financial_acs %>% transmute(GEOID = GEOID, NAME = NAME, year = year,
          
          #poverty rate for whole population
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
          income_200K_more = S1901_C01_011E, income_200K_more_moe = S1901_C01_011M
)


#fwrite(financial_acs,"~/git/dspg20wasco/data/acs/financial.csv", sep = ",")


######### Employment ########################
employment_vars <- c(#Employment and unemployment to population ratio for adults 20-64:
  "S2301_C03_021","S2301_C04_021",
  #Employment and unemployment to population ratio for adults 16 and older:
  "S2301_C03_001","S2301_C04_001",
  
  ### median earnings
  # population 16 years and older with earnings
  # Total | Median Earning $$
  "S2001_C01_001", "S2001_C01_002", 
  
  # Median by education attainment | population 25 years and older with earnings
  "S2001_C01_015", #total 
  "S2001_C01_016", # less than HS
  "S2001_C01_017", #high school grad
  "S2001_C01_018", #Some college or associates
  "S2001_C01_019", #Bachelor's degree
  "S2001_C01_020" #graduate or professional

  )

employment_acs <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                               variables = employment_vars, output = "wide", cache = TRUE) %>%
                         filter(NAME == "South Wasco County School District 1, Oregon"),
                       get_acs(geography = "tract", state= "OR", county="Wasco", year = 2018, survey = "acs5",
                               variables = employment_vars, output = "wide", cache = TRUE),
                       get_acs(geography = "county", state= "OR", county = "Wasco", year = 2018, survey = "acs5",
                               variables = employment_vars, output = "wide", cache = TRUE),
                       get_acs(geography = "state", state= "OR", year = 2018, survey = "acs1",
                               variables = employment_vars, output = "wide", cache = TRUE)) %>% mutate(year = 2018)
for(i in 2:length(years)){
  temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = years[i], survey = "acs5",
                        variables = employment_vars, output = "wide", cache = TRUE) %>%
                  filter(NAME == "South Wasco County School District 1, Oregon"),
                get_acs(geography = "tract", state= "OR", county="Wasco", year = years[i], survey = "acs5",
                        variables = employment_vars, output = "wide", cache = TRUE),
                get_acs(geography = "county", state= "OR", county = "Wasco", year = years[i], survey = "acs5",
                        variables = employment_vars, output = "wide", cache = TRUE),
                get_acs(geography = "state", state= "OR", year = years[i], survey = "acs1",
                        variables = employment_vars, output = "wide", cache = TRUE)) %>% mutate(year =years[i]) 
  employment_acs <- rbind(employment_acs, temp)
}

employment_acs  <- employment_acs  %>% transmute(GEOID = GEOID, NAME = NAME, year = year,
                                             
                                             #employment | unemployment for adults, poverty rate for whole population
                                             employment_over_16 = S2301_C03_001E, employment_over_16_moe = S2301_C03_001M,
                                             unemployment_over_16 = S2301_C04_001E, unemployment_over_16_moe = S2301_C04_001M,
                                             employment_20_to_64 = S2301_C03_021E, employment_20_to_64_moe = S2301_C03_021M,
                                             unemployment_20_to_64 = S2301_C04_021E, unemployment_20_to_64_moe = S2301_C04_021M,
                                             
                                             earnings_16_older_total = S2001_C01_001E, earnings_16_older_total_moe = S2001_C01_001M,
                                             earnings_16_older_median = S2001_C01_002E, earnings_16_older_median_moe = S2001_C01_002M,
                                             earnings_25_older_total = S2001_C01_015E, earnings_25_older_total_moe = S2001_C01_015M,
                                             earnings_25_older_less_hs = S2001_C01_016E,earnings_25_older_less_hs_moe = S2001_C01_016M,
                                             earnings_25_older_hs_grad = S2001_C01_017E,earnings_25_older_hs_grad_moe = S2001_C01_017M,
                                             earnings_25_older_some_college = S2001_C01_018E,earnings_25_older_some_college_moe = S2001_C01_018M,
                                             earnings_25_older_bachelors = S2001_C01_019E,earnings_25_older_bachelors_moe = S2001_C01_019M,
                                             earnings_25_older_graduate = S2001_C01_020E,earnings_25_older_graduate_moe = S2001_C01_020M
                                             )

#fwrite(employment_acs,"~/git/dspg20wasco/data/acs/employment.csv", sep = ",")

#### Cannot retrieve block groups from subject tables.
block_test <- get_acs(geography = "block group", state= "OR", county = "Wasco", year = 2017, survey = "acs5",
        variables = c("S2301_C03_001","S2301_C04_001","S2301_C03_021","S2301_C04_021","S1701_C03_001"), output = "wide")


#fwrite(econ_fin,"~/git/dspg20wasco/data/acs/employment.csv", sep = ",")
