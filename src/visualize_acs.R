library(tidycensus)
library(tidyverse)
library(data.table)
library(ggplot2)
library(sf)
library(ggthemes);library(maps)

acs_data <- fread("~/git/dspg20wasco/data/acs/combined_acs.csv")

acs18 <- filter(acs_data, year == 2018, NAME == "South Wasco" | NAME == "Wasco"| NAME == "Oregon")


# financial domain: Household income and poverty rate
ggplot(acs18, aes(x = NAME, y = median_household_income))+
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = median_household_income - median_household_income_moe, 
                    ymax = median_household_income + median_household_income_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3)+ ggtitle("Median Household Income")
ggsave(path="~/git/dspg20wasco/output", device = "png", filename="median_income18.png", plot=last_plot())

ggplot(acs18, aes(x = NAME, y = below_poverty)) +
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = below_poverty - below_poverty_moe, 
                    ymax = below_poverty + below_poverty_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3) + ggtitle("% of Population Below Poverty Line")
ggsave(path="~/git/dspg20wasco/output", device = "png", filename="poverty18.png", plot=last_plot())





ggplot(acs18, aes(x = NAME, y = employment_20_to_64)) +
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = employment_20_to_64 - employment_20_to_64_moe, 
                    ymax = employment_20_to_64 + employment_20_to_64_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3) + ggtitle("% of Adults (20-64) Employed")
ggsave(path="~/git/dspg20wasco/output", device = "png", filename="employment18.png", plot=last_plot())


#grouped columns for racial diversity, education attainment and household income.
race <- select(acs18, NAME, contains("race")) # select appropriate variables
race_moe <- race %>% select(NAME,contains("moe")) #separate moe estimates
race_moe <- race_moe %>% melt(id.vars = "NAME", measure.vars = colnames(race_moe)[-1]) %>%
  rename(moe = value)
race <- race %>% select(!contains("moe"))
race <- cbind(melt(race, id.vars = "NAME", measure.vars = colnames(race)[-1]), moe = race_moe$moe)
ggplot(race)+
  geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge")+ 
  #geom_errorbar(aes(x = as.factor(NAME), ymin=value-moe, ymax=value+moe), width=.2,position="dodge") +
  scale_fill_discrete(name="Groups",labels = c("White", "Black or African American", "American Indian or Alaskan Native",
                                 "Asian", "Native Hawaiian or Pacific Islander", "Hispanic or Latino", 
                                 "Two or More","Other")) +
  ylab("% of Population") + xlab("Region") +
  ggtitle("% Racial and Ethnic Diversity")
ggsave(path="~/git/dspg20wasco/output", device = "png", filename="diversityt18.png", plot=last_plot())

#grouped columns for income, education attainment and household income.
income <- select(acs18, NAME, contains("income"))
income <- income %>% select(!contains("moe"), -median_household_income)
income <- melt(income, id.vars = "NAME", measure.vars = colnames(income)[-1])
ggplot(income)+
  geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge")+ 
  scale_fill_discrete(name = "Income Bracket", labels = c("Less than 10,000", "10,000-14,999", "15,000-24,999",
                                 "25,000-34,999", "35,000-49,999", "50,000-74,999", 
                                 "75,000-99,999","100,000-149,999", "150,000-199,999", "above 200,000")) +
  ylab("% of Population") + xlab("Region") +
  ggtitle("Income Distribution for 2018")
ggsave(path="~/git/dspg20wasco/output", device = "png", filename="incomedist18.png", plot=last_plot())


##### Mapping Data ########
#pulling acs data....for geometry
#IDEA: create function for creating dataframe with geometry
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
  #Racial Diversity Percentages: alone , alone or in combination 
  "DP05_0037P", "DP05_0064P", # % Whte 
  "DP05_0038P", "DP05_0065P", # % Black or African American
  "DP05_0039P", "DP05_0066P", # % American Indian or Alaskan Native
  "DP05_0044P", "DP05_0067P", # % Asian
  "DP05_0052P", "DP05_0068P", # % Native Hawaiian or Other Pacific Islander
  "DP05_0057P", "DP05_0069P", # % Some other race
  "DP05_0071P", # % Hispanic or Latino of any race
  
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

wasco_tract_acs<- get_acs(geography = "tract", state= "OR", county = "Wasco", year = 2018, 
                          survey = "acs5", variables = acsvars, output = "wide", 
                          geometry=TRUE, cache_table = TRUE) %>% mutate(year = 2018) %>% 
  transmute(GEOID = GEOID, NAME = NAME, year = year,
            
            #employment | unemployment for adults, poverty rate for whole population
            employment_over_16 = S2301_C03_001E, employment_over_16moe = S2301_C03_001M,
            unemployment_over_16 = S2301_C04_001E, unemployment_over_16moe = S2301_C04_001M,
            employment_20_to_64 = S2301_C03_021E, employment_20_to_64moe = S2301_C03_021M,
            unemployment_20_to_64 = S2301_C04_021E, unemployment_20_to_64moe = S2301_C04_021M,
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
            total_pop = DP05_0001E, total_pop_moe = DP05_0001M, white = DP05_0037PE, white_moe=DP05_0037PM, 
            black = DP05_0038PE, black_moe = DP05_0038PM, american_indian = DP05_0039PE, american_indian_moe = DP05_0039PM,
            asian = DP05_0044PE, asian_moe =DP05_0044PM, native_hawaiian = DP05_0052PE, native_hawaiian_moe = DP05_0052PM,
            hispanic = DP05_0071PE, hispanic_moe = DP05_0071PM, other = DP05_0057PE, other_moe = DP05_0057PM,
            
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

# pulling block group failed after 3 attempts :(

  
  
  
  
#### Plot Employment Ratio ####  
ggplot() +
  geom_sf(data = wasco_tract_acs, aes(fill = employment_20_to_64)) +
  labs(title = "Percent of employed adults adults 20 to 64 by census track") #+ 
  #theme_map)()
ggsave(path="~/git/dspg20wasco/output", device = "png", filename="employment_by_tract18.png", plot=last_plot())

# ggplot() +
#   geom_sf(data = wasco_block, aes(fill = employment_20_to_64)) +
#   labs(title = "Percent of employed adults adults 20 to 64 by census track") + 
#   theme_map()
