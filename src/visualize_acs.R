library(tidycensus)
library(tidyverse)
library(data.table)
library(ggplot2)
library(sf)
library(ggthemes);library(maps);library(tigris)
library(here)

#read in combined dataset
acs_data <- fread(here("/data/acs/combined_acs.csv")) 
acs_data$GEOID <- as.character(acs_data$GEOID)
acs_counties <- filter(acs_data, NAME == "South Wasco County School District 1, Oregon" | 
                         NAME == "Wasco County, Oregon"| NAME == "Hood River County, Oregon" |
                         NAME == "Sherman County, Oregon" | NAME == "Jefferson County, Oregon" |
                         NAME == "Skamania County, Washington" | NAME == "Klickitat County, Washington" | 
                         NAME == "Oregon")


#get tract level geography
or_tracts <- tracts(state = "OR", county = c("Wasco", "Hood River", "Sherman", "Jefferson"),
                       cb = TRUE)
wa_tracts <- tracts(state = "WA", county = c("Skamania", "Klickitat"),
                    cb = TRUE)
tract_geo <- rbind(or_tracts, wa_tracts)
acs_tracts <- acs_data %>% filter(grepl("Tract",NAME)) 
acs_tracts <- geo_join(tract_geo, acs_tracts, by = "GEOID")  



##### FINANCIAL ########
# financial <- fread(here("/data/acs/financial.csv")) %>% 
#   filter(acs_data, year == 2018, NAME == "South Wasco" | NAME == "Wasco"| NAME == "Oregon")

# Median Household Income Bar chart for 2018
ggplot(filter(acs_counties, year == 2018), aes(x = NAME, y = median_household_income))+
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = median_household_income - median_household_income_moe, 
                    ymax = median_household_income + median_household_income_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3)+ ggtitle("Median Household Income")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="median_income18.png", plot=last_plot())


# Median Household Income grouped line chart for all years
ggplot(acs_counties, aes(x=year, y=median_household_income, group = NAME, color = NAME)) +
  geom_line() + 
  geom_point() +
  geom_pointrange(aes(ymin=median_household_income - median_household_income_moe, ymax=median_household_income + median_household_income_moe)) +
  theme_minimal() + ggtitle("Median Household Income 2015-2018") + ylab("Median Household Income") + xlab("Year")
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.


# Median Household Income tract map for 2018
ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = median_household_income)) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
  labs(title = "Median Household Income by census track") 



#grouped columns for household income brackets.
income <- select(filter(acs_counties, year == 2018), NAME, contains("income"))
income <- income %>% select(!contains("moe"), -median_household_income)
income <- melt(income, id.vars = "NAME", measure.vars = colnames(income)[-1])
ggplot(income)+
  geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge")+ 
  scale_fill_discrete(name = "Income Bracket", labels = c("Less than 10,000", "10,000-14,999", "15,000-24,999",
                                                          "25,000-34,999", "35,000-49,999", "50,000-74,999", 
                                                          "75,000-99,999","100,000-149,999", "150,000-199,999", "above 200,000")) +
  ylab("% of Population") + xlab("Region") +
  ggtitle("Income Distribution for 2018")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="incomedist18.png", plot=last_plot())



# poverty rate
ggplot(filter(acs_counties, year == 2018), aes(x = NAME, y = below_poverty)) +
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = below_poverty - below_poverty_moe, 
                    ymax = below_poverty + below_poverty_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3) + ggtitle("% of Population Below Poverty Line")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="poverty18.png", plot=last_plot())

# poverty rate tract map for 2018
ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = below_poverty)) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
  labs(title = paste("Percent of population below poverty by census track in", 2018, sep=" "))



##### Employment ########

ggplot(filter(acs_counties, year == 2018), aes(x = NAME, y = employment_20_to_64)) +
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = employment_20_to_64 - employment_20_to_64_moe, 
                    ymax = employment_20_to_64 + employment_20_to_64_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3) + ggtitle("% of Adults (20-64) Employed")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="employment18.png", plot=last_plot())



ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = employment_20_to_64)) +
  labs(title = "Percent of employed adults adults 20 to 64 by census track") #+ 
#theme_map)()
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="employment_by_tract18.png", plot=last_plot())




####### Social ########
#grouped columns for racial diversity
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
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="diversityt18.png", plot=last_plot())




#### Housing ######