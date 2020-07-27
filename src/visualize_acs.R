library(tidycensus)
library(tidyverse);library(forcats)
library(data.table)
library(ggplot2);library(plotly)
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




######## USE THE FOLLOWING ##########
# color palette from : https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
dspgpal = c("#232D4B", "#2C4F6B", "#0E879C", "#60999A", "#D1E0BF", 
               "#D9E12B", "#E6CE3A", "#E6A01D", "#E57200", "#ADB5BD")


# grouped line chart for all years: south wasco colored, rest of the geographies gray.
p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), 
                                                            other_level = "Neighboring Counties"))
            , aes(x=year, y=median_household_income, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Median Household Income: $", median_household_income,
                                "<br>Margin of Error: $", median_household_income_moe))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(dspgpal[1], dspgpal[9], dspgpal[2], dspgpal[10])) +
  #geom_pointrange(aes(ymin=median_household_income - median_household_income_moe, ymax=median_household_income + median_household_income_moe)) +
  theme_minimal() + ggtitle("Median Household Income 2015-2018") + ylab("Median Household Income") + xlab("Year")
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
#notes: standard of living --> financial --> median household income 
#       no additional interactivity because showing all years. use renderPlotly()



#POVERTY rate
ggplotly(ggplot(filter(acs_counties, year == 2018), aes(x = NAME, y = below_poverty,
                                                        text = paste0("Region: ", NAME,
                                                                      "<br>Year: ", year,
                                                                      "<br>Percent Below Federal Poverty: ", below_poverty, "%",
                                                                      "<br>Margin of Error: ", below_poverty_moe, "%"))) +
           geom_col(fill = "dark blue") +
           geom_errorbar(aes(x = NAME, ymin = below_poverty - below_poverty_moe, 
                             ymax = below_poverty + below_poverty_moe), color = "dark orange") + 
           geom_point(color = "dark orange", size = 3) + theme_minimal() + theme(axis.text.x = element_text(angle=30)) +
           xlab("Region") + ylab("% Below Poverty") + ggtitle("% of Population Below Federal Poverty Line"), tooltip = "text") %>% 
  config(displayModeBar = "static", displaylogo = FALSE,
         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
#note: standard of living -> financial -> poverty rate
#      additional interactivity, replace 2018 with input$year



# standard of living --> housing --- > affordable housing| line chart for all counties and years (both rent and own| out of all occupied housing units)
p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), 
                                                            other_level = "Neighboring Counties"))
            , aes(x=year, y=affordable_housing_all_perc, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Affordable Housing: ", round(affordable_housing_all_perc, digits = 1), "%"))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(dspgpal[1], dspgpal[9], dspgpal[2], dspgpal[10])) +
  #geom_pointrange(aes(ymin=median_household_income - median_household_income_moe, ymax=median_household_income + median_household_income_moe)) +
  theme_minimal() + ggtitle("Affordable Housing 2015-2018") + ylab("Affordable Housing") + xlab("Year")
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

#note :  no extra inputs, uses dspgpal color palette




# learn and earn --> employment --> employment ratio | grouped line graph
ggplotly(ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), 
                                                                other_level = "Neighboring Counties")), 
                aes(x=year, y=employment_20_to_64, group = NAME, color = south_wasco,
                    text = paste0("Region: ", NAME,
                                  "<br>Year: ", year,
                                  "<br>% of Adults (20-64) with Employment Status: ", employment_20_to_64, "%",
                                  "<br>Margin of Error: ", employment_20_to_64_moe, "%"))) +
           geom_line(size = 1.5) +  geom_point(size = 2) +
           scale_colour_manual(name = "Region", values = c(dspgpal[1], dspgpal[9], dspgpal[2], dspgpal[10])) +
           #geom_pointrange(aes(ymin=employment_20_to_64 - employment_20_to_64_moe, ymax =employment_20_to_64 + employment_20_to_64_moe)) +
           theme_minimal() + ggtitle("% of Adults (20-64) with Employment Status 2015-2018") + ylab("% of Adults (20-64) with Employment Status") + xlab("Year"), 
         tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d", "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
# note: 
# no extra interactivity with user





##### FINANCIAL ########
# financial <- fread(here("/data/acs/financial.csv")) %>% 
#   filter(acs_data, year == 2018, NAME == "South Wasco" | NAME == "Wasco"| NAME == "Oregon")

# -------------- Median Household Income ------------------------
# Bar chart for 2018
ggplotly(ggplot(filter(acs_counties, year == 2018), aes(x = NAME, y = median_household_income,
                                                        text = paste0("Region: ", NAME,
                                                                      "<br>Year: ", year,
                                                                      "<br>Median Household Income: $", median_household_income,
                                                                      "<br>Margin of Error: $", median_household_income_moe)))+
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = median_household_income - median_household_income_moe, 
                    ymax = median_household_income + median_household_income_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3)+ theme_minimal()+theme(axis.text.x = element_text(angle=45)) +
    ggtitle("Median Household Income") + ylab("Median Household Income") + xlab("Region"), tooltip="text")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="median_income18.png", plot=last_plot())


# grouped line chart for all years: each geography is its own color
p <- ggplot(acs_counties, aes(x=year, y=median_household_income, group = NAME, color = NAME,
                              text = paste0("Region: ", NAME,
                                            "<br>Year: ", year,
                                            "<br>Median Household Income: $", median_household_income,
                                            "<br>Margin of Error: $", median_household_income_moe))) +
  geom_line() + 
  geom_point() +
  scale_colour_manual(values = dspg) +
  #geom_pointrange(aes(ymin=median_household_income - median_household_income_moe, ymax=median_household_income + median_household_income_moe)) +
  theme_minimal() + ggtitle("Median Household Income 2015-2018") + ylab("Median Household Income") + xlab("Year")
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                       modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                   "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))











# tract map for 2018
ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = median_household_income)) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
  labs(title = "Median Household Income by census track") 



#--------- Household Income Distrubtion brackets-------------
#grouped bar charts
income <- select(filter(acs_counties, year == 2018), NAME, contains("income"))
income <- income %>% select(!contains("moe"), -median_household_income)
income <- melt(income, id.vars = "NAME", measure.vars = colnames(income)[-1])
ggplotly(ggplot(income)+
  geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge")+ 
  scale_fill_discrete(name = "Income Bracket", labels = c("Less than 10,000", "10,000-14,999", "15,000-24,999",
                                                          "25,000-34,999", "35,000-49,999", "50,000-74,999", 
                                                          "75,000-99,999","100,000-149,999", "150,000-199,999", "above 200,000")) +
  ylab("% of Population") + xlab("Region") +
  ggtitle("Income Distribution for 2018"))
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="incomedist18.png", plot=last_plot())

#stacked bar charts
ggplotly(ggplot(income, aes(fill=variable, y=value, x=NAME))+
           geom_bar(position = position_stack(reverse = TRUE), stat="identity")+ 
           scale_fill_discrete(name = "Income Bracket", labels = c("Less than 10,000", "10,000-14,999", "15,000-24,999",
                                                                   "25,000-34,999", "35,000-49,999", "50,000-74,999", 
                                                                   "75,000-99,999","100,000-149,999", "150,000-199,999", "above 200,000")) +
           ylab("% of Population") + xlab("Region") +
           ggtitle("Income Distribution for 2018"))%>% config(displayModeBar = "static", displaylogo = FALSE, 
                                                              modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                                          "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))



# ------------- poverty rate --------------------







#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="poverty18.png", plot=last_plot())

# poverty rate tract map for 2018
ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = below_poverty)) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
  labs(title = paste("Percent of population below poverty by census track in", 2018, sep=" "))



##### EMPLOYMENT ########

#------------- Employment ratio for adults 20-64 ----------------------
# bar graphs
ggplot(filter(acs_counties, year == 2018), aes(x = NAME, y = employment_20_to_64)) +
  geom_col(fill = "dark blue")+
  geom_errorbar(aes(x = NAME, ymin = employment_20_to_64 - employment_20_to_64_moe, 
                    ymax = employment_20_to_64 + employment_20_to_64_moe), color = "dark orange") + 
  geom_point(color = "dark orange", size = 3) + ggtitle("% of Adults (20-64) with Employment Status")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="employment18.png", plot=last_plot())


















# sf map
ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = employment_20_to_64)) +
  labs(title = "Percent of employed adults adults 20 to 64 by census track") #+ 
#theme_map)()
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="employment_by_tract18.png", plot=last_plot())


#### HOUSING ######
#--------- housing affordability-------------














# prepare for grouped charts convert wide to long.
housing <- select(filter(acs_counties, year == 2018), NAME, contains("affordable_housing"))
housing <- housing %>% select(NAME, contains("perc"))
housing <- melt(housing, id.vars = "NAME", measure.vars = colnames(housing)[-c(1,4)])
#grouped bar chart for own and rent occupancy
ggplotly(ggplot(housing)+
           geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge",
                    text = paste0("Region: ", NAME,
                                  "<br>Year: ", year,
                                  "<br>Affordable Housing: ", round(affordable_housing_all_perc, digits = 1), "%")) + 
           scale_fill_discrete(name = "Housing Ownership", labels = c("Own", "Rent")) +
           theme_minimal() + ylab("% of Occupied housing units") + xlab("Region") +
           ggtitle("Affordable Housing 2015-2018", subtitle = "Occupied households where monthly costs are less than 30% of houshold income"))

#divergent bar chart to split up own and rent occupancy
housing_diverge <- housing %>% mutate(value = as.numeric(ifelse(variable == "affordable_housing_own_perc",
                                                     value, -1*value)))
ggplotly(ggplot(housing_diverge,
                aes(x = NAME, y = value, fill = variable,
                    text = paste0("Region: ", NAME,
                                  "<br>Year: ", 2018,
                                  "<br>Affordable Housing: ", round(abs(housing_diverge$value), digits = 1), "%")))+
           geom_bar(stat = "identity") + 
           scale_y_continuous(breaks = pretty(housing_diverge$value), labels = abs(pretty(housing_diverge$value))) +
           scale_colour_manual(name = "Housing Ownership", values = c(dspgpal[1], dspgpal[9])) +
           scale_fill_discrete(name = "Housing Ownership", labels = c("Own", "Rent")) +
           theme_minimal() + labs(x="Region",y="% of Occupied Housing Units") +
           coord_flip(), tooltip = "text") %>% layout(title = list(text = paste0("Affordable Housing 2015-2018",
                                           '<br>','<sup>',
                                           "% of occupied households where monthly costs are less than 30% of houshold income",
                                            '</sup>'))) 



####### SOCIAL ########
#----------Racial Diversity---------------
#convert wide to long (split up moe and remerge....)
race <- acs_counties %>% filter(year == 2018) %>% select(GEOID,NAME, year, contains("race")) # select appropriate variables
race_moe <- race %>% select(NAME,contains("moe")) #separate moe estimates
race_moe <- race_moe %>% melt(id.vars = "NAME", measure.vars = colnames(race_moe)[-1]) %>%
  rename(moe = value)
race <- race %>% select(!contains("moe"))
race <- melt(race, id.vars = "NAME", measure.vars = colnames(race)[-c(1:3)])
race <- cbind(race, moe = race_moe$moe)

#plot all races onto one large set of grouped bars for every county.
ggplot(race)+
  geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge")+ 
  #geom_errorbar(aes(x = as.factor(NAME), ymin=value-moe, ymax=value+moe), width=.2,position="dodge") +
  scale_fill_discrete(name="Groups",labels = c("White", "Black or African American", "American Indian or Alaskan Native",
                                 "Asian", "Native Hawaiian or Pacific Islander", "Hispanic or Latino", 
                                 "Two or More","Other")) +
  ylab("% of Population") + xlab("Region") +
  ggtitle("% Racial and Ethnic Diversity")
#ggsave(path="~/git/dspg20wasco/output", device = "png", filename="diversityt18.png", plot=last_plot())





#one race being filled in on map for a single year in all tracts and counties
ggplot() +
  geom_sf(data = filter(acs_tracts, year == 2018), aes(fill = race_hispanic)) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
  labs(title = paste("Percent of population Hispanic or Latino by census track in", 2018, sep=" "))



#----------Family Stability--------------#



