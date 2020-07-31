library(tidycensus)
library(tidyverse);library(forcats)
library(data.table)
library(ggplot2);library(plotly)
library(sf)
library(ggthemes);library(maps);library(tigris)
library(here)


# generally... aesthetics need to be improved and plotly features improved.... 

#read in combined dataset
acs_data <- fread(here("/data/acs/combined_acs.csv")) 
acs_data$GEOID <- as.character(acs_data$GEOID)
acs_counties <- filter(acs_data, NAME == "South Wasco County School District 1, Oregon" | 
                         NAME == "Wasco County, Oregon"| NAME == "Hood River County, Oregon" |
                         NAME == "Sherman County, Oregon" | NAME == "Jefferson County, Oregon" |
                         NAME == "Multnomah County, Oregon" | NAME == "Clackamas County, Oregon" |
                         NAME == "Marion County, Oregon" | NAME == "Washington County, Oregon" |
                         NAME == "Deschutes County, Oregon" | NAME == "Lane County, Oregon" | 
                         NAME == "Umatilla County, Oregon" |
                         NAME == "Skamania County, Washington" | NAME == "Klickitat County, Washington" | 
                         NAME == "Oregon")  
acs_counties_neighbors <- filter(acs_data, NAME == "South Wasco County School District 1, Oregon" | 
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

# color palette from : https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
graypal = "#ADB5BD"
# Use viridis



##### FINANCIAL ########

# -------------- Median Household Income ------------------------
p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), other_level = "Neighboring Counties"))
            , aes(x=year, y=median_household_income, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Median Household Income: $", median_household_income,
                                "<br>Margin of Error: $", median_household_income_moe))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(viridis(3, option = "D"), graypal)) +
  scale_alpha_manual(values=c(1,1,1,0.1)) +
  theme_minimal() + ggtitle("Median Household Income 2015-2018") + ylab("Median Household Income") + xlab("Year") 
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))





#--------- Household Income Distrubtion brackets-------------
#stacked bar charts
income <- acs_counties %>% select(NAME, year, contains("income"))
income_perc <- income %>% select(!contains("moe"), -median_household_income, NAME, year)
income_moe <- income %>% select(NAME, year, contains("moe"), -median_household_income_moe)
income_perc <- melt(income_perc, id.vars = c("NAME", "year"), measure.vars = colnames(income_perc)[-c(1,2)])
income_moe <- income_moe %>% melt(id.vars = c("NAME","year"), measure.vars = colnames(income_moe)[-c(1,2)]) %>%
  rename(moe = value)  %>% mutate(variable =gsub("_moe", "", variable))
income_table <- merge(x = income_perc, y = income_moe, by=c("NAME", "variable", "year"))

ggplotly(ggplot(filter(income_table, year ==2018))+
           geom_bar(aes(fill=variable, y=value, x=NAME), position = position_stack(reverse = TRUE), stat="identity")+ 
           scale_fill_manual(name ="Income Bracket",values = viridis(10, option = "D")) +
           # scale_fill_discrete(name = "Income Bracket", labels = c("Less than 10,000", "10,000-14,999", "15,000-24,999",
           #                                                         "25,000-34,999", "35,000-49,999", "50,000-74,999", 
           #                                                         "75,000-99,999","100,000-149,999", "150,000-199,999", "above 200,000")) +
           ylab("% of Population") + xlab("Region") +
           scale_colour_manual(values = viridis_pal(option = "D")(10)) +
           ggtitle("Income Distribution for 2018") + coord_flip()) %>% 
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian",
                                     "hoverCompareCartesian","resetScale2d"), tooltip = "text") 



# ------------- poverty rate --------------------

p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), other_level = "Neighboring Counties"))
            , aes(x=year, y=below_poverty, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Percent Below Federal Poverty: ", below_poverty, "%",
                                "<br>Margin of Error: ", below_poverty_moe, "%"))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(viridis(3, option = "D"), graypal)) +
  scale_alpha_manual(values=c(1,1,1,0.1)) +
  theme_minimal() + ggtitle("Percent Below Federal Poverty: 2015-2018") + ylab("Percent Below Federal Poverty") + xlab("Year") 
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))



##### EMPLOYMENT ########

#------------- Employment ratio for adults 20-64 ----------------------
# bar graphs
p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), other_level = "Neighboring Counties"))
            , aes(x=year, y=employment_20_to_64, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Percent Employed: ", employment_20_to_64, "%",
                                "<br>Margin of Error: ", employment_20_to_64_moe, "%"))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(viridis(3, option = "D"), graypal)) +
  scale_alpha_manual(values=c(1,1,1,0.1)) +
  theme_minimal() + ggtitle("Employment Ratio for Adults 20 to 64: 2015-2018") + ylab("Employment Ratio") + xlab("Year") 
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))




#------------- Labor Force Participation Rate for adults 20-64 ----------------------
p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), other_level = "Neighboring Counties"))
            , aes(x=year, y=labor_force_20_to_64, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Labor Force Participation Rate: ", labor_force_20_to_64, "%",
                                "<br>Margin of Error: ", labor_force_20_to_64_moe, "%"))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(viridis(3, option = "D"), graypal)) +
  scale_alpha_manual(values=c(1,1,1,0.1)) +
  theme_minimal() + ggtitle("Labor Force Participation Rate for Adults 20 to 64: 2015-2018") + ylab("Labor Force Participation Rate") + xlab("Year") 
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))




#### HOUSING ######
#--------- housing affordability-------------
p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"), 
                                                            other_level = "Neighboring Counties"))
            , aes(x=year, y=affordable_housing_all_perc, group = NAME, color = south_wasco,
                  text = paste0("Region: ", NAME,
                                "<br>Year: ", year,
                                "<br>Affordable Housing: ", round(affordable_housing_all_perc, digits = 1), "%"))) +
  geom_line(size = 1.5) + 
  geom_point(size = 2) +
  scale_colour_manual(name = "Region", values = c(viridis(3, option = "D"), graypal))  +
  theme_minimal() + ggtitle("Affordable Housing 2015-2018") + ylab("Affordable Housing") + xlab("Year")
#Note: Wasco and south wasco are from ACS5 year estimates. Moving averages.
ggplotly(p, tooltip = "text") %>% config(displayModeBar = "static", displaylogo = FALSE, 
                                         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

# --------- rent vs. own housing affordability -------------------------#
# unsure of how this will add to the narrative.... but here it is anyway.
housing <- select(acs_counties, NAME, year, contains("affordable_housing"))
housing_rent_own_perc <- housing %>% select(NAME, year, affordable_housing_own_perc, affordable_housing_rent_perc)
housing_rent_own_moe <- housing %>% select(NAME, year, affordable_housing_own_perc_moe, affordable_housing_rent_perc_moe) 
housing_rent_own_perc <- melt(housing_rent_own_perc, id.vars = c("NAME", "year"),measure.vars = colnames(housing_rent_own_perc)[-c(1,2)])
housing_rent_own_moe <- melt(housing_rent_own_moe, id.vars = c("NAME", "year"),measure.vars = colnames(housing_rent_own_moe)[-c(1,2)]) %>%
  rename(moe = value)  %>% mutate(variable =gsub("_moe", "", variable))
housing_rent_own_table <- merge(x = housing_rent_own_perc, y = housing_rent_own_moe, by=c("NAME", "variable", "year"))


#grouped bar chart for own and rent occupancy
ggplotly(ggplot(filter(housing_rent_own_table, year == 2018),aes(x = NAME, y = value, fill = variable), 
                text = paste0("Region: ", NAME,
                              "<br>Year: ", year,
                              "<br>Affordable Housing: ", round(value, digits = 1), "%")) +
           geom_col(position = "dodge") + 
           scale_fill_discrete(name = "Housing Ownership", labels = c("Own", "Rent")) +
           #theme_minimal() + theme(axis.text.x = element_text(angle=30)) + 
           ylab("% of Occupied housing units") + xlab("Region") + coord_flip() + theme_minimal() +
           ggtitle("Affordable Housing 2015-2018", subtitle = "Occupied households where monthly costs are less than 30% of houshold income"), tooltip = "text")

####### SOCIAL ########
#----------Racial Diversity---------------
race <- acs_counties_neighbors %>% select(GEOID,NAME, year, contains("race")) # select appropriate variables
race_moe <- race %>% select(NAME,year, contains("moe")) #separate moe estimates
race_moe <- race_moe %>% melt(id.vars = c("NAME","year"), measure.vars = colnames(race_moe)[-c(1,2)]) %>%
  rename(moe = value)  %>% mutate(variable =gsub("_moe", "", variable))
race <- race %>% select(!contains("moe"), NAME, year)
race <- melt(race, id.vars = c("NAME", "year"),measure.vars = colnames(race)[-c(1,2)])
race_table <- merge(x = race, y = race_moe, by=c("NAME", "variable", "year"))

#plot all races onto one large set of grouped bars for every county.
ggplotly(ggplot(filter(race_table, year == 2018)) +
           geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge")+ 
           #geom_errorbar(aes(x = as.factor(NAME), ymin=value-moe, ymax=value+moe), width=.2,position="dodge") +
           scale_fill_manual(values = viridis(8, option="D"),name="Groups",labels = c("White", "Black or African American", "American Indian or Alaskan Native",
                                                                                      "Asian", "Native Hawaiian or Pacific Islander", "Hispanic or Latino", 
                                                                                      "Two or More","Other")) +
           #theme_minimal() + theme(axis.text.x = element_text(angle=45)) + 
           ylab("% of Population") + xlab("Region") + coord_flip() + theme_minimal() +
           ggtitle("% Racial and Ethnic Diversity"))


#####----------Family Stability--------------#####
family <- select(filter(acs_counties_neighbors), NAME, year,contains("family"))
family_perc <- family %>% select(NAME, year, family_married_parent_perc, family_single_parent_perc, 
                                 family_children_nonfamily_perc, family_nonfamily_household_perc,
                                 family_nonfamily_household_perc)
family_moe <- family %>% select(NAME, year, family_married_parent_perc_moe, family_single_parent_perc_moe,
                                family_children_nonfamily_perc_moe, family_nonfamily_household_perc_moe,
                                family_nonfamily_household_perc_moe)
family_moe <- melt(family_moe, id.vars = c("NAME","year"), measure.vars = colnames(family_moe)[-c(1,2)]) %>% 
  rename("moe" ="value") %>% mutate(variable =gsub("_moe", "", variable))
family_perc <- melt(family_perc, id.vars = c("NAME","year"), measure.vars = colnames(family_perc)[-c(1,2)])
family_table <- merge(x = family_perc, y = family_moe, by=c("NAME", "variable", "year"))
#grouped bar chart for family type

ggplotly(ggplot(filter(family_table, year == 2018), aes(x = NAME, y = value, fill = variable), 
                text = paste0("Region: ", NAME,
                              "<br>Year: ", year,
                              "<br>% Children: ", round(value, digits = 1), "%")) +
           geom_col(position = "dodge") + 
           scale_fill_manual(values = viridis(4, option="D"), name="Family Type")  +
           ylab("% of children") + xlab("Region") + coord_flip()+ theme_minimal() +
           ggtitle("Family Structure for Children under 18-2018"))



#----------Educational Attainment--------------#
ed <- select(filter(acs_counties_neighbors), NAME, year, contains("education"))
ed_perc <- ed %>% select(NAME, year,education_less_hs, education_hs_grad, education_assoc_some_college, education_bachelors_or_higher)
ed_moe <- ed %>% select(NAME, year, education_less_hs_moe, education_hs_grad_moe, 
                        education_assoc_some_college_moe, education_bachelors_or_higher_moe)
ed_moe <- melt(ed_moe, id.vars = c("NAME", "year"), measure.vars = colnames(ed_moe)[-c(1,2)]) %>% 
  rename("moe" ="value") %>% mutate(variable =gsub("_moe", "", variable))
ed_perc <- melt(ed_perc, id.vars = c("NAME", "year"), measure.vars = colnames(ed_perc)[-c(1,2)])
ed_table <- merge(x = ed_perc, y = ed_moe, by=c("NAME", "variable", "year"))
#grouped bar chart for own and rent occupancy
ggplotly(ggplot(filter(ed_table, year == 2018)) +
           geom_col(aes(x = NAME, y = value, fill = variable), position = "dodge") +
           scale_fill_manual(values = viridis(4, option = "D"),
                             name = "Educational Attainment",
                             breaks = c("education_less_hs","education_hs_grad","education_assoc_some_college", "education_bachelors_or_higher"),
                             labels = c("Less than High School", "High School Graduate or Equivalent (GED)","Associates Degree or Some College", "Bachelors or Higher")) +
           #theme_minimal() + theme(axis.text.x = element_text(angle=30)) + 
           ylab("% of Adults 25 and Older") + xlab("Region") + 
           coord_flip()+ theme_minimal() +
           ggtitle("Educational Attainment for Adults 25 and older-2018"))