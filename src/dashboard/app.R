## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tmaptools)
library(sf)
library(htmltools)
library(here)
library(shinyWidgets)
library(data.table)
library(dashboardthemes)
library(stringr)
library(tigris)
library(forcats)
library(ggplot2)
library(plotly)
library(ggthemes)
library(maps)
library(R.utils)
library(dplyr)
library(readr)
library(DT)
library(viridis)
library(colorspace)
library(osmdata)
library(formattable)
library(shinyjs)
library(esri2sf)
library(traveltime)
library(rmapzen)


# DATA: Sourcing theme, shp files ------
source("theme.R")
source("loadbaselayers.R")
source("loadoverlays.R")
options(tigris_use_cache = TRUE)

# DATA: Loading data -----
#read in combined dataset
acs_data <- fread(("Data/combined_acs.csv"))
acs_data$GEOID <- as.character(acs_data$GEOID)
acs_counties <- readRDS(("Data/acs_counties.Rds"))
acs_counties <- acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, OR",
                                                                               "Wasco County, OR", "Oregon"),
                                                                other_level = "Neighboring Counties"),
                                        #### To get the south wasco lines to be most visible, order the factor levels
                                        #### so that south wasco is drawn last (drawn over the rest)
                                        #### Plotly eliminates transparency of lines
                                        south_wasco = factor(south_wasco , levels= c("Neighboring Counties",
                                                                                     "Oregon","Wasco County, OR",
                                                                                     "South Wasco County School District 1, OR")))
acs_counties_neighbors <- filter(acs_counties, NAME == "South Wasco County School District 1, OR" |
                                   NAME == "Wasco County, OR"| NAME == "Hood River County, OR" |
                                   NAME == "Sherman County, OR" | NAME == "Jefferson County, OR" |
                                   NAME == "Skamania County, WA" | NAME == "Klickitat County, WA" |
                                   NAME == "Oregon")
#get tract level geography
acs_tracts <- readRDS(("Data/acs_tracts.Rds"))
acs_tracts$NAME.x <- NULL

or_county_lines <- counties(state = "OR")
wa_county_lines <- counties(state = "WA")
county_lines <- rbind(filter(or_county_lines, NAME %in%
                               c("Wasco", "Hood River", "Sherman", "Jefferson")),
                      filter(wa_county_lines, NAME %in% c("Skamania", "Klickitat"))
)

income_dist <- dplyr::select(acs_tracts, NAME, year, contains("income"), geometry)%>% dplyr::select(!contains("moe"), -median_household_income)
income_dist_2018 <- filter(income_dist, year == 2018)
income_dist_2017 <- filter(income_dist, year == 2017)
income_dist_2016 <- filter(income_dist, year == 2016)
income_dist_2015 <- filter(income_dist, year == 2015)
income_dist_moe <- dplyr::select(acs_tracts, NAME, year, contains("income"), geometry) %>% select(!contains("total"))

edu_attain <- dplyr::select(acs_tracts, NAME, year, contains("education"), geometry) %>% dplyr::select(!contains("moe"))
edu_attain_2018 <- filter(edu_attain, year == 2018)
edu_attain_2017 <- filter(edu_attain, year == 2017)
edu_attain_2016 <- filter(edu_attain, year == 2016)
edu_attain_2015 <- filter(edu_attain, year == 2015)
edu_attain_moe <- dplyr::select(acs_tracts, NAME, year, contains("education"), geometry) %>% select(!contains("total"))

fam_stab <- dplyr::select(acs_tracts, NAME, year, contains("family"), geometry)
fam_stab <- select(fam_stab, !contains("moe")) %>% select(!contains("total"))
fam_stab_2018 <- filter(fam_stab, year == 2018)
fam_stab_2017 <- filter(fam_stab, year == 2017)
fam_stab_2016 <- filter(fam_stab, year == 2016)
fam_stab_2015 <- filter(fam_stab, year == 2015)
fam_stab_moe <- dplyr::select(acs_tracts, NAME, year, contains("family"), geometry) %>% select(!contains("total"))

race_div <- dplyr::select(acs_tracts, NAME, year, contains("race"), geometry)
race_div <- select(race_div, !contains("moe"))
race_div_2018 <- filter(race_div, year == 2018)
race_div_2017 <- filter(race_div, year == 2017)
race_div_2016 <- filter(race_div, year == 2016)
race_div_2015 <- filter(race_div, year == 2015)
race_div_moe <- dplyr::select(acs_tracts, NAME, year, contains("race"), geometry) %>%
  select(!contains("total"))

ed <- fread("Data/education_dash.csv")
ed.increase <- ed %>% select("year" ,"District.Name","On.Time.Grad.Rate", "Teacher.Experience.Pct","Percent.ELA.Proficient.Change")
ed.decrease <- ed %>% select("year" ,"District.Name","Percent.Economically.Disadvantaged", "Percent.Chronically.Absent","Dropout.Rate")

alice_counties <- fread("Data/alice_counties.csv") %>%
  mutate(County = paste0(County, " County, " ,State_Abbr),
         Wasco = fct_other(County, keep = c("Wasco County, OR", "Hood River County, OR",
                                            "Klickitat County, WA", "Jefferson County, OR",
                                            "Sherman County, OR", "Skamania County, WA"),
                           other_level = "Other Counties"),
         Wasco = factor(Wasco, levels = c("Other Counties", "Hood River County, OR",
                                          "Klickitat County, WA", "Jefferson County, OR",
                                          "Sherman County, OR", "Skamania County, WA",
                                          "Wasco County, OR"))) %>%
  select(-c("GEO.id" ,"GEO.display_label","State","State_Abbr","Poverty_Household","ALICE_Household"))

# color palette from : https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
graypal = "#ADB5BD"
## Loading in LODES
top_12_in <- data.table(read_csv("Data/app_12_inflows_wasco.csv"))
top_12_out <- data.table(read_csv("Data/app_12_outflows_wasco.csv"))
agg_17 <- readRDS("Data/app_lodes_od_agg_2017.Rds")
agg_16 <- readRDS("Data/app_lodes_od_agg_2016.Rds")
agg_15 <- readRDS("Data/app_lodes_od_agg_2015.Rds")
#wasco_points <- blocks("OR", county = "Wasco")
#wasco_lines <- data.frame(wasco_points)
south_wasco_points <- st_read("Data/shps/swsd")
wasco_geo_points <- st_read("Data/shps/county")
water_use_by_sector_t <- data.table(readRDS("Data/app_usgs_water_use.Rds"))
acres_17 <- readRDS("Data/app_acres_17.Rds")
acres_16 <- readRDS("Data/app_acres_16.Rds")
acres_15 <- readRDS("Data/app_acres_15.Rds")

# Color palettes -----
# dspgpal <- c("#232D4B", "#2C4F6B", "#0E879C", "#60999A", "#D1E0BF",
#             "#D9E12B", "#E6CE3A", "#E6A01D", "#E57200", "#ADB5BD")
foodpal <- colorFactor("Set1", domain = food_points$shop)
isochronepal <- colorFactor("Blues", domain = isochrones$value)


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('logo');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_white-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColors-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "


## UI: Begins -------

ui <- dashboardPagePlus(

## UI: Dashboard header -------
  dashboardHeaderPlus(
  left_menu = tagList(div("Creating an Economic Mobility Baseline for the South Wasco County Area", style="height:35px; display:flex; align-items: center;"))
),

## UI: Dashboard sidebar --------
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem(
        tabName = "overview",
        text = "Project Overview",
        icon = icon("info circle")
      ),
      menuItem(
        tabName = "datamethods",
        text = "Methodology & Data",
        icon = icon("database"),
        menuSubItem(
          tabName = "methods",
          text = "Methods & Frameworks",
          icon = icon("project-diagram")
        ),
        menuSubItem(
          tabName = "data",
          text = "Data Collection",
          icon = icon("search")
        )
      ),

## UI: Findings menu ------
      menuItem(
        tabName = "findings",
        text = "Findings",
        icon = icon("chart-pie"),
## UI: Findings menu subitems (clusters and drivers) ---------
menuItem(
  tabName = "food",
  text = "Cluster: Food Systems",
  icon = icon("utensils")
),
menuItem(
  tabName = "infrastructure",
  text = "Cluster: Infrastructure",
  icon = icon("truck-pickup")
),
menuItem(
  tabName = "learnearn",
  text = "Driver: Opportunities to Learn and Earn",
  icon = icon("graduation-cap"),
  menuSubItem(tabName = "learn",
              text = "Education",
              icon = icon("school")),
  menuSubItem(tabName = "earn",
              text = "Employment",
              icon = icon("briefcase"))),
menuItem(
  tabName = "living",
  text = "Driver: Quality Standard of Living",
  icon = icon("laugh-beam"),
  menuSubItem(tabName = "financial",
              text = "Financial",
              icon = icon("money-bill-wave")),
  menuSubItem(tabName = "housing",
              text = "Housing",
              icon = icon("home")),
  menuSubItem(tabName = "social",
              text = "Social",
              icon = icon("users"))
)),
# Team menu
menuItem(
  tabName = "team",
  text = "Team",
  icon = icon("users")
)
)), # END OF SIDEBAR

## UI: Dashboard body -------
  dashboardBody(
    useShinyjs(),
    customTheme,
    fluidPage(
      tabItems(
## UI: TAB - Overview --------
        tabItem(tabName = "overview",
                fluidRow(
                  boxPlus(
                    closable = FALSE,
                    width = NULL,
                    enable_label = FALSE,
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    h1("Creating an Economic Mobility Baseline for the South Wasco County Area"),
                    h2("Project Description"),
                    p("The southern region of Wasco County, Oregon (South Wasco) experienced significant economic decline in the 1980s, causing closure of schools and consolidation of students into the South Wasco School District. This precipitated an eventual out-migration, disruption of social fabric, and a steady decline in overall economic stability, community health, standard of living, and quality of life."),
                    ## Here we will add a picture of SW for description
                    h2("Project Goals"),
                    p("The purpose of our project is to provide community decisionmakers with baseline datasets and comparative analyses to similar rural areas of the likely factors affecting economic mobility"),
                    h2("Project Scope"),
                    p("The term South Wasco is defined by the South Wasco County School District, which encompasses the southernmost region of Wasco County, Oregon."),
                    leafletOutput("overviewmap")
                  )
                )),

## UI: TAB - Food systems cluster ---------
        tabItem(tabName = "food",
                fluidRow(
                  selectInput(
                    inputId = "foodselect",
                    label = "I'm wondering...",
                    choices = list(" " = " ",
                      "How accessible is healthy and affordable food in South Wasco?" = "Foodmap",
                      "What is the food insecurity rate in South Wasco?" = "Insecurity",
                      "What local crops are grown in South Wasco?" = "Crops"),
                    width = "500px", selected = "Foodmap"
                  ),
## UI: PANEL - Food systems map  ------
                conditionalPanel(
                  condition = "input.foodselect == 'Foodmap'",
                  h2("Interactive food systems map"),
                  fluidRow(
                  tabBox(
                    id = 1,
                    side = "left",
                    width = "12",
                    selected = "Food Map",
                    tabPanel("Food Map",
                             div(img(src="https://image.flaticon.com/icons/svg/3175/3175153.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("Because of our particular focus on food systems, creating the interactive food systems map
                                 was a critical piece of this project. Food insecurity is a complex idea, as is
                                 geographic isolation from food. Here, we've mapped out this idea, with a special focus on
                                 quality and affordable food sources. Notably, there are many areas of South Wasco,
                                 particularly in the eastern townships, that experience low access to quality food sources.",
                                 style = "font-size:18px")
                             ),
                             selectInput("iso", "Show driving time for...",
                                         choices = isochrones$name,
                                         selectize = TRUE,
                                         multiple = TRUE,
                                         width = "500px"),
                             leafletOutput("mymap")
                    ),
                    tabPanel("Data",
                             DTOutput("foodDT"))
                  ))

                  ),
## UI: PANEL - Food insecurity  ----
                conditionalPanel(
                  condition = "input.foodselect == 'Insecurity'",
                  fluidRow(
                  tabBox(
                    id = 2,
                    side = "left",
                    width = "12",
                    selected = "Food Insecurity",
                    tabPanel("Food Insecurity",
                             div(img(src="https://image.flaticon.com/icons/svg/3175/3175153.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("Food insecurity is described as a household-level economic and social condition
                                 characterized by limited or uncertain access to food. To calculate food insecurity,
                                 we've used Feeding America's Map the Meal Gap research. Both the overall and childhood
                                 food insecurity rate for Wasco County has declined slightly from 2014-2017",
                                 style = "font-size:18px")
                             ),
                             selectInput("ratetype", "Which Food Insecurity Rate?",
                                         c("Overall", "Childhood")),
                             leafletOutput("foodinsecuritymap"),
                             box(
                               width = 12,
                               p(em("Data Citation: Gundersen, C., A. Dewey, A. Crumbaugh, M. Kato & E. Engelhard.
                                    Map the Meal Gap 2016-2019:  A Report on County and Congressional District
                                    Food Insecurity and County Food Cost in the United States in 2014-2017.
                                    Feeding America, 2016-2019." , style = "font-size:8px"))
                             )
                    ),
                    tabPanel("Data", "Data Tab Content")
                  ))
                  ),
## UI: PANEL - Local crops panel -----
                conditionalPanel(
                  condition = "input.foodselect == 'Crops'",
                  fluidRow(
                  tabBox(
                    id = 4,
                    side = "left",
                    width = "12",
                    selected = "Crop Map",
                    tabPanel("Crop Map",
                             div(img(src="https://image.flaticon.com/icons/svg/3175/3175153.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("Local farming is an integral piece of a thriving food system.
                                 Here we've mapped the acreage dedicated to several crops throughout Wasco County.
                                 In the future, we would like to find data sources to measure consumption of
                                 local food to complement this measure.",
                                 style = "font-size:18px")
                             ),
                             selectInput("crops", "Which crop?",
                                         c("Winter Wheat", "Barley",
                                           "Alfalfa", "Cherries")),
                             leafletOutput("cropmap")
                    ),
                    tabPanel("Data", "Data Tab Content")
                  ))
                  )
        )), # END OF FOOD SYSTEMS CLUSTER

## UI: TAB - Infrastructure cluster -----------------------------
        tabItem(tabName = "infrastructure",
                # Just topical question: wind and solar, broadband, water, transit
                # Add infocards for all (except water)
                # Add plot for water
                # Infocards don't need a back
                # Water needs a back, data and source
                fluidRow(
                  selectInput(
                  inputId = "infrastructureselect",
                  label = "I'm wondering...",
                  list(" " = " ",
                    "What is the importance of wind & solar projects to South Wasco?" = "WindSolar",
                    "What is access to broadband like and why is it important?" = "Broadband",
                    "What is water use like in Wasco?" = "Water",
                    "What is the transit system like in South Wasco?" = "Transit"),
                  width = "500px", selected = "WindSolar"
                )),
## UI: PANEL - Wind and solar -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'WindSolar'",
                  fluidRow(
                   h1("What is the importance of wind & solar projects to South Wasco?"),
                    #img of wind solar infocard,
                   HTML('<div class="canva-embed" data-design-id="DAEDe8MzN7A" data-height-ratio="2.5000" style="padding:250.0000% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAEDe8MzN7A&#x2F;view?utm_content=DAEDe8MzN7A&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Wind &amp; Solar Projects in Wasco</a>')
                   )
                  ),
## UI: PANEL - Broadband -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Broadband'",
                  fluidRow(
                  h1("What is access to broadband like and why is it important?"),
                  #img of broadband infocard
                  HTML('<div class="canva-embed" data-design-id="DAEDhGP13C4" data-height-ratio="2.5000" style="padding:250.0000% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAEDhGP13C4&#x2F;view?utm_content=DAEDhGP13C4&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Broadband Access</a> by Owen Hart')
                  )),
## UI: PANEL - Water -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Water'",
                  fluidRow(
                  tabBox(
                    id = 5,
                    side = "left",
                    width = "12",
                    selected = "Water Use",
                    tabPanel("Water Use",
                            div(img(src="https://image.flaticon.com/icons/svg/3175/3175152.svg", width = "15%"), style="text-align: center;"),
                            hr(),
                            p("Water use is an interesting infrastructure cluster to analyze because it can help
                                   describe the economic climate and importance of certain sectors through resource allocation.
                                   Over time we can see that Commercial water use is rising drastically, and then the
                                   category stops being recorded.  At the same time, new categories like Aquaculture and Mining
                                   appear, which may mean that Commercial water use was split into two subcategories.
                                   Of these, aquaculture had peaked in 2010 to levels similar to Commercial in 1995
                                   but is now falling.  These findings may point to decline in the profitability of the
                                   aquaculture sector in the recent years.",
                            style = "font-size:14px"),
                            hr(),
                            plotlyOutput("waterplot")
                    ),
                    tabPanel("Data", "Data Tab Content")
                  ))
                ),
## UI: PANEL - Transit -------
               conditionalPanel(
                 condition = "input.infrastructureselect == 'Transit'",
                 fluidRow(
                   h1("What is the transit system like in South Wasco?"),
                  # img of transit infocard
                   HTML('<div class="canva-embed" data-design-id="DAEDgP5Krv8" data-height-ratio="2.5000" style="padding:250.0000% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAEDgP5Krv8&#x2F;view?utm_content=DAEDgP5Krv8&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Transportation in Wasco County</a> by Owen Hart')
                ))), # END OF INFRASTRUCTURE


## UI: TAB -  Learn and earn driver -----------
       # tabItem(tabName = "learnearn",
       #         fluidRow(
       #           selectInput(
       #             inputId = "learnearnselect",
       #             label = "Select domain",
       #             list(" " = " ",
       #               "Education" = "Education",
       #               "Employment" = "Employment"),
       #             width = "300px", selected = NULL
       #           )),
## UI: PANEL - Education composite -------
tabItem(tabName = "learn",
        fluidRow(
                #conditionalPanel(
                  #condition = "input.learnearnselect == 'Education'",
                  # Education heatmaps
                  # Data tab will have data and indicator snippet/sources
          fluidRow(
          tabBox(
                    id = 6,
                    side = "left",
                    width = "12",
                    selected = "Education",
                    tabPanel("Education",
                             div(img(src="https://image.flaticon.com/icons/svg/1089/1089128.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("K-12 education is a formative experience that can increase economic and social mobility.
                                 We present heat maps of educational indicators that can effect student mobility below.
                                 Darker green corresponds to an increase of benefits to success and darker purple corresponds
                                 to an increase in barriers to student success. South Wasco has high percentages for on time
                                 graduation and teacher experience, implying the district is well resourced in these areas.
                                 However, all of the students are economically disadvantaged and chronic absenteeism rates are higher
                                 than nearby districts. This shows a need for more resources to aid students in overcoming
                                 the barriers that may be inhibiting their success in school and beyond.",
                                 style = "font-size:14px"),
                               br(),
                               p(" Teacher experience is defined as the percentage of teachers in the district who are certified
                                 and have taught for at least three years (not collected for 2016 and 2017).
                                 ELA Proficient Change is the percent change between 3rd and 8th grade in English Language Arts Proficiency",
                                 style = "font-size:14px")
                                  ),
                             selectInput("ed", "Educational Benefits or Educational Barriers?",
                                         c("Benefits", "Barriers")),
                              plotOutput("edplot", width = "100%", height = 800)
                    ),
                    tabPanel("Data", DTOutput("edDT")))
        ))), # END OF EDUCATION
# Employment select panel
tabItem(tabName = "earn",
                #conditionalPanel(
                #  condition = "input.learnearnselect == 'Employment'",
                #  # We are missing access to jobs that pay a living wage
                  fluidRow(
                    selectInput(
                      inputId = "employmentselect",
                      label = "I'm wondering...",
                      list(" " = " ",
                        "What is the employment ratio in South Wasco?" = "EmpRatio",
                        "What is the labor force participation rate in South Wasco?" = "LaborForce",
                        "How do workers flow in and out of South Wasco?" = "Flows",
                        "What types of jobs are in South Wasco?" = "Sectors"),
                      width = "300px", selected = NULL
                    )),
# UI: PANEL - Employment ratio  -------
conditionalPanel(
  condition = "input.employmentselect == 'EmpRatio'",
  fluidRow(
  tabBox(
    id = 7,
    side = "left",
    width = "12",
    selected = "Employment Ratio",
    tabPanel("Employment Ratio",
             div(img(src="https://image.flaticon.com/icons/svg/1724/1724966.svg", width = "15%"), style="text-align: center;"),
             box(
               width = 12,
               p("Over four years the employment ratio (number of employed / population)
        has decreased by 1% in South Wasco, while all of Wasco County experienced a ~4% increase.
        In 2018, South Wasco has about a 10% lower employment ratio than the state of Oregon.",
                 style = "font-size:18px")
             ),
            leafletOutput("percempmap"),
             plotlyOutput("empratioplot")),
    tabPanel("Data", DTOutput("empDT"))
  ))),
# UI: PANEL - Labor force participation rate -------
                conditionalPanel(
                  condition = "input.employmentselect == 'LaborForce'",
                  fluidRow(
                  tabBox(
                    id = 8,
                    side = "left",
                    width = "12",
                    selected = "Labor Force Rate",
                    tabPanel("Labor Force Rate",
                             div(img(src="https://image.flaticon.com/icons/svg/1724/1724966.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("The Labor force includes all people classified in the
                                civilian labor force in addition to members of the U.S. Armed Forces.
                                The civilian labor force consists of employed or unemployed people.
                                 Over 2015 to 2018 Wasco County hovered around 73% which is below the LFPR of all of Oregon at 77.9%.
                                 The south Wasco region hovered around 70, the most recently dropped to 67%, below all of Oregon as well.",
                                 style = "font-size:18px")
                             ),
                             leafletOutput("laborforcemap"),
                             plotlyOutput("laborforceplot")),
                    tabPanel("Data", DTOutput("LaborForceDT"))
                  ))
                ),
# UI: PANEL - Job flows  -------
                        conditionalPanel(
                          condition = "input.employmentselect == 'Flows'",
                          fluidRow(
                          tabBox(
                            id = 9,
                            side = "left",
                            width = "12",
                            selected = "Job Flows",
                            tabPanel("Job Flows",
                                     div(img(src="https://image.flaticon.com/icons/svg/1724/1724966.svg", width = "15%"), style="text-align: center;"),
                                     box(
                                       width = 12,
                                       p("Job inflows are the number of employees from
                                        outside counties traveling into the South Wasco region for work.
                                        Job outflows are the number of residents from the South Wasco region
                                        traveling to other counties for work. To tie in the outward migration story
                                         and show rural job networks, we tracked the flows of jobs in and out of Wasco county.
                                         The main conclusion here is that more jobs are flowing out of Wasco than are
                                         flowing into Wasco on all levels. Of those outflows, around 460 jobs are
                                         going out of state to Washington.",
                                         style = "font-size:16px")
                                     ),
                            selectInput("flows", "Inflows or Outflows?",
                                        c("Inflows", "Outflows")),
                            plotlyOutput("flowsplot")
                            ),
                            tabPanel("Data", "Data Tab Content")
                          ))
                        ),
## UI: PANEL - Industry Sectors  ------
                conditionalPanel(
                  condition = "input.employmentselect == 'Sectors'",
                  fluidRow(
                  tabBox(
                    id = 10,
                    side = "left",
                    width = "12",
                    selected = "Job Sectors",
                    tabPanel("Job Sectors",
                             div(img(src="https://image.flaticon.com/icons/svg/1724/1724966.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("Using the LODES data, we were able to breakdown jobs for
                                 people who work in Wasco by census tracts. The first map shows all
                                 the jobs in Wasco and then number of each, and you can see just how
                                 sparse employment opportunities are for the South Wasco region.",
                                 style = "font-size:18px")
                             ),
                             selectInput("sect", "What sectors?",
                                         c("All" = "All", "Goods Producing" = "Goods", "Trade,
                                           Transportation, and Utilities" = "Trade", "All Other
                                           Services" = "AllOther")),
                             leafletOutput("odleaf")),
                    tabPanel("Data", "Data Tab Content")
                  ))
                )
        ), ## END OF EMPLOYMENT

## UI: TAB - Quality standard of living driver -----------
       # tabItem(tabName = "living",
       #         fluidRow(
       #             # Remove dropdown button
       #             # Select domain, then select question
       #               selectInput(
       #                 inputId = "livingdomainselect",
       #                 label = "Select domain",
       #                 list(" " = " ",
       #                   "Financial" = "Financial",
       #                   "Housing" = "Housing",
       #                   "Social" = "Social"),
       #                 width = "300px", selected = NULL
       #               )),
# Financial select question
      #          conditionalPanel(
      #            condition = "input.livingdomainselect == 'Financial'",
tabItem(tabName = "financial",
        fluidRow(
                    selectInput(
                      inputId = "financialselect",
                      label = "I'm wondering...",
                      list(" " = " ",
                        "What is the median income in South Wasco?" = "MedIncome",
                        "What is the poverty rate in South Wasco?" = "Poverty",
                        "What is the ALICE poverty rate in Wasco county?" = "ALICE",
                        "What is the income distribution in South Wasco" = "DisIncome"),
                      width = "300px", selected = NULL
                    )),
## UI: PANEL - Median income  ------
                    conditionalPanel(
                      condition = "input.financialselect == 'MedIncome'",
                      fluidRow(
                      tabBox(
                        id = 11,
                        side = "left",
                        width = "12",
                        selected = "Median Income",
                        tabPanel("Median Income",
                                 div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                                 box(
                                   width = 12,
                                   p("The South Wasco region follows the trends of rising median income in all of Oregon,
                                  but from 2017 to 2018 their rate increased.  From 2015 to 2018 the median household income
                                  in South Wasco rose about $7,000.  This is compared to a similar increase in Wasco County generally,
                                  and a $9,000 increase in the state of Oregon.",
                                     style = "font-size:18px")
                                 ),
                       # Median income only here, poverty, income brackets are the questions
                        leafletOutput("medincomemap"),
                        plotlyOutput("medincomeplot")),
                        tabPanel("Data", DTOutput("MedianIncomeDT"))
                      ))
                    ),
## UI: PANEL - Poverty rate ------
                conditionalPanel(
                  condition = "input.financialselect == 'Poverty'",
                  fluidRow(
                  tabBox(
                    id = 12,
                    side = "left",
                    width = "12",
                    selected = "Poverty Rate",
                    tabPanel("Poverty Rate",
                             div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("The South Wasco region from 2015 to 2018 experienced a ~6% decline in poverty rates,
                                 while Wasco generally experienced a ~3% decline and the state of Oregon a ~3% as well.",
                                 style = "font-size:18px")
                             ),
                             leafletOutput("povertymap"),
                             plotlyOutput(outputId = "povertyplot")),
                    tabPanel("Data", DTOutput("povertyDT"))
                  ))
                ),
## UI: PANEL - ALICE Threshold rate ------

      conditionalPanel(
        condition = "input.financialselect == 'ALICE'",
        fluidRow(
        tabBox(
            id = 12,
            side = "left",
            width = "12",
            selected = "ALICE Poverty Rate",
            div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
            tabPanel("ALICE Poverty Rate",
                     hr(),
                p("ALICE is an acronym for Asset Limited, Income Constrained, Employed.
                These are households that earn above the federal poverty level, but not enough to
                afford a 'bare-bones' household budget. The ALICE threshold provides more granularity
                that is more suited for a rural community than the federal poverty line.
                Looking at South Wasco, over time we see only about a 2% increase in those who
                follow below the threshold, however in 2014, South Wasco experienced a peak of 37.25% of residents
                falling below this threshold.", style = "font-size:16px"),
                hr(),
            plotlyOutput(outputId = "aliceplot")),
            tabPanel("Data", "Data Tab Content")
        ))
      ),

## UI: PANEL - Income Distribution  ------
                conditionalPanel(
                  condition = "input.financialselect == 'DisIncome'",
                  fluidRow(
                  tabBox(
                    id = 13,
                    side = "left",
                    width = "12",
                    selected = "Income Distribution",
                    # Median income only here, poverty, income brackets are the questions
                    tabPanel("Income Distribution",
                             div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("We’ve broken the population down by income bracket for South Wasco and surrounding counties.
                                 Using the plot below, we can see that specifically in South Wasco, the income bracket
                                 held by most residents is 50K-75K, which is right around the median national income.
                                 It is important to note that South Wasco has more people in this bracket than Wasco County generally,
                                 and also that South Wasco has more people in the lowest income bracket than Wasco County generally.",
                                 style = "font-size:16px")
                             ),
                           selectInput("incomedisyear", "Which year?",
                                c("2018", "2017", "2016", "2015")),
                           leafletOutput("incomedismap"),
                           plotlyOutput("incomedisplot")),
                    tabPanel("Data", DTOutput("incomedistDT"))
                  ))
                )
), # END FINANCIAL
# Housing select question
tabItem(tabName = "housing",
#conditionalPanel(
#  condition = "input.livingdomainselect == 'Housing'",
  fluidRow(
    selectInput(
      inputId = "housingselect",
      label = "I'm wondering...",
      list(" " = " ",
        "How much affordable housing is in South Wasco?" = "Housing",
           "What is the home ownership rate in South Wasco?" = "RentOwn"),
      width = "300px", selected = NULL
    )),
## UI: PANEL - Affordable housing -----
                  conditionalPanel(
                    condition = "input.housingselect == 'Housing'",
                    fluidRow(
                    tabBox(
                      id = 14,
                      side = "left",
                      width = "12",
                      selected = "Affordable Housing",
                      div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                      tabPanel("Affordable Housing",
                               hr(),
                               p("The ratio of affordable housing is defined as the number housing units
                         where monthly costs are less than or equal to 30% of a household's income
                         divided by the total number of occupied houses. Overall, most of the regions have comparable housing affordability to the state level,
                         ranging from 60-70%. Generally, we can observe that Wasco county increased from one of
                         the lowest rates of affordable housing in 2015 to one of the highest in 2018.
                         However, the margin of error is quite high for the Wasco county estimates
                         which means there is a possibility the true percentage of affordable housing
                         could be at either the lower or higher range of the estimates.",
                                   style = "font-size:16px"),
                               hr(),
                      # Overall and ownership/rental (both lines and maps?)
                      # Full back with table and indicator snippet
                      plotlyOutput("housingplot"),
                      h4("For this metric, the margin of error for the South Wasco region was
                         almost equal to that of the estimate. Therefore, the estimate of
                         affordable housing was too unreliable to include in this chart.
                         ")),
                      tabPanel("Data", DTOutput("AffordableHousingDT"))
                    ))
                  ),
## UI: PANEL - Rent vs own -------
                conditionalPanel(
                  condition = "input.housingselect == 'RentOwn'",
                  fluidRow(
                  tabBox(
                    id = 15,
                    side = "left",
                    width = "12",
                    selected = "Home Ownership",
                    tabPanel("Home Ownership",
                             div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                             hr(),
                               p("A Housing unit is owner occupied if the owner or co-owner lives in the unit
                             even if it is mortgaged or not fully paid for. The South Wasco region has some of
                                 the highest owner occupied housing percentages accross the years compared to the rest
                                 of the regions, including the state and the rest of the county.
                                 However, it is important to note that there are strict land zoning laws in South Wasco
                                 which have restricted the development of rental properties in the area.",
                                 style = "font-size:16px"),
                             hr(),
                    # Overall and ownership/rental (both lines and maps?)
                    # Full back with table and indicator snippet
                    plotlyOutput("rentownplot")),
                    tabPanel("Data", DTOutput("homeownDT"))
                  ))
                )
), # END HOUSING



# Social select question
tabItem(tabName = "social",
#conditionalPanel(
#  condition = "input.livingdomainselect == 'Social'",
  fluidRow(
    selectInput(
      inputId = "socialselect",
      label = "I'm wondering...",
      list(" " = " ",
        "What is the racial diversity of South Wasco?" = "Race",
           "What types of familiy structures are in South Wasco?" = "Family",
           "What is the educational background of people in South Wasco?" = "Education"),
      width = "300px", selected = NULL
    )),
# UI: PANEL - Race  --------
                conditionalPanel(
                  condition = "input.socialselect == 'Race'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  fluidRow(
                  tabBox(
                    id = 16,
                    side = "left",
                    width = "12",
                    selected = "Race",
                    tabPanel("Race",
                             div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("For all years and all counties, 75% or more of the population identify as white.
                                 The only exception is Jefferson county where American Indian and Hispanic identities
                                 make up around 20% of the population each. The large American Indian pouplation is because
                                 the Warm Springs Reservation exists through parts of South Wasco and Northern Jefferson County.
                                 In South Wasco, Hispanic or Latino is identified as the second largest group.
                                 However, you will notice the margin of error is quite high for this estimate,
                                 which is shows that estimates from federal data sources tend to be unreliable for minority groups.",
                                 style = "font-size:16px")
                             ),
                             selectInput("raceyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                             leafletOutput("racemap"),
                             plotlyOutput("raceplot")),
                    tabPanel("Data", DTOutput("raceDT"))
                  ))
                ),
# UI: PANEL - Family ------
                conditionalPanel(
                  condition = "input.socialselect == 'Family'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  fluidRow(
                  tabBox(
                    id = 17,
                    side = "left",
                    width = "12",
                    selected = "Family",
                    tabPanel("Family",
                             div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("Generally, Wasco's percentages of family types for children under 18 are comparable to the state level.
                                 Like other counties, the majority of children are in married parent families (~70%)
                                 followed by single mothers (~20%) and then single fathers (>10%).
                                 The estimates for Wasco county are stable over time. South Wasco School District is removed because 
                                 the margins of error for all categories were too high, therefore making the estimates too unreliable to report. ",
                                 style = "font-size:16px")
                             ),
                             selectInput("familyyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                             leafletOutput("familymap"),
                             plotlyOutput("familyplot")),
                    tabPanel("Data", DTOutput("familyDT"))
                  ))
                ),
# UI: PANEL - Education attainment -------
                conditionalPanel(
                  condition = "input.socialselect == 'Education'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  fluidRow(
                  tabBox(
                    id = 18,
                    side = "left",
                    width = "12",
                    selected = "Education Degrees",
                    tabPanel("Education Degree",
                             div(img(src="https://image.flaticon.com/icons/svg/2692/2692837.svg", width = "15%"), style="text-align: center;"),
                             box(
                               width = 12,
                               p("The distribution of highest educational degree attained in South Wasco
                                 has stayed fairly consistent over the years. It has one of the lowest percentages
                                 of adults who have a bachelor's degree or higher. But the large majority
                                 of the population have at least a high school diploma or equivalent.",
                                 style = "font-size:16px")
                             ),
                             selectInput("degreeyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                             leafletOutput("degreemap"),
                             plotlyOutput("degreeplot")),
                    tabPanel("Data", DTOutput("degreeDT"))
                  ))
                )), # END SOCIAL TAB

## UI: TAB - Methods ----------
tabItem(tabName = "methods",
        fluidRow(
          boxPlus(
            closable = FALSE,
            height = "1300px",
            width = NULL,
            enable_label = FALSE,
            solidHeader = FALSE,
            collapsible = FALSE,
            h2("Methods and Frameworks"),
            # Subheadings for clusters
            # Dropdown menu to select cluster
            # Description with cluster visual
            # Just add more info/basics about these
            p("Our project weaves in principles and recommendations of Good Rural Data from the Urban Institute.
              Collecting data and performing analysis on a rural area like South Wasco poses unique challenges.
              Some of these challenges identified by Good Rural Data are small sample sizes and
              large margins of error. Federal agencies such as the American Community Survey
              sample a subset of an already small population in rural areas. Therefore, the resulting samples
              are too small to be representative of all sub groups in a rural population.
              This in turn results in large margins of error, particularly for
              minority and underrepresented groups in the population. With these issues in mind, we are reporting
              all margins of error to bring awareness to the possible inaccuracies in the data and
              caution users from extrapolating interpretations from certain estimates. "),
            p("Boosting Upward Mobility from the Urban Institute provides a multidimensional approach
              to economic mobility. The Urban Institute outlines three key drivers that propel
              individuals and families out of poverty over their lifetime.
              Our project adopts two of the drivers in the form of ‘Opportunities to Learn and Earn’ and
              ‘Quality Standard of Living’. The key predictors from these drivers can guide
              community leaders to make decision and take action to increase
              economic mobility of their community. This framework is used in conjunction with
              the Rural Clusters of Innovation Framework. "),
            # More info/basics
            p("The Rural Clusters of Innovation framework visualizes the community agencies and organizations
              that contribute to economic mobility increasing sectors. This framework can be tailored to
              specific communities and can serve as a guide towards the economic development of
              the entire rural area. As a result of close collaboration with community stakeholders,
              we have chosen three main clusters of Food Systems, Broadband and Infrastructure.
              You can explore these clusters with the dropdown menu below. "),
            selectInput("cluster", "Which cluster?",
                        c("Food Systems", "Infrastructure", "Maupin Broadband")),
            imageOutput("clusters")
            # Full indicators table
            # Select input for the "sheet" of indicator cluster/driver
            # General overview table of data sources and what they're meant for, include every "major" data source (not table necessarily)
          )
        )),
## UI: TAB - Data ----------
      tabItem(tabName = "data",
              fluidRow(
                boxPlus(
                  closable = FALSE,
                  width = NULL,
                  enable_label = FALSE,
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  # Subheadings for clusters
                  # Dropdown menu to select cluster
                  # Description with cluster visual
                  h2("Data Collection and Analysis"),
                  HTML('<div class="canva-embed" data-design-id="DAED3TW1C8w" data-height-ratio="1.2941" style="padding:129.4118% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAED3TW1C8w&#x2F;view?utm_content=DAED3TW1C8w&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Data Collection &amp; Sources</a> by Owen Hart'),
                  selectInput("indicators", "Which indicator?",
                              c("Cluster: Food Systems", "Cluster: Infrastructure", "Driver: Opportunities to Learn and Earn", "Driver: Quality Standard of Living")),
                  DTOutput("indicators_all_DT")
                  # Full indicators table
                  # Select input for the "sheet" of indicator cluster/driver
                  # General overview table of data sources and what they're meant for, include every "major" data source (not table necessarily)
                )
              )),

## UI: TAB - Findings  ---------
      tabItem(tabName = "findings",
              fluidRow(
                boxPlus(
                  title = "Findings",
                  closable = FALSE,
                  width = NULL,
                  enable_label = TRUE,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h2("General Overview of the Project"),
                  h3("Project description here"),
                  h2("Results Section One"),
                  h2("Results Section Two"),
                  h2("Results Section Three")
                )
              )),

## UI: TAB - Team ---------
      tabItem(tabName = "team",
              fluidRow(
                boxPlus(
                  closable = FALSE,
                  width = NULL,
                  enable_label = FALSE,
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  h2("DSPG Team Members"),
                  # Add headshots
                  img(src = "mary.jpg", width = "250px"),
                  p("Mary Solomon, DSPG Graduate Fellow (M.S. Applied Statistics), Bowling Green State University"),
                  img(src = "owen.png", width = "250px"),
                  p("Owen Hart, DSPG Intern, University of California Berkeley"),
                  img(src = "joanna.jpg", width = "250px"),
                  p("Joanna Schroeder, DSPG Intern, William & Mary"),
                  # Reach out an ask about headshots for them
                  h2("UVA SDAD Team Members"),
                  p(tags$a(href="https://biocomplexity.virginia.edu/aaron-schroeder", "Aaron Schroeder (PI), Research Associate Professor & Data Scientist (Ph.D. Public Policy)")),
                  p(tags$a(href="https://biocomplexity.virginia.edu/eric-oh", "Eric Oh, Research Assistant Professor of Statistics (Ph.D Biostatistics)")),
                  p(tags$a(href="https://biocomplexity.virginia.edu/alyssa-mikytuck", "Alyssa Mikytuck, Postdoctoral Associate (Ph.D Human Development)")),
                  # Add logos for these people
                  h2("Project Sponsors"),
                  img(src = "south_wasco_alliance_logo.png", width = "200px"),
                  p("Kathleen Willis, coordinating stakeholder, South Wasco Alliance"), p("Kathleen's team: Elle Christensen, Eva Kahn, Hannah Fuller"),
                  h2("Acknowledgements"),
                  p("Carrie Pipinich, Senior Project Manager, Mid-Columbia Economic District"),
                  p("Shannon Caplan, Program Coordinator, Rural Communities Explorer"),
                  p("Kelly Howsley-Glover, Long Range/Special Projects Planner, Wasco County Planning Department"),
                  p("Nick Green, City Manager of John Day, Oregon")
                )
              ))
      ) # end of TAB ITEMS (global dashboard body)
    ) # end of FLUID PAGE (global dashboard body)
  ) # end of DASHBOARD BODY
) # end of DASHBOARD UI



## SERVER: Begins --------
server <- function(input, output, session) {

  # Run JavaScript Code
  runjs(jscode)
  
  graypal = "#ADB5BD"


## SERVER: CLUSTER IMAGES -------
## imageOutput("clusters")
  output$clusters <- renderImage({
    if (input$cluster == "Food Systems"){
      list(src = "www/FoodSystemsFinal.png", width = "100%")
    }
    else if (input$cluster == "Infrastructure"){
      list(src = "www/InfrastructureFinal.png", width = "100%")
    }
    else if (input$cluster == "Maupin Broadband"){
      list(src = "www/MaupinBroadbandFinal.png", width = "100%")
    }
  })



## SERVER: INDICATOR TABLES -------
## DTOutput("indicators_all_DT") -----

    output$indicators_all_DT <- renderDT({
    if (input$indicators == "Cluster: Food Systems"){
      datatable(food_systems[,1:9], extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
                options = list(
                  dom = 'Bfrtip',
                  searching = TRUE,
                  autoWidth = TRUE,
                  rownames = FALSE,
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px",
                  fixedHeader = TRUE,
                  class = 'cell-border stripe',
                  buttons =
                    list("copy", list(
                      extend = "collection"
                      , buttons = c("csv", "excel", "pdf")
                      , text = "Download"
                    ))
                  #fixedColumns = list(
                  #leftColumns = 3,
                  #heightMatch = 'none')
                ))
    }
    else if (input$indicators == "Cluster: Infrastructure"){
      datatable(infrastructure[,1:9], extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
                options = list(
                  dom = 'Bfrtip',
                  searching = TRUE,
                  autoWidth = TRUE,
                  rownames = FALSE,
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px",
                  fixedHeader = TRUE,
                  class = 'cell-border stripe',
                  buttons =
                    list("copy", list(
                      extend = "collection"
                      , buttons = c("csv", "excel", "pdf")
                      , text = "Download"
                    ))
                  #fixedColumns = list(
                  #leftColumns = 3,
                  #heightMatch = 'none')
                ))
      }
    else if (input$indicators == "Driver: Opportunities to Learn and Earn"){
      datatable(learn_earn[,1:9], extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
                options = list(
                  dom = 'Bfrtip',
                  searching = TRUE,
                  autoWidth = TRUE,
                  rownames = FALSE,
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px",
                  fixedHeader = TRUE,
                  class = 'cell-border stripe',
                  buttons =
                    list("copy", list(
                      extend = "collection"
                      , buttons = c("csv", "excel", "pdf")
                      , text = "Download"
                    ))
                  #fixedColumns = list(
                  #leftColumns = 3,
                  #heightMatch = 'none')
                ))
      }
    else if (input$indicators == "Driver: Quality Standard of Living"){
      datatable(living[,1:9], extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
                options = list(
                  dom = 'Bfrtip',
                  searching = TRUE,
                  autoWidth = TRUE,
                  rownames = FALSE,
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px",
                  fixedHeader = TRUE,
                  class = 'cell-border stripe',
                  buttons =
                    list("copy", list(
                      extend = "collection"
                      , buttons = c("csv", "excel", "pdf")
                      , text = "Download"
                    ))
                  #fixedColumns = list(
                  #leftColumns = 3,
                  #heightMatch = 'none')
                ))
      }
    })

## SERVER: DATA TABLES -----
## SERVER: DATA TABLE - Food systems map -----
  output$foodDT <- renderDT({
    datatable(food_points,
                        extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
                        options = list(
                          dom = 'Bfrtip',
                          searching = TRUE,
                          autoWidth = TRUE,
                          rownames = FALSE,
                          scroller = TRUE,
                          scrollX = TRUE,
                          scrollY = "500px",
                          fixedHeader = TRUE,
                          class = 'cell-border stripe',
                          buttons =
                            list("copy", list(
                              extend = "collection"
                              , buttons = c("csv", "excel", "pdf")
                              , text = "Download"
                            ))
                          #fixedColumns = list(
                            #leftColumns = 3,
                            #heightMatch = 'none')
                          ))
  })
## SERVER: DATA TABLE - Industry sectors -----
  output$sectorsDT <- renderDT({
    datatable(datatable(rbind(agg_15, agg_16, agg_17),
                        extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                        options = list(
                          # dom = 't',
                          # deferRender = TRUE,
                          searching = TRUE,
                          autoWidth = TRUE,
                          # scrollCollapse = TRUE,
                          rownames = FALSE,
                          scroller = TRUE,
                          scrollX = TRUE,
                          scrollY = "500px",
                          fixedHeader = TRUE,
                          class = 'cell-border stripe',
                          fixedColumns = list(
                            leftColumns = 3,
                            heightMatch = 'none'))))})
  output$acscountiesDT <- renderDT({datatable(acs_counties,
                                              extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                                              options = list(
                                                # dom = 't',
                                                # deferRender = TRUE,
                                                searching = TRUE,
                                                autoWidth = TRUE,
                                                # scrollCollapse = TRUE,
                                                rownames = FALSE,
                                                scroller = TRUE,
                                                scrollX = TRUE,
                                                scrollY = "500px",
                                                fixedHeader = TRUE,
                                                class = 'cell-border stripe',
                                                fixedColumns = list(
                                                  leftColumns = 3,
                                                  heightMatch = 'none'
                                                )))})
## SERVER: DATA TABLE - Job flows  -----
    output$flowsDT <- renderDT({
    datatable(datatable(rbind(top_10_in, top_10_out),
                        extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                        options = list(
                          # dom = 't',
                          # deferRender = TRUE,
                          searching = TRUE,
                          autoWidth = TRUE,
                          # scrollCollapse = TRUE,
                          rownames = FALSE,
                          scroller = TRUE,
                          scrollX = TRUE,
                          scrollY = "500px",
                          fixedHeader = TRUE,
                          class = 'cell-border stripe',
                          fixedColumns = list(
                            leftColumns = 3,
                            heightMatch = 'none'))))})
## SERVER: TAB - Overview ------
## leafletOutput("overviewmap") -------
  output$overviewmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-121, 45.2, zoom = 9) %>%
      addPolygons(data = filter(neighboring_counties, NAME != "Wasco"), color = "#696969", label = ~NAME, group = "Basemap") %>%
      addPolygons(data = countyline, color = "#404788FF", label = "Wasco", opacity = 1, group = "Basemap") %>%
      addPolygons(data = swsd, color = "#440154FF", opacity = 1, label = ~NAME, group = "Basemap") %>%
      addPolygons(data = townships, color = "blue", opacity = .4, weight = 1, label = ~NAME, group = "Basemap") %>%
      addPolygons(data = unincorporated, color = "blue", opacity = .4, weight = 1, label = ~NAME, group = "Basemap") %>%
      addPolylines(data = roads,
                   color = "gray", weight = .75, group = "Basemap") %>%
      addLegend(colors = c("#404788FF", "#440154FF", "#696969", "blue"),
                labels = c("Wasco County", "South Wasco County School District", "Neighboring Counties", "Townships and Unincorporated Areas"),
                title = "Boundary Lines")
  })
## SERVER: TAB - Food systems cluster ----
## SERVER: PANEL - Food systems map ----
  ## Here is a reactive function filter the isochrone data by the selected input. I think the issue could be here because this function is not reacting to deselection.
  filteredData <- reactive({
    data <- isochrones %>% filter(name %in% input$iso)
    data
  })

  output$mymap <- renderLeaflet({
    #lonlat <- geocode_OSM(input$city)
    #mylon <- as.double(lonlat$coords[1])
    #mylat <- as.double(lonlat$coords[2])
    foodpal <- colorFactor("Set1", domain = food_points$shop)
    isochronepal <- colorFactor(viridis_pal(option = "A", begin = 0.2, end = 0.8)(2), domain = isochrones$values)

    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, group = "Stores"){
      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ",
                               sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
                               labels, "</div>")
      return(addLegend(map, "bottomright", colors = colorAdditions,
                       labels = labelAdditions, opacity = opacity, group = group))
      }

    leaflet() %>%
      addTiles() %>%
      setView(-121, 45.2, zoom = 9) %>%
      addPolylines(data = swsd, color = "purple", opacity = 1, group = "Basemap") %>%
      addPolylines(data = countyline, color = "grey", group = "Basemap") %>%
      addPolygons(data = townships, color = "blue", opacity = .4, weight = 1, label = ~NAME, group = "Basemap") %>%
      addPolygons(data = unincorporated, color = "blue", opacity = .4, weight = 1, label = ~NAME, group = "Basemap") %>%
      addPolylines(data = roads,
                   color = "gray", weight = .75, group = "Basemap") %>%
      #addMarkers(data = food_points, label = "HI", labelOptions = labelOptions(permanent = TRUE, textOnly = TRUE))
      addCircleMarkers(data = food_points,
                       color = ~foodpal(shop), fillOpacity = 1,
                       radius = 10,
                       stroke = FALSE,
                       popup = ~htmlEscape(name),
                       group = "Stores",
                       label = ~pymnt_types, labelOptions = labelOptions(permanent = TRUE, textOnly = TRUE, textsize = "10px", offset = c(0,0), direction = "center")) %>%
      addPolygons(data = filteredData(), color = ~isochronepal(value),
                  group = "isochrones") %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Stores"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(colors = c("white", "white"),
                labels = c("S = SNAP", "W = WIC"),
                position = "bottomright",
                opacity = 1,
                title = "Accepted Payment Types") %>%
      addLegend(colors = paste0(c("red", "blue", "green"), "; border-radius: 50%; width: 10px; height: 10px;"),
                labels = paste0("<div style='display: inline-block;'>", c("convenience", "farm", "supermarket"), "</div>"),
                position = "bottomright",
                opacity = 1,
                title = "Food Location Types") %>%
      # addLegendCustom(colors = c("red", "blue", "green", "gray", "gray", "gray"),
      #                 labels = c("convenience", "farm", "supermarket", "no acceptance",
      #                            "snap", "snap and wic"),
      #                 sizes = c(10, 10, 10, 6, 10, 14)) %>%
      addLegend(colors = c("grey", "purple", "blue"),
                labels = c("Wasco County", "South Wasco County School District", "Townships and Unincorporated Areas"),
                title = "Boundary Lines") %>%
      # addLegend(data = countyline, "topright",
      #           colors = "grey", labels = "Wasco County", group = "Basemap") %>%
      # addLegend(data = swsd, "topright", opacity = 1,
      #           colors = "purple", labels = "South Wasco County School District",
      #           group = "Basemap") %>%
      # addLegend(data = unincorporated, "topright", opacity = 0.4,
      #           colors = "blue", labels = "Townships and Unincorporated Areas",
      #           group = "Basemap") %>%
      addLegend(data = filteredData(), position = "bottomleft", colors = ~isochronepal(value), values = ~value, labels = c("30 minutes", "1 hour"), group = "isochrones", title = "Driving time")

  }) # end of leaflet food map

  ## Food table




  # leafletProxy("mymap") %>%
  #   addPolygons(data = filteredData, color = ~isochronepal(value),
  #              group = "Driving times") %>%

  ## Here is the observe function for showing the isochrone for the filtered data from the above function
  #  observe({
  #    leafletProxy("mymap", data = filteredData()) %>%
  #      addPolygons(color = ~isochronepal(value))
  ##  })


  #  })

## SERVER: PANEL - Food insecurity ----
## leafletOutput("foodinsecuritymap") ------



  output$foodinsecuritymap <- renderLeaflet({
  food_counties_pal <- colorNumeric(viridis_pal(option = "D")(3), domain = food_insecurity_counties$FdIR)
  food_counties_pal_c <- colorNumeric(viridis_pal(option = "D")(3), domain = food_insecurity_counties$Cfir)
  if (input$ratetype == "Overall"){
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = shp2014,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text,
        fillColor = ~food_counties_pal(FdIR),
        group = "2014") %>%
      addPolygons(
        data = shp2015,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text,
        fillColor = ~food_counties_pal(FdIR),
        group = "2015") %>%
      addPolygons(
        data = shp2016,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text,
        fillColor = ~food_counties_pal(FdIR),
        group = "2016") %>%
      addPolygons(
        data = shp2017,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text,
        fillColor = ~food_counties_pal(FdIR),
        group = "2017") %>%
      addPolylines(
        data = south_wasco_points,
        color = "#5e4b6b",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap",
        label = "South Wasco County Region") %>%
      addLegend(
        data = shp2017,
        "bottomright",
        pal = food_counties_pal,
        values = ~FdIR,
        title = "Food Insecurity Rate by County",
        na.label = "NA") %>%
      addLayersControl(
        baseGroups = c("2017", "2016", "2015", "2014"),
        options = layersControlOptions(collapsed = FALSE))
  }
  else if (input$ratetype == "Childhood"){
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = shp2014,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text_child,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2014") %>%
      addPolygons(
        data = shp2015,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text_child,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2015") %>%
      addPolygons(
        data = shp2016,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text_child,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2016") %>%
      addPolygons(
        data = shp2017,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~popup_text_child,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2017") %>%
      addPolylines(
        data = south_wasco_points,
        color = "#5e4b6b",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap",
        label = "South Wasco County Region") %>%
      addLegend(
        data = shp2017,
        "bottomright",
        pal = food_counties_pal_c,
        values = ~Cfir,
        title = "Childhood Food Insecurity Rate by County",
        na.label = "NA") %>%
      addLayersControl(
        baseGroups = c("2017", "2016", "2014", "2013"),
        options = layersControlOptions(collapsed = FALSE))
  }

  })

## SERVER: PANEL - Local crops ----
## leafletOutput("cropmap") -----

  colors_ww <- colorQuantile(viridis_pal(option = "D")(3),
                             domain = rbind(acres_17[acres_17$desc == "Winter Wheat", ],
                                            acres_16[acres_16$desc == "Winter Wheat", ],
                                            acres_15[acres_15$desc == "Winter Wheat", ])$acres)
  colors_bar <- colorQuantile(viridis_pal(option = "D")(3),
                              domain = rbind(acres_17[acres_17$desc == "Barley", ],
                                             acres_16[acres_16$desc == "Barley", ],
                                             acres_15[acres_15$desc == "Barley", ])$acres)
  colors_alf <- colorQuantile(viridis_pal(option = "D")(3),
                              domain = rbind(acres_17[acres_17$desc == "Alfalfa", ],
                                             acres_16[acres_16$desc == "Alfalfa", ],
                                             acres_15[acres_15$desc == "Alfalfa", ])$acres)
  colors_cher <- colorQuantile(viridis_pal(option = "D")(3),
                               domain = rbind(acres_17[acres_17$desc == "Cherries", ],
                                              acres_16[acres_16$desc == "Cherries", ],
                                              acres_15[acres_15$desc == "Cherries", ])$acres)
  output$cropmap <- renderLeaflet({
# colors

# winter wheat
    if (input$crops == "Winter Wheat"){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(
          data = south_wasco_points,
          color = "red",
          weight = 2,
          opacity = .7,
          group = "Basemap",
          label = "South Wasco Region") %>%
        addPolygons(data = acres_17[acres_17$desc == "Winter Wheat", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2017",
                    fillColor = ~ colors_ww(acres),
                    label = acres_17[acres_17$desc == "Winter Wheat", ]$acres) %>%
        addLegend(
          data = rbind(acres_17[acres_17$desc == "Winter Wheat", ],
                       acres_16[acres_16$desc == "Winter Wheat", ],
                       acres_15[acres_15$desc == "Winter Wheat", ]),
          "bottomright",
          pal = colors_ww,
          values = ~ acres,
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of Acres of Winter Wheat<br>by Block Group",
          opacity = .7,
          na.label = "NA") %>%
        addPolygons(data = acres_16[acres_16$desc == "Winter Wheat", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2016",
                    fillColor = ~ colors_ww(acres),
                    label = acres_16[acres_16$desc == "Winter Wheat", ]$acres) %>%
        addPolygons(data = acres_15[acres_15$desc == "Winter Wheat", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2015",
                    fillColor = ~ colors_ww(acres),
                    label = acres_15[acres_15$desc == "Winter Wheat", ]$acres) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE))
    }
# barley
    else if (input$crops == "Barley"){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(
          data = south_wasco_points,
          color = "red",
          weight = 2,
          opacity = .7,
          group = "Basemap",
          label = "South Wasco Region") %>%
        addPolygons(data = acres_17[acres_17$desc == "Barley", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2017",
                    fillColor = ~ colors_bar(acres),
                    label = acres_17[acres_17$desc == "Barley", ]$acres) %>%
        addLegend(
          data = rbind(acres_17[acres_17$desc == "Barley", ],
                       acres_16[acres_16$desc == "Barley", ],
                       acres_15[acres_15$desc == "Barley", ]),
          "bottomright",
          pal = colors_bar,
          values = ~ acres,
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of Acres of Barley<br>by Block Group",
          opacity = .7,
          na.label = "NA") %>%
        addPolygons(data = acres_16[acres_16$desc == "Barley", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2016",
                    fillColor = ~ colors_bar(acres),
                    label = acres_16[acres_16$desc == "Barley", ]$acres) %>%
        addPolygons(data = acres_15[acres_15$desc == "Barley", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2015",
                    fillColor = ~ colors_bar(acres),
                    label = acres_15[acres_15$desc == "Barley", ]$acres) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE))
    }
    # alfalfa
    else if (input$crops == "Alfalfa"){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(
          data = south_wasco_points,
          color = "red",
          weight = 2,
          opacity = .7,
          group = "Basemap",
          label = "South Wasco Region") %>%
        addPolygons(data = acres_17[acres_17$desc == "Alfalfa", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2017",
                    fillColor = ~ colors_alf(acres),
                    label = acres_17[acres_17$desc == "Alfalfa", ]$acres) %>%
        addLegend(
          data = rbind(acres_17[acres_17$desc == "Alfalfa", ],
                       acres_16[acres_16$desc == "Alfalfa", ],
                       acres_15[acres_15$desc == "Alfalfa", ]),
          "bottomright",
          pal = colors_alf,
          values = ~ acres,
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of Acres of Alfalfa<br>by Block Group",
          opacity = .7,
          na.label = "NA") %>%
        addPolygons(data = acres_16[acres_16$desc == "Alfalfa", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2016",
                    fillColor = ~ colors_alf(acres),
                    label = acres_16[acres_16$desc == "Alfalfa", ]$acres) %>%
        addPolygons(data = acres_15[acres_15$desc == "Alfalfa", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2015",
                    fillColor = ~ colors_alf(acres),
                    label = acres_15[acres_15$desc == "Alfalfa", ]$acres) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE))
    }
    # cherries
    else if (input$crops == "Cherries"){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(
          data = south_wasco_points,
          color = "red",
          weight = 2,
          opacity = .7,
          group = "Basemap",
          label = "South Wasco Region") %>%
        addPolygons(data = acres_17[acres_17$desc == "Cherries", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2017",
                    fillColor = ~ colors_cher(acres),
                    label = acres_17[acres_17$desc == "Cherries", ]$acres) %>%
        addLegend(
          data = rbind(acres_17[acres_17$desc == "Cherries", ],
                       acres_16[acres_16$desc == "Cherries", ],
                       acres_15[acres_15$desc == "Cherries", ]),
          "bottomright",
          pal = colors_cher,
          values = ~ acres,
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of Acres of Cherries<br>by Block Group",
          opacity = .7,
          na.label = "NA") %>%
        addPolygons(data = acres_16[acres_16$desc == "Cherries", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2016",
                    fillColor = ~ colors_cher(acres),
                    label = acres_16[acres_16$desc == "Cherries", ]$acres) %>%
        addPolygons(data = acres_15[acres_15$desc == "Cherries", ],
                    weight = .3,
                    opacity = 1,
                    fillOpacity = .7,
                    group = "2015",
                    fillColor = ~ colors_cher(acres),
                    label = acres_15[acres_15$desc == "Cherries", ]$acres) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE))
    }

  })

## SERVER: TAB - Infrastructure cluster ----
## SERVER: PANEL - Water ----
## plotlyOutput("waterplot") ----

  output$waterplot <- renderPlotly({

    water_use_melt <- melt(data = water_use_by_sector_t, id.vars = c("year"),
                                                         measure.vars = colnames(water_use_by_sector_t)[-length(water_use_by_sector_t)]) %>%
    rename(c("sector" = "variable", "gallons" = "value"))
  water_use_melt$sector <- recode(water_use_melt$sector, "Aquaculture Water Use (mGal/D)" = "Aquaculture",
                                  "Commercial Water Use (mGal/D)" = "Commercial",
                                  "Domestic Water Use (mGal/D)" ="Domestic",
                                  "Industrial Water Use (mGal/D)" = "Industrial",
                                  "Irrigation Water Use (mGal/D)" = "Irrigation",
                                  "Livestock Water Use (mGal/D)" = "Livestock",
                                  "Mining Water Use (mGal/D)" = "Mining",
                                  "Total Water supplied to Public (mGal/D)"= "Total Water Supplied to Public",
                                  "Wastewater Treatment (mGal/D)" = "Wastewater Treatment")

  ggplotly(ggplot(water_use_melt, aes(x=year, y=gallons, group = sector, color = sector,
                                      text = paste0("Sector: ", sector,
                                                    "<br>Year: ", year,
                                                    "<br>Water Use: ", gallons, " (mGal/D)"))) +
             geom_line(size = 1) +
             geom_point(size = 1.5) +
             scale_colour_manual(name = "Sector", values = viridis(9, option = "D")) +
             theme_minimal() + ggtitle("Water Use in Wasco County by Sector (1985-2015)") +
             ylab("Millions of Gallons per Day (mGal/D)") + xlab("Year"), tooltip = "text") %>%
    config(displayModeBar = "static", displaylogo = FALSE,
           modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                       "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

  })

## SERVER: TAB - Learn and earn driver ----
## SERVER: PANEL - Education composite ----
## plotOutput("education1") -----
  output$edplot <- renderPlot({
    if (input$ed == "Benefits"){
      #Benefits
      ed.melt.increase = melt(ed.increase, id.vars = c("year", "District.Name"),
                              measure.vars = c("On.Time.Grad.Rate", "Teacher.Experience.Pct",
                                               "Percent.ELA.Proficient.Change")) %>%
        mutate(variable = factor(variable, levels = c("On.Time.Grad.Rate", "Teacher.Experience.Pct",
                                                      "Percent.ELA.Proficient.Change"))) %>%
        mutate(variable = recode(variable, "On.Time.Grad.Rate" = "On Time Graduation",
                                 "Teacher.Experience.Pct" = "Teacher Experience",
                                 "Percent.ELA.Proficient.Change" = "ELA Proficiency Change"))

      ggplot(ed.melt.increase, aes(y = District.Name, x = year, fill = value)) +
        geom_tile(color = "#ADB5BD") + #gray
        geom_text(aes(label = round(value,0)), color = "black", size = 3.5) +
        coord_equal() +
        #facet_grid(rows = vars(variable)) +
        facet_wrap(~variable, ncol=1) +
        #scale_fill_gradientn(colors = pgcol, values = c(-60, 0, 100)) +
        scale_fill_continuous_divergingx(palette = "PRGn", mid = 0,
                                         breaks= c(-75, -50, -25, 0, 25, 50, 75, 100),
                                         limits = c(-75, 100)) +
        theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1),
              strip.background = element_rect(
                color="black", fill="#ADB5BD", size=1, linetype="solid"),
              strip.text.y = element_text(size = 5, color = "black", face = "bold"),
              legend.key.size = unit(1.5, "cm")) +
        labs(title = "Benefits to Student Success", x ="School Year", y = "", fill="Percent")
      #plot
      #benefits
    }

    else if (input$ed == "Barriers"){
      #Barriers
      ed.melt.decrease = melt(ed.decrease, id.vars = c("year", "District.Name"),
                              measure.vars = c("Percent.Economically.Disadvantaged", "Percent.Chronically.Absent",
                                               "Dropout.Rate")) %>%
        mutate(variable = recode(variable, "Percent.Economically.Disadvantaged" = "Economic Disadvantage",
                                 "Percent.Chronically.Absent" = "Chronic Absenteeism",
                                 "Dropout.Rate"="Dropout Rate"))
      ggplot(ed.melt.decrease, aes(y = District.Name, x = year, fill = value)) +
        geom_tile(color = "#ADB5BD") + #gray
        geom_text(aes(label = round(value,0)), color = "black", size = 3.5) +
        coord_equal() +
        #facet_grid(rows = vars(variable)) +
        facet_wrap(~variable, ncol=1) +
        #scale_fill_gradientn(colors = pgcol, values = c(-60, 0, 100)) +
        scale_fill_continuous_sequential(palette = "Purples 3") +
        theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1),
              strip.background = element_rect(
                color="black", fill="#ADB5BD", size=1, linetype="solid"),
              strip.text.y = element_text(size = 4, color = "black", face = "bold"),
              legend.key.size = unit(1.5, "cm")) +
        labs(title = "Barriers to Student Success", x ="School Year", y = "", fill="Percent")

    }
  })
  ## SERVER: DATA TABLE - Education Heatmaps -----
  output$edDT <- renderDT({
    datatable(ed,
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })
  
  


## SERVER: PANEL - Employment ratio ----
## plotlyOutput("empratioplot") -----
## leafletOutput("percempmap") -----
  output$percempmap <- renderLeaflet({
    perc_emp_pal <- colorQuantile(viridis_pal(option = "D")(3), domain = acs_tracts$employment_20_to_64)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2018),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2018",
        fillColor = ~perc_emp_pal(employment_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(employment_20_to_64_moe, 1), "%<br/>",
                              "<strong> Percent employed between 20-64: <strong>",
                              round(employment_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2017),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2017",
        fillColor = ~perc_emp_pal(employment_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(employment_20_to_64_moe, 1), "%<br/>",
                              "<strong> Percent employed between 20-64: <strong>",
                              round(employment_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2016),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2016",
        fillColor = ~perc_emp_pal(employment_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(employment_20_to_64_moe, 1), "%<br/>",
                              "<strong> Percent employed between 20-64: <strong>",
                              round(employment_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2015),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2015",
        fillColor = ~perc_emp_pal(employment_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(employment_20_to_64_moe, 1), "%<br/>",
                              "<strong> Percent employed between 20-64: <strong>",
                              round(employment_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolylines(
        data = south_wasco_points,
        color = "#5e4b6b",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap",
        label = "South Wasco County Region") %>%
      addPolylines(
        data = county_lines,
        color = "#8d9fcc",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap") %>%
      addLegend(
        data = acs_tracts,
        "bottomright",
        pal = perc_emp_pal,
        values = ~ employment_20_to_64,
        labFormat = function(type, cuts, p) {
          n = length(cuts)
          p = paste0(round(p * 100), '%')
          cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
        title = "Percent of Employed Adults<br>20 to 64 by Census Tract",
        na.label = "NA") %>%
      addLayersControl(
        baseGroups = c("2018", "2017", "2016", "2015"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$empratioplot <- renderPlotly({
    ggplotly(ggplot(acs_counties, aes(x=year, y=employment_20_to_64, group = NAME, color = south_wasco,
                                      text = paste0("Region: ", NAME,
                                                    "<br>Year: ", year,
                                                    "<br>Percent Employed: ", employment_20_to_64, "%",
                                                    "<br>Margin of Error: ", employment_20_to_64_moe, "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_colour_manual(name = "Region", values = c(graypal, viridis(3, option = "D"))) +
               theme_minimal() + ggtitle("Employment Ratio for Adults 20 to 64: 2015-2018") +
               ylab("Percent Employed (%)") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
  })
  ## SERVER: DATA TABLE - Employment -----
  output$empDT <- renderDT({
    datatable(select(acs_data,NAME, year, employment_20_to_64, employment_20_to_64_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })
  
  
  
  
  
  
## SERVER: PANEL - Labor force participation ----
## leafletOutput("laborforcemap")------
  output$laborforcemap <- renderLeaflet({
    lfpr_pal <- colorQuantile(viridis_pal(option = "D")(3), domain = acs_tracts$labor_force_20_to_64)
    lfpr_leaf <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2018),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2018",
        fillColor = ~lfpr_pal(labor_force_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(labor_force_20_to_64_moe, 1), "%<br/>",
                              "<strong> Labor Force Participation Rate: <strong>",
                              round(labor_force_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2017),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2017",
        fillColor = ~lfpr_pal(labor_force_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(labor_force_20_to_64_moe, 1), "%<br/>",
                              "<strong> Labor Force Participation Rate: <strong>",
                              round(labor_force_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2016),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2016",
        fillColor = ~lfpr_pal(labor_force_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(labor_force_20_to_64_moe, 1), "%<br/>",
                              "<strong> Labor Force Participation Rate: <strong>",
                              round(labor_force_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2015),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2015",
        fillColor = ~lfpr_pal(labor_force_20_to_64),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(labor_force_20_to_64_moe, 1), "%<br/>",
                              "<strong> Labor Force Participation Rate: <strong>",
                              round(labor_force_20_to_64, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolylines(
        data = south_wasco_points,
        color = "#5e4b6b",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap",
        label = "South Wasco County Region") %>%
      addPolylines(
        data = county_lines,
        color = "#8d9fcc",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap") %>%
      addLegend(
        data = acs_tracts,
        "bottomright",
        pal = lfpr_pal,
        values = ~ labor_force_20_to_64,
        labFormat = function(type, cuts, p) {
          n = length(cuts)
          p = paste0(round(p * 100), '%')
          cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
        title = "Labor Force Participation Rate for<br>ages 20 to 64 by Census Tract",
        na.label = "NA") %>%
      addLayersControl(
        baseGroups = c("2018", "2017", "2016", "2015"),
        options = layersControlOptions(collapsed = FALSE))
  })
## plotlyOutput("laborforceplot") ------

  output$laborforceplot <- renderPlotly({
    ggplotly(ggplot(acs_counties, aes(x=year, y=labor_force_20_to_64, group = NAME, color = south_wasco,
                                      text = paste0("Region: ", NAME,
                                                    "<br>Year: ", year,
                                                    "<br>Labor Force Participation Rate: ", labor_force_20_to_64, "%",
                                                    "<br>Margin of Error: ", labor_force_20_to_64_moe, "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_colour_manual(name = "Region", values = c(graypal, viridis(3, option = "D"))) +
               #scale_alpha_manual(values=c(1,1,1,0.1)) +
               theme_minimal() + ggtitle("Labor Force Participation Rate for Adults 20 to 64: 2015-2018") + ylab("Labor Force Participation Rate") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
  })
  ## SERVER: DATA TABLE - Labor Force -----
  output$LaborForceDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, labor_force_20_to_64, labor_force_20_to_64_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })
  
  
  
  
  
## SERVER: PANEL - Job flows ----
## plotlyOutput("flows") ------

  output$flowsplot <- renderPlotly({

    top_12_in_melt <- melt(data = top_12_in, id.vars = c("year"),
                           measure.vars = colnames(top_12_in)[-length(top_12_in)]) %>%
      rename(c("county" = "variable", "jobs" = "value")) %>%
      mutate(neighbors = fct_other(county, keep = c("Hood River County, OR", "Klickitat County, WA",
                                                    "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA"),
                                   other_level = "Other Counties"),
             neighbors = factor(neighbors , levels= c("Other Counties","Hood River County, OR", "Klickitat County, WA",
                                                      "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA")))

    top_12_out_melt <- melt(data = top_12_out, id.vars = c("year"),
                            measure.vars = colnames(top_12_out)[-length(top_12_out)]) %>%
      rename(c("county" = "variable", "jobs" = "value"))%>%
      mutate(neighbors = fct_other(county, keep = c("Hood River County, OR", "Klickitat County, WA",
                                                    "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA"),
                                   other_level = "Other Counties"),
             neighbors = factor(neighbors , levels= c("Other Counties","Hood River County, OR", "Klickitat County, WA",
                                                      "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA")))

    if (input$flows == "Inflows"){
      ggplotly(ggplot(top_12_in_melt, aes(x=year, y=jobs, group = county, color = neighbors,
                                          text = paste0("County: ", county,
                                                        "<br>Year: ", year,
                                                        "<br>Number of Jobs: ", jobs))) +
                 geom_line(size = 1) +
                 geom_point(size = 1.5) +
                 scale_colour_manual(name = "County", values = c(graypal, viridis(5, option = "D"))) +
                 scale_x_continuous(breaks = 0:2100) +
                 ylim(min(top_12_in_melt$jobs), max(top_12_out_melt$jobs)) +
                 theme_minimal() + ggtitle("Number of jobs flowing into Wasco County (2015-2017)") +
                 ylab("Number of Jobs") + xlab("Year"), tooltip = "text") %>%
        config(displayModeBar = "static", displaylogo = FALSE,
               modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                           "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

    }
    else if (input$flows == "Outflows"){
      ggplotly(ggplot(top_12_out_melt, aes(x=year, y=jobs, group = county, color = neighbors,
                                           text = paste0("County: ", county,
                                                         "<br>Year: ", year,
                                                         "<br>Number of Jobs: ", jobs))) +
                 geom_line(size = 1) +
                 geom_point(size = 1.5) +
                 scale_colour_manual(name = "County", values = c(graypal, viridis(5, option = "D"))) +
                 scale_x_continuous(breaks = 0:2100) +
                 ylim(min(top_12_in_melt$jobs), max(top_12_out_melt$jobs)) +
                 theme_minimal() + ggtitle("Number of jobs flowing out of Wasco County (2015-2017)") +
                 ylab("Number of Jobs") + xlab("Year"), tooltip = "text") %>%
        config(displayModeBar = "static", displaylogo = FALSE,
               modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                           "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

    }
  })

## SERVER: PANEL - Industry Sectors -----
## leafletOutput("odleaf")----
  od_S000leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = wasco_geo_points,
      color = "black",
      weight = 1,
      group = "Basemap",
      label = "Wasco County") %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 2,
      opacity = 1,
      group = "Basemap",
      label = "South Wasco Region")

  od_SI01leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = wasco_geo_points,
      color = "black",
      weight = 1,
      group = "Basemap",
      label = "Wasco County") %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 2,
      opacity = 1,
      group = "Basemap",
      label = "South Wasco Region")

  od_SI02leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = wasco_geo_points,
      color = "black",
      weight = 1,
      group = "Basemap",
      label = "Wasco County") %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 2,
      opacity = 1,
      group = "Basemap",
      label = "South Wasco Region")

  od_SI03leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = wasco_geo_points,
      color = "black",
      weight = 1,
      group = "Basemap",
      label = "Wasco County") %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 2,
      opacity = 1,
      group = "Basemap",
      label = "South Wasco Region")

  output$odleaf <- renderLeaflet({
    if (input$sect == "All"){
      #S000 (all jobs) by year -------
      od_S000leaf  %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2017",
          fillColor = ~colorQuantile(viridis_pal(option = "D")(5), domain = agg_17$S000)(agg_17$S000),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of All Jobs: <strong>",
                                agg_17$S000), htmltools::HTML)) %>%
        addLegend(
          data = rbind(agg_17, agg_16, agg_15),
          "bottomright",
          pal = colorQuantile(viridis_pal(option = "D")(5), domain = rbind(agg_17, agg_16, agg_15)$S000),
          values = ~ S000,
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of All Jobs<br>by Census Block<br>in Wasco County",
          na.label = "NA") %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2016",
          fillColor = ~colorQuantile(viridis_pal(option = "D")(5), domain = agg_16$S000)(agg_16$S000),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of All Jobs: <strong>",
                                agg_16$S000), htmltools::HTML)) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2015",
          fillColor = ~colorQuantile(viridis_pal(option = "D")(5), domain = agg_15$S000)(agg_15$S000),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of All Jobs: <strong>",
                                agg_15$S000), htmltools::HTML)) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))
      }
    #SI01 (Goods Producing industry sectors) by year -------
    else if (input$sect == "Goods"){
      colors_SI01 <- colorQuantile(viridis_pal(option = "D")(3), domain = unique(rbind(agg_17, agg_16, agg_15)$SI01))
      #output$od_SI01leaf <- renderLeaflet({
      od_SI01leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2017",
          fillColor = ~colors_SI01((agg_17$SI01)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Goods Producing Jobs: <strong>",
                                agg_17$SI01), htmltools::HTML)) %>%
        addLegend(
          data = rbind(agg_17, agg_16, agg_15),
          "bottomright",
          pal = colors_SI01,
          values = ~ unique(SI01),
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of Goods Producing<br> Jobs by Census Block<br>in Wasco County",
          na.label = "NA") %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2016",
          fillColor = ~colors_SI01((agg_16$SI01)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Goods Producing Jobs: <strong>",
                                agg_16$SI01), htmltools::HTML)) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2015",
          fillColor = ~colors_SI01((agg_15$SI01)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Goods Producing Jobs: <strong>",
                                agg_15$SI01), htmltools::HTML)) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))
      }
    #SI02 (Trade, Transportation, and Utilities industry sectors) by year --------
    else if (input$sect == "Trade"){
      colors_SI02 <- colorQuantile(viridis_pal(option = "D")(3), domain = unique(rbind(agg_17, agg_16, agg_15)$SI02))
      # output$od_SI02leaf <- renderLeaflet({
      od_SI02leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2017",
          fillColor = ~colors_SI02((agg_17$SI02)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Utility Jobs: <strong>",
                                agg_17$SI02), htmltools::HTML)) %>%
        addLegend(
          data = rbind(agg_17, agg_16, agg_15),
          "bottomright",
          pal = colors_SI02,
          values = ~ unique(SI02),
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of Trade, Transportation,<br>and Utilities Jobs<br>by Census Block in<br>
          Wasco County",
          na.label = "NA") %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2016",
          fillColor = ~colors_SI02((agg_16$SI02)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Utility Jobs: <strong>",
                                agg_16$SI02), htmltools::HTML)) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2015",
          fillColor = ~colors_SI02((agg_15$SI02)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Utility Jobs: <strong>",
                                agg_15$SI02), htmltools::HTML)) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))
      }
    #SI03 (All Other Services industry sectors) by year ----------
    else if (input$sect == "AllOther"){
      colors_SI03 <- colorQuantile(viridis_pal(option = "D")(3), domain = unique(rbind(agg_17, agg_16, agg_15)$SI03))
      # output$od_SI03leaf <- renderLeaflet({
      od_SI03leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2017",
          fillColor = ~colors_SI03((agg_17$SI03)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Other Service Jobs: <strong>",
                                agg_17$SI03), htmltools::HTML)) %>%
        addLegend(
          data = rbind(agg_17, agg_16, agg_15),
          "bottomright",
          pal = colors_SI03,
          values = ~ unique(SI03),
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            p = paste0(round(p * 100), '%')
            cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
          title = "Number of All Other Services<br>Sector Jobs by Census Block<br>in Wasco County",
          na.label = "NA") %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2016",
          fillColor = ~colors_SI03((agg_16$SI03)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Other Service Jobs: <strong>",
                                agg_16$SI03), htmltools::HTML)) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2015",
          fillColor = ~colors_SI03((agg_15$SI03)),
          label = ~lapply(paste(sep = "",
                                "<strong> Number of Other Service Jobs: <strong>",
                                agg_15$SI03), htmltools::HTML)) %>%
        addLayersControl(
          baseGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))
      }
  })


## SERVER: TAB - Quality standard of living driver ----
## SERVER: PANEL - Median income -----
## leafletOutput("medincomemap") -----
  output$medincomemap <- renderLeaflet({
    med_inc_pal <- colorQuantile(viridis_pal(option = "D")(3),
                                 domain = acs_tracts$median_household_income)
    med_inc_leaf <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2018),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2018",
        fillColor = ~med_inc_pal(median_household_income),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: $",
                              round(median_household_income_moe, 1), "<br/>",
                              "<strong> Median Household Income: $<strong>",
                              round(median_household_income, 1)),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2017),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2017",
        fillColor = ~med_inc_pal(median_household_income),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: $",
                              round(median_household_income_moe, 1), "<br/>",
                              "<strong> Median Household Income: $<strong>",
                              round(median_household_income, 1)),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2016),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2016",
        fillColor = ~med_inc_pal(median_household_income),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: $",
                              round(median_household_income_moe, 1), "<br/>",
                              "<strong> Median Household Income: $<strong>",
                              round(median_household_income, 1)),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2015),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2015",
        fillColor = ~med_inc_pal(median_household_income),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: $",
                              round(median_household_income_moe, 1), "<br/>",
                              "<strong> Median Household Income: $<strong>",
                              round(median_household_income, 1)),
                        htmltools::HTML)) %>%
      addPolylines(
        data = south_wasco_points,
        color = "#5e4b6b",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap",
        label = "South Wasco County Region") %>%
      addPolylines(
        data = county_lines,
        color = "#8d9fcc",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap") %>%
      addLegend(
        data = acs_tracts,
        "bottomright",
        pal = med_inc_pal,
        values = ~ median_household_income,
        labFormat = function(type, cuts, p) {
          n = length(cuts)
          p = paste0(round(p * 100), '%')
          cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
        title = "Median Household Income by Census Tract",
        na.label = "NA") %>%
      addLayersControl(
        baseGroups = c("2018", "2017", "2016", "2015"),
        options = layersControlOptions(collapsed = FALSE))
  })
## plotlyOutput("medincomeplot") ----

  output$medincomeplot <- renderPlotly({
    ggplotly(ggplot(acs_counties,
                    aes(x=year, y=median_household_income, group = NAME, color = south_wasco,
                        text = paste0("Region: ", NAME,
                                      "<br>Year: ", year,
                                      "<br>Median Household Income: $", median_household_income,
                                      "<br>Margin of Error: $", median_household_income_moe))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_color_manual(name = "Region", values = c(graypal,viridis(3, option = "D")), labels=c("Oregon", "South Wasco", "Wasco", "Neighboring Counties")) +
               theme_minimal() + ggtitle("Median Household Income 2015-2018") +
               ylab("Median Household Income") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d", "hoverClosestCartesian",
                                         "hoverCompareCartesian","resetScale2d"))
  })
  ## SERVER: DATA TABLE - Median Income -----
  output$MedianIncomeDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, median_household_income, median_household_income_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })
  
  
  
  
  
  
## SERVER: PANEL - ALICE Poverty rate -----
## plotlyOutput("aliceplot") -----
  output$aliceplot <- renderPlotly({
    ggplotly(ggplot(alice_counties,
                    aes(x=Year, y=Percent_ALICE_Households, group = County, color = Wasco,
                        text = paste0("County: ", County,
                                      "<br>Year: ", Year,
                                      "<br>Below ALICE Threshold: ", Percent_ALICE_Households, "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_color_manual(name = "County", values = c(graypal, viridis(6, option = "D"))) +
               theme_minimal() + ggtitle("Households Below ALICE Threshold 2010-2018") +
               ylab("Below ALICE Threshold (%)") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
  })

## SERVER: PANEL - Poverty rate -----
## plotlyOutput("povertyplot") -----
## leafletOutput("povertymap") -----
  perc_pov_pal <- colorQuantile(viridis_pal(option = "D")(3), domain = acs_tracts$below_poverty)

  output$povertymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2018),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2018",
        fillColor = ~perc_pov_pal(below_poverty),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(below_poverty_moe, 1), "%<br/>",
                              "<strong> Percent below the poverty line: <strong>",
                              round(below_poverty, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2017),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2017",
        fillColor = ~perc_pov_pal(below_poverty),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(below_poverty_moe, 1), "%<br/>",
                              "<strong> Percent below the poverty line: <strong>",
                              round(below_poverty, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2016),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2016",
        fillColor = ~perc_pov_pal(below_poverty),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(below_poverty_moe, 1), "%<br/>",
                              "<strong> Percent below the poverty line: <strong>",
                              round(below_poverty, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolygons(
        data = filter(acs_tracts, year == 2015),
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        group = "2015",
        fillColor = ~perc_pov_pal(below_poverty),
        label = ~lapply(paste(sep = "",
                              substr(NAME, 20, 60), "<br/>",
                              substr(NAME, 1, 17),
                              "<br/>Margins of error: ",
                              round(below_poverty_moe, 1), "%<br/>",
                              "<strong> Percent below the poverty line: <strong>",
                              round(below_poverty, 1), "<strong>%"),
                        htmltools::HTML)) %>%
      addPolylines(
        data = south_wasco_points,
        color = "#5e4b6b",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap",
        label = "South Wasco County Region") %>%
      addPolylines(
        data = county_lines,
        color = "#8d9fcc",
        weight = 2,
        opacity = 1,
        fillOpacity= 0,
        group = "Basemap") %>%
      addLegend(
        data = acs_tracts,
        "bottomright",
        pal = perc_pov_pal,
        values = ~ below_poverty,
        labFormat = function(type, cuts, p) {
          n = length(cuts)
          p = paste0(round(p * 100), '%')
          cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
        title = "Percent of Population below<br>Poverty Line by Census Tract",
        na.label = "NA") %>%
      addLayersControl(
        baseGroups = c("2018", "2017", "2016", "2015"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$povertyplot <- renderPlotly({
    ggplotly(ggplot(acs_counties, aes(x=year, y=below_poverty, group = NAME, color = south_wasco,
                                      text = paste0("Region: ", NAME,
                                                    "<br>Year: ", year,
                                                    "<br>Percent Below Federal Poverty: ", below_poverty, "%",
                                                    "<br>Margin of Error: ", below_poverty_moe, "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_colour_manual(name = "Region", values = c(graypal,viridis(3, option = "D"))) +
               theme_minimal() + ggtitle("Percent Below Federal Poverty: 2015-2018") + ylab("Percent Below Federal Poverty") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
  })
  
## SERVER: DATA TABLE - Poverty Rate ----
  output$povertyDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, below_poverty, below_poverty_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })  
  
  
  
  
  

## SERVER: PANEL - Income distribution -----
## plotlyOutput("incomedisplot") -----
## leafletOutput("incomedismap") -----
  income_dist_max_perc_2018 <- max(apply(X = select(data_frame(income_dist_2018),
                                                    -year, -NAME, -geometry), 1, max, TRUE))
  income_dist_18_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = c(0, income_dist_max_perc_2018))
  income_dist_max_perc_2017 <- max(apply(X = select(data_frame(income_dist_2017),
                                                    -year, -NAME, -geometry), 1, max, TRUE))
  income_dist_17_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = c(0, income_dist_max_perc_2017))
  income_dist_max_perc_2016 <- max(apply(X = select(data_frame(income_dist_2016),
                                                    -year, -NAME, -geometry), 1, max, TRUE))
  income_dist_16_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = c(0, income_dist_max_perc_2016))
  income_dist_max_perc_2015 <- max(apply(X = select(data_frame(income_dist_2015),
                                                    -year, -NAME, -geometry), 1, max, TRUE))
  income_dist_15_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = c(0, income_dist_max_perc_2015))
  output$incomedismap <- renderLeaflet({
    if (input$incomedisyear == "2018"){
      leaflet(income_dist_2018) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "< $10K",
          fillColor = ~income_dist_18_pal(income_less_than_10k),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_less_than_10k_moe, 1), "%<br/>",
                                "<strong> Percent earning < $10K: <strong>",
                                round(income_less_than_10k, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$10K - $14999",
          fillColor = ~income_dist_18_pal(income_10k_14999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_10k_14999_moe, 1), "%<br/>",
                                "<strong> Percent earning $10K - $14999: <strong>",
                                round(income_10k_14999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$15K - $24999",
          fillColor = ~income_dist_18_pal(income_15k_24999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_15k_24999_moe, 1), "%<br/>",
                                "<strong> Percent earning $15K - $24999: <strong>",
                                round(income_15k_24999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$25K - $34999",
          fillColor = ~income_dist_18_pal(income_25k_34999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_25k_34999_moe, 1), "%<br/>",
                                "<strong> Percent earning $25K - $34999: <strong>",
                                round(income_25k_34999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$35K - $49999",
          fillColor = ~income_dist_18_pal(income_35K_49999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_35K_49999_moe, 1), "%<br/>",
                                "<strong> Percent earning $35K - $49999: <strong>",
                                round(income_35K_49999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$50K - $74999",
          fillColor = ~income_dist_18_pal(income_50K_74999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_50K_74999_moe, 1), "%<br/>",
                                "<strong> Percent earning $50K - $74999: <strong>",
                                round(income_50K_74999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$75K - $99999",
          fillColor = ~income_dist_18_pal(income_75K_99999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_75K_99999_moe, 1), "%<br/>",
                                "<strong> Percent earning $75K - $99999: <strong>",
                                round(income_75K_99999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$100K - $149999",
          fillColor = ~income_dist_18_pal(income_100K_149999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_100K_149999_moe, 1), "%<br/>",
                                "<strong> Percent earning $100K - $149999: <strong>",
                                round(income_100K_149999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$150K - $199999",
          fillColor = ~income_dist_18_pal(income_150K_199999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_150K_199999_moe, 1), "%<br/>",
                                "<strong> Percent earning $150K - $199999: <strong>",
                                round(income_150K_199999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "> $200K",
          fillColor = ~income_dist_18_pal(income_200K_more),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2018$NAME, 20, 60), "<br/>",
                                substr(income_dist_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2018)$income_200K_more, 1), "%<br/>",
                                "<strong> Percent earning > $200K: <strong>",
                                round(income_200K_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = income_dist_18_pal,
          values = ~ c(0, income_dist_max_perc_2018),
          title = "% of Population in selected<br>Income Bracket by Census Tract (2018)",
          na.label = "NA") %>%
        addLayersControl(
          position = "topleft",
          baseGroups = c("< $10K", "$15K - $24999", "$25K - $34999", "$35K - $49999",
                         "$50K - $74999", "$75K - $99999","$100K - $149999", "$150K - $199999",
                         "> $200K"),
          options = layersControlOptions(collapsed = T))
    }
    else if (input$incomedisyear == "2017"){
      leaflet(income_dist_2017) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "< $10K",
          fillColor = ~income_dist_17_pal(income_less_than_10k),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_less_than_10k_moe, 1), "%<br/>",
                                "<strong> Percent earning < $10K: <strong>",
                                round(income_less_than_10k, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$10K - $14999",
          fillColor = ~income_dist_17_pal(income_10k_14999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_10k_14999_moe, 1), "%<br/>",
                                "<strong> Percent earning $10K - $14999: <strong>",
                                round(income_10k_14999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$15K - $24999",
          fillColor = ~income_dist_17_pal(income_15k_24999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_15k_24999_moe, 1), "%<br/>",
                                "<strong> Percent earning $15K - $24999: <strong>",
                                round(income_15k_24999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$25K - $34999",
          fillColor = ~income_dist_17_pal(income_25k_34999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_25k_34999_moe, 1), "%<br/>",
                                "<strong> Percent earning $25K - $34999: <strong>",
                                round(income_25k_34999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$35K - $49999",
          fillColor = ~income_dist_17_pal(income_35K_49999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_35K_49999_moe, 1), "%<br/>",
                                "<strong> Percent earning $35K - $49999: <strong>",
                                round(income_35K_49999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$50K - $74999",
          fillColor = ~income_dist_17_pal(income_50K_74999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_50K_74999_moe, 1), "%<br/>",
                                "<strong> Percent earning $50K - $74999: <strong>",
                                round(income_50K_74999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$75K - $99999",
          fillColor = ~income_dist_17_pal(income_75K_99999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_75K_99999_moe, 1), "%<br/>",
                                "<strong> Percent earning $75K - $99999: <strong>",
                                round(income_75K_99999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$100K - $149999",
          fillColor = ~income_dist_17_pal(income_100K_149999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_100K_149999_moe, 1), "%<br/>",
                                "<strong> Percent earning $100K - $149999: <strong>",
                                round(income_100K_149999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$150K - $199999",
          fillColor = ~income_dist_17_pal(income_150K_199999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_150K_199999_moe, 1), "%<br/>",
                                "<strong> Percent earning $150K - $199999: <strong>",
                                round(income_150K_199999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "> $200K",
          fillColor = ~income_dist_17_pal(income_200K_more),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2017$NAME, 20, 60), "<br/>",
                                substr(income_dist_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2017)$income_200K_more, 1), "%<br/>",
                                "<strong> Percent earning > $200K: <strong>",
                                round(income_200K_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = income_dist_17_pal,
          values = ~ c(0, income_dist_max_perc_2017),
          title = "% of Population in selected<br>Income Bracket by Census Tract (2017)",
          na.label = "NA") %>%
        addLayersControl(
          position = "topleft",
          baseGroups = c("< $10K", "$15K - $24999", "$25K - $34999", "$35K - $49999",
                         "$50K - $74999", "$75K - $99999","$100K - $149999", "$150K - $199999",
                         "> $200K"),
          options = layersControlOptions(collapsed = T))
    }
    else if (input$incomedisyear == "2016"){
      leaflet(income_dist_2016) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "< $10K",
          fillColor = ~income_dist_16_pal(income_less_than_10k),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_less_than_10k_moe, 1), "%<br/>",
                                "<strong> Percent earning < $10K: <strong>",
                                round(income_less_than_10k, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$10K - $14999",
          fillColor = ~income_dist_16_pal(income_10k_14999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_10k_14999_moe, 1), "%<br/>",
                                "<strong> Percent earning $10K - $14999: <strong>",
                                round(income_10k_14999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$15K - $24999",
          fillColor = ~income_dist_16_pal(income_15k_24999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_15k_24999_moe, 1), "%<br/>",
                                "<strong> Percent earning $15K - $24999: <strong>",
                                round(income_15k_24999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$25K - $34999",
          fillColor = ~income_dist_16_pal(income_25k_34999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_25k_34999_moe, 1), "%<br/>",
                                "<strong> Percent earning $25K - $34999: <strong>",
                                round(income_25k_34999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$35K - $49999",
          fillColor = ~income_dist_16_pal(income_35K_49999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_35K_49999_moe, 1), "%<br/>",
                                "<strong> Percent earning $35K - $49999: <strong>",
                                round(income_35K_49999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$50K - $74999",
          fillColor = ~income_dist_16_pal(income_50K_74999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_50K_74999_moe, 1), "%<br/>",
                                "<strong> Percent earning $50K - $74999: <strong>",
                                round(income_50K_74999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$75K - $99999",
          fillColor = ~income_dist_16_pal(income_75K_99999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_75K_99999_moe, 1), "%<br/>",
                                "<strong> Percent earning $75K - $99999: <strong>",
                                round(income_75K_99999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$100K - $149999",
          fillColor = ~income_dist_16_pal(income_100K_149999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_100K_149999_moe, 1), "%<br/>",
                                "<strong> Percent earning $100K - $149999: <strong>",
                                round(income_100K_149999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$150K - $199999",
          fillColor = ~income_dist_16_pal(income_150K_199999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_150K_199999_moe, 1), "%<br/>",
                                "<strong> Percent earning $150K - $199999: <strong>",
                                round(income_150K_199999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "> $200K",
          fillColor = ~income_dist_16_pal(income_200K_more),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2016$NAME, 20, 60), "<br/>",
                                substr(income_dist_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2016)$income_200K_more, 1), "%<br/>",
                                "<strong> Percent earning > $200K: <strong>",
                                round(income_200K_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = income_dist_16_pal,
          values = ~ c(0, income_dist_max_perc_2016),
          title = "% of Population in selected<br>Income Bracket by Census Tract (2016)",
          na.label = "NA") %>%
        addLayersControl(
          position = "topleft",
          baseGroups = c("< $10K", "$15K - $24999", "$25K - $34999", "$35K - $49999",
                         "$50K - $74999", "$75K - $99999","$100K - $149999", "$150K - $199999",
                         "> $200K"),
          options = layersControlOptions(collapsed = T))

    }
    else if (input$incomedisyear == "2015"){
      leaflet(income_dist_2015) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "< $10K",
          fillColor = ~income_dist_15_pal(income_less_than_10k),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_less_than_10k_moe, 1), "%<br/>",
                                "<strong> Percent earning < $10K: <strong>",
                                round(income_less_than_10k, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$10K - $14999",
          fillColor = ~income_dist_15_pal(income_10k_14999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_10k_14999_moe, 1), "%<br/>",
                                "<strong> Percent earning $10K - $14999: <strong>",
                                round(income_10k_14999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$15K - $24999",
          fillColor = ~income_dist_15_pal(income_15k_24999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_15k_24999_moe, 1), "%<br/>",
                                "<strong> Percent earning $15K - $24999: <strong>",
                                round(income_15k_24999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$25K - $34999",
          fillColor = ~income_dist_15_pal(income_25k_34999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_25k_34999_moe, 1), "%<br/>",
                                "<strong> Percent earning $25K - $34999: <strong>",
                                round(income_25k_34999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$35K - $49999",
          fillColor = ~income_dist_15_pal(income_35K_49999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_35K_49999_moe, 1), "%<br/>",
                                "<strong> Percent earning $35K - $49999: <strong>",
                                round(income_35K_49999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$50K - $74999",
          fillColor = ~income_dist_15_pal(income_50K_74999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_50K_74999_moe, 1), "%<br/>",
                                "<strong> Percent earning $50K - $74999: <strong>",
                                round(income_50K_74999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$75K - $99999",
          fillColor = ~income_dist_15_pal(income_75K_99999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_75K_99999_moe, 1), "%<br/>",
                                "<strong> Percent earning $75K - $99999: <strong>",
                                round(income_75K_99999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$100K - $149999",
          fillColor = ~income_dist_15_pal(income_100K_149999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_100K_149999_moe, 1), "%<br/>",
                                "<strong> Percent earning $100K - $149999: <strong>",
                                round(income_100K_149999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "$150K - $199999",
          fillColor = ~income_dist_15_pal(income_150K_199999),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_150K_199999_moe, 1), "%<br/>",
                                "<strong> Percent earning $150K - $199999: <strong>",
                                round(income_150K_199999, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "> $200K",
          fillColor = ~income_dist_15_pal(income_200K_more),
          label = ~lapply(paste(sep = "",
                                substr(income_dist_2015$NAME, 20, 60), "<br/>",
                                substr(income_dist_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(income_dist_moe,
                                             year == 2015)$income_200K_more, 1), "%<br/>",
                                "<strong> Percent earning > $200K: <strong>",
                                round(income_200K_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = income_dist_15_pal,
          values = ~ c(0, income_dist_max_perc_2015),
          title = "% of Population in selected<br>Income Bracket by Census Tract (2015)",
          na.label = "NA") %>%
        addLayersControl(
          position = "topleft",
          baseGroups = c("< $10K", "$15K - $24999", "$25K - $34999", "$35K - $49999",
                         "$50K - $74999", "$75K - $99999","$100K - $149999", "$150K - $199999",
                         "> $200K"),
          options = layersControlOptions(collapsed = T))

    }
  })

  output$incomedisplot <- renderPlotly({
    #stacked bar charts
    income <- acs_counties %>% select(NAME, year, contains("income"))
    income_perc <- income %>% select(!contains("moe"), -median_household_income, NAME, year)
    income_moe <- income %>% select(NAME, year, contains("moe"), -median_household_income_moe)
    income_perc <- melt(income_perc, id.vars = c("NAME", "year"), measure.vars = colnames(income_perc)[-c(1,2)])
    income_moe <- income_moe %>% melt(id.vars = c("NAME","year"), measure.vars = colnames(income_moe)[-c(1,2)]) %>%
      rename(moe = value)  %>% mutate(variable =gsub("_moe", "", variable))
    income_table <- merge(x = income_perc, y = income_moe, by=c("NAME", "variable", "year"))%>%
      mutate(variable = recode_factor(variable,
                                      "income_less_than_10k" =  "Less Than $10,000", "income_10k_14999" = "$10,000-$14,999",
                                      "income_15k_24999" = "$15,000-$24,999", "income_25k_34999"="$25,000-$34,999",
                                      "income_35K_49999" = "$35,000-$49,999", "income_50K_74999" ="$50,000-$74,999",
                                      "income_75K_99999" = "$75,000-$99,999", "income_100K_149999" = "$100,000-$149,999",
                                      "income_150K_199999" = "$150,000-$199,999", "income_200K_more" = "Above $200,000"))

    ggplotly(ggplot(filter(income_table, year ==input$incomedisyear))+
               geom_bar(aes(fill=variable, y=value, x=NAME,
                            text = paste0("Region: ", NAME,
                                          "<br>Year: ", year,
                                          "<br>Percent of Population: ", value, "%",
                                          "<br>Margin of Error: ", moe, "%")),
                        position = position_stack(reverse = TRUE), stat="identity")+
               scale_fill_manual(name ="Income Bracket",
                                 values = viridis(10, option = "D")) +
               ylab("% of Population") + xlab("") + theme_minimal() +
               ggtitle(paste0("Income Distribution for ", input$incomedisyear)) + coord_flip(), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian",
                                         "hoverCompareCartesian","resetScale2d"))
  })
## SERVER: DATA TABLE - Income Distribution ----
  output$incomedistDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, income_10k_14999,income_10k_14999_moe,
                     income_15k_24999, income_15k_24999_moe, income_25k_34999, income_25k_34999_moe,
                     income_35K_49999, income_35K_49999_moe, income_50K_74999, income_50K_74999_moe,
                     income_75K_99999, income_75K_99999_moe, income_100K_149999, income_100K_149999_moe,
                     income_150K_199999, income_150K_199999_moe, income_200K_more, income_200K_more_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })    
  
  
  
  

## SERVER: PANEL - Affordable housing -----
## plotlyOutput("housingplot") ------

  output$housingplot <- renderPlotly({
    ggplotly(ggplot(filter(acs_counties, NAME != "South Wasco County School District 1, OR"),
                    aes(x=year, y=affordable_housing_all_perc, group = NAME, color = south_wasco,
                          text = paste0("Region: ", NAME,
                                        "<br>Year: ", year,
                                        "<br>Affordable Housing: ", round(affordable_housing_all_perc, digits = 1), "%",
                                        "<br>Margin of Error: ", round(affordable_housing_all_perc_moe, digits = 1), "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_colour_manual(name = "Region", values = c(graypal, viridis(3, option = "D")))  +
               theme_minimal() + ggtitle("Affordable Housing 2015-2018") + ylab("Affordable Housing") +
               xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

  })
  ## SERVER: DATA TABLE - Affordable housing ----
  output$AffordableHousingDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, affordable_housing_all_perc, affordable_housing_all_perc_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })    
  
  
  

## SERVER: PANEL - Rent vs own -----
## plotlyOutput("rentownplot") -----

  output$rentownplot <- renderPlotly({
    ggplotly(ggplot(acs_counties, aes(x=year, y=owner_occupied_housing_perc, group = NAME, color = south_wasco,
                                      text = paste0("Region: ", NAME,
                                                    "<br>Year: ", year,
                                                    "<br>Percent of Owner Occupied Homes: ", round(owner_occupied_housing_perc, digits = 1), "%",
                                                    "<br>Margin of Error: ", round(owner_occupied_housing_perc_moe, digits = 1), "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_colour_manual(name = "Region", values = c(graypal, viridis(3, option = "D")))  +
               theme_minimal() + ggtitle("Owner Occupied Housing 2015-2018") + ylab("Percent of Owners (%)") +
               xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

  })
## SERVER: DATA TABLE - rent vs own ----
  output$homeownDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, owner_occupied_housing_perc, owner_occupied_housing_perc_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })  
  
  
  

## SERVER: PANEL - Race -----
## plotlyOutput("raceplot") ----
## leafletOutput("racemap") -----
  # add years filter
  race_div_white_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = race_div$race_white)
  race_div_black_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = race_div$race_black)
  race_div_na_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                  domain = race_div$race_american_indian)
  race_div_asian_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = race_div$race_asian)
  race_div_nh_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                  domain = race_div$race_native_hawaiian)
  race_div_hisp_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                    domain = race_div$race_hispanic)
  race_div_oth_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                   domain = race_div$race_other)
  race_div_multi_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                     domain = race_div$race_two_more)
  output$racemap <- renderLeaflet({
    if (input$raceyears == "2018") {
      leaflet(race_div_2018) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        #white
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "White",
          fillColor = ~race_div_white_pal(race_white),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_white_moe, 1), "%<br/>",
                                "<strong> Percent White: <strong>",
                                round(race_white, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_white_pal,
          values = ~ race_white,
          group = "White",
          title = "% of Population identifying as<br>White by Census Tract (2018)",
          na.label = "NA")  %>%
        #black
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Black",
          fillColor = ~race_div_black_pal(race_black),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_black_moe, 1), "%<br/>",
                                "<strong> Percent Black: <strong>",
                                round(race_black, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_black_pal,
          values = ~ race_black,
          group = "Black",
          title = "% of Population identifying as<br>Black by Census Tract (2018)",
          na.label = "NA") %>%
        #NA
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native American",
          fillColor = ~race_div_na_pal(race_american_indian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_american_indian_moe, 1), "%<br/>",
                                "<strong> Percent Native American: <strong>",
                                round(race_american_indian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_na_pal,
          values = ~ race_american_indian,
          group = "Native American",
          title = "% of Population identifying as<br>Native American by Census Tract (2018)",
          na.label = "NA") %>%
        #asian
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Asian",
          fillColor = ~race_div_asian_pal(race_asian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_asian_moe, 1), "%<br/>",
                                "<strong> Percent Asian: <strong>",
                                round(race_asian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_asian_pal,
          values = ~ race_asian,
          group = "Asian",
          title = "% of Population identifying as<br>Asian by Census Tract (2018)",
          na.label = "NA") %>%
        #nh
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native Hawaiian",
          fillColor = ~race_div_nh_pal(race_native_hawaiian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_native_hawaiian_moe, 1), "%<br/>",
                                "<strong> Percent Native Hawaiian: <strong>",
                                round(race_native_hawaiian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_nh_pal,
          values = ~ race_native_hawaiian,
          group = "Native Hawaiian",
          title = "% of Population identifying as<br>Native Hawaiian by Census Tract (2018)",
          na.label = "NA") %>%
        #hispanic
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Hispanic",
          fillColor = ~race_div_hisp_pal(race_hispanic),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_hispanic_moe, 1), "%<br/>",
                                "<strong> Percent Hispanic: <strong>",
                                round(race_hispanic, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_hisp_pal,
          values = ~ race_hispanic,
          group = "Hispanic",
          title = "% of Population identifying as<br>Hispanic by Census Tract (2018)",
          na.label = "NA") %>%
        #other
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Other",
          fillColor = ~race_div_oth_pal(race_other),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_other_moe, 1), "%<br/>",
                                "<strong> Percent Other: <strong>",
                                round(race_other, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_oth_pal,
          values = ~ race_other,
          group = "Other",
          title = "% of Population identifying as<br>Other by Census Tract (2018)",
          na.label = "NA") %>%
        #multi
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Multiple Groups",
          fillColor = ~race_div_multi_pal(race_two_more),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2018)$race_two_more_moe, 1), "%<br/>",
                                "<strong> Percent Multiple Groups: <strong>",
                                round(race_two_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_multi_pal,
          values = ~ race_two_more,
          group = "Multiple Groups",
          title = "% of Population identifying with<br>multiple racial groups by Census Tract (2018)",
          na.label = "NA") %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0) %>%
        addLayersControl(
          position = "topleft",
          overlayGroups = c("White", "Black", "Native American", "Asian", "Native Hawaiian",
                            "Hispanic", "Other", "Multiple Groups"),
          options = layersControlOptions(collapsed = T)) %>%
        hideGroup(c("Black", "Native American", "Asian", "Native Hawaiian",
                    "Hispanic", "Other", "Multiple Groups"))
    }
    else if (input$raceyears == "2017"){
      leaflet(race_div_2017) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        #white
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "White",
          fillColor = ~race_div_white_pal(race_white),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_white_moe, 1), "%<br/>",
                                "<strong> Percent White: <strong>",
                                round(race_white, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_white_pal,
          values = ~ race_white,
          group = "White",
          title = "% of Population identifying as<br>White by Census Tract (2017)",
          na.label = "NA")  %>%
        #black
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Black",
          fillColor = ~race_div_black_pal(race_black),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_black_moe, 1), "%<br/>",
                                "<strong> Percent Black: <strong>",
                                round(race_black, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_black_pal,
          values = ~ race_black,
          group = "Black",
          title = "% of Population identifying as<br>Black by Census Tract (2017)",
          na.label = "NA") %>%
        #NA
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native American",
          fillColor = ~race_div_na_pal(race_american_indian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_american_indian_moe, 1), "%<br/>",
                                "<strong> Percent Native American: <strong>",
                                round(race_american_indian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_na_pal,
          values = ~ race_american_indian,
          group = "Native American",
          title = "% of Population identifying as<br>Native American by Census Tract (2017)",
          na.label = "NA") %>%
        #asian
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Asian",
          fillColor = ~race_div_asian_pal(race_asian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_asian_moe, 1), "%<br/>",
                                "<strong> Percent Asian: <strong>",
                                round(race_asian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_asian_pal,
          values = ~ race_asian,
          group = "Asian",
          title = "% of Population identifying as<br>Asian by Census Tract (2017)",
          na.label = "NA") %>%
        #nh
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native Hawaiian",
          fillColor = ~race_div_nh_pal(race_native_hawaiian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_native_hawaiian_moe, 1), "%<br/>",
                                "<strong> Percent Native Hawaiian: <strong>",
                                round(race_native_hawaiian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_nh_pal,
          values = ~ race_native_hawaiian,
          group = "Native Hawaiian",
          title = "% of Population identifying as<br>Native Hawaiian by Census Tract (2017)",
          na.label = "NA") %>%
        #hispanic
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Hispanic",
          fillColor = ~race_div_hisp_pal(race_hispanic),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_hispanic_moe, 1), "%<br/>",
                                "<strong> Percent Hispanic: <strong>",
                                round(race_hispanic, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_hisp_pal,
          values = ~ race_hispanic,
          group = "Hispanic",
          title = "% of Population identifying as<br>Hispanic by Census Tract (2017)",
          na.label = "NA") %>%
        #other
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Other",
          fillColor = ~race_div_oth_pal(race_other),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_other_moe, 1), "%<br/>",
                                "<strong> Percent Other: <strong>",
                                round(race_other, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_oth_pal,
          values = ~ race_other,
          group = "Other",
          title = "% of Population identifying as<br>Other by Census Tract (2017)",
          na.label = "NA") %>%
        #multi
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Multiple Groups",
          fillColor = ~race_div_multi_pal(race_two_more),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2017)$race_two_more_moe, 1), "%<br/>",
                                "<strong> Percent Multiple Groups: <strong>",
                                round(race_two_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_multi_pal,
          values = ~ race_two_more,
          group = "Multiple Groups",
          title = "% of Population identifying with<br>multiple racial groups by Census Tract (2017)",
          na.label = "NA") %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLayersControl(
          position = "topleft",
          overlayGroups = c("White", "Black", "Native American", "Asian", "Native Hawaiian",
                            "Hispanic", "Other", "Multiple Groups"),
          options = layersControlOptions(collapsed = T)) %>%
        hideGroup(c("Black", "Native American", "Asian", "Native Hawaiian",
                    "Hispanic", "Other", "Multiple Groups"))
    }
    else if (input$raceyears == "2016"){
      leaflet(race_div_2016) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        #white
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "White",
          fillColor = ~race_div_white_pal(race_white),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_white_moe, 1), "%<br/>",
                                "<strong> Percent White: <strong>",
                                round(race_white, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_white_pal,
          values = ~ race_white,
          group = "White",
          title = "% of Population identifying as<br>White by Census Tract (2016)",
          na.label = "NA")  %>%
        #black
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Black",
          fillColor = ~race_div_black_pal(race_black),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_black_moe, 1), "%<br/>",
                                "<strong> Percent Black: <strong>",
                                round(race_black, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_black_pal,
          values = ~ race_black,
          group = "Black",
          title = "% of Population identifying as<br>Black by Census Tract (2016)",
          na.label = "NA") %>%
        #NA
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native American",
          fillColor = ~race_div_na_pal(race_american_indian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_american_indian_moe, 1), "%<br/>",
                                "<strong> Percent Native American: <strong>",
                                round(race_american_indian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_na_pal,
          values = ~ race_american_indian,
          group = "Native American",
          title = "% of Population identifying as<br>Native American by Census Tract (2016)",
          na.label = "NA") %>%
        #asian
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Asian",
          fillColor = ~race_div_asian_pal(race_asian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_asian_moe, 1), "%<br/>",
                                "<strong> Percent Asian: <strong>",
                                round(race_asian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_asian_pal,
          values = ~ race_asian,
          group = "Asian",
          title = "% of Population identifying as<br>Asian by Census Tract (2016)",
          na.label = "NA") %>%
        #nh
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native Hawaiian",
          fillColor = ~race_div_nh_pal(race_native_hawaiian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_native_hawaiian_moe, 1), "%<br/>",
                                "<strong> Percent Native Hawaiian: <strong>",
                                round(race_native_hawaiian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_nh_pal,
          values = ~ race_native_hawaiian,
          group = "Native Hawaiian",
          title = "% of Population identifying as<br>Native Hawaiian by Census Tract (2016)",
          na.label = "NA") %>%
        #hispanic
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Hispanic",
          fillColor = ~race_div_hisp_pal(race_hispanic),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_hispanic_moe, 1), "%<br/>",
                                "<strong> Percent Hispanic: <strong>",
                                round(race_hispanic, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_hisp_pal,
          values = ~ race_hispanic,
          group = "Hispanic",
          title = "% of Population identifying as<br>Hispanic by Census Tract (2016)",
          na.label = "NA") %>%
        #other
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Other",
          fillColor = ~race_div_oth_pal(race_other),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_other_moe, 1), "%<br/>",
                                "<strong> Percent Other: <strong>",
                                round(race_other, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_oth_pal,
          values = ~ race_other,
          group = "Other",
          title = "% of Population identifying as<br>Other by Census Tract (2016)",
          na.label = "NA") %>%
        #multi
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Multiple Groups",
          fillColor = ~race_div_multi_pal(race_two_more),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2016)$race_two_more_moe, 1), "%<br/>",
                                "<strong> Percent Multiple Groups: <strong>",
                                round(race_two_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_multi_pal,
          values = ~ race_two_more,
          group = "Multiple Groups",
          title = "% of Population identifying with<br>multiple racial groups by Census Tract (2016)",
          na.label = "NA") %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLayersControl(
          position = "topleft",
          overlayGroups = c("White", "Black", "Native American", "Asian", "Native Hawaiian",
                            "Hispanic", "Other", "Multiple Groups"),
          options = layersControlOptions(collapsed = T)) %>%
        hideGroup(c("Black", "Native American", "Asian", "Native Hawaiian",
                    "Hispanic", "Other", "Multiple Groups"))
    }
    else if (input$raceyears == "2015"){
      leaflet(race_div_2015) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        #white
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "White",
          fillColor = ~race_div_white_pal(race_white),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_white_moe, 1), "%<br/>",
                                "<strong> Percent White: <strong>",
                                round(race_white, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_white_pal,
          values = ~ race_white,
          group = "White",
          title = "% of Population identifying as<br>White by Census Tract (2015)",
          na.label = "NA")  %>%
        #black
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Black",
          fillColor = ~race_div_black_pal(race_black),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_black_moe, 1), "%<br/>",
                                "<strong> Percent Black: <strong>",
                                round(race_black, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_black_pal,
          values = ~ race_black,
          group = "Black",
          title = "% of Population identifying as<br>Black by Census Tract (2015)",
          na.label = "NA") %>%
        #NA
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native American",
          fillColor = ~race_div_na_pal(race_american_indian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_american_indian_moe, 1), "%<br/>",
                                "<strong> Percent Native American: <strong>",
                                round(race_american_indian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_na_pal,
          values = ~ race_american_indian,
          group = "Native American",
          title = "% of Population identifying as<br>Native American by Census Tract (2015)",
          na.label = "NA") %>%
        #asian
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Asian",
          fillColor = ~race_div_asian_pal(race_asian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_asian_moe, 1), "%<br/>",
                                "<strong> Percent Asian: <strong>",
                                round(race_asian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_asian_pal,
          values = ~ race_asian,
          group = "Asian",
          title = "% of Population identifying as<br>Asian by Census Tract (2015)",
          na.label = "NA") %>%
        #nh
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Native Hawaiian",
          fillColor = ~race_div_nh_pal(race_native_hawaiian),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_native_hawaiian_moe, 1), "%<br/>",
                                "<strong> Percent Native Hawaiian: <strong>",
                                round(race_native_hawaiian, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_nh_pal,
          values = ~ race_native_hawaiian,
          group = "Native Hawaiian",
          title = "% of Population identifying as<br>Native Hawaiian by Census Tract (2015)",
          na.label = "NA") %>%
        #hispanic
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Hispanic",
          fillColor = ~race_div_hisp_pal(race_hispanic),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_hispanic_moe, 1), "%<br/>",
                                "<strong> Percent Hispanic: <strong>",
                                round(race_hispanic, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_hisp_pal,
          values = ~ race_hispanic,
          group = "Hispanic",
          title = "% of Population identifying as<br>Hispanic by Census Tract (2015)",
          na.label = "NA") %>%
        #other
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Other",
          fillColor = ~race_div_oth_pal(race_other),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_other_moe, 1), "%<br/>",
                                "<strong> Percent Other: <strong>",
                                round(race_other, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_oth_pal,
          values = ~ race_other,
          group = "Other",
          title = "% of Population identifying as<br>Other by Census Tract (2015)",
          na.label = "NA") %>%
        #multi
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Multiple Groups",
          fillColor = ~race_div_multi_pal(race_two_more),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(race_div_moe,
                                             year == 2015)$race_two_more_moe, 1), "%<br/>",
                                "<strong> Percent Multiple Groups: <strong>",
                                round(race_two_more, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addLegend(
          "bottomright",
          pal = race_div_multi_pal,
          values = ~ race_two_more,
          group = "Multiple Groups",
          title = "% of Population identifying with<br>multiple racial groups by Census Tract (2015)",
          na.label = "NA") %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLayersControl(
          position = "topleft",
          overlayGroups = c("White", "Black", "Native American", "Asian", "Native Hawaiian",
                            "Hispanic", "Other", "Multiple Groups"),
          options = layersControlOptions(collapsed = T)) %>%
        hideGroup(c("Black", "Native American", "Asian", "Native Hawaiian",
                    "Hispanic", "Other", "Multiple Groups"))
    }
  })

  output$raceplot <- renderPlotly({
    race <- acs_counties_neighbors %>% select(GEOID,NAME, year, contains("race")) # select appropriate variables
    race_moe <- race %>% select(NAME,year, contains("moe")) #separate moe estimates
    race_moe <- race_moe %>% melt(id.vars = c("NAME","year"), measure.vars = colnames(race_moe)[-c(1,2)]) %>%
      rename(moe = value)  %>% mutate(variable =gsub("_moe", "", variable))
    race <- race %>% select(!contains("moe"), NAME, year)
    race <- melt(race, id.vars = c("NAME", "year"),measure.vars = colnames(race)[-c(1,2)])
    race_table <- merge(x = race, y = race_moe, by=c("NAME", "variable", "year")) %>%
      mutate(variable = recode(variable, "race_american_indian" = "American Indian or Alaskan Native",
                               "race_asian" ="Asian", "race_black"="Black or African American",
                               "race_hispanic" = "Hispanic or Latino of any race",
                               "race_native_hawaiian" = "Native Hawaiian or Other Pacific Islander",
                               "race_other" = "Some Other Race",
                               "race_two_more" ="Two or More Races", "race_white"="Whte"))

    #plot all races onto one large set of grouped bars for every county.
    ggplotly(ggplot(filter(race_table, year == input$raceyears), aes(x = NAME, y = value, fill = variable,
                                                          text = paste0("Region: ", NAME,
                                                                        "<br>Year: ", year,
                                                                        "<br>Percent of Population: ", round(value, digits = 1), "%",
                                                                        "<br>Margin of Error: ", round(moe, digits = 1), "%"))) +
               geom_col(position = "dodge") +
               scale_fill_manual(values = viridis(8, option="D"), name="Groups") +
               ylab("% of Population") + xlab("") + coord_flip() + theme_minimal() +
               ggtitle(paste0("% Racial and Ethnic Diversity: ", input$raceyears)), tooltip="text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian",
                                         "hoverCompareCartesian","resetScale2d"))
  })
## SERVER: DATA TABLE - Race ----
  output$raceDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, race_white, race_white_moe, race_black, race_black_moe, 	
                     race_american_indian, race_american_indian_moe, race_asian, race_asian_moe, 
                     race_native_hawaiian, race_native_hawaiian_moe, race_hispanic, race_hispanic_moe, 
                     race_other, race_other_moe, race_two_more, race_two_more_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })    
  
  
  
  

## SERVER: PANEL - Family -----
## plotlyOutput("familyplot") ----
## leafletOutput("familymap") ----

  fam_stab_max_perc_2018 <- max(apply(X = select(data_frame(fam_stab_2018), -year, -NAME, -geometry), MARGIN = 1, FUN = max, na.rm = TRUE))
  fam_stab_2018_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                    domain = c(0, fam_stab_max_perc_2018))
  fam_stab_max_perc_2017 <- max(apply(X = select(data_frame(fam_stab_2017), -year, -NAME, -geometry), MARGIN = 1, FUN = max, na.rm = TRUE))
  fam_stab_2017_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                    domain = c(0, fam_stab_max_perc_2017))
  fam_stab_max_perc_2016 <- max(apply(X = select(data_frame(fam_stab_2016), -year, -NAME, -geometry), MARGIN = 1, FUN = max, na.rm = TRUE))
  fam_stab_2016_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                    domain = c(0, fam_stab_max_perc_2016))
  fam_stab_max_perc_2015 <- max(apply(X = select(data_frame(fam_stab_2015), -year, -NAME, -geometry), MARGIN = 1, FUN = max, na.rm = TRUE))
  fam_stab_2015_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                    domain = c(0, fam_stab_max_perc_2015))

  output$familymap <- renderLeaflet({
    if (input$familyyears == "2018") {
      leaflet(fam_stab_2018) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Parents who are Married",
          fillColor = ~fam_stab_2018_pal(family_married_parent_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2018$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2018)$family_married_parent_perc_moe, 1), "%<br/>",
                                "<strong> Percent Married: <strong>",
                                round(family_married_parent_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Fathers",
          fillColor = ~fam_stab_2018_pal(family_single_parent_male_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2018$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2018)$family_single_parent_male_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Fathers: <strong>",
                                round(family_single_parent_male_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Mothers",
          fillColor = ~fam_stab_2018_pal(family_single_parent_female_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2018$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2018)$family_single_parent_female_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Mothers: <strong>",
                                round(family_single_parent_female_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        # addPolygons(
        #   weight = 1,
        #   opacity = 0,
        #   fillOpacity = .7,
        #   group = "% of Children in Nonfamily Household",
        #   fillColor = ~fam_stab_2018_pal(family_children_nonfamily_perc),
        #   label = ~lapply(paste(sep = "",
        #                         substr(fam_stab_2018$NAME, 20, 60), "<br/>",
        #                         substr(fam_stab_2018$NAME, 1, 17),
        #                         "<br/>Margins of error: ",
        #                         round(filter(fam_stab_moe,
        #                                      year == 2018)$family_children_nonfamily_perc_moe,
        #                               1), "%<br/>",
        #                         "<strong> Percent Children in Nonfamily Household: <strong>",
        #                         round(family_children_nonfamily_perc, 1), "<strong>%"),
        #                   htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = fam_stab_2018_pal,
          values = ~ c(0, fam_stab_max_perc_2018),
          title = "% of Children under 18 with selected Family Stability<br>Indicator by Census Tract (2018)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers"),
          options = layersControlOptions(collapsed = F))
    }
    else if (input$familyyears == "2017") {
      leaflet(fam_stab_2017) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Parents who are Married",
          fillColor = ~fam_stab_2017_pal(family_married_parent_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2017$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2017)$family_married_parent_perc_moe, 1), "%<br/>",
                                "<strong> Percent Married: <strong>",
                                round(family_married_parent_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Fathers",
          fillColor = ~fam_stab_2017_pal(family_single_parent_male_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2017$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2017)$family_single_parent_male_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Fathers: <strong>",
                                round(family_single_parent_male_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Mothers",
          fillColor = ~fam_stab_2017_pal(family_single_parent_female_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2017$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2017)$family_single_parent_female_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Mothers: <strong>",
                                round(family_single_parent_female_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        # addPolygons(
        #   weight = 1,
        #   opacity = 0,
        #   fillOpacity = .7,
        #   group = "% of Children in Nonfamily Household",
        #   fillColor = ~fam_stab_2017_pal(family_children_nonfamily_perc),
        #   label = ~lapply(paste(sep = "",
        #                         substr(fam_stab_2017$NAME, 20, 60), "<br/>",
        #                         substr(fam_stab_2017$NAME, 1, 17),
        #                         "<br/>Margins of error: ",
        #                         round(filter(fam_stab_moe,
        #                                      year == 2017)$family_children_nonfamily_perc_moe,
        #                               1), "%<br/>",
        #                         "<strong> Percent Children in Nonfamily Household: <strong>",
        #                         round(family_children_nonfamily_perc, 1), "<strong>%"),
        #                   htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = fam_stab_2017_pal,
          values = ~ c(0, fam_stab_max_perc_2017),
          title = "% of Children under 18 with selected Family Stability<br>Indicator by Census Tract (2017)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers"),
          options = layersControlOptions(collapsed = F))
    }
    else if (input$familyyears == "2016") {
      leaflet(fam_stab_2016) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Parents who are Married",
          fillColor = ~fam_stab_2016_pal(family_married_parent_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2016$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2016)$family_married_parent_perc_moe, 1), "%<br/>",
                                "<strong> Percent Married: <strong>",
                                round(family_married_parent_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Fathers",
          fillColor = ~fam_stab_2016_pal(family_single_parent_male_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2016$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2016)$family_single_parent_male_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Fathers: <strong>",
                                round(family_single_parent_male_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Mothers",
          fillColor = ~fam_stab_2016_pal(family_single_parent_female_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2016$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2016)$family_single_parent_female_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Mothers: <strong>",
                                round(family_single_parent_female_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        # addPolygons(
        #   weight = 1,
        #   opacity = 0,
        #   fillOpacity = .7,
        #   group = "% of Children in Nonfamily Household",
        #   fillColor = ~fam_stab_2016_pal(family_children_nonfamily_perc),
        #   label = ~lapply(paste(sep = "",
        #                         substr(fam_stab_2016$NAME, 20, 60), "<br/>",
        #                         substr(fam_stab_2016$NAME, 1, 17),
        #                         "<br/>Margins of error: ",
        #                         round(filter(fam_stab_moe,
        #                                      year == 2016)$family_children_nonfamily_perc_moe,
        #                               1), "%<br/>",
        #                         "<strong> Percent Children in Nonfamily Household: <strong>",
        #                         round(family_children_nonfamily_perc, 1), "<strong>%"),
        #                   htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = fam_stab_2016_pal,
          values = ~ c(0, fam_stab_max_perc_2016),
          title = "% of Children under 18 with selected Family Stability<br>Indicator by Census Tract (2016)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers"),
          options = layersControlOptions(collapsed = F))
    }
    else if (input$familyyears == "2015") {
      leaflet(fam_stab_2015) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Parents who are Married",
          fillColor = ~fam_stab_2015_pal(family_married_parent_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2015$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2015)$family_married_parent_perc_moe, 1), "%<br/>",
                                "<strong> Percent Married: <strong>",
                                round(family_married_parent_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Fathers",
          fillColor = ~fam_stab_2015_pal(family_single_parent_male_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2015$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2015)$family_single_parent_male_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Fathers: <strong>",
                                round(family_single_parent_male_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Single Mothers",
          fillColor = ~fam_stab_2015_pal(family_single_parent_female_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2015$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2015)$family_single_parent_female_perc_moe, 1),
                                "%<br/>",
                                "<strong> Percent of Single Mothers: <strong>",
                                round(family_single_parent_female_perc, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        # addPolygons(
        #   weight = 1,
        #   opacity = 0,
        #   fillOpacity = .7,
        #   group = "% of Children in Nonfamily Household",
        #   fillColor = ~fam_stab_2015_pal(family_children_nonfamily_perc),
        #   label = ~lapply(paste(sep = "",
        #                         substr(fam_stab_2015$NAME, 20, 60), "<br/>",
        #                         substr(fam_stab_2015$NAME, 1, 17),
        #                         "<br/>Margins of error: ",
        #                         round(filter(fam_stab_moe,
        #                                      year == 2015)$family_children_nonfamily_perc_moe,
        #                               1), "%<br/>",
        #                         "<strong> Percent Children in Nonfamily Household: <strong>",
        #                         round(family_children_nonfamily_perc, 1), "<strong>%"),
        #                   htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = fam_stab_2015_pal,
          values = ~ c(0, fam_stab_max_perc_2015),
          title = "% of Children under 18 with selected Family Stability<br>Indicator by Census Tract (2015)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers"),
          options = layersControlOptions(collapsed = F))
    }
  })

  output$familyplot <- renderPlotly({
    family <- select(filter(acs_counties_neighbors), NAME, year,contains("family"))
    family_perc <- family %>% select(NAME, year, family_married_parent_perc, family_single_parent_female_perc,
                                     family_single_parent_male_perc, family_children_nonfamily_perc)
    family_moe <- family %>% select(NAME, year, family_married_parent_perc_moe, family_single_parent_female_perc_moe,
                                    family_single_parent_male_perc_moe,
                                    family_children_nonfamily_perc_moe)
    family_moe <- melt(family_moe, id.vars = c("NAME","year"), measure.vars = colnames(family_moe)[-c(1,2)]) %>%
      rename("moe" ="value") %>% mutate(variable =gsub("_moe", "", variable))
    family_perc <- melt(family_perc, id.vars = c("NAME","year"), measure.vars = colnames(family_perc)[-c(1,2)])
    family_table <- merge(x = family_perc, y = family_moe, by=c("NAME", "variable", "year")) %>%
      mutate(variable = recode_factor(variable, "family_married_parent_perc" ="Married Parents",
                                      "family_single_parent_perc" = "Single Parent",
                                      "family_single_parent_female_perc" = "Single Mother",
                                      "family_single_parent_male_perc" = "Single Father",
                                      "family_children_nonfamily_perc" = "Living with Nonfamily"))
    #grouped bar chart for family type
    ggplotly(ggplot(filter(family_table, year == input$familyyears, variable != "Living with Nonfamily",
                           NAME != "South Wasco County School District 1, OR"), aes(x = NAME, y = value, fill = variable,
                                                                                    text = paste0("Region: ", NAME,
                                                                                                  "<br>Year: ", year,
                                                                                                  "<br>Percent of Children: ", round(value, digits = 1), "%",
                                                                                                  "<br>Margin of Error: ", round(moe, digits = 1), "%"))) +
               geom_col(position = "dodge") +
               scale_fill_manual(values = viridis(4, option="D"), name="Family Type")  +
               ylab("% of children")+xlab("") + coord_flip()+ theme_minimal() +
               ggtitle(paste0("Family Structure for Children Under 18 <br>", input$familyyears)), tooltip = "text")%>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian",
                                         "hoverCompareCartesian","resetScale2d"))
  })
  ## SERVER: DATA TABLE - Family ----
  output$familyDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, family_married_parent_perc, family_married_parent_perc_moe, 	
                     family_single_parent_male_perc, family_single_parent_male_perc_moe, 	
                     family_single_parent_female_perc, family_single_parent_female_perc_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })      
  
  
  
  

## SERVER: PANEL - Education attainment -----
## plotlyOutput("degreeplot") -----
## leafletOutput("degreemap") ----

  edu_attain_max_perc_2018 <- max(apply(X = select(data_frame(edu_attain_2018),
                                                   -year, -NAME, -geometry), 1, max, TRUE))
  edu_attain_2018_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                      domain = c(0, edu_attain_max_perc_2018))
  edu_attain_max_perc_2017 <- max(apply(X = select(data_frame(edu_attain_2017),
                                                   -year, -NAME, -geometry), 1, max, TRUE))
  edu_attain_2017_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                      domain = c(0, edu_attain_max_perc_2017))
  edu_attain_max_perc_2016 <- max(apply(X = select(data_frame(edu_attain_2016),
                                                   -year, -NAME, -geometry), 1, max, TRUE))
  edu_attain_2016_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                      domain = c(0, edu_attain_max_perc_2016))
  edu_attain_max_perc_2015 <- max(apply(X = select(data_frame(edu_attain_2015),
                                                   -year, -NAME, -geometry), 1, max, TRUE))
  edu_attain_2015_pal <- colorNumeric(viridis_pal(option = "D")(3),
                                      domain = c(0, edu_attain_max_perc_2015))

  output$degreemap <- renderLeaflet({
    if (input$degreeyears == "2018"){
      leaflet(edu_attain_2018) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Less than High School",
          fillColor = ~edu_attain_2018_pal(education_less_hs),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2018$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2018)$education_less_hs_moe, 1), "%<br/>",
                                "<strong> Percent < HS Education: <strong>",
                                round(education_less_hs, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "High School Graduate",
          fillColor = ~edu_attain_2018_pal(education_hs_grad),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2018$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2018)$education_hs_grad_moe, 1), "%<br/>",
                                "<strong> Percent HS Graduate: <strong>",
                                round(education_hs_grad, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Associates or Some College",
          fillColor = ~edu_attain_2018_pal(education_assoc_some_college),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2018$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2018)$education_assoc_some_college_moe, 1), "%<br/>",
                                "<strong> Percent AA or Some College: <strong>",
                                round(education_assoc_some_college, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Bachelors or Higher",
          fillColor = ~edu_attain_2018_pal(education_bachelors_or_higher),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2018$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2018)$education_bachelors_or_higher_moe, 1), "%<br/>",
                                "<strong> Percent Bachelors or Higher: <strong>",
                                round(education_bachelors_or_higher, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = edu_attain_2018_pal,
          values = ~ c(0, edu_attain_max_perc_2018),
          title = "% of Population with selected<br>Education Level by Census Tract (2018)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("Less than High School", "High School Graduate",
                         "Associates or Some College", "Bachelors or Higher"),
          options = layersControlOptions(collapsed = T))
    }
    else if (input$degreeyears == "2017"){
      leaflet(edu_attain_2017) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "2017",
          fillColor = ~edu_attain_2017_pal(education_less_hs),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2017$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2017)$education_less_hs_moe, 1), "%<br/>",
                                "<strong> Percent < HS Education: <strong>",
                                round(education_less_hs, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "High School Graduate",
          fillColor = ~edu_attain_2017_pal(education_hs_grad),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2017$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2017)$education_hs_grad_moe, 1), "%<br/>",
                                "<strong> Percent HS Graduate: <strong>",
                                round(education_hs_grad, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Associates or Some College",
          fillColor = ~edu_attain_2017_pal(education_assoc_some_college),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2017$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2017)$education_assoc_some_college_moe, 1), "%<br/>",
                                "<strong> Percent AA or Some College: <strong>",
                                round(education_assoc_some_college, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Bachelors or Higher",
          fillColor = ~edu_attain_2017_pal(education_bachelors_or_higher),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2017$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2017)$education_bachelors_or_higher_moe, 1), "%<br/>",
                                "<strong> Percent Bachelors or Higher: <strong>",
                                round(education_bachelors_or_higher, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = edu_attain_2017_pal,
          values = ~ c(0, edu_attain_max_perc_2017),
          title = "% of Population with selected<br>Education Level by Census Tract (2017)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("Less than High School", "High School Graduate",
                         "Associates or Some College", "Bachelors or Higher"),
          options = layersControlOptions(collapsed = T))
    }
    else if (input$degreeyears == "2016"){
      leaflet(edu_attain_2016) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Less than High School",
          fillColor = ~edu_attain_2016_pal(education_less_hs),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2016$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2016)$education_less_hs_moe, 1), "%<br/>",
                                "<strong> Percent < HS Education: <strong>",
                                round(education_less_hs, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "High School Graduate",
          fillColor = ~edu_attain_2016_pal(education_hs_grad),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2016$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2016)$education_hs_grad_moe, 1), "%<br/>",
                                "<strong> Percent HS Graduate: <strong>",
                                round(education_hs_grad, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Associates or Some College",
          fillColor = ~edu_attain_2016_pal(education_assoc_some_college),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2016$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2016)$education_assoc_some_college_moe, 1), "%<br/>",
                                "<strong> Percent AA or Some College: <strong>",
                                round(education_assoc_some_college, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Bachelors or Higher",
          fillColor = ~edu_attain_2016_pal(education_bachelors_or_higher),
          label = ~lapply(paste(sep = "",
                                substr(edu_attain_2016$NAME, 20, 60), "<br/>",
                                substr(edu_attain_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2016)$education_bachelors_or_higher_moe, 1), "%<br/>",
                                "<strong> Percent Bachelors or Higher: <strong>",
                                round(education_bachelors_or_higher, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = edu_attain_2016_pal,
          values = ~ c(0, edu_attain_max_perc_2016),
          title = "% of Population with selected<br>Education Level by Census Tract (2016)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("Less than High School", "High School Graduate",
                         "Associates or Some College", "Bachelors or Higher"),
          options = layersControlOptions(collapsed = T))
    }
    else if (input$degreeyears == "2015"){
      leaflet(edu_attain_2015) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Less than High School",
          fillColor = ~edu_attain_2015_pal(education_less_hs),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2015)$education_less_hs_moe, 1), "%<br/>",
                                "<strong> Percent < HS Education: <strong>",
                                round(education_less_hs, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "High School Graduate",
          fillColor = ~edu_attain_2015_pal(education_hs_grad),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2015)$education_hs_grad_moe, 1), "%<br/>",
                                "<strong> Percent HS Graduate: <strong>",
                                round(education_hs_grad, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Associates or Some College",
          fillColor = ~edu_attain_2015_pal(education_assoc_some_college),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2015)$education_assoc_some_college_moe, 1), "%<br/>",
                                "<strong> Percent AA or Some College: <strong>",
                                round(education_assoc_some_college, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "Bachelors or Higher",
          fillColor = ~edu_attain_2015_pal(education_bachelors_or_higher),
          label = ~lapply(paste(sep = "",
                                substr(NAME, 20, 60), "<br/>",
                                substr(NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(edu_attain_moe,
                                             year == 2015)$education_bachelors_or_higher_moe, 1), "%<br/>",
                                "<strong> Percent Bachelors or Higher: <strong>",
                                round(education_bachelors_or_higher, 1), "<strong>%"),
                          htmltools::HTML)) %>%
        addPolylines(
          data = south_wasco_points,
          color = "#ffce9e",
          weight = 2,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap",
          label = "South Wasco County Region") %>%
        addPolylines(
          data = county_lines,
          color = "#d48537",
          weight = 1,
          opacity = 1,
          fillOpacity= 0,
          group = "Basemap") %>%
        addLegend(
          "bottomright",
          pal = edu_attain_2015_pal,
          values = ~ c(0, edu_attain_max_perc_2015),
          title = "% of Population with selected<br>Education Level by Census Tract (2015)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("Less than High School", "High School Graduate",
                         "Associates or Some College", "Bachelors or Higher"),
          options = layersControlOptions(collapsed = T))
    }
  })


  output$degreeplot <- renderPlotly({
    ed <- select(filter(acs_counties_neighbors), NAME, year, contains("education"))
    ed_perc <- ed %>% select(NAME, year,education_less_hs, education_hs_grad, education_assoc_some_college, education_bachelors_or_higher)
    ed_moe <- ed %>% select(NAME, year, education_less_hs_moe, education_hs_grad_moe,
                            education_assoc_some_college_moe, education_bachelors_or_higher_moe)
    ed_moe <- melt(ed_moe, id.vars = c("NAME", "year"), measure.vars = colnames(ed_moe)[-c(1,2)]) %>%
      rename("moe" ="value") %>% mutate(variable =gsub("_moe", "", variable))
    ed_perc <- melt(ed_perc, id.vars = c("NAME", "year"), measure.vars = colnames(ed_perc)[-c(1,2)])
    ed_table <- merge(x = ed_perc, y = ed_moe, by=c("NAME", "variable", "year")) %>%
      mutate(value = round(value,1), moe = round(moe,1),
             variable = recode_factor(variable, "education_less_hs" ="Less than High School",
                                      "education_hs_grad" = "High School Graduate or Equivalent (GED)",
                                      "education_assoc_some_college" ="Associates Degree or Some College",
                                      "education_bachelors_or_higher" ="Bachelors or Higher"))

    #grouped bar chart for own and rent occupancy
    ggplotly(ggplot(filter(ed_table, year == input$degreeyears)) +
               geom_bar(aes(x = NAME, y = value, fill = variable,
                            text = paste0("Region: ", NAME,
                                          "<br>Year: ", year,
                                          "<br>Percent of Adults 25 and Older: ", value, "%",
                                          "<br>Margin of Error: ", moe, "%")),
                        position = position_stack(reverse = TRUE), stat="identity") +
               scale_fill_manual(values = viridis(4, option = "D"),
                                 name = "Educational Attainment") +
               #theme_minimal() + theme(axis.text.x = element_text(angle=30)) +
               ylab("% of Adults 25 and Older") + xlab("") +
               coord_flip()+ theme_minimal() +
               ggtitle(paste("Educational Attainment for Adults 25 and Older",input$degreeyears, sep = " ")), tooltip = "text")%>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian",
                                         "hoverCompareCartesian","resetScale2d"))
  })
  
  ## SERVER: DATA TABLE - Educational Attainment ----
  output$degreeDT <- renderDT({ 
    datatable(select(acs_data, NAME, year, education_less_hs, education_less_hs_moe, 	
                     education_hs_grad, education_hs_grad_moe, 	
                     education_assoc_some_college, education_assoc_some_college_moe,
                     education_bachelors_or_higher, education_bachelors_or_higher_moe),
              extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
              options = list(
                dom = 'Bfrtip',
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                buttons =
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ))
              ))
  })      


} # end of BUILDING SERVER



shinyApp(ui, server)
