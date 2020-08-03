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

######## USE THE FOLLOWING ##########
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

## UI: Begins -------

ui <- dashboardPagePlus(

## UI: Dashboard header -------
  dashboardHeaderPlus(
    title = "DSPG 2020 Wasco EM",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info"
  ),

## UI: Dashboard sidebar --------
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem(
        tabName = "overview",
        text = "Project Overview",
        icon = icon("info circle")
      ),
      menuItem(
        tabName = "data",
        text = "Data & Methodology",
        icon = icon("database")
      ),

## UI: Findings menu ------
      menuItem(
        tabName = "findings",
        text = "Findings",
        icon = icon("chart-pie"),
## UI: Findings menu subitems (clusters and drivers) ---------
        menuSubItem(
          tabName = "food",
          text = "Cluster: Food Systems",
          icon = icon("utensils")
        ),
        menuSubItem(
          tabName = "infrastructure",
          text = "Cluster: Infrastructure",
          icon = icon("truck-pickup")
        ),
        menuSubItem(
          tabName = "learnearn",
          text = "Driver: Opportunities to Learn and Earn",
          icon = icon("graduation-cap")
        ),
        menuSubItem(
          tabName = "living",
          text = "Driver: Quality Standard of Living",
          icon = icon("laugh-beam")
        )),
      menuItem(
        tabName = "team",
        text = "Team",
        icon = icon("user-friends")
      ))),

## UI: Dashboard body -------
  dashboardBody(
    customTheme,
    fluidPage(
      tabItems(
## UI: TAB - Overview --------
        tabItem(tabName = "overview",
                fluidRow(
                  boxPlus(
                    title = "Project overview",
                    closable = FALSE,
                    width = NULL,
                    enable_label = TRUE,
                    label_text = 1,
                    label_status = "danger",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h1("2020 South Wasco Region Project"),
                    h2("Project Description"),
                    p("The southern region of Wasco County, Oregon (South Wasco) experienced significant economic decline in the 1980s, causing closure of schools and consolidation of students into the South Wasco School District. This precipitated an eventual out-migration, disruption of social fabric, and a steady decline in overall economic stability, community health, standard of living, and quality of life."),
                    ## Here we will add a picture of SW for description
                    h2("Project Goals"),
                    p("The purpose of our project is to provide community decisionmakers with baseline datasets and comparative analyses to similar rural areas of the likely factors affecting economic mobility"),
                    h2("Project Scope"),
                    p("The term South Wasco is defined by the South Wasco County School District, which encompasses the southernmost region of Wasco County, Oregon.")
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
                      "How many students in South Wasco are ellgible for Free and Reduced Price Lunch?" = "Lunch",
                      "What local crops are grown in South Wasco?" = "Crops"),
                    width = "300px", selected = NULL
                  )),
## UI: PANEL - Food systems map  ------
                conditionalPanel(
                  condition = "input.foodselect == 'Foodmap'",
                  # Add selection for domain, theme questions
                  # Leaflet map for food insecurity data or line chart
                  # Free and reduced price lunch data (?)
                  # Crop maps in here, major agricultural crops
                  flipBox(
                    id = 1,
                    main_img = "https://image.flaticon.com/icons/svg/1445/1445611.svg",
                    header_img = "https://image.flaticon.com/icons/svg/3175/3175193.svg",
                    front_title = "How accessible is healthy and affordable food in South Wasco?",
                    back_title = "Data",
                    h2("Interactive food systems map"),
#                    "Explore the different types of stores available for people #living in South Wasco to purchase food. The legend indicates whether stores #accept payment from federal food assistance programs and WIC. Use the Show #driving time tool to show 30 minute and 1 hour driving time areas from stores.",
                    # Fix the symbology (Aaron),
                    # Fix the driving time legend
                    # See why some points don't have names
                    leafletOutput("mymap"),
                    selectInput("iso", "Show driving time for...",
                                choices = isochrones$name,
                                selectize = TRUE,
                                multiple = TRUE,
                                width = "300px"),
                    "Explore the data...",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        h2("Methods: Clusters of Innovation"),
                        #img(src="FoodSystems.png", align = "center", width = 60),
                       # "Data for this map is from here",
                        # Remove the cluster
                        # Add indicator table snippet
                        # Add the data table for this
                        DTOutput("foodDT"))))),
## UI: PANEL - Food insecurity  ----
                conditionalPanel(
                  condition = "input.foodselect == 'Insecurity'",
                  flipBox(
                    id = 2,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the food insecurity rate in South Wasco?",
                    selectInput("ratetype", "Which Food Insecurity Rate?",
                                c("Overall", "Childhood")),
                    leafletOutput("foodinsecuritymap"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      #Indicator table snippet,
                      #DTOutput("insecurityDT")
                    ))
                  )),
## UI: PANEL - Free and reduced lunch ------
                conditionalPanel(
                  condition = "input.foodselect == 'Lunch'",
                  flipBox(
                    id = 3,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "How many students in South Wasco are ellgible for Free and Reduced Price Lunch?",
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      #Indicator table snippet,
                      #DTOutput("lunchDT")
                    ))
                  )),
## UI: PANEL - Local crops panel -----
                conditionalPanel(
                  condition = "input.foodselect == 'Crops'",
                  flipBox(
                    id = 4,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What local crops are grown in South Wasco?",
                    selectInput("crops", "Which crop?",
                                c("Winter Wheat", "Barley",
                                  "Alfalfa", "Cherries")),
                    leafletOutput("cropmap"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      #Indicator table snippet,
                      #DTOutput("cropDT")
                    ))
                  ))
        ), # END OF FOOD SYSTEMS CLUSTER

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
                    "What is the importance of wind & wolar projects to South Wasco?" = "WindSolar",
                    "What is access to broadband like and why is it important?" = "Broadband",
                    "What is water use like in Wasco?" = "Water",
                    "What is the transit system like in South Wasco?" = "Transit"),
                  width = "300px", selected = NULL
                )),
## UI: PANEL - Wind and solar -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'WindSolar'",
                  boxPlus(
                   title = "What is the importance of wind & wolar projects to South Wasco?",
                   width = 4,
                    #img of wind solar infocard,
                   HTML('<div class="canva-embed" data-design-id="DAEDe8MzN7A" data-height-ratio="2.5000" style="padding:250.0000% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAEDe8MzN7A&#x2F;view?utm_content=DAEDe8MzN7A&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Wind &amp; Solar Projects in Wasco</a>')
                   )
                  ),
## UI: PANEL - Broadband -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Broadband'",
                  boxPlus(
                  title = "What is access to broadband like and why is it important?",
                  width = 4,
                  #img of broadband infocard
                  HTML('<div class="canva-embed" data-design-id="DAEDhGP13C4" data-height-ratio="2.5000" style="padding:250.0000% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAEDhGP13C4&#x2F;view?utm_content=DAEDhGP13C4&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Broadband Access</a> by Owen Hart')
                  )),
## UI: PANEL - Water -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Water'",
                  flipBox(
                    id = 5,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is water use like in Wasco?",
                    back_title = "Data",
                    "",
                    plotlyOutput("waterplot"),
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      #Indicator table snippet,
                      #DTOutput("infrastructureDT")
                    ))
                    )),
## UI: PANEL - Transit -------
               conditionalPanel(
                 condition = "input.infrastructureselect == 'Transit'",
                 boxPlus(
                   title = "What is the transit system like in South Wasco?",
                   width = 4,
                  # img of transit infocard
                   HTML('<div class="canva-embed" data-design-id="DAEDgP5Krv8" data-height-ratio="2.5000" style="padding:250.0000% 5px 5px 5px;background:rgba(0,0,0,0.03);border-radius:8px;"></div><script async src="https:&#x2F;&#x2F;sdk.canva.com&#x2F;v1&#x2F;embed.js"></script><a href="https:&#x2F;&#x2F;www.canva.com&#x2F;design&#x2F;DAEDgP5Krv8&#x2F;view?utm_content=DAEDgP5Krv8&amp;utm_campaign=designshare&amp;utm_medium=embeds&amp;utm_source=link" target="_blank" rel="noopener">Transportation in Wasco County</a> by Owen Hart')
                ))), # END OF INFRASTRUCTURE


## UI: TAB -  Learn and earn driver -----------
        tabItem(tabName = "learnearn",
                fluidRow(
                  selectInput(
                    inputId = "learnearnselect",
                    label = "Select domain",
                    list(" " = " ",
                      "Education" = "Education",
                      "Employment" = "Employment"),
                    width = "300px", selected = NULL
                  )),
## UI: PANEL - Education composite -------
                conditionalPanel(
                  condition = "input.learnearnselect == 'Education'",
                  # How are we visualizing this? State and school district, line chart, maybe map?
                  # Back will have data and indicator snippet/sources
                  flipBox(
                    id = 6,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    #Education line chart
                    #Education map
                    front_title = "Education",
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      #Education indicator snippet
                      #DTOutput("educationDT")
                    ))
                  )
                ),
# Employment select panel
                conditionalPanel(
                  condition = "input.learnearnselect == 'Employment'",
                  # We are missing access to jobs that pay a living wage
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
                    ))),
# UI: PANEL - Employment ratio  -------
conditionalPanel(
  condition = "input.employmentselect == 'EmpRatio'",
  flipBox(
    id = 7,
    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
    front_title = "What is the employment ratio in South Wasco?",
    leafletOutput("percempmap"),
    plotlyOutput("empratioplot"),
    back_title = "Data",
    # Full back with table and indicator snippet
    "",
    back_content = tagList(
      column(
        width = 12,
        align = "center"
        #Indicator table snippet
        #DTOutput("acs_counties")
        )))),
# UI: PANEL - Label force participation rate -------
                conditionalPanel(
                  condition = "input.employmentselect == 'LaborForce'",
                  flipBox(
                    id = 8,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the labor force participation rate in South Wasco?",
                    leafletOutput("laborforcemap"),
                    plotlyOutput("laborforceplot"),
                    back_title = "Data",
                    # Full back with table and indicator snippet
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                        #Indicator table snippet
                        #DTOutput("acs_counties")
                        )
                        ))),
# UI: PANEL - Job flows  -------
                        conditionalPanel(
                          condition = "input.employmentselect == 'Flows'",
                          flipBox(
                            id = 9,
                            main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                            header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                            front_title = "How do workers flow in and out of South Wasco?",
                            selectInput("flows", "Inflows or Outflows?",
                                        c("Inflows", "Outflows")),
                            plotlyOutput("flowsplot"),
                            back_title = "Flows Data",
                            # Full back with table and indicator snippet
                            "",
                            back_content = tagList(
                              column(
                                width = 12,
                                align = "center",
                                "Flows data comes from LODES xyz"
                                #DTOutput("flowsDT")
                                )
                                ))),
## UI: PANEL - Industry Sectors  ------
                conditionalPanel(
                  condition = "input.employmentselect == 'Sectors'",
                  flipBox(
                    id = 10,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What types of jobs are in South Wasco?",
                    selectInput("sect", "What sectors?",
                                c("All" = "All", "Goods Producing" = "Goods", "Trade, Transportation, and Utilities" = "Trade", "All Other Services" = "AllOther")),
                    leafletOutput("odleaf"),
                    back_title = "Sectors Data",
                    # Full back with table and indicator snippet
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        "Sectors data comes from LODES xyz"
                        #DTOutput("sectorsDT")
                      )))
                  )
        ), ## END OF LEARN EARN TAB

## UI: TAB - Quality standard of living driver -----------
        tabItem(tabName = "living",
                fluidRow(
                    # Remove dropdown button
                    # Select domain, then select question
                      selectInput(
                        inputId = "livingdomainselect",
                        label = "Select domain",
                        list(" " = " ",
                          "Financial" = "Financial",
                          "Housing" = "Housing",
                          "Social" = "Social"),
                        width = "300px", selected = NULL
                      )),
# Financial select question
                conditionalPanel(
                  condition = "input.livingdomainselect == 'Financial'",
                  fluidRow(
                    selectInput(
                      inputId = "financialselect",
                      label = "I'm wondering...",
                      list(" " = " ",
                        "What is the median income in South Wasco?" = "MedIncome",
                        "What is the poverty rate in South Wasco?" = "Poverty",
                        "What is the income distribution in South Wasco" = "DisIncome"),
                      width = "300px", selected = NULL
                    ))),
## UI: PANEL - Median income  ------
                    conditionalPanel(
                      condition = "input.financialselect == 'MedIncome'",
                      flipBox(
                        id = 11,
                        main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                        header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                        front_title = "What is the median income in South Wasco?",
                        # Median income only here, poverty, income brackets are the questions
                        plotlyOutput("medincomeplot"),
                        back_title = "Data",
                        "",
                        back_content = tagList(
                          column(
                            width = 12,
                            align = "center"
                          # Indicator table snippet
                          # DTOutput("acs_counties")
                        ))
                      )),
## UI: PANEL - Poverty rate ------
                conditionalPanel(
                  condition = "input.financialselect == 'Poverty'",
                  flipBox(
                    id = 12,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the poverty rate in South Wasco?",
                    leafletOutput("povertymap"),
                    plotlyOutput(outputId = "povertyplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    ))
                  )),
## UI: PANEL - Income Distribution  ------
                conditionalPanel(
                  condition = "input.financialselect == 'DisIncome'",
                  flipBox(
                    id = 13,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the income distribution in South Wasco?",
                    # Median income only here, poverty, income brackets are the questions
                    selectInput("incomedisyear", "Which year?",
                                c("2018", "2017", "2016", "2015")),
                    leafletOutput("incomedismap"),
                    plotlyOutput("incomedisplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    ))
                  )),
# Housing select question
conditionalPanel(
  condition = "input.livingdomainselect == 'Housing'",
  fluidRow(
    selectInput(
      inputId = "housingselect",
      label = "I'm wondering...",
      list(" " = " ",
        "How much affordable housing is in South Wasco?" = "Housing",
           "What is the home ownership rate in South Wasco?" = "RentOwn"),
      width = "300px", selected = NULL
    ))),
## UI: PANEL - Affordable housing -----
                  conditionalPanel(
                    condition = "input.housingselect == 'Housing'",
                    flipBox(
                      id = 14,
                      main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                      header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                      front_title = "How much affordable housing is in South Wasco?",
                      # Overall and ownership/rental (both lines and maps?)
                      # Full back with table and indicator snippet
                      plotlyOutput("housingplot"),
                      back_title = "Data",
                      "",
                      back_content = tagList(
                        column(
                          width = 12,
                          align = "center"
                        # Indicator table snippet
                        # DTOutput("acs_counties")
                      ))
                    )
                ),
## UI: PANEL - Rent vs own -------
                conditionalPanel(
                  condition = "input.housingselect == 'RentOwn'",
                  flipBox(
                    id = 15,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the home ownership rate in South Wasco?",
                    # Overall and ownership/rental (both lines and maps?)
                    # Full back with table and indicator snippet
                    plotlyOutput("rentownplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    ))
                  )
                ),
# Social select question
conditionalPanel(
  condition = "input.livingdomainselect == 'Social'",
  fluidRow(
    selectInput(
      inputId = "socialselect",
      label = "I'm wondering...",
      list(" " = " ",
        "What is the racial diversity of South Wasco?" = "Race",
           "What types of familiy structures are in South Wasco?" = "Family",
           "What is the educational background of people in South Wasco?" = "Education"),
      width = "300px", selected = NULL
    ))),
# UI: PANEL - Race  --------
                conditionalPanel(
                  condition = "input.socialselect == 'Race'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  flipBox(
                    id = 16,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the racial diversity of South Wasco?",
                    selectInput("raceyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                    leafletOutput("racemap"),
                    plotlyOutput("raceplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    ))
                  )),
# UI: PANEL - Family ------
                conditionalPanel(
                  condition = "input.socialselect == 'Family'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  flipBox(
                    id = 17,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What types of familiy structures are in South Wasco?",
                    selectInput("familyyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                    leafletOutput("familymap"),
                    plotlyOutput("familyplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    ))
                  )
                ),
# UI: PANEL - Education attainment -------
                conditionalPanel(
                  condition = "input.socialselect == 'Education'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  flipBox(
                    id = 18,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the educational background of people in South Wasco?",
                    selectInput("degreeyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                    leafletOutput("degreemap"),
                    plotlyOutput("degreeplot"),
                    back_title = "",
                    "",
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center"
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    ))
                  )
                )), # END OF QUALITY STANDARD OF LIVING TAB


## UI: TAB - Data and Methods ----------
      tabItem(tabName = "data",
              fluidRow(
                boxPlus(
                  title = "Data & Methodology",
                  closable = FALSE,
                  width = NULL,
                  enable_label = TRUE,
                  label_text = 1,
                  label_status = "danger",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h2("Methods and Frameworks"),
                  # Subheadings for clusters
                  # Dropdown menu to select cluster
                  # Description with cluster visual
                  p("Rural Clusters of Innovation from Berkshires Strategy Project. Visualizes community agencies and organizations that contribute to economic mobility increasing sectors. Tailored to specific communities, narrows focus on areas of sponsor interest."),
                  p("Boosting Upward Mobility from Urban Institute. Multidimensional approach to economic mobility. Includes ideas for relevant metrics at the local level."),
                  # Just add more info/basics about these
                  p("Weaving in Good Rural Data from Urban Institute"),
                  # More info/basics
                  h2("Data Collection and Analysis"),
                  p("This is how we collected the data. Explore the right panel for more data information!")
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
                  label_text = 1,
                  label_status = "danger",
                  status = "warning",
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
                  title = "Findings",
                  closable = FALSE,
                  width = NULL,
                  enable_label = TRUE,
                  label_text = 1,
                  label_status = "danger",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h2("DSPG Team Members"),
                  # Add headshots
                  p("Mary Solomon, DSPG Graduate Fellow (M.S. Applied Statistics), Bowling Green State University"),
                  p("Joanna Schroeder, DSPG Intern, William & Mary"),
                  p("Owen Hart, DSPG Intern, University of California Berkeley"),
                  # Reach out an ask about headshots for them
                  h2("UVA SDAD Team Members"),
                  p("Aaron Schroeder (PI), Research Associate Professor & Data Scientist (Ph.D. Public Policy)"),
                  p("Eric Oh, Research Assistant Professor of Statistics (Ph.D Biostatistics)"),
                  p("Alyssa Mikytuck, Postdoctoral Associate (Ph.D Human Development)"),
                  # Add logos for these people
                  h2("Project Sponsors"),
                  p("Kathleen Willis, coordinating stakeholder, South Wasco Alliance"), p("Kathleen's team: Elle Christensen, Eva Kahn, Hannah Fuller"),
                  p("Carrie Pipinich, Senior Project Manager, Mid-Columbia Economic District"),
                  p("Shannon Caplan, Program Coordinator, Rural Communities Explorer"),
                  p("Kelly Howsley-Glover, Long Range/Special Projects Planner, Wasco County Planning Department"),
                  h2("Acknowledgements"),
                  p(
                    "[Optional: You can also include external collaborators in this section or a separate section.]"
                  )
                )
              ))
      ) # end of TAB ITEMS (global dashboard body)
    ) # end of FLUID PAGE (global dashboard body)
  )# end of DASHBOARD BODY
) # end of DASHBOARD UI



## SERVER: Begins --------
server <- function(input, output, session) {

  graypal = "#ADB5BD"

## SERVER: INDICATOR TABLES -------
## SERVER: DATA TABLES -----
## SERVER: DATA TABLE - Food systems map -----
  output$foodDT <- renderDT({
    datatable(food_points,
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
                          )))
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
## SERVER: PANEL - Free and reduced price lunch ----
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
        label = ~`WAFBS`,
        fillColor = ~food_counties_pal(FdIR),
        group = "2014") %>%
      addPolygons(
        data = shp2015,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~`WAFBS`,
        fillColor = ~food_counties_pal(FdIR),
        group = "2015") %>%
      addPolygons(
        data = shp2016,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~`WAFBS`,
        fillColor = ~food_counties_pal(FdIR),
        group = "2016") %>%
      addPolygons(
        data = shp2017,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~`WAFBS`,
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
        label = ~`WAFBS`,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2014") %>%
      addPolygons(
        data = shp2015,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~`WAFBS`,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2015") %>%
      addPolygons(
        data = shp2016,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~`WAFBS`,
        fillColor = ~food_counties_pal_c(Cfir),
        group = "2016") %>%
      addPolygons(
        data = shp2017,
        weight = 1,
        opacity = 0,
        fillOpacity = .7,
        label = ~`WAFBS`,
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
               ylab("Employment Ratio (%)") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
  })

## SERVER: PANEL - Labor force participation ----
## plotlyOutput("laborforcemap")WORKING ON ------
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

## SERVER: PANEL - Job flows ----
## plotlyOutput("flows") ------

  output$flowsplot <- renderPlotly({
    if (input$flows == "Inflows"){
      top_12_in_melt <- melt(data = top_12_in, id.vars = c("year"),
                             measure.vars = colnames(top_12_in)[-length(top_12_in)]) %>%
        rename(c("county" = "variable", "jobs" = "value"))

      ggplotly(ggplot(top_12_in_melt, aes(x=year, y=jobs, group = county, color = county,
                                          text = paste0("County: ", county,
                                                        "<br>Year: ", year,
                                                        "<br>Number of Jobs: ", jobs))) +
                 geom_line(size = 1) +
                 geom_point(size = 1.5) +
                 scale_colour_manual(name = "County", values = viridis(12, option = "D")) +
                 scale_x_continuous(breaks = 0:2100) +
                 theme_minimal() + ggtitle("Number of jobs flowing into Wasco County (2015-2017)") +
                 ylab("Number of Jobs") + xlab("Year"), tooltip = "text") %>%
        config(displayModeBar = "static", displaylogo = FALSE,
               modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                           "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

    }
    else if (input$flows == "Outflows"){
      top_12_out_melt <- melt(data = top_12_out, id.vars = c("year"),
                              measure.vars = colnames(top_12_out)[-length(top_12_out)]) %>%
        rename(c("county" = "variable", "jobs" = "value"))

      ggplotly(ggplot(top_12_out_melt, aes(x=year, y=jobs, group = county, color = county,
                                           text = paste0("County: ", county,
                                                         "<br>Year: ", year,
                                                         "<br>Number of Jobs: ", jobs))) +
                 geom_line(size = 1) +
                 geom_point(size = 1.5) +
                 scale_colour_manual(name = "County", values = viridis(12, option = "D")) +
                 scale_x_continuous(breaks = 0:2100) +
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
          title = "Number of All Jobs<br>by Census Tract<br>in Wasco County",
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
          title = "Number of Goods Producing<br> Jobs by Census Tract<br>in Wasco County",
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
          title = "Number of Trade, Transportation,<br>and Utilities Jobs<br>by Census Tract in<br>
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
          title = "Number of All Other Services<br>Sector Jobs by Census Tract<br>in Wasco County",
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

## SERVER: PANEL - Affordable housing -----
## plotlyOutput("housingplot") ------

  output$housingplot <- renderPlotly({
    ggplotly(ggplot(acs_counties, aes(x=year, y=affordable_housing_all_perc, group = NAME, color = south_wasco,
                                      text = paste0("Region: ", NAME,
                                                    "<br>Year: ", year,
                                                    "<br>Affordable Housing: ", round(affordable_housing_all_perc, digits = 1), "%",
                                                    "<br>Margin of Error: ", round(affordable_housing_all_perc_moe, digits = 1), "%"))) +
               geom_line(size = 1) +
               geom_point(size = 1.5) +
               scale_colour_manual(name = "Region", values = c(graypal, viridis(3, option = "D")))  +
               theme_minimal() + ggtitle("Affordable Housing 2015-2018",subtitle = "Occupied households where monthly costs are less than 30% of houshold income") +
               ylab("Affordable Housing") + xlab("Year"), tooltip = "text") %>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

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
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Children in Nonfamily Household",
          fillColor = ~fam_stab_2018_pal(family_children_nonfamily_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2018$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2018$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2018)$family_children_nonfamily_perc_moe,
                                      1), "%<br/>",
                                "<strong> Percent Children in Nonfamily Household: <strong>",
                                round(family_children_nonfamily_perc, 1), "<strong>%"),
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
          pal = fam_stab_2018_pal,
          values = ~ c(0, fam_stab_max_perc_2018),
          title = "% of Parents with selected Family Stability<br>Indicator by Census Tract (2018)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers",
                         "% of Children in Nonfamily Household"),
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
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Children in Nonfamily Household",
          fillColor = ~fam_stab_2017_pal(family_children_nonfamily_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2017$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2017$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2017)$family_children_nonfamily_perc_moe,
                                      1), "%<br/>",
                                "<strong> Percent Children in Nonfamily Household: <strong>",
                                round(family_children_nonfamily_perc, 1), "<strong>%"),
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
          pal = fam_stab_2017_pal,
          values = ~ c(0, fam_stab_max_perc_2017),
          title = "% of Parents with selected Family Stability<br>Indicator by Census Tract (2017)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers",
                         "% of Children in Nonfamily Household"),
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
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Children in Nonfamily Household",
          fillColor = ~fam_stab_2016_pal(family_children_nonfamily_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2016$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2016$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2016)$family_children_nonfamily_perc_moe,
                                      1), "%<br/>",
                                "<strong> Percent Children in Nonfamily Household: <strong>",
                                round(family_children_nonfamily_perc, 1), "<strong>%"),
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
          pal = fam_stab_2016_pal,
          values = ~ c(0, fam_stab_max_perc_2016),
          title = "% of Parents with selected Family Stability<br>Indicator by Census Tract (2016)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers",
                         "% of Children in Nonfamily Household"),
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
        addPolygons(
          weight = 1,
          opacity = 0,
          fillOpacity = .7,
          group = "% of Children in Nonfamily Household",
          fillColor = ~fam_stab_2015_pal(family_children_nonfamily_perc),
          label = ~lapply(paste(sep = "",
                                substr(fam_stab_2015$NAME, 20, 60), "<br/>",
                                substr(fam_stab_2015$NAME, 1, 17),
                                "<br/>Margins of error: ",
                                round(filter(fam_stab_moe,
                                             year == 2015)$family_children_nonfamily_perc_moe,
                                      1), "%<br/>",
                                "<strong> Percent Children in Nonfamily Household: <strong>",
                                round(family_children_nonfamily_perc, 1), "<strong>%"),
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
          pal = fam_stab_2015_pal,
          values = ~ c(0, fam_stab_max_perc_2015),
          title = "% of Parents with selected Family Stability<br>Indicator by Census Tract (2015)",
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("% of Parents who are Married", "% of Single Fathers", "% of Single Mothers",
                         "% of Children in Nonfamily Household"),
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
                                      "family_children_nonfamily_perc" ="Living with Nonfamily"))
    #grouped bar chart for family type
    ggplotly(ggplot(filter(family_table, year == input$familyyears), aes(x = NAME, y = value, fill = variable,
                                                            text = paste0("Region: ", NAME,
                                                                          "<br>Year: ", year,
                                                                          "<br>Percent of Children: ", round(value, digits = 1), "%",
                                                                          "<br>Margin of Error: ", round(moe, digits = 1), "%"))) +
               geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
               scale_fill_manual(values = viridis(4, option="D"), name="Family Type")  +
               ylab("% of children")+xlab("") + coord_flip()+ theme_minimal() +
               ggtitle(paste0("Family Structure for Children Under 18 <br>", input$familyyears)), tooltip = "text")%>%
      config(displayModeBar = "static", displaylogo = FALSE,
             modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d","hoverClosestCartesian",
                                         "hoverCompareCartesian","resetScale2d"))
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
    else if (input$incomedisyear == "2017"){
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
    else if (input$incomedisyear == "2016"){
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
    else if (input$incomedisyear == "2015"){
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


} # end of BUILDING SERVER



shinyApp(ui, server)
