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

# DATA: Sourcing theme, shp files ------
source("theme.R")
source("loadbaselayers.R")
source("loadoverlays.R")
options(tigris_use_cache = TRUE)

# DATA: Loading data -----
acs_counties <- readRDS("Data/acs_counties.RDS")
#get tract level geography
acs_tracts <- readRDS("Data/acs_tracts.RDS")
## Loading in LODES
top_10_in <- read_csv("Data/app_10_inflows_wasco.csv")
top_10_out <- read_csv("Data/app_10_outflows_wasco.csv")
agg_17 <- readRDS("Data/app_lodes_od_agg_2017.Rds")
agg_16 <- readRDS("Data/app_lodes_od_agg_2016.Rds")
agg_15 <- readRDS("Data/app_lodes_od_agg_2015.Rds")
#wasco_points <- blocks("OR", county = "Wasco")
#wasco_lines <- data.frame(wasco_points)
south_wasco_points <- st_read("Data/shps/swsd")

# Color palettes -----
dspgpal = c("#232D4B", "#2C4F6B", "#0E879C", "#60999A", "#D1E0BF",
            "#D9E12B", "#E6CE3A", "#E6A01D", "#E57200", "#ADB5BD")
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
        icon = icon("chart-pie")
        ),
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
        ),
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
                    c('' = "Choose",
                      "How accessible is healthy and affordable food in South Wasco?" = "Foodmap",
                      "What is the food insecurity rate in South Wasco?" = "Insecurity",
                      "How many students in South Wasco are ellgible for Free and Reduced Price Lunch?" = "Lunch",
                      "What local crops are grown in South Wasco?" = "Crops"),
                    width = "300px"
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
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      #Indicator table snippet,
                      #DTOutput("insecurityDT")
                    )
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
                      #Indicator table snippet,
                      #DTOutput("lunchDT")
                    )
                  )),
## UI: PANEL - Local crops panel -----
                conditionalPanel(
                  condition = "input.foodselect == 'Crops'",
                  flipBox(
                    id = 4,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What local crops are grown in South Wasco?",
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      #Indicator table snippet,
                      #DTOutput("cropDT")
                    )
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
                  c("" = "Choose",
                    "What are the wind and solar projects in South Wasco?" = "WindSolar",
                    "What is access to broadband like in South Wasco?" = "Broadband",
                  "What is access to water like in South Wasco?" = "Water",
                  "What is public transit like in South Wasco?" = "Transit"),
                  width = "300px"
                )),
## UI: PANEL - Wind and solar -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'WindSolar'",
                  boxPlus(
                    title = "What are the wind and solar projects in South Wasco?"
                    #img of wind solar infocard
                  )),
## UI: PANEL - Broadband -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Broadband'",
                  boxPlus(
                  title = "What is access to broadband like in South Wasco?"
                  #img of broadband infocard
                  )),
## UI: PANEL - Water -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Water'",
                  flipBox(
                    id = 5,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is access to water like in South Wasco?",
                    back_title = "Data",
                    "",
                    #plotlyOutput("waterplot)
                    back_content = tagList(
                      #Indicator table snippet,
                      #DTOutput("infrastructureDT")
                    ))),
## UI: PANEL - Transit -------
                conditionalPanel(
                  condition = "input.infrastructureselect == 'Transit'",
                  boxPlus(
                  title = "What is public transit like in South Wasco?"
                  # img of transit infocard
                ))), # END OF INFRASTRUCTURE

## UI: TAB -  Learn and earn driver -----------
        tabItem(tabName = "learnearn",
                fluidRow(
                  selectInput(
                    inputId = "learnearnselect",
                    label = "Select domain",
                    c(Choose = '',
                      "Education" = "Education",
                      "Employment" = "Employment"),
                    width = "300px"
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
                      #Education indicator snippet
                      #DTOutput("educationDT")
                    )
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
                      c(Choose = '',
                        "How do workers flow in and out of South Wasco?" = "Flows",
                        "What types of jobs are in South Wasco?" = "Sectors"),
                      width = "300px"
                    ))),
# UI: PANEL - Job Flows  -------
                conditionalPanel(
                  condition = "input.employmentselect == 'Flows'",
                  flipBox(
                    id = 7,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "How do workers flow in and out of South Wasco?",
                    selectInput("flows", "Inflows or Outflows?",
                                c("Inflows", "Outflows")),
                    plotOutput("flows"),
                    back_title = "Flows Data",
                    # Full back with table and indicator snippet
                    "",
                    back_content = tagList(
                      fluidRow(
                        "Flows data comes from LODES xyz",
                        DTOutput("flowsDT"))))),
## UI: PANEL - Industry Sectors  ------
                conditionalPanel(
                  condition = "input.employmentselect == 'Sectors'",
                  flipBox(
                    id = 8,
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
                      fluidRow(
                        "Sectors data comes from LODES xyz",
                        DTOutput("sectorsDT")
                      ))

                  )
                )
        ), ## END OF INFRASTRUCTURE TAB

## UI: TAB - Quality standard of living driver -----------
        tabItem(tabName = "living",
                fluidRow(
                  boxPlus(
                    width = 800,
                    enable_label = TRUE,
                    label_text = 1,
                    label_status = "danger",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Quality Standard of Living",
                    # Remove dropdown button
                    # Select domain, then select question
                      selectInput(
                        inputId = "livingdomainselect",
                        label = "Select domain",
                        c("Financial" = "Financial",
                          "Housing" = "Housing",
                          "Social" = "Social"),
                        width = "300px"
                      ))),
# Financial select question
                conditionalPanel(
                  condition = "input.livingdomainselect == 'Financial'",
                  fluidRow(
                    selectInput(
                      inputId = "financialselect",
                      label = "I'm wondering...",
                      c(Choose = '',
                        "What is the median income in South Wasco?" = "MedIncome",
                        "What is the poverty rate in South Wasco?" = "Poverty",
                        "What is the income distribution in South Wasco" = "DisIncome"),
                      width = "300px"
                    ))),
# Housing select question
                conditionalPanel(
                  condition = "input.livingdomainselect == 'Housing'",
                  fluidRow(
                    selectInput(
                      inputId = "housingselect",
                      label = "I'm wondering...",
                      c(Choose = '',
                        "How much affordable housing is in South Wasco?" = "Housing",
                        "What is the home ownership rate in South Wasco?" = "RentOwn"),
                      width = "300px"
                    ))),
# Social select question
                conditionalPanel(
                  condition = "input.socialdomainselect == 'Social'",
                  fluidRow(
                    selectInput(
                      inputId = "socialselect",
                      label = "I'm wondering...",
                      c(Choose = '',
                        "What is the racial diversity of South Wasco?" = "Race",
                        "What types of familiy structures are in South Wasco?" = "Family",
                        "What is the highest educational achievement of people in South Wasco?" = "Education"),
                      width = "300px"
                    ))),
## UI: PANEL - Median income  ------
                    conditionalPanel(
                      condition = "input.financialselect == 'MedIncome'",
                      flipBox(
                        id = 9,
                        main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                        header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                        front_title = "What is the median income in South Wasco?",
                        # Median income only here, poverty, income brackets are the questions
                        plotlyOutput(outputId = "financials"),
                        back_title = "Data",
                        "",
                        back_content = tagList(
                          # Indicator table snippet
                          # DTOutput("acs_counties")
                        )
                      )),
## UI: PANEL - Poverty rate ------
                conditionalPanel(
                  condition = "input.financialselect == 'Poverty'",
                  flipBox(
                    id = 10,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the poverty rate in South Wasco?",
                    #plotlyOutput(outputId = "povertyplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    )
                  )),
## UI: PANEL - Income Distribution  ------
                conditionalPanel(
                  condition = "input.financialselect == 'DisIncome'",
                  flipBox(
                    id = 11,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the income distribution in South Wasco?",
                    # Median income only here, poverty, income brackets are the questions
                    plotlyOutput(outputId = "DisIncome"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    )
                  )),
## UI: PANEL - Affordable housing -----
                  conditionalPanel(
                    condition = "input.housingselect == 'Housing'",
                    flipBox(
                      id = 12,
                      main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                      header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                      front_title = "How much affordable housing is in South Wasco?",
                      # Overall and ownership/rental (both lines and maps?)
                      # Full back with table and indicator snippet
                      plotlyOutput("housing"),
                      back_title = "Data",
                      "",
                      back_content = tagList(
                        # Indicator table snippet
                        # DTOutput("acs_counties")
                      )
                    )
                ),
## UI: PANEL - Rent vs own -------
                conditionalPanel(
                  condition = "input.housingselect == 'RentOwn'",
                  flipBox(
                    id = 13,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the home ownership rate in South Wasco?",
                    # Overall and ownership/rental (both lines and maps?)
                    # Full back with table and indicator snippet
                    #plotlyOutput("rentownplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    )
                  )
                ),
# UI: PANEL - Race  --------
                conditionalPanel(
                  condition = "input.socialselect == 'Race'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  flipBox(
                    id = 14,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the racial diversity of South Wasco?",
                    selectInput("raceyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                    #plotlyOutput("raceplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    )
                  )),
# UI: PANEL - Family ------
                conditionalPanel(
                  condition = "input.socialselect == 'Family'",
                  # Racial diversity, family stability, educational attainment as questions
                  # We are unsure about mapping vs bar charts
                  # We might need a select for time
                  # Full back with table and indicator snippet
                  flipBox(
                    id = 15,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What types of familiy structures are in South Wasco?",
                    selectInput("familyyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                    #plotlyOutput("familyplot"),
                    back_title = "Data",
                    "",
                    back_content = tagList(
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    )
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
                    id = 16,
                    main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                    header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                    front_title = "What is the highest educational achievement of people in South Wasco?",
                    selectInput("degreeyears", "What year?",
                                c("2015", "2016", "2017", "2018")),
                    #plotlyOutput("degreeplot"),
                    back_title = "",
                    "",
                    back_content = tagList(
                      # Indicator table snippet
                      # DTOutput("acs_counties")
                    )
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

## SERVER: INDICATOR TABLES -------
## SERVER: DATA TABLES -----
## SERVER: DATA TABLE - Food systems map -----
  output$foodDT <- renderDT({
    datatable(datatable(food_points,
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
                          ))))
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
    isochronepal <- colorFactor("Blues", domain = c("1800","3600"))

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
      addPolygons(data = townships, color = "blue", opacity = .4, weight = 1, popup = ~htmlEscape(NAME), group = "Basemap") %>%
      addPolygons(data = unincorporated, color = "blue", opacity = .4, weight = 1, popup = ~htmlEscape(NAME), group = "Basemap") %>%
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
      addLegend(data = isochrones, position = "bottomleft", pal = isochronepal, values = ~value, labels = c("30 minutes", "1 hour"),
                group = "isochrones", title = "driving time")

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
## SERVER: PANEL - Local crops ----

## SERVER: TAB - Infrastructure cluster ----
## SERVER: PANEL - Water ----

## SERVER: TAB - Learn and earn driver ----
## SERVER: PANEL - Education composite ----
## SERVER: PANEL - Job flows ----

  output$flows <- renderPlot({
    if (input$flows == "Inflows"){
      ggplot(top_10_in, aes(x = year)) +
        ggtitle("Number of jobs flowing into Wasco County\nfrom other counties in Oregon from\n2015-2017") +
        labs(x = "Year", y = "Number of Jobs", colour = "County") +
        geom_line(aes(y = `Hood River County, OR`, color = "Hood River County, OR")) +
        geom_line(aes(y = `Multnomah County, OR`, color = "Multnomah County, OR")) +
        geom_line(aes(y = `Clackamas County, OR`, color = "Clackamas County, OR")) +
        geom_line(aes(y = `Marion County, OR`, color = "Marion County, OR")) +
        geom_line(aes(y = `Washington County, OR`, color = "Washington County, OR")) +
        geom_line(aes(y = `Deschutes County, OR`, color = "Deschutes County, OR")) +
        geom_line(aes(y = `Jefferson County, OR`, color = "Jefferson County, OR")) +
        geom_line(aes(y = `Lane County, OR`, color = "Lane County, OR")) +
        geom_line(aes(y = `Umatilla County, OR`, color = "Umatilla County, OR")) +
        geom_line(aes(y = `Sherman County, OR`, color = "Sherman County, OR"))

    }
    else if (input$flows == "Outflows"){
      ggplot(top_10_out, aes(x = year)) +
        ggtitle("Number of jobs flowing from Wasco County\ninto other counties in Oregon from\n2015-2017") +
        labs(x = "Year", y = "Number of Jobs", colour = "County") +
        geom_line(aes(y = `Hood River County, OR`, color = "Hood River County, OR")) +
        geom_line(aes(y = `Multnomah County, OR`, color = "Multnomah County, OR")) +
        geom_line(aes(y = `Clackamas County, OR`, color = "Clackamas County, OR")) +
        geom_line(aes(y = `Deschutes County, OR`, color = "Deschutes County, OR")) +
        geom_line(aes(y = `Washington County, OR`, color = "Washington County, OR")) +
        geom_line(aes(y = `Marion County, OR`, color = "Marion County, OR")) +
        geom_line(aes(y = `Jefferson County, OR`, color = "Jefferson County, OR")) +
        geom_line(aes(y = `Umatilla County, OR`, color = "Umatilla County, OR")) +
        geom_line(aes(y = `Lane County, OR`, color = "Lane County, OR")) +
        geom_line(aes(y = `Sherman County, OR`, color = "Sherman County, OR"))
    }
  })

## SERVER: PANEL - Industry Sectors ----

  ### Quality standard of living output ----
  #### Financials -------
  # Med income ----
  output$financials <- renderPlotly({
    if (input$financial == "Median Household Income") {
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
      }
    ## Poverty rate -----
    else if (input$financial == "Poverty Rate") {
      ggplotly(ggplot(filter(acs_counties, year == input$year), aes(x = NAME, y = below_poverty,
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
    }
  })

  ## Housing ----

  output$housing <- renderPlotly({p <- ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"),
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
      }
    )


  output$employment <- renderPlotly({ggplotly(ggplot(acs_counties %>% mutate(south_wasco = fct_other(NAME, keep = c("South Wasco County School District 1, Oregon", "Wasco County, Oregon", "Oregon"),
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
      }
    )

  ## Flows plots -------


  ## Owen Leaflets --------
  #S000 (all jobs) by year -------
  qtileS000 <- colorQuantile(c('#D1E0BF', '#E57200'), agg_17$S000, 5)
  od_S000leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 1,
      opacity = 1,
      group = "Basemap")

  qtileS000 <- colorQuantile(c('#D1E0BF', '#E57200'), agg_17$S000, 5)
  od_SI01leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 1,
      opacity = 1,
      group = "Basemap")

  qtileS000 <- colorQuantile(c('#D1E0BF', '#E57200'), agg_17$S000, 5)
  od_SI02leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 1,
      opacity = 1,
      group = "Basemap")

  qtileS000 <- colorQuantile(c('#D1E0BF', '#E57200'), agg_17$S000, 5)
  od_SI03leaf <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = south_wasco_points,
      color = "purple",
      weight = 1,
      opacity = 1,
      group = "Basemap")

  output$odleaf <- renderLeaflet({
    if (input$sect == "All"){
      od_S000leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2017",
          fillColor = ~qtileS000(agg_17$S000),
          label = agg_17$S000) %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2016",
          fillColor = ~qtileS000(agg_16$S000),
          label = agg_16$S000) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2015",
          fillColor = ~qtileS000(agg_15$S000),
          label = agg_15$S000) %>%
        addLegend(
          data = agg_17,
          "bottomright",
          pal = qtileS000,
          values = ~ S000,
          title = "Wasco County All Job Density",
          opacity = 1,
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("South Wasco School District"),
          overlayGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))}
    #SI01 (Goods Producing industry sectors) by year -------
    else if (input$sect == "Goods"){
      #output$od_SI01leaf <- renderLeaflet({
      od_SI01leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2017",
          fillColor = ~qtileS000(agg_17$SI01),
          label = agg_17$SI01) %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2016",
          fillColor = ~qtileS000(agg_16$SI01),
          label = agg_16$SI01) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2015",
          fillColor = ~qtileS000(agg_15$SI01),
          label = agg_15$SI01) %>%
        addLegend(
          data = agg_17,
          "bottomright",
          pal = qtileS000,
          values = ~ S000,
          title = "Goods Producing Industry\nJob Density",
          opacity = 1,
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("South Wasco School District"),
          overlayGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))}
    #SI02 (Trade, Transportation, and Utilities industry sectors) by year --------
    else if (input$sect == "Trade"){
      # output$od_SI02leaf <- renderLeaflet({
      od_SI02leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2017",
          fillColor = ~qtileS000(agg_17$SI02),
          label = agg_17$SI02) %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2016",
          fillColor = ~qtileS000(agg_16$SI02),
          label = agg_16$SI02) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2015",
          fillColor = ~qtileS000(agg_15$SI02),
          label = agg_15$SI02) %>%
        addLegend(
          data = agg_17,
          "bottomright",
          pal = qtileS000,
          values = ~ S000,
          title = "Trade, Transportation,\nand Utilities Industry\nJob Density",
          opacity = 1,
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("South Wasco School District"),
          overlayGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))}
    #SI03 (All Other Services industry sectors) by year ----------
    else if (input$sect == "AllOther"){
      # output$od_SI03leaf <- renderLeaflet({
      od_SI03leaf %>%
        addPolygons(
          data = st_as_sf(agg_17),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2017",
          fillColor = ~qtileS000(agg_17$SI03),
          label = agg_17$SI03) %>%
        addPolygons(
          data = st_as_sf(agg_16),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2016",
          fillColor = ~qtileS000(agg_16$SI03),
          label = agg_16$SI03) %>%
        addPolygons(
          data = st_as_sf(agg_15),
          weight = 1,
          opacity = 0,
          fillOpacity = 1,
          group = "2015",
          fillColor = ~qtileS000(agg_15$SI03),
          label = agg_15$SI03) %>%
        addLegend(
          data = agg_17,
          "bottomright",
          pal = qtileS000,
          values = ~ S000,
          title = "All Other Services Industry\nJob Density",
          opacity = 1,
          na.label = "NA") %>%
        addLayersControl(
          baseGroups = c("South Wasco School District"),
          overlayGroups = c("2017", "2016", "2015"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("2016", "2015"))}
  }) # end of odleaf



} # end of BUILDING SERVER



shinyApp(ui, server)
