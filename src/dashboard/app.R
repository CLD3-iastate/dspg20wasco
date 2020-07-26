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

source("theme.R")
source(here("/src/dashboard/loadbaselayers.R"))
source(here("/src/dashboard/loadoverlays.R"))

#load acs data#
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


foodpal <- colorFactor("Set1", domain = food_points$shop)
isochronepal <- colorFactor("Blues", domain = isochrones$value)

## Building UI -------

ui <- dashboardPagePlus(

## Dashboard header -------
  dashboardHeaderPlus(
    title = "DSPG 2020 Wasco EM",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info"
  ),

## Dashboard sidebar --------
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

## Findings menu ------
      menuItem(
        tabName = "findings",
        text = "Findings",
        icon = icon("chart-pie"),

## Findings menu subitems (indicators) ---------
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
        )
      ),
      menuItem(
        tabName = "team",
        text = "Team",
        icon = icon("user-friends")
      ))), #,

      # menuItem(tabName = "widgets", "Widgets",
      #          icon = icon("th"),
      #          badgeLabel = "new",
      #          badgeColor = "green"),
      #menuItem("Controls",
      #sliderInput("slider", "Number of observations:", 1, 100, 50)),
      #menuItem(selectInput("city", "Cities", c("Chicago", "Boston")))

      # radioButtons(
      #   inputId = "group",
      #   label = "",
      #   choices = c("Food Systems", "Infrastructure")
      # ),

## Dashboard body -------
  dashboardBody(
    customTheme,
    fluidPage(
      tabItems(
## Overview tab --------
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
                    h2("Project Goals"),
                    p("The purpose of our project is to provide community decisionmakers with baseline datasets and comparative analyses to similar rural areas of the likely factors affecting economic mobility"),
                    h2("Project Scope"),
                    p("The term South Wasco is defined by the South Wasco County School District, which encompasses the southernmost region of Wasco County, Oregon.")
                  )
                )),

## Cluster food systems tab ---------
        tabItem(tabName = "food",
                fluidRow(
                  boxPlus(
                    title = "Interactive Food Systems Map",
                    closable = FALSE,
                    width = NULL,
                    enable_label = TRUE,
                    label_text = 1,
                    label_status = "danger",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
## Here is the select input functionality. I have fiddled with the different arguments for this but I think the issue is with the filteredData() function below
                    selectInput("iso", "Show isochrones for...",
                                choices = c(Choose='', isochrones$name),
                                selectize = TRUE,
                                multiple = FALSE,
                                width = "150px"),
                    leafletOutput("mymap")
                  )
                )),

## Driver quality standard of living tab -----------
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
                    dropdownButton(
                      tags$p("Choose a Domain to Explore"),
                      inputId = 'livingdomains',
                      label = '',
                      circle = TRUE,
                      status = "danger",
                      icon = icon("dharmachakra"),
                      selectInput(
                      inputId = "domainselect",
                      label = "",
                      choices = c("Financial", "Housing", "Health", "Social"),
                      width = "300px"
                    )),
                    #radioButtons(
                         #inputId = "group",
                      #   label = "",
                      #   choices = c("Food Systems", "Infrastructure")
                      # ),
                    selectInput("year", "Year:",c(2015, 2016, 2017, 2018)),
                    #dropdownButton(
                      #tags$h3("List of Indicators"),
                      selectInput(
                        inputId = 'food_system',
                        label = '',
                        choices = c(
                          "Food Insecurity Rate",
                          "Free and reduced-price Lunch",
                          "Food Access"
                        )
                      ),

                      #circle = TRUE,
                      #status = "danger",
                      #icon = icon("leaf"),
                      #width = "300px"
                    #),
                    #dropdownButton(
                      #tags$h3("List of Indicators"),
                      selectInput(
                        inputId = 'financial',
                        label = '',
                        choices = c("Median Household Income", "Poverty Rate")
                      ),

                      #circle = TRUE,
                      #status = "danger",
                      #icon = icon("dollar"),
                      #width = "300px"
                    #)
                  #)
                #),
                    plotOutput("plot1")
                  )
                )),

## Data tab ----------
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
                    p("Rural Clusters of Innovation from Berkshires Strategy Project. Visualizes community agencies and organizations that contribute to economic mobility increasing sectors. Tailored to specific communities, narrows focus on areas of sponsor interest."),
p("Boosting Upward Mobility from Urban Institute. Multidimensional approach to economic mobility. Includes ideas for relevant metrics at the local level."),
p("Weaving in Good Rural Data from Urban Institute"),
h2("Data Collection and Analysis"),
p("This is how we collected the data. Explore the right panel for more data information!")
                    )
                    )),

## Findings tab ---------
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

## Team tab
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
                    p("Mary Solomon, DSPG Graduate Fellow (M.S. Applied Statistics), Bowling Green State University"),
                    p("Joanna Schroeder, DSPG Intern, William & Mary"),
                    p("Owen Hart, DSPG Intern, University of California Berkeley"),
                    h2("UVA SDAD Team Members"),
                    p("Aaron Schroeder (PI), Research Associate Professor & Data Scientist (Ph.D. Public Policy)"),
                    p("Eric Oh, Research Assistant Professor of Statistics (Ph.D Biostatistics)"),
p("Alyssa Mikytuck, Postdoctoral Associate (Ph.D Human Development)"),
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
                ))))))
                    #)
        #))
  #,

## Right sidebar --------
  #rightSidebar(
    #background = "light",
    #rightSidebarTabContent(
      #id = 1,
      #icon = "desktop",
      #active = TRUE,
      #title = "Tab 1",
      #uiOutput("r2")
    #),
    #rightSidebarTabContent(
      #id = 2,
      #title = "Tab 2",
      #textInput("caption", "Caption", "Data Summary")
    #),
    #rightSidebarTabContent(
      #id = 3,
      #icon = "paint-brush",
      #title = "Tab 3",
      #numericInput("obs", "Observations:", 10, min = 1, max = 100)
    #),
    #title = "Right Sidebar"
  #)


## Building Server --------
server <- function(input, output, session) {
  # set.seed(122)
  # histdata <- rnorm(500)
  #
  #
  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  #
  # output$mycity <- renderText({
  #   lonlat <- geocode_OSM(input$city)
  #   lon <- lonlat$coords[1]
  #   lat <- lonlat$coords[2]
  #   paste(lon, lat)
  # })
  #observeEvent(input$tabs, {
    #if (input$tabs == "map") {
      #output$r2 <- renderUI("Map")
   # } else {
      #output$r2 <- renderUI("Not Map")
   # }
  #})

## Here is a reactive function filter the isochrone data by the selected input. I think the issue could be here because this function is not reacting to deselection.
  filteredData <- reactive({
    isochrones %>% filter(name %in% input$iso)
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
      addCircleMarkers(data = food_points,
                       color = ~foodpal(shop), fillOpacity = 1,
                       radius = ~radius,
                       stroke = FALSE,
                       popup = ~htmlEscape(name),
                       group = "Stores") %>%
      #addPolygons(data = isochrones, color = ~isochronepal(value),
      #group = "isochrones") %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Stores"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegendCustom(colors = c("red", "blue", "green", "gray", "gray", "gray"),
                      labels = c("convenience", "farm", "supermarket", "no acceptance",
                                 "snap", "snap and wic"),
                      sizes = c(10, 10, 10, 6, 10, 14)) %>%
      addLegend(data = countyline, "topright",
                colors = "grey", labels = "Wasco County", group = "Basemap") %>%
      addLegend(data = swsd, "topright", opacity = 1,
                colors = "purple", labels = "South Wasco County School District",
                group = "Basemap") %>%
      addLegend(data = unincorporated, "topright", opacity = 0.4,
                colors = "blue", labels = "Townships and Unincorporated Areas",
                group = "Basemap")
  })

## Here is the observe function for showing the isochrone for the filtered data from the above function
  observe({
    leafletProxy("mymap", data = filteredData()) %>%
      addPolygons(color = ~isochronepal(value))
  })

### Quality standard of living output ----
#### Financials -------
  #histograms
  output$plot1 <- renderPlot({
    if (input$financial == "Median Household Income") {
      ggplot(filter(acs_counties, year == input$year), aes(x = NAME, y = median_household_income)) +
        geom_col(fill = "dark blue") +
        geom_errorbar(
          aes(
            x = NAME,
            ymin = median_household_income - median_household_income_moe,
            ymax = median_household_income + median_household_income_moe
          ),
          color = "dark orange"
        ) +
        ylab("Median Household Income") + xlab("Region") +
        geom_point(color = "dark orange", size = 3) +
        ggtitle(paste("Median Household Income in", input$year, sep = " "))
    }
    else if (input$financial == "Poverty Rate") {
      ggplot(filter(acs_counties, year == input$year), aes(x = NAME, y = below_poverty)) +
        geom_col(fill = "dark blue") +
        geom_errorbar(
          aes(
            x = NAME,
            ymin = below_poverty - below_poverty_moe,
            ymax = below_poverty + below_poverty_moe
          ),
          color = "dark orange"
        ) +
        ylab("% of Population") + xlab("Region") +
        geom_point(color = "dark orange", size = 3) +
        ggtitle(paste("% of Population Below Poverty Line in", input$year, sep =
                        " "))
    }
    #### Maps### 
    # output$plot2 <- renderPlot({
    #   if (input$financial == "Median Household Income"){
    #     ggplot() +
    #       geom_sf(data = filter(acs_tracts, year == input$year), aes(fill = median_household_income)) +
    #       geom_sf(fill = "transparent", color = "gray20", size = 1, 
    #               data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
    #       labs(title = "Median Household Income by census track") 
    #   }
    # })
    
    # output$plot2 <- renderPlot({
    #   if (input$financial == "Poverty Rate"){
    #     ggplot() +
    #       geom_sf(data = filter(acs_tracts, year == input$year), aes(fill = median_household_income)) +
    #       geom_sf(fill = "transparent", color = "gray20", size = 1, 
    #               data = acs_tracts %>% group_by(COUNTYFP) %>% summarise()) +
    #       labs(title = "Poverty by census track") 
    #   }
    # })
    
    
    # Median Household Income tract map for 2018
   
    #   data <- histdata[seq_len(input$slider)]
    #   hist(data)
  })
}

shinyApp(ui, server)
