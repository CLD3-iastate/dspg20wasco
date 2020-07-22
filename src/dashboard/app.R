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


source("theme.R")
source(here("/src/dashboard/loadbaselayers.R"))
source(here("/src/dashboard/loadoverlays.R"))
acs_data <- fread(here("/data/acs/combined_acs.csv"))


### Set leaflet palettes
foodpal <- colorFactor("Set1", domain = allfood$type)

ui <- dashboardPagePlus(
  dashboardHeaderPlus(
    title = "DSPG 2020 Wasco EM",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        tabName = "map",
        text = "Interactive Map",
        icon = icon("map-marked-alt")
      ),
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
      menuItem(
        tabName = "findings",
        text = "Findings",
        icon = icon("chart-pie")
      ),
      menuItem(
        tabName = "team",
        text = "Team",
        icon = icon("user-friends")
      ),
      
      # menuItem(tabName = "widgets", "Widgets",
      #          icon = icon("th"),
      #          badgeLabel = "new",
      #          badgeColor = "green"),
      #menuItem("Controls",
      #sliderInput("slider", "Number of observations:", 1, 100, 50)),
      #menuItem(selectInput("city", "Cities", c("Chicago", "Boston")))
      
      #menuItem(selectInput("year", "Year:",c(2015, 2016, 2017, 2018))),
      
      # radioButtons(
      #   inputId = "group",
      #   label = "",
      #   choices = c("Food Systems", "Infrastructure")
      # ),
      dropdownButton(
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
        
        circle = TRUE,
        status = "danger",
        icon = icon("leaf"),
        width = "300px"
      ),
      dropdownButton(
        #tags$h3("List of Indicators"),
        selectInput(
          inputId = 'financial',
          label = '',
          choices = c("Median Household Income", "Poverty Rate")
        ),
        
        circle = TRUE,
        status = "danger",
        icon = icon("dollar"),
        width = "300px"
      )
    )
  ),
  
  dashboardBody(
    customTheme,
    fluidPage(
      tabItems(
        tabItem(tabName = "overview",
                fluidRow(
                  boxPlus(
                    title = "Interactive Map",
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
                    "Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut.",
                    h2("Project Goals"),
                    "Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut.",
                    h2("Our Approach"),
                    "Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut."
                  )
                )),
        tabItem(tabName = "map",
                fluidRow(
                  boxPlus(
                    title = "Interactive Map",
                    closable = FALSE,
                    width = NULL,
                    enable_label = TRUE,
                    label_text = 1,
                    label_status = "danger",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    selectInput("year", "Year:", c(2015, 2016, 2017, 2018), width = "150px"),
                    leafletOutput("mymap")
                  )
                )),
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
                    div(
                      "Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut.
                      
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut."
                    ),
                    br(),
                    div("[Visualizations go about here.]"),
                    br(),
                    div(
                      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut.
                      
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex. Nulla convallis ante elit, at consequat ipsum ultricies ut. Aliquam erat volutpat. nderline. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in variu convallis ante elit, at consequat ipsum ultricies ut."
                    )
                    )
                    )),
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
                    p("[Photos go about here.]"),
                    h2("UVA SDAD Team Members"),
                    p("[Photos go about here.]"),
                    h2("Project Sponsors"),
                    p(
                      "[Photos, information, and/or links about your sponsor go about here. You may want to use materials that your sponsors have already shared with you about their institution or coordinate with your stakeholders to include pertinent information here.]"
                    ),
                    h2("Acknowledgements"),
                    p(
                      "[Optional: You can also include external collaborators in this section or a separate section.]"
                    )
                  )
                ))
                    )
        ))
  ,
  
  rightSidebar(
    background = "light",
    rightSidebarTabContent(
      id = 1,
      icon = "desktop",
      active = TRUE,
      title = "Tab 1",
      uiOutput("r2")
    ),
    rightSidebarTabContent(
      id = 2,
      title = "Tab 2",
      textInput("caption", "Caption", "Data Summary")
    ),
    rightSidebarTabContent(
      id = 3,
      icon = "paint-brush",
      title = "Tab 3",
      numericInput("obs", "Observations:", 10, min = 1, max = 100)
    ),
    title = "Right Sidebar"
  )
  )

server <- function(input, output) {
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
  observeEvent(input$tabs, {
    if (input$tabs == "map") {
      output$r2 <- renderUI("Map")
    } else {
      output$r2 <- renderUI("Not Map")
    }
  })
  
  
  output$mymap <- renderLeaflet({
    #lonlat <- geocode_OSM(input$city)
    #mylon <- as.double(lonlat$coords[1])
    #mylat <- as.double(lonlat$coords[2])
    
    leaflet() %>%
      addTiles() %>%
      setView(-121, 45.2, zoom = 8) %>%
      addPolylines(
        data = swsd,
        color = "purple",
        opacity = 1,
        group = "Basemap"
      ) %>%
      addPolylines(data = countyline,
                   color = "grey",
                   group = "Basemap") %>%
      addPolygons(
        data = townships,
        color = "blue",
        opacity = .4,
        weight = 1,
        popup = ~ htmlEscape(NAME),
        group = "Basemap"
      ) %>%
      addPolygons(
        data = unincorporated,
        color = "blue",
        opacity = .4,
        weight = 1,
        popup = ~ htmlEscape(NAME),
        group = "Basemap"
      ) %>%
      addPolylines(
        data = roads,
        color = "gray",
        weight = .75,
        group = "Basemap"
      ) %>%
      addCircleMarkers(
        data = allfood,
        color = ~ foodpal(type),
        fillOpacity = 1,
        radius = 3,
        stroke = FALSE,
        popup = ~ htmlEscape(Store_Name),
        group = "Food"
      ) %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Food"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        data = allfood,
        "bottomright",
        pal = foodpal,
        values = ~ type,
        title = "Food type",
        opacity = 1,
        group = "Food",
        na.label = "NA"
      ) %>%
      addLegend(
        data = countyline,
        "topright",
        colors = "grey",
        labels = "Wasco County",
        group = "Basemap"
      ) %>%
      addLegend(
        data = swsd,
        "topright",
        opacity = 1,
        colors = "purple",
        labels = "South Wasco County School District",
        group = "Basemap"
      ) %>%
      addLegend(
        data = unincorporated,
        "topright",
        opacity = 0.4,
        colors = "blue",
        labels = "Townships and Unincorporated Areas",
        group = "Basemap"
      )
    
  })
  
  #histograms
  output$plot1 <- renderPlot({
    acs <-
      filter(acs_data,
             year == input$year,
             NAME == "South Wasco" | NAME == "Wasco" | NAME == "Oregon")
    if (input$financial == "Median Household Income") {
      ggplot(acs, aes(x = NAME, y = median_household_income)) +
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
      ggplot(acs, aes(x = NAME, y = below_poverty)) +
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
    #   data <- histdata[seq_len(input$slider)]
    #   hist(data)
  })
}

shinyApp(ui, server)
