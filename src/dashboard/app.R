## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tmaptools)
library(sf)


#Check wds
allfood <- read_csv("~/git/DSPG2020/wasco/data/shps/allfood.csv")
allfood <- allfood %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
schools <- read.csv("~/git/DSPG2020/wasco/data/shps/allfood.csv")
schools <- schools %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
townships <- read.csv("~/git/DSPG2020/wasco/data/shps/allfood.csv")
townships <- townships %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
unincorporated <- read.csv("~/git/DSPG2020/wasco/data/shps/allfood.csv")
unincorporated <- unincorporated %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")
countyline <- read.csv("~/git/DSPG2020/wasco/data/shps/allfood.csv")
countyline <- countyline %>% st_as_sf(coords = c("X", "Y"), crs = 4326, agr  = "constant")

ui <- dashboardPagePlus(
  dashboardHeaderPlus(title = "DSPG 2020 Wasco EM",
                      enable_rightsidebar = TRUE,
                      rightSidebarIcon = "info"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green") #,
      #menuItem("Controls",
               #sliderInput("slider", "Number of observations:", 1, 100, 50)),
      #menuItem(selectInput("city", "Cities", c("Chicago", "Boston")))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    #fluidRow(
      #box(plotOutput("plot1", height = 250))
    #),
    #fluidRow(
      #box(textOutput("mycity"))
    #),
    fluidRow(
      # box(leafletOutput("mymap"), width = 10),
      boxPlus(
        title = "Drafting map",
        closable = FALSE,
        width = NULL,
        enable_label = TRUE,
        label_text = 1,
        label_status = "danger",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        leafletOutput("mymap")
      )
    )
  ),
  rightSidebar(background = "light",
               rightSidebarTabContent(
                 id = 1,
                 icon = "desktop",
                 active = TRUE,
                 title = "Tab 1",
                 sliderInput(
                   "obs",
                   "Number of observations:",
                   min = 0, max = 1000, value = 500
                 )
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
               ))
)

server <- function(input, output) {
  #set.seed(122)
  #histdata <- rnorm(500)


  #output$plot1 <- renderPlot({
    #data <- histdata[seq_len(input$slider)]
    #hist(data)
  #})

  #output$mycity <- renderText({
    #lonlat <- geocode_OSM(input$city)
    #lon <- lonlat$coords[1]
    #lat <- lonlat$coords[2]
    #paste(lon, lat)
  #})

  output$mymap <- renderLeaflet({
    #lonlat <- geocode_OSM(input$city)
    #mylon <- as.double(lonlat$coords[1])
    #mylat <- as.double(lonlat$coords[2])

    leaflet() %>%
      addTiles() %>%
      setView(-121, 45.2, zoom = 8) %>%
      addPolylines(data = swsd, color = "purple", opacity = 1, group = "Basemap") %>%
      addPolylines(data = countyline, color = "grey", group = "Basemap") %>%
      addPolylines(data = townships, color = "blue", opacity = 1, weight = 1, group = "Basemap") %>%
      addPolylines(data = unincorporated, color = "blue", opacity = 1, weight = 1, group = "Basemap") %>%
      addPolylines(data = small_streets$osm_lines,
                   color = "gray", weight = .5, group = "Basemap") %>%
      addPolylines(data = med_streets$osm_lines,
                   color = "black", weight = .75, group = "Basemap") %>%
      addPolylines(data = big_streets$osm_lines,
                   color = "black", weight = 1, group = "Basemap") %>%
      addCircleMarkers(data = allfood,
                       color = ~foodpal(type), fillOpacity = 1, radius = 3,
                       stroke = FALSE, group = "Food") %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Food"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(data = allfood, "bottomright", pal = foodpal, values = ~type,
                title = "Food type",
                opacity = 1, group = "Food", na.label = "NA")
  })
}

shinyApp(ui, server)
