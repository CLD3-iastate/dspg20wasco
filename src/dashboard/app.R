## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tmaptools)

ui <- dashboardPagePlus(
  dashboardHeaderPlus(title = "Basic dashboard",
                      enable_rightsidebar = TRUE,
                      rightSidebarIcon = "info"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Controls",
               sliderInput("slider", "Number of observations:", 1, 100, 50)),
      menuItem(selectInput("city", "Cities", c("Chicago", "Boston")))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250))
    ),
    fluidRow(
      box(textOutput("mycity"))
    ),
    fluidRow(
      # box(leafletOutput("mymap"), width = 10),
      boxPlus(
        title = "Closable box, with label", 
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
  set.seed(122)
  histdata <- rnorm(500)

  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$mycity <- renderText({
    lonlat <- geocode_OSM(input$city)
    lon <- lonlat$coords[1]
    lat <- lonlat$coords[2]
    paste(lon, lat)
  }) 
  
  
  output$mymap <- renderLeaflet({
    lonlat <- geocode_OSM(input$city)
    mylon <- as.double(lonlat$coords[1])
    mylat <- as.double(lonlat$coords[2])
    
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng=mylon, lat=mylat , zoom=10)
    m
  }) 
}

shinyApp(ui, server)
