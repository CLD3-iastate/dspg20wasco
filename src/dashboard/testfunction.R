tabItemOverview <- function() {
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
          ))
}

tabItemFood <- function() {
  tabItem(tabName = "food",
          fluidRow(
            selectInput(
              inputId = "foodselect",
              label = "I'm wondering...",
              choices = list(
                "How accessible is healthy and affordable food in South Wasco?" = "Foodmap",
                "What is the food insecurity rate in South Wasco?" = "Insecurity",
                "How many students in South Wasco are ellgible for Free and Reduced Price Lunch?" = "Lunch",
                "What local crops are grown in South Wasco?" = "Crops"
              ),
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
  )
}


sidebar <- function() {
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
      )))
}