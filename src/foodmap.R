## Code from Shiny app to draw the food systems visualization

## Reactive function to draw selected isochrones
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

## Drawing a custom legend to show both color for type of store and size for store SNAP and WIC acceptance (I am in the process of creating a more color-friendly color scheme and changing size to shape at the request of my team)
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, group = "Stores"){
    colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ",
                             sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
                             labels, "</div>")
    return(addLegend(map, "bottomright", colors = colorAdditions,
                     labels = labelAdditions, opacity = opacity, group = group))
  }

# Building the map, adding baselayers for South Wasco County School District, South Wasco county line, etc. Isochrones are drawn using fliteredData() function and Stores are overlayed.
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
    addPolygons(data = filteredData(), color = ~isochronepal(value),
                group = "isochrones") %>%
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
              group = "Basemap") %>%
    addLegend(data = isochrones, position = "bottomleft", pal = isochronepal, values = ~value, labels = c("30 minutes", "1 hour"),
              group = "isochrones", title = "driving time")

})

library(dplyr)
## Food insecurity leaflet -----
food_insecurity_counties <- read_csv("Data/food_insecurity_counties.csv")
food_insecurity_states <- read_csv("Data/food_insecurity_state.csv")
food_insecurity_counties$FIPS <- as.character(food_insecurity_counties$FIPS)

names(food_insecurity_counties) <- abbreviate(names(food_insecurity_counties))
food_insecurity_counties <- food_insecurity_counties %>% rename("GEOID" = "FIPS")

or_county_lines <- counties(state = "OR")
wa_county_lines <- counties(state = "WA")

county_lines <- rbind(or_county_lines, wa_county_lines)
neighbors <- c("Wasco", "Hood River", "Sherman", "Jefferson", "Skamania", "Klickitat")


data2014 <- food_insecurity_counties %>% filter(Year == "2014")
shp2014 <- geo_join(county_lines, data2014, by = c("GEOID"))
data2015 <- food_insecurity_counties %>% filter(Year == "2015")
shp2015 <- geo_join(county_lines, data2015, by = c("GEOID"))
data2016 <- food_insecurity_counties %>% filter(Year == "2016")
shp2016 <- geo_join(county_lines, data2016, by = c("GEOID"))
data2017 <- food_insecurity_counties %>% filter(Year == "2017")
shp2017 <- geo_join(county_lines, data2017, by = c("GEOID"))


shp2014 <- shp2014 %>% filter(NAME %in% neighbors)
shp2014 <- shp2014[!rownames(shp2014) == 5, ]
shp2015 <- shp2015 %>% filter(NAME %in% neighbors)
shp2015 <- shp2015[!rownames(shp2015) == 5, ]
shp2016 <- shp2016 %>% filter(NAME %in% neighbors)
shp2016 <- shp2016[!rownames(shp2016) == 5, ]
shp2017 <- shp2017 %>% filter(NAME %in% neighbors)
shp2017 <- shp2017[!rownames(shp2017) == 5, ]

food_counties_pal <- colorNumeric(viridis_pal(option = "D")(3), domain = food_insecurity_counties$FdIR)
food_counties_pal_c <- colorNumeric(viridis_pal(option = "D")(3), domain = food_insecurity_counties$Cfir)

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




