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
