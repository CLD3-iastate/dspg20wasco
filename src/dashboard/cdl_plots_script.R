library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(leaflet)
library(mapview)
library(reshape2)
library(raster)
library(tigris)
library(viridis)


acres_17 <- readRDS("../data/app_acres_17.Rds")
acres_16 <- readRDS("../data/app_acres_16.Rds")
acres_15 <- readRDS("../data/app_acres_15.Rds")
south_wasco_points <- st_read("../data/shps/swsd")

#winter wheat
cdl_ww <- leaflet() %>%
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
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_17[acres_17$desc == "Winter Wheat", ]$acres)(acres),
              label = acres_17[acres_17$desc == "Winter Wheat", ]$acres) %>%
  addLegend(
    data = acres_17[acres_17$desc == "Winter Wheat", ],
    "bottomright",
    pal = colorBin(viridis_pal(option = "D")(5), domain =
                     acres_17[acres_17$desc == "Winter Wheat", ]$acres),
    values = ~ acres,
    title = "Number of Acres",
    opacity = .7,
    na.label = "NA") %>%
  addPolygons(data = acres_16[acres_16$desc == "Winter Wheat", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2016",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_16[acres_16$desc == "Winter Wheat", ]$acres)(acres),
              label = acres_16[acres_16$desc == "Winter Wheat", ]$acres) %>%
  addPolygons(data = acres_15[acres_15$desc == "Winter Wheat", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_15[acres_15$desc == "Winter Wheat", ]$acres)(acres),
              label = acres_15[acres_15$desc == "Winter Wheat", ]$acres) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))

#barley
cdl_barley <- leaflet() %>%
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
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_17[acres_17$desc == "Barley", ]$acres)(acres),
              label = acres_17[acres_17$desc == "Barley", ]$acres) %>%
  addLegend(
    data = acres_17[acres_17$desc == "Barley", ],
    "bottomright",
    pal = colorBin(viridis_pal(option = "D")(5), domain =
                     acres_17[acres_17$desc == "Barley", ]$acres),
    values = ~ acres,
    title = "Number of Acres",
    opacity = .7,
    na.label = "NA") %>%
  addPolygons(data = acres_16[acres_16$desc == "Barley", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2016",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_16[acres_16$desc == "Barley", ]$acres)(acres),
              label = acres_16[acres_16$desc == "Barley", ]$acres) %>%
  addPolygons(data = acres_15[acres_15$desc == "Barley", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_15[acres_15$desc == "Barley", ]$acres)(acres),
              label = acres_15[acres_15$desc == "Barley", ]$acres) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))

#alfalfa
cdl_alfalfa <- leaflet() %>%
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
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_17[acres_17$desc == "Alfalfa", ]$acres)(acres),
              label = acres_17[acres_17$desc == "Alfalfa", ]$acres) %>%
  addLegend(
    data = acres_17[acres_17$desc == "Alfalfa", ],
    "bottomright",
    pal = colorBin(viridis_pal(option = "D")(5), domain =
                     acres_17[acres_17$desc == "Alfalfa", ]$acres),
    values = ~ acres,
    title = "Number of Acres",
    opacity = .7,
    na.label = "NA") %>%
  addPolygons(data = acres_16[acres_16$desc == "Alfalfa", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2016",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_16[acres_16$desc == "Alfalfa", ]$acres)(acres),
              label = acres_16[acres_16$desc == "Alfalfa", ]$acres) %>%
  addPolygons(data = acres_15[acres_15$desc == "Alfalfa", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_15[acres_15$desc == "Alfalfa", ]$acres)(acres),
              label = acres_15[acres_15$desc == "Alfalfa", ]$acres) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))

#cherries
cdl_cherries <- leaflet() %>%
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
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_17[acres_17$desc == "Cherries", ]$acres)(acres),
              label = acres_17[acres_17$desc == "Cherries", ]$acres) %>%
  addLegend(
    data = acres_17[acres_17$desc == "Cherries", ],
    "bottomright",
    pal = colorBin(viridis_pal(option = "D")(5), domain =
                     acres_17[acres_17$desc == "Cherries", ]$acres),
    values = ~ acres,
    title = "Number of Acres",
    opacity = .7,
    na.label = "NA") %>%
  addPolygons(data = acres_16[acres_16$desc == "Cherries", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2016",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_16[acres_16$desc == "Cherries", ]$acres)(acres),
              label = acres_16[acres_16$desc == "Cherries", ]$acres) %>%
  addPolygons(data = acres_15[acres_15$desc == "Cherries", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colorBin(viridis_pal(option = "D")(5), domain =
                                       acres_15[acres_15$desc == "Cherries", ]$acres)(acres),
              label = acres_15[acres_15$desc == "Cherries", ]$acres) %>%
  addLayersControl(
    #    baseGroups = c("South Wasco School District"),
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))
cdl_cherries
