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


acres_17 <- readRDS("~/git/dspg20wasco/data/app_acres_17.Rds")
acres_16 <- readRDS("~/git/dspg20wasco/data/app_acres_16.Rds")
acres_15 <- readRDS("~/git/dspg20wasco/data/app_acres_15.Rds")
south_wasco_points <- st_read("~/git/dspg20wasco/data/shps/swsd")

#winter wheat
colors_ww <- colorQuantile(viridis_pal(option = "D")(3),
                           domain = rbind(acres_17[acres_17$desc == "Winter Wheat", ],
                                          acres_16[acres_16$desc == "Winter Wheat", ],
                                          acres_15[acres_15$desc == "Winter Wheat", ])$acres)

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
              fillColor = ~ colors_ww(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_17[acres_17$desc == "Winter Wheat", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_17[acres_17$desc == "Winter Wheat", ]$acres), 0),
                              htmltools::HTML)) %>%
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
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_16[acres_16$desc == "Winter Wheat", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_16[acres_16$desc == "Winter Wheat", ]$acres), 0),
                              htmltools::HTML)) %>%
  addPolygons(data = acres_15[acres_15$desc == "Winter Wheat", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colors_ww(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_15[acres_15$desc == "Winter Wheat", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_15[acres_15$desc == "Winter Wheat", ]$acres), 0),
                              htmltools::HTML)) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))


#Barley
colors_bar <- colorQuantile(viridis_pal(option = "D")(3),
                            domain = rbind(acres_17[acres_17$desc == "Barley", ],
                                           acres_16[acres_16$desc == "Barley", ],
                                           acres_15[acres_15$desc == "Barley", ])$acres)
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
              fillColor = ~ colors_bar(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_17[acres_17$desc == "Barley", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_17[acres_17$desc == "Barley", ]$acres), 0),
                              htmltools::HTML)) %>%
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
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_16[acres_16$desc == "Barley", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_16[acres_16$desc == "Barley", ]$acres), 0),
                              htmltools::HTML)) %>%
  addPolygons(data = acres_15[acres_15$desc == "Barley", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colors_bar(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_15[acres_15$desc == "Barley", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_15[acres_15$desc == "Barley", ]$acres), 0),
                              htmltools::HTML)) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))

#Alfalfa
colors_alf <- colorQuantile(viridis_pal(option = "D")(3),
                            domain = rbind(acres_17[acres_17$desc == "Alfalfa", ],
                                           acres_16[acres_16$desc == "Alfalfa", ],
                                           acres_15[acres_15$desc == "Alfalfa", ])$acres)
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
              fillColor = ~ colors_alf(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_17[acres_17$desc == "Alfalfa", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_17[acres_17$desc == "Alfalfa", ]$acres), 0),
                              htmltools::HTML)) %>%
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
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_16[acres_16$desc == "Alfalfa", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_16[acres_16$desc == "Alfalfa", ]$acres), 0),
                              htmltools::HTML)) %>%
  addPolygons(data = acres_15[acres_15$desc == "Alfalfa", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colors_alf(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_15[acres_15$desc == "Alfalfa", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_15[acres_15$desc == "Alfalfa", ]$acres), 0),
                              htmltools::HTML)) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))

#Cherries
colors_cher <- colorQuantile(viridis_pal(option = "D")(3),
                             domain = rbind(acres_17[acres_17$desc == "Cherries", ],
                                            acres_16[acres_16$desc == "Cherries", ],
                                            acres_15[acres_15$desc == "Cherries", ])$acres)
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
              fillColor = ~ colors_cher(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_17[acres_17$desc == "Cherries", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_17[acres_17$desc == "Cherries", ]$acres), 0),
                              htmltools::HTML)) %>%
  addLegend(
    data = rbind(acres_17[acres_17$desc == "Cherries", ],
                 acres_16[acres_16$desc == "Cherries", ],
                 acres_15[acres_15$desc == "Cherries", ]),
    "bottomright",
    pal = colors_cher,
    values = ~acres,
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
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_16[acres_16$desc == "Cherries", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_16[acres_16$desc == "Cherries", ]$acres), 0),
                              htmltools::HTML)) %>%
  addPolygons(data = acres_15[acres_15$desc == "Cherries", ],
              weight = .3,
              opacity = 1,
              fillOpacity = .7,
              group = "2015",
              fillColor = ~ colors_cher(acres),
              label = ~lapply(paste(sep = "", "Census Tract: ",
                                    acres_15[acres_15$desc == "Cherries", ]$TRACTCE, "<br/>",
                                    "<strong> Number of Acres: <strong>",
                                    round(acres_15[acres_15$desc == "Cherries", ]$acres), 0),
                              htmltools::HTML)) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE))

cdl_cherries
