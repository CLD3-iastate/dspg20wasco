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

agg_17 <- readRDS("~/git/dspg20wasco/data/app_lodes_od_agg_2017.Rds")
agg_16 <- readRDS("~/git/dspg20wasco/data/app_lodes_od_agg_2016.Rds")
agg_15 <- readRDS("~/git/dspg20wasco/data/app_lodes_od_agg_2015.Rds")

wasco_points <- (blocks("OR", county = "Wasco"))
wasco_lines <- data.frame(wasco_points)
south_wasco_points <- st_read("../data/shps/swsd")

#S000 (all jobs) by year
qtileS000 <- colorQuantile(c('#D1E0BF', '#E57200'), agg_17$S000, 5)
od_S000leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region")

od_S000leaf <- od_S000leaf %>%
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
    #    baseGroups = c("South Wasco School District"),
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))

#SI01 (Goods Producing industry sectors) by year
od_SI01leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region")

od_SI01leaf <- od_SI01leaf %>%
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
    #    baseGroups = c("South Wasco School District"),
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))

#SI02 (Trade, Transportation, and Utilities industry sectors) by year
od_SI02leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region")

od_SI02leaf <- od_SI02leaf %>%
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
    #    baseGroups = c("South Wasco School District"),
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))

#SI03 (All Other Services industry sectors) by year
od_SI03leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region")

od_SI03leaf <- od_SI03leaf %>%
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
    #    baseGroups = c("South Wasco School District"),
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))
