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

agg_17 <- read_sf("~/git/dspg20wasco/data/app_lodes_od_agg_2017.shp")
agg_16 <- read_sf("~/git/dspg20wasco/data/app_lodes_od_agg_2016.shp")
agg_15 <- read_sf("~/git/dspg20wasco/data/app_lodes_od_agg_2015.shp")

#2017 map
map2017leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 1,
    opacity = 1,
    group = "Basemap")

map2017leaf <- map2017leaf %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "All Jobs",
    fillColor = ~qtileS000(agg_17$S000),
    label = agg_17$S000)
map2017leaf <- map2017leaf %>%
  addLegend(
    data = agg_17,
    "bottomright",
    pal = qtileS000,
    values = ~ S000,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "All Jobs",
    na.label = "NA")

#SI01
map2017leaf <- map2017leaf %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "Goods Producing Sector",
    fillColor = ~qtileS000(agg_17$SI01),
    label = agg_17$SI01) %>%
  addLegend(
    data = agg_17,
    "bottomright",
    pal = qtileS000,
    values = ~ SI01,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "Goods Producing Sector",
    na.label = "NA")

#SI02
map2017leaf <- map2017leaf %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "Trade, Transportation,and Utilities Sector",
    fillColor = ~qtileS000(agg_17$SI02),
    label = agg_17$SI02) %>%
  addLegend(
    data = agg_17,
    "bottomright",
    pal = qtileS000,
    values = ~ SI02,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "Trade, Transportation,and Utilities Sector",
    na.label = "NA")

#SI03
map2017leaf <- map2017leaf %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "All Other Services Sector",
    fillColor = ~qtileS000(agg_17$SI03),
    label = agg_17$SI03) %>%
  addLegend(
    data = agg_17,
    "bottomright",
    pal = qtileS000,
    values = ~ SI03,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "All Other Services Sector",
    na.label = "NA")

map2017leaf <- map2017leaf %>%
  addLayersControl(
    baseGroups = c("South Wasco School District"),
    overlayGroups = c("All Jobs", "Goods Producing Sector", "Trade, Transportation,and Utilities Sector", "All Other Services Sector"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Goods Producing Sector", "Trade, Transportation,and Utilities Sector", "All Other Services Sector"))

#2016 map
map2016leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 1,
    opacity = 1,
    group = "Basemap")
map2016leaf <- map2016leaf %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "All Jobs",
    fillColor = ~qtileS000(agg_16$S000),
    label = agg_16$S000)
map2016leaf <- map2016leaf %>%
  addLegend(
    data = agg_16,
    "bottomright",
    pal = qtileS000,
    values = ~ S000,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "All Jobs",
    na.label = "NA")

#SI01
map2016leaf <- map2016leaf %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "Goods Producing Sector",
    fillColor = ~qtileS000(agg_16$SI01),
    label = agg_16$SI01) %>%
  addLegend(
    data = agg_16,
    "bottomright",
    pal = qtileS000,
    values = ~ SI01,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "Goods Producing Sector",
    na.label = "NA")

#SI02
map2016leaf <- map2016leaf %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "Trade, Transportation,and Utilities Sector",
    fillColor = ~qtileS000(agg_16$SI02),
    label = agg_16$SI02) %>%
  addLegend(
    data = agg_16,
    "bottomright",
    pal = qtileS000,
    values = ~ SI02,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "Trade, Transportation,and Utilities Sector",
    na.label = "NA")

#SI03
map2016leaf <- map2016leaf %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "All Other Services Sector",
    fillColor = ~qtileS000(agg_16$SI03),
    label = agg_16$SI03) %>%
  addLegend(
    data = agg_16,
    "bottomright",
    pal = qtileS000,
    values = ~ SI03,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "All Other Services Sector",
    na.label = "NA")

map2016leaf <- map2016leaf %>%
  addLayersControl(
    baseGroups = c("South Wasco School District"),
    overlayGroups = c("All Jobs", "Goods Producing Sector", "Trade, Transportation,and Utilities Sector", "All Other Services Sector"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Goods Producing Sector", "Trade, Transportation,and Utilities Sector", "All Other Services Sector"))

#2015 map
map2015leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 1,
    opacity = 1,
    group = "Basemap")

map2015leaf <- map2015leaf %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "All Jobs",
    fillColor = ~qtileS000(agg_15$S000),
    label = agg_15$S000)
map2015leaf <- map2015leaf %>%
  addLegend(
    data = agg_15,
    "bottomright",
    pal = qtileS000,
    values = ~ S000,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "All Jobs",
    na.label = "NA")

#SI01
map2015leaf <- map2015leaf %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "Goods Producing Sector",
    fillColor = ~qtileS000(agg_15$SI01),
    label = agg_15$SI01) %>%
  addLegend(
    data = agg_15,
    "bottomright",
    pal = qtileS000,
    values = ~ SI01,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "Goods Producing Sector",
    na.label = "NA")

#SI02
map2015leaf <- map2015leaf %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "Trade, Transportation,and Utilities Sector",
    fillColor = ~qtileS000(agg_15$SI02),
    label = agg_15$SI02) %>%
  addLegend(
    data = agg_15,
    "bottomright",
    pal = qtileS000,
    values = ~ SI02,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "Trade, Transportation,and Utilities Sector",
    na.label = "NA")

#SI03
map2015leaf <- map2015leaf %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = 1,
    group = "All Other Services Sector",
    fillColor = ~qtileS000(agg_15$SI03),
    label = agg_15$SI03) %>%
  addLegend(
    data = agg_15,
    "bottomright",
    pal = qtileS000,
    values = ~ SI03,
    title = "Wasco County Job Density",
    opacity = 1,
    group = "All Other Services Sector",
    na.label = "NA")

map2015leaf <- map2015leaf %>%
  addLayersControl(
    baseGroups = c("South Wasco School District"),
    overlayGroups = c("All Jobs", "Goods Producing Sector", "Trade, Transportation,and Utilities Sector", "All Other Services Sector"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Goods Producing Sector", "Trade, Transportation,and Utilities Sector", "All Other Services Sector"))
