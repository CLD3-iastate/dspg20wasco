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
south_wasco_points <- st_read("~/git/dspg20wasco/data/shps/swsd")

#S000 (all jobs) by year
od_S000leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region") %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2017",
    fillColor = ~colorQuantile(viridis_pal(option = "D")(5), domain = agg_17$S000)(agg_17$S000),
    label = agg_17$S000) %>%
  addLegend(
    data = rbind(agg_17, agg_16, agg_15),
    "bottomright",
    pal = colorQuantile(viridis_pal(option = "D")(5), domain = rbind(agg_17, agg_16, agg_15)$S000),
    values = ~ S000,
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(p * 100), '%')
      cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
    title = "Number of All Jobs<br>in Wasco County",
    na.label = "NA") %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2016",
    fillColor = ~colorQuantile(viridis_pal(option = "D")(5), domain = agg_16$S000)(agg_16$S000),
    label = agg_16$S000) %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2015",
    fillColor = ~colorQuantile(viridis_pal(option = "D")(5), domain = agg_15$S000)(agg_15$S000),
    label = agg_15$S000) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))

#SI01 (Goods Producing industry sectors) by year
colors_SI01 <- colorQuantile(viridis_pal(option = "D")(3), domain = unique(rbind(agg_17, agg_16, agg_15)$SI01))

od_SI01leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region") %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2017",
    fillColor = ~colors_SI01((agg_17$SI01)),
    label = agg_17$SI01) %>%
  addLegend(
    data = rbind(agg_17, agg_16, agg_15),
    "bottomright",
    pal = colors_SI01,
    values = ~ unique(SI01),
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(p * 100), '%')
      cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
    title = "Number of Goods Producing<br>Sector Jobs in Wasco County",
    na.label = "NA") %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2016",
    fillColor = ~colors_SI01((agg_16$SI01)),
    label = agg_16$SI01) %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2015",
    fillColor = ~colors_SI01((agg_15$SI01)),
    label = agg_15$SI01) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))

#SI02 (Trade, Transportation, and Utilities industry sectors) by year
colors_SI02 <- colorQuantile(viridis_pal(option = "D")(3), domain = unique(rbind(agg_17, agg_16, agg_15)$SI02))

od_SI02leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region") %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2017",
    fillColor = ~colors_SI02((agg_17$SI02)),
    label = agg_17$SI02) %>%
  addLegend(
    data = rbind(agg_17, agg_16, agg_15),
    "bottomright",
    pal = colors_SI02,
    values = ~ unique(SI02),
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(p * 100), '%')
      cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
    title = "Number of Trade, Transportation,<br>and Utilities Sector Jobs<br>in Wasco County",
    na.label = "NA") %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2016",
    fillColor = ~colors_SI02((agg_16$SI02)),
    label = agg_16$SI02) %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2015",
    fillColor = ~colors_SI02((agg_15$SI02)),
    label = agg_15$SI02) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))

#SI03 (All Other Services industry sectors) by year
colors_SI03 <- colorQuantile(viridis_pal(option = "D")(3), domain = unique(rbind(agg_17, agg_16, agg_15)$SI03))

od_SI03leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = south_wasco_points,
    color = "purple",
    weight = 2,
    opacity = 1,
    group = "Basemap",
    label = "South Wasco Region") %>%
  addPolygons(
    data = st_as_sf(agg_17),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2017",
    fillColor = ~colors_SI03((agg_17$SI03)),
    label = agg_17$SI03) %>%
  addLegend(
    data = rbind(agg_17, agg_16, agg_15),
    "bottomright",
    pal = colors_SI03,
    values = ~ unique(SI03),
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(p * 100), '%')
      cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))},
    title = "Number of All Other Services<br>Sector Jobs in Wasco County",
    na.label = "NA") %>%
  addPolygons(
    data = st_as_sf(agg_16),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2016",
    fillColor = ~colors_SI03((agg_16$SI03)),
    label = agg_16$SI03) %>%
  addPolygons(
    data = st_as_sf(agg_15),
    weight = 1,
    opacity = 0,
    fillOpacity = .7,
    group = "2015",
    fillColor = ~colors_SI03((agg_15$SI03)),
    label = agg_15$SI03) %>%
  addLayersControl(
    baseGroups = c("2017", "2016", "2015"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("2016", "2015"))
