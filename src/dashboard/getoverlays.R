## Building isochrones

# devtools::install_github("tlorusso/traveltime")
# devtools::install_github("rCarto/osrm")
# install.packages("rmapzen")
# install.packages("rgdal")
library(traveltime)
library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
# webshot::install_phantomjs()

# traveltime ----------------------------------------------------------------------
readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

for(i in 1:nrow(emsstations)){
  i=9
  ems_iso8 <- traveltime_map(appId= traveltime_id,
                             apiKey = traveltime_api,
                             location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                             traveltime=480,
                             type="driving",
                             departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso8, file = paste0('ems_iso_8_',i,'.RDS'))
  ems_iso10 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                              traveltime=600,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso10, file = paste0('ems_iso_10_',i,'.RDS'))
  ems_iso12 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                              traveltime=720,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso12, file = paste0('ems_iso_12_',i,'.RDS'))
  residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
  m1 = mapview(ems_iso8, layer.name = "8 minute isochrone", col.regions = colors[1])
  m2 = mapview(ems_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
  m3 = mapview(ems_iso12, layer.name = "12 minute isochrone", col.regions = colors[3])
  # add the second layer on top
  m1 = m1 + m2 + m3 + residential
  mapshot(m1, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_",i, ".png", sep = ""))
}
