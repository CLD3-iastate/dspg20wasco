library(tigris)
library(leaflet)
library(tidyverse)
library(ggplot2)
#install_github("yonghah/esri2sf")
library(esri2sf)
library(osmdata)
library(sf)
library(tidygeocoder)
library(stringr)

### Base layers ---------------

## Pulling in SWSD ------
# 41 is FIPS code for Oregon, or use "Oregon"
schools <- school_districts("Oregon")
schools <- st_as_sf(schools)
swsd <- schools %>% filter(NAME == "South Wasco County School District 1")

## Wasco roads
url <- "https://public.co.wasco.or.us/gisserver/rest/services/Roads/MapServer/0"
roads <- esri2sf(url)
roads <- st_as_sf(roads)

## Pulling in OSM Street data ------
big_streets
q <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%  osmdata_sf()
med_streets <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>% osmdata_sf()
small_streets <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  ))  %>% osmdata_sf()

## Pulling in cities and towns -----
url <- "https://public.co.wasco.or.us/gisserver/rest/services/CityLimits/MapServer/55"
townships <- esri2sf(url)
townships <- st_as_sf(townships)

url = "https://public.co.wasco.or.us/gisserver/rest/services/CityLimits/MapServer/57"
unincorporated <- esri2sf(url)
unincorporated <- st_as_sf(unincorporated)

## Pulling in county line -------
url = "https://public.co.wasco.or.us/gisserver/rest/services/WascoCountyBoundary/MapServer/0"
countyline <- esri2sf(url)
countyline <- st_as_sf(countyline)

# Other county lines
oregon <- counties("Oregon", cb = FALSE)
washington <- counties("Washington", cb = FALSE)
oregon <- st_as_sf(oregon)
washington <- st_as_sf(washington)
or_counties <- c("Hood River", "Sherman", "Jefferson")
wa_counties <- c("Klickitat", "Skamania")
oregon <- oregon %>% dplyr::filter(NAME %in% or_counties)
washington <- washington %>% dplyr::filter(NAME %in% wa_counties)
neighboring_counties <- rbind(oregon, washington)
st_write(neighboring_counties, "neighboring_counties.shp")
