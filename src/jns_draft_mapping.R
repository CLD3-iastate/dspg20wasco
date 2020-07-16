#install.packages("tigris")
#install.packages("remotes")

library(tigris)
library(leaflet)
#library(remotes)
#remotes::install_github("jamgreen/lehdR")
#library(lehdr)
library(tidyverse)
library(ggplot2)
#library(devtools)
#install_github("yonghah/esri2sf")
library(esri2sf)
library(osmdata)
library(sf)
library(tidygeocoder)

## Pulling in SWSD ------
# 41 is FIPS code for Oregon, or use "Oregon"
schools <- school_districts("Oregon")
schools <- st_as_sf(schools)
swsd <- schools %>% filter(NAME == "South Wasco County School District 1")

## Pulling in OSM Street data ------

big_streets <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()
med_streets <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()
small_streets <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

## Pulling in OSM buildings data -------
buildings <- getbb("Wasco County United States") %>%
  opq() %>%
  add_osm_feature("building", "grocery") %>%
  osmdata_sf()

## Pulling in cities and towns -----
url <- "https://public.co.wasco.or.us/gisserver/rest/services/CityLimits/MapServer/55"
townships <- esri2sf(url)
townships <- st_as_sf(townships)

url = "https://public.co.wasco.or.us/gisserver/rest/services/CityLimits/MapServer/57"
unincorporated <- esri2sf(url)
unincorporated <- st_as_sf(unincorporated)

## Wasco roads
url <- "https://public.co.wasco.or.us/gisserver/rest/services/Roads/MapServer/0"
roads <- esri2sf(url)
roads <- st_as_sf(roads)


## Pulling in food systems data ------
## SNAP stores
url <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/ArcGIS/rest/services/Store_Locations/FeatureServer/0"
snap <- esri2sf(url)
snap <- st_as_sf(snap)
snap <- snap %>% filter(State == "OR")
snap_wasco <- snap %>% filter(County == "WASCO")
## WIC stores
url <- "https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_WIC_PUBLIC/FeatureServer/1"
wic <- esri2sf(url)
wic <- st_as_sf(wic)

## Query USDA API for farmers markets in Wasco County (can add surrounding)
library(jsonlite)

zips <- c("97001", "97021", "97037", "97040", "97057", "97058", "97063")
zipurl <- "http://search.ams.usda.gov/farmersmarkets/v1/data.svc/zipSearch?zip="
results <- data.frame(id = character(), marketname = character())
getids <- lapply(zips, function(zip) {
  data <- fromJSON(paste0(zipurl, zip), flatten = TRUE)
  message("Retrieving zip ", zip)
  data.frame(
    id = data[["results"]][["id"]],
    marketname = data[["results"]][["marketname"]]
  )
})

ids <- do.call(rbind.data.frame, getids)
ids$marketname <- str_replace(ids$marketname, c("[[:digit:]]+\\.[[:digit:]]+ "), "")
ids <- ids %>% distinct()

marketurl <- "http://search.ams.usda.gov/farmersmarkets/v1/data.svc/mktDetail?id="
getmarkets <- lapply(ids$id, function(id) {
  data <- fromJSON(paste0(marketurl, id), flatten = TRUE)
  message("Getting information for ", id)
  data.frame(
    id = id,
    address = data[["marketdetails"]][["Address"]],
    googlelink = data[["marketdetails"]][["GoogleLink"]],
    products = data[["marketdetails"]][["Products"]],
    schedule = data[["marketdetails"]][["Schedule"]]
  )
})

markets <- do.call(rbind.data.frame, getmarkets)

localfood <- left_join(markets, ids, by = "id")

# There are big issues when geocoding, have to drop several
localfood <- localfood %>% select("marketname", "address") %>% geocode(address, lat = latitude, long = longitude) %>% drop_na() %>% st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326, agr  = "constant")

## Create one layer of all stores with columns: Store_Name, Address, City, County, and type
snap <- snap %>% select(Store_Name, Address, City, County) %>% mutate(type = "snap")

wic <- wic %>% select(Store_Name = VENDOR_ID, Address = STREET_ADDRESS1, City = STREET_CITY, County = COUNTY_NAME) %>% mutate(type = "wic")

localfood <- localfood %>% select(Store_Name = marketname, Address = address, geoms = geometry) %>% mutate(City = NA, County = NA, type = "farmers market")

allfood <- rbind(snap, wic)
allfood <- rbind(allfood, localfood)

## Static mapping of selected layers (school district, streets, grocery) -----
ggplot() +
  geom_sf(data = swsd,
          inherit.aes = FALSE,
          color = "red") +
  geom_sf(data = townships,
          inherit.aes = FALSE,
          color = "blue",
          size = .2) +
  geom_sf(data = unincorporated,
          inherit.aes = FALSE,
          color = "blue",
          size = .2) +
  geom_sf(data = roads,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = countyline)


+
  geom_sf(data = allfood, color = "purple", size = .2) +
  xlim(122, 120) +
  ylim(44.5, 45.75)




library("esri2sf")

geom_sf(data = oregon_live, aes(x = long, y = lat), color = "yellow") +
geom_sf(data = local_food, color = "purple") +

## Taxlots
url <- "https://public.co.wasco.or.us/gisserver/rest/services/Taxlots/FeatureServer/0"
df <- esri2sf(url)
df <- st_as_sf(df)
plot(df)




library(data.table)
library(stringr)

## Creating all stores DF
snap_wasco <- snap_wasco %>% select(Store_Name, geoms) %>% mutate(snap = "yes")
wic_wasco <- wic %>% filter(COUNTY_NAME == "Wasco/Sherman") %>% select(VENDOR_NAME)

names <- c("SAFEWAY 1489 | Walgreens 09651 | FRED MEYER 00372")

snap_wasco$Store_Name <- lapply(snap_wasco$Store_Name, trimws)
wasco_food <- snap_wasco %>% mutate(wic = if_else(str_detect(names, Store_Name), "yes", NA))

snap_wasco %>% filter(str_detect("Walgreens 09651 | FRED MEYER 00372 | SAFEWAY 1489
", Store_Name))

mutate(player_team = if_else(str_detect(home_lineup, player), home, away))

library(dplyr)

## Leaflet mapping -------

leaflet(schools) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5)

library(RColorBrewer)

leaflet() %>%
  addTiles() %>%
  setView(-121, 45, zoom = 7) %>%
  addPolylines(data = swsd, color = "red") %>%
  addPolylines(data = small_streets$osm_lines,
               color = "gray", weight = .5) %>%
  addPolylines(data = med_streets$osm_lines,
               color = "black", weight = .75) %>%
  addPolylines(data = big_streets$osm_lines,
               color = "black", weight = 1) %>%
  addCircleMarkers(data = allfood,
             color = ~foodpal(type), fillOpacity = 1, size = 3)



(domain <- range(allfood$type))

foodpal <- colorFactor("Set1", domain = allfood$type)

leaflet() %>%
  addTiles() %>%
  setView(-121, 45.2, zoom = 8) %>%
  addPolylines(data = swsd, color = "purple", opacity = 1, group = "Basemap") %>%
  addPolylines(data = countyline, color = "grey", group = "Basemap") %>%
  addPolylines(data = townships, color = "blue", opacity = 1, weight = 1, group = "Basemap") %>%
  addPolylines(data = unincorporated, color = "blue", opacity = 1, weight = 1, group = "Basemap") %>%
  addPolylines(data = small_streets$osm_lines,
               color = "gray", weight = .5, group = "Basemap") %>%
  addPolylines(data = med_streets$osm_lines,
               color = "black", weight = .75, group = "Basemap") %>%
  addPolylines(data = big_streets$osm_lines,
               color = "black", weight = 1, group = "Basemap") %>%
  addCircleMarkers(data = allfood,
                   color = ~foodpal(type), fillOpacity = 1, radius = 3,
                   stroke = FALSE, group = "Food") %>%
  addLayersControl(
    baseGroups = c("Basemap"),
    overlayGroups = c("Food"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(data = allfood, "bottomright", pal = foodpal, values = ~type,
            title = "Food type",
            opacity = 1, group = "Food", na.label = "NA")

st_write(allfood, "~/git/dspg20wasco/data/shps/food/allfood.csv", layer_options = "GEOMETRY=AS_XY")


st_write(swsd, "~/git/dspg20wasco/data/shps/swsd.shp")
st_write(townships, "~/git/dspg20wasco/data/shps/townships.shp")
st_write(unincorporated, "~/git/dspg20wasco/data/shps/unincorporated.shp")
st_write(countyline, "~/git/dspg20wasco/data/shps/countyline.shp")
st_write(roads, "~/git/dspg20wasco/data/shps/roads.shp")


library(jsonlite)
fromJSON("api.ed.gov/data/crdc_enrollment_2013-14?api_key=IKcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY/", flatten = TRUE)
fromJSON("api.ed.gov/data/crdc_enrollment_2013-14?api_key=KcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY/_4100021_", flatten = TRUE)

library(devtools)
devtools::install_github("kpersauddavis/rCRDC")
library(rCRDC)
https://api.ed.gov/data/mbk-highschool-dropout?api_key=KcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY

CRDC_data <- CRDC_Query(api_key = "KcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY", dataset = "suspension", per_page = 100, page = 2, preprocess = FALSE)

str(CRDC_data, list.len = 10)
