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
library(dplyr)
library(stringr)

## OSM grocery code -------

bb <- c(-121.8521, 44.6139, -120.2673, 45.7215)

#building the query
q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature("shop")

#query
shops <- osmdata_sf(q)

grocery_p <- shops$osm_points %>%
  filter(str_detect(shop, "convenience|supermarket|farm")) %>%
  select(name, shop, payment.snap, payment.wic)

cntrd = st_centroid(grocery)

stores <- rbind(cntrd, grocery_p)

## Editing for proper filtering, manual deduplication
stores["791573620", "name"] = "The Outpost (Madras)"
stores["7708392087", "name"] = "The Outpost (The Dalles)"
stores["146717290", "name"] = "Safeway (Madras)"
stores["219077848", "name"] = "Safeway (The Dalles)"
stores["371944173", "name"] = "Safeway (Hood River)"
stores["781471220", "name"] = "Grocery Outlet (Madras)"
stores["4139660089", "name"] = "Grocery Outlet (The Dalles)"
stores["383548025", "name"] = "Food Mart (Hood River, Cascade Ave.)"
stores["403413315", "name"] = "Food Mart (Hood River, East Marina Dr. Chevron)"
stores["403413317", "name"] = "Food Mart (Hood River, East Marina Dr. Shell)"
stores["438300809", "name"] = "Food Mart (Madras)"
stores["438300809", "name"] = "Food Mart (Hood River, Pacific Ave.)"
stores["619840103", "name"] = "Food Mart (The Dalles)"
stores["3905855641", "name"] = "Food Mart (Biggs Junction)"
stores["4358176653", "name"] = "Food Mart (Mt. Hood National Forest)"
stores["387276212", "name"] = "Circle K (Biggs Junction)"
stores["438300805", "name"] = "Circle K (Madras)"
stores["438300804", "name"] = "Chevron (Madras)"
stores["593082948", "name"] = "Chevron (The Dalles)"
stores["441287009", "name"] = "The Boys Pine Grove Grocery"
stores["7178227164", "name"] = "MsIssac's Store"

duplicates <- c("7708382086", "383523137", "7705702785", "619840105", "282760541", "298871008", "7706134885")

stores <- stores[!rownames(stores) %in% duplicates, ]

st_write(stores, "~/git/DSPG2020/wasco/data/stores.shp")



#coords <- do.call(rbind, st_geometry(cntrd)) %>%
  #as_tibble() %>% setNames(c("lon","lat"))

#cntrd %>% filter(str_detect(payment.snap, "yes"))


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

st_write(cntrd, "~/git/DSPG2020/wasco/data/cntrd.shp")

library(jsonlite)
fromJSON("api.ed.gov/data/crdc_enrollment_2013-14?api_key=IKcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY/", flatten = TRUE)
fromJSON("api.ed.gov/data/crdc_enrollment_2013-14?api_key=KcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY/_4100021_", flatten = TRUE)

library(devtools)
devtools::install_github("kpersauddavis/rCRDC")
library(rCRDC)
https://api.ed.gov/data/mbk-highschool-dropout?api_key=KcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY

CRDC_data <- CRDC_Query(api_key = "KcpkyBEXGRF1mblpDUjonQiJ4ePk37YsEc2U3KDY", dataset = "suspension", per_page = 100, page = 2, preprocess = FALSE)

str(CRDC_data, list.len = 10)


devtools::install_github('UrbanInstitute/education-data-package-r')
library(educationdata)

education <-  get_education_data(level = 'schools',
                                source = 'crdc',
                                topic = 'chronic-absenteeism',
                                by = list('disability', 'sex'),
                                filters = list(year = 2015,
                                               #grade = 9:12,
                                               ncessch = '410002101167'),
                                add_labels = TRUE)

## extra shop code --------
shops <- getbb("Wasco County United States") %>%
  opq() %>%
  add_osm_feature("shop", ) %>%
  osmdata_sf()


q <- bb %>%
  opq (timeout = 25*100) %>%
  add_osm_feature("shop", "supermarket") %>%
  osmdata_sf()


### Pulling education data ------------

library(openxlsx)

"https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2019/pagr_State_ELA_1819.xlsx"
"https://www.oregon.gov/ode/educator-resources/assessment/TestResults2018/pagr_State_ELA_1718.xlsx"
"https://www.oregon.gov/ode/educator-resources/assessment/TestResults2017/pagr_State_ELA_1617.xlsx"
"https://www.oregon.gov/ode/educator-resources/assessment/TestResults2016/pagr_State_ELA_1516.xlsx"
"https://www.oregon.gov/ode/educator-resources/assessment/TestResults2015/pagr_State_ELA_1415.xlsx"

"https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2019/pagr_Districts_ELA_1819.xlsx"
"https://www.oregon.gov/ode/educator-resources/assessment/TestResults2018/pagr_Districts_ELA_1718.xlsx"

make_url <- function(year = 2019, school_year = "1819") {
  base_url <- "https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults"
  middle <- "/pagr_Districts_ELA_"
  paste0(base_url, year, middle, school_year, ".xlsx")
}

get_file <- function(year = 2019, school_year = "1819") {
  file_url <- make_url(year, school_year)
  openxlsx::read.xlsx(file_url)
}

performance_districts_1819 <-  get_file("2019", "1819")
performance_districts_1718 <- get_file("2018", "1718")
performance_districts_1617 <- get_file("2017", "1617")
performance_districts_1516 <- get_file("2016", "1516")
performance_districts_1415 <- get_file("2015", "1415")

write.csv(performance_districts_1415,
          "performance_districts_1415.csv")

read_csv("~/git/DSPG2020/wasco/data/education/absenteeism.csv") %>% distinct(year)

2018-2019
"https://www.ode.state.or.us/data/reportcard/media/20/RCmediaDistrictsAggregate.csv"
2017-2018
"https://www.ode.state.or.us/data/reportcard/media/19/RCmediaDistrictsAggregate.csv"
2016-2017
"https://www.ode.state.or.us/data/reportcard/media/18/RCmediaDistrictsAggregate.csv"
2015-2016
"https://www.ode.state.or.us/data/reportcard/media/17/RCmediaDistrictsAggregate.csv"
2014-2015
"https://www.ode.state.or.us/data/reportcard/media/16/RCmediaDistrictsAggregate.csv"

make_url <- function(year = "20") {
  base_url <- "https://www.ode.state.or.us/data/reportcard/media/"
  end <- "/RCmediaDistrictsAggregate.csv"
  paste0(base_url, year, end)
}

get_file <- function(year = "20") {
  file_url <- make_url(year)
  read_csv(file_url)
}

rc_districts_1819 <-  get_file("20")
rc_districts_1718 <- get_file("19")
rc_districts_1617 <- get_file("18")
rc_districts_1516 <- get_file("17")
rc_districts_1415 <- get_file("16")

write.csv(rc_districts_1415,
          "rc_districts_1415.csv")

rc <- read_csv("~/git/DSPG2020/wasco/data/rc_districts_1819.csv")
colnames(rc)

### crdc for ap/act/sat -------------------
library(educationdata)

districts <- c("4104410", "4105250", "4106740", "4100021", "4111250", "4100048")

education <-  get_education_data(level = 'schools',
                                 source = 'crdc',
                                 topic = 'ap-exams',
                                 by = c("race", "sex"),
                                 filters = list(year = 2011,
                                                #grade = 9:12,
                                                fips = "41"),
                                 add_labels = TRUE)

"https://www2.ed.gov/about/offices/list/opepd/ppss/crdc/crdc2015-16-pa-oct-2018.csv"

crdc <- read_csv("~/git/DSPG2020/wasco/data/report.csv")

