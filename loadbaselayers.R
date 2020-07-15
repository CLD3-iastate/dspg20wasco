library(sf)
library(here)
library(tidyverse)
library(osmdata)

## Loading in baselayer data, check file names

## roads
# big
q <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link"))
doc <- osmdata_xml(q, "~/git/dspg20wasco/data/shps/big_streets.osm")
big_streets <- osmdata_sf(q, doc)
# medium
q <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link"))
doc <- osmdata_xml(q, "~/git/dspg20wasco/data/shps/med_streets.osm")
med_streets <- osmdata_sf(q, doc)
q <- getbb("Wasco County United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  ))
doc <- osmdata_xml(q, "~/git/dspg20wasco/data/shps/small_streets.osm")
small_streets <- osmdata_sf(q, doc)
# townships
townships <- here("/data/shps/townships/townships.shp") %>% st_read()
# unincorporated
unincorporated <- here("/data/shps/unincorporated/unincorporated.shp") %>% st_read()
# schools
swsd <- here("/data/shps/swsd/swsd.shp") %>% st_read()
# county
countyline <- here("/data/shps/county/countyline.shp") %>% st_read()
