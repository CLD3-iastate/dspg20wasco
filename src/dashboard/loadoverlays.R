library(sf)
library(sp)
library(tidyverse)
library(here)
library(dplyr)
library(formattable)
library(readxl)

### loading overlay data, check file names
## food points
food_points <- ("Data/shps/stores/stores.shp") %>% st_read() %>%
  mutate(radius = case_when(pymnt_w == "yes" ~ 8,
                            pymnt_s == "yes" ~ 6,
                            is.na(pymnt_s) ~ 3)) %>%
  mutate(pymnt_types = case_when(
    pymnt_s == "yes" & pymnt_w == "yes" ~ "SW",
    pymnt_s == "yes" & is.na(pymnt_w) ~ "S",
    is.na(pymnt_s) & pymnt_w == "yes" ~ "W",
    is.na(pymnt_s) & is.na(pymnt_w) ~ ""
  ))

## isochrones
isochrones <- ("Data/shps/isochrones/isochrones.shp") %>% st_read()
#isochrones <- spTransform(isochrones, CRS("+proj=longlat +datum=WGS84"))

## food insecurity
food_insecurity_counties <- read_csv("Data/food_insecurity_counties.csv")
food_insecurity_states <- read_csv("Data/food_insecurity_state.csv")
food_insecurity_counties$FIPS <- as.character(food_insecurity_counties$FIPS)

names(food_insecurity_counties) <- abbreviate(names(food_insecurity_counties))
food_insecurity_counties <- food_insecurity_counties %>% rename("GEOID" = "FIPS")

food_insecurity_counties$popup_text <-
  paste0(food_insecurity_counties$`Cn,S`, "<br/>",
         food_insecurity_counties$Year, "<br/>",
         "<strong> Food Insecurity Rate:", "<br/>",
         round(food_insecurity_counties$FdIR, 3)*100, "% </strong> <br/>",
         "Annual Food Budget Shortfall", "<br/>",
         "$", comma(food_insecurity_counties$WAFBS)) %>%
  lapply(htmltools::HTML)

food_insecurity_counties$popup_text_child <-
  paste0(food_insecurity_counties$`Cn,S`, "<br/>",
         food_insecurity_counties$Year, "<br/>",
         "<strong> Childhood Food Insecurity Rate:", "<br/>",
         round(food_insecurity_counties$Cfir, 3)*100,  "% </strong> <br/>",
         "Annual Food Budget Shortfall", "<br/>",
         "$", comma(food_insecurity_counties$WAFBS)) %>%
  lapply(htmltools::HTML)

or_county_lines <- counties(state = "OR")
wa_county_lines <- counties(state = "WA")

county_lines <- rbind(or_county_lines, wa_county_lines)
neighbors <- c("Wasco", "Hood River", "Sherman", "Jefferson", "Skamania", "Klickitat")


data2014 <- food_insecurity_counties %>% filter(Year == "2014")
shp2014 <- geo_join(county_lines, data2014, by = c("GEOID"))
data2015 <- food_insecurity_counties %>% filter(Year == "2015")
shp2015 <- geo_join(county_lines, data2015, by = c("GEOID"))
data2016 <- food_insecurity_counties %>% filter(Year == "2016")
shp2016 <- geo_join(county_lines, data2016, by = c("GEOID"))
data2017 <- food_insecurity_counties %>% filter(Year == "2017")
shp2017 <- geo_join(county_lines, data2017, by = c("GEOID"))


shp2014 <- shp2014 %>% filter(NAME %in% neighbors)
shp2014 <- shp2014[!rownames(shp2014) == 5, ]
shp2015 <- shp2015 %>% filter(NAME %in% neighbors)
shp2015 <- shp2015[!rownames(shp2015) == 5, ]
shp2016 <- shp2016 %>% filter(NAME %in% neighbors)
shp2016 <- shp2016[!rownames(shp2016) == 5, ]
shp2017 <- shp2017 %>% filter(NAME %in% neighbors)
shp2017 <- shp2017[!rownames(shp2017) == 5, ]

## Indicators
infrastructure <- read_excel("Data/Indicators.xlsx", sheet = 1, skip = 1)
food_systems <- read_excel("Data/Indicators.xlsx", sheet = 2, skip = 1)
learn_earn <- read_excel("Data/Indicators.xlsx", sheet = 3, skip = 1)
living <- read_excel("Data/Indicators.xlsx", sheet = 4, skip = 1)
