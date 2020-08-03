library(readxl)
library(here)
library(tidyverse)
library(data.table)

### Reading in data from Feeding America's Map the Meal Gap research
## Notes only years 2017 and earlier can be compared to one another.
## 2018 uses different methodology


### Sheet 1 : counties
### Sheet 2 : congressional Dsitrict
### Sheet 3 : State
### Sheet 4 : Citations


## Read and combine all years for county level data. ##
years <- c(2014, 2015, 2016, 2017)
county_data <- data.frame()
for (i in 1:length(years)){
  ## read from xlsx 
  temp <- read_excel(here("data",paste0("MMG", years[i], "Data.xlsx")), sheet = 1) %>% mutate(Year = years[i])
  ## remove year specifics in the column names
  colnames(temp) <- gsub(paste0(years[i]," "), "", gsub(paste0(" in ",years[i]), "", colnames(temp)))
  county_data <- rbind(county_data, temp)
}
#Only keep Oregon and Washington
county_data <- filter(county_data, State == "OR" | State == "WA")
fwrite(county_data, here("data", "food_insecurity_counties.csv"))


state_data <- data.frame()
for (i in 1:length(years)){
  ## read from xlsx 
  temp <- read_excel(here("data",paste0("MMG", years[i], "Data.xlsx")), sheet = 3) %>% mutate(Year = years[i])
  ## remove any years from the column names
  colnames(temp) <- gsub(paste0("[0-9]{4}"," "), "", gsub(paste0(" in ","[0-9]{4}"), "", colnames(temp)))
  state_data <- rbind(state_data, temp)
}
fwrite(state_data, here("data", "food_insecurity_state.csv"))
