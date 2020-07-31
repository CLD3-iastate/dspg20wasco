library(data.table)
library(here)
library(tidyverse)


prek <- fread(here("/data/education/preschool_enrollment.csv"))%>% 
  filter(District.Name == "South Wasco County SD 1" |
           District.Name == "North Wasco SD 21" |
           District.Name == "Sherman County SD 1" |
           District.Name == "Dufur SD 29" |
           District.Name == "Hood River County SD 1")
ed <- fread(here("/data/education/combined_education.csv")) %>% 
  filter(District.Name == "South Wasco County SD 1" |
           District.Name == "North Wasco County SD 21" |
           District.Name == "Sherman County SD" |
           District.Name == "Dufur SD 29" |
           District.Name == "Hood River County SD")

