library(openxlsx)
library(dplyr)
library(data.table)
library(here)


#school years 1415 -> 1718 have the same structure on sheet two.
dropout1415 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2014-2015.xlsx", sheet=2) %>% 
  filter(grepl("SD",Resident.School.Name), Subgroup.Name == "All Students") %>%
  select(School.Year, County, Resident.District.ID, Resident.School.Name, Fall.Membership,Dropout.Rate)%>% 
  mutate(School.Year = 1415)

dropout1516 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2015-2016.xlsx", sheet=2)%>% 
  filter(grepl("SD",Resident.School.Name), Subgroup.Name == "All Students") %>%
  select(School.Year, County, Resident.District.ID, Resident.School.Name, Fall.Membership,Dropout.Rate)%>% 
  mutate(School.Year = 1516)

dropout1617 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2016-2017.xlsx", sheet=2) %>% 
  filter(grepl("SD",Resident.School.Name), Student.Group == "All Students") %>%
  select(School.Year, County, Resident.District.ID, Resident.School.Name, Fall.Membership,Dropout.Rate)%>% 
  mutate(School.Year = 1617)


dropout1718 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2017-2018.xlsx", sheet=2) %>% 
  filter(grepl("SD",Resident.School.Name), Student.Group == "All Students") %>%
  select(School.Year, County, Resident.District.ID, Resident.School.Name, Fall.Membership,Dropout.Rate)%>% 
  mutate(School.Year = 1718)

#1819 has an introductory note on sheet 1
dropout1819 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2018-2019.xlsx", sheet=3) %>%
  filter(grepl("SD",Resident.School.Name), Student.Group == "All Students") %>%
  select(School.Year, County, Resident.District.ID, Resident.School.Name,Fall.Membership, Dropout.Rate)%>% 
  mutate(School.Year = 1819)

dropout_district <- rbind(dropout1415, dropout1516, dropout1617, dropout1718, dropout1819)




### get the data for state
#school years 1415 -> 1718 have the same structure on sheet two.
dropout1415s <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2014-2015.xlsx", sheet=1) %>% 
  filter(County == "State Level", Subgroup.Name == "All Students") %>%
  select(School.Year, County, Fall.Membership, Dropout.Rate)%>% 
  mutate(School.Year = 1415)

dropout1516s <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2015-2016.xlsx", sheet=1)%>% 
  filter(County == "State Level", Subgroup.Name == "All Students") %>%
  select(School.Year, County, Fall.Membership, Dropout.Rate)%>% 
  mutate(School.Year = 1516)

dropout1617s <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2016-2017.xlsx", sheet=1) %>% 
  filter(County == "State Level", Student.Group == "All Students") %>%
  select(School.Year, County, Fall.Membership, Dropout.Rate)%>%
  mutate(School.Year = 1617)


dropout1718s <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2017-2018.xlsx", sheet=1) %>% 
  filter(County == "State Level", Student.Group == "All Students") %>%
  select(School.Year, County, Fall.Membership, Dropout.Rate)%>%
  mutate(School.Year = 1718)

#1819 has an introductory note on sheet 1
dropout1819s <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/dropouttables2018-2019.xlsx", sheet=2) %>%
  filter(County == "State Level", Student.Group == "All Students") %>%
  select(School.Year, County, Fall.Membership, Dropout.Rate)%>% 
  mutate(School.Year = 1819)

dropout_state <- rbind(dropout1415s, dropout1516s, dropout1617s, dropout1718s, dropout1819s)

#fwrite(dropout_district, here("data/education/dropout_district.csv"))
#fwrite(dropout_state, here("data/education/dropout_state.csv"))