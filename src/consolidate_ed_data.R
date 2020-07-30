library(data.table)
library(tidyverse)
library(tidycensus)
library(here)

##### Perfomance data: #####
years <- c(1415,1516,1617,1718,1819)
# %ELA proficiency change between 3rd & 8th grade | columns consistent for all years

ela <- unique(fread(here("/data/education/performance_districts_1415.csv")) %>% 
  filter(Student.Group == "Total Population (All Students)",
         Grade.Level == "Grade 3" | Grade.Level == "Grade 8") %>% 
  mutate(perc_proficient = as.numeric(`Percent.Proficient.(Level.3.or.4)`), Academic.Year = 1415) %>%
  group_by(District) %>% summarise(year=Academic.Year, District.ID=District.ID, Percent.ELA.Proficient.Change = diff(perc_proficient))) %>%
  rename("District.Name"= "District")

for(i in 2:length(years)){
  csv <- fread(here(paste0("/data/education/performance_districts_", years[i], ".csv")))
  temp <- unique(csv %>% 
                   filter(Student.Group == "Total Population (All Students)",
                          Grade.Level == "Grade 3" | Grade.Level == "Grade 8") %>% 
                   mutate(perc_proficient = as.numeric(`Percent.Proficient.(Level.3.or.4)`), Academic.Year = years[i]) %>%
                   group_by(District) %>% summarise(year=Academic.Year, District.ID=District.ID, Percent.ELA.Proficient.Change = diff(perc_proficient))) %>%
                   rename("District.Name"= "District")
  ela <- rbind(ela, temp)
} 


##### Absenteeism data: %regular attenders and %economically disadvantaged #####
# Includes District AND state
absenteeism_og <- fread(here("/data/education/absenteeism.csv")) %>% mutate(Student.Group = gsub("Total", "All Students", Student.Group))
#only retrieves District
absenteeism <- unique(absenteeism_og %>% filter(grepl("SD",Institution), Student.Group == "All Students" | Student.Group == "Economically Disadvantaged") %>%
  group_by(District.ID, year) %>% summarise(year=year, District.ID=District.ID, District.Name = Institution, Student.Group = Student.Group,
                                            Percent.Regular.Attenders = Percent.Regular.Attenders, Percent.Chronically.Absent = Percent.Chronically.Absent,
                                            Percent.Economically.Disadvantaged = round(Students.Included[Student.Group == "Economically Disadvantaged"] / Students.Included[Student.Group == "All Students"] * 100,1))) %>%
  filter(Student.Group =="All Students")



##### Dropout Data ##### 
dropout_district <- fread(here("/data/education/dropout_district.csv")) %>% 
  rename(c("year" = "School.Year", "District.Name" = "Resident.School.Name", "District.ID"="Resident.District.ID"))


##### Report card data: % qualified teachers, on time graduation#####  
#Columns differ per report :(
rc_dist_1415 <- fread(here("/data/education/rc_districts_1415.csv")) %>% 
  select("District ID", "District Name", "County", "Highly Qualified Teachers Pct", "State Four Year Graduation Rate") %>% mutate(year = 1415) %>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", 
           "Teacher.Experience.Pct"="Highly Qualified Teachers Pct", "State.On.Time.Grad.Rate" = "State Four Year Graduation Rate"))


rc_dist_1516 <- fread(here("/data/education/rc_districts_1516.csv")) %>%
  select("District ID", "District Name", "County", "Highly Qualified Teachers Pct", "Four Year Graduation Rate 2014-15", "State Four Year Graduation Rate") %>% mutate(year =1516)%>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", "Teacher.Experience.Pct"="Highly Qualified Teachers Pct", 
           "State.On.Time.Grad.Rate" = "State Four Year Graduation Rate"))


### HIghly qualified Teachers doesn't exist here :(
rc_dist_1617 <- fread(here("/data/education/rc_districts_1617.csv")) %>%
  select("District ID", "District Name", "County", "Four Year Graduation Rate 2015-16", "State Four Year Graduation Rate") %>% mutate(year=1617)%>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name",  
           "State.On.Time.Grad.Rate" = "State Four Year Graduation Rate"))

### HIghly qualified Teachers doesn't exist here :( | dropout rate not here
rc_dist_1718 <- fread(here("/data/education/rc_districts_1718.csv")) %>%
  select("District ID", "District Name", "County", "Free/Reduced Priced Lunch","On-Time Graduation", "On-Track to Graduate Change Value", "Oregon On-Time Graduation Average")%>% mutate(year = 1718) %>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", "Free.Reduced.Priced.Lunch" = "Free/Reduced Priced Lunch",
           "On.Time.Grad.Rate" = "On-Time Graduation","On.Time.Grad.Rate.Change" = "On-Track to Graduate Change Value","State.On.Time.Grad.Rate" = "Oregon On-Time Graduation Average")) 


#percentages included as character
rc_dist_1819 <- fread(here("/data/education/rc_districts_1819.csv")) %>%
  select("District ID", "District Name", "County", "Free/Reduced Priced Lunch","Teacher Experience","On-Time Graduation", "Oregon On-Time Graduation Average") %>% mutate(year = 1819)%>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", "Free.Reduced.Priced.Lunch" = "Free/Reduced Priced Lunch",
           "Teacher.Experience" = "Teacher Experience",
           "On.Time.Grad.Rate" = "On-Time Graduation","State.On.Time.Grad.Rate" = "Oregon On-Time Graduation Average")) 


#merge on.time.grad rates to the correct data table. left outer join
rc_dist_1516 <- merge(x = rc_dist_1516, y = select(rc_dist_1617, -c(State.On.Time.Grad.Rate, year)), 
                      by = c("District.ID","District.Name","County"), all.x = TRUE) 

rc_dist_1415 <- merge(x = rc_dist_1415, y = select(rc_dist_1516, -c(State.On.Time.Grad.Rate, year)), 
                      by = c("District.ID","District.Name","County"), all.x = TRUE)

#extract State on time grad rate and store it separately to be merged later.
rc_keep <- c("State.On.Time.Grad.Rate", "year")
rc_state <- data.frame(rbind(rc_dist_1415[1,c(State.On.Time.Grad.Rate, year)], rc_dist_1516[1,c(State.On.Time.Grad.Rate, year)],
                  rc_dist_1617[1,c(State.On.Time.Grad.Rate, year)], rc_dist_1718[1,c(State.On.Time.Grad.Rate, year)],
                  rc_dist_1819[1,c(State.On.Time.Grad.Rate, year)])) 
colnames(rc_state) <- c("State.On.Time.Grad.Rate", "year")
rc_state <- rc_state %>% mutate(State.On.Time.Grad.Rate = as.numeric(gsub("%", "", State.On.Time.Grad.Rate)), 
                                District.Name = "State Level", District.ID = 9999)

##### Percent Enrolled in Pre-school #####
#C01 = total
#C02 = Percent
#C03 = Public school total 
# ...
prek18 <- get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
       variables = c("S1401_C01_013","S1401_C01_014","S1401_C02_014"), output = "wide", cache_table = TRUE) %>%
  filter(NAME == "South Wasco County School District 1, Oregon" | NAME == "North Wasco School District 21, Oregon"| NAME == "Dufur School District 29, Oregon" | NAME == "Sherman School District 1, Oregon" |
           NAME == "Hood River County School District 1, Oregon" | NAME == "Jefferson County School District 509J, Oregon") %>%
  transmute(GEOID = GEOID, NAME = NAME, year = 2018, total_prek = S1401_C01_013E, total_prek_moe = S1401_C01_013M, total_prek_enroll = S1401_C01_014E, total_prek_enroll_moe = S1401_C01_014M,
            perc_prek_enroll = S1401_C02_014E, perc_prek_enroll_moe = S1401_C02_014M)


# filter(NAME == "South Wasco County School District 1, Oregon" || NAME == "	
# North Wasco School District 21, Oregon"|| NAME == "Dufur School District 29, Oregon" || NAME == "Sherman School District 1, Oregon" || 
#          NAME == "Hood River County School District 1, Oregon" || NAME == "Jefferson County School District 509J, Oregon")




##### CONSOLIDATE #####
# stackoverflow on merge/join: https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
combined_education <- merge(x=ela, y=absenteeism, by = c("District.ID", "year", "District.Name"))