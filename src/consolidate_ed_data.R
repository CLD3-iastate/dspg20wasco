library(data.table)
library(tidyverse)
library(tidycensus)
library(here)

##### Perfomance data: #####
years <- c(1415,1516,1617,1718,1819)
# %ELA proficiency change between 3rd & 8th grade | columns consistent for all years
# Change is 8th - 3rd grade
ela_districts <- unique(fread(here("/data/education/performance_districts_1415.csv")) %>% 
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
  ela_districts <- rbind(ela_districts, temp)
} 


#by state level only
ela_state <- unique(fread(here("/data/education/performance_state_1415.csv"), header =TRUE) %>% 
                      filter(Student.Group == "Total Population (All Students)",
                                 Grade.Level == "Grade 3" | Grade.Level == "Grade 8") %>% 
                          mutate(perc_proficient = as.numeric(`Percent.Proficient.(Level.3.or.4)`), Academic.Year = 1415) %>%
                          summarise(year=Academic.Year, District.Name = "State Level", District.ID=9999, Percent.ELA.Proficient.Change = diff(perc_proficient))) 

for(i in 2:length(years)){
  csv <- fread(here(paste0("/data/education/performance_state_", years[i], ".csv")), header = TRUE)
  temp <- unique(csv %>% 
                   filter(Student.Group == "Total Population (All Students)",
                          Grade.Level == "Grade 3" | Grade.Level == "Grade 8") %>% 
                   mutate(perc_proficient = as.numeric(`Percent.Proficient.(Level.3.or.4)`), Academic.Year = years[i]) %>%
                   summarise(year=Academic.Year, District.Name = "State Level", District.ID=9999, Percent.ELA.Proficient.Change = diff(perc_proficient)))
  ela_state <- rbind(ela_state, temp)
}

#bind state and district into one ela ?
ela <- rbind(ela_state, ela_districts)






##### Absenteeism data: %regular attenders and %economically disadvantaged #####
# Includes District AND state
absenteeism_og <- fread(here("/data/education/absenteeism.csv")) %>% mutate(Student.Group = gsub("Total", "All Students", Student.Group))
#only retrieves District
absenteeism_districts <- unique(absenteeism_og %>% filter(grepl("SD",Institution), Student.Group == "All Students" | Student.Group == "Economically Disadvantaged") %>%
  group_by(District.ID, year) %>% summarise(year=year, District.ID=District.ID, District.Name = Institution, Student.Group = Student.Group,
                                            Percent.Regular.Attenders = Percent.Regular.Attenders, Percent.Chronically.Absent = Percent.Chronically.Absent,
                                            Percent.Economically.Disadvantaged = round(Students.Included[Student.Group == "Economically Disadvantaged"] / Students.Included[Student.Group == "All Students"] * 100,1))) %>%
  filter(Student.Group =="All Students")
absenteeism_state <- unique(absenteeism_og %>% filter(Institution=="State Level", Student.Group == "All Students" | Student.Group == "Economically Disadvantaged") %>%
                                  group_by(District.ID, year) %>% summarise(year=year, District.ID=District.ID, District.Name = Institution, Student.Group = Student.Group,
                                                                            Percent.Regular.Attenders = Percent.Regular.Attenders, Percent.Chronically.Absent = Percent.Chronically.Absent,
                                                                            Percent.Economically.Disadvantaged = round(Students.Included[Student.Group == "Economically Disadvantaged"] / Students.Included[Student.Group == "All Students"] * 100,1))) %>%
  filter(Student.Group =="All Students")
absenteeism <- rbind(absenteeism_state, absenteeism_districts)






##### Dropout Data ##### 
dropout_district <- fread(here("/data/education/dropout_district.csv")) %>% 
  rename(c("year" = "School.Year", "District.Name" = "Resident.School.Name", "District.ID"="Resident.District.ID")) %>%
  mutate(Dropout.Rate = as.numeric(Dropout.Rate)) %>% select(-County)
dropout_state <- fread(here("/data/education/dropout_state.csv")) %>%
  rename("year" = "School.Year", "District.Name" = "County") %>% 
  mutate(District.ID = 9999, Dropout.Rate = as.numeric(Dropout.Rate)) %>%
  select("year", "District.ID","District.Name","Fall.Membership","Dropout.Rate")
dropout <- rbind(dropout_state, dropout_district)






##### Report card data: % qualified teachers, on time graduation#####  
#Columns differ per report :(
rc_dist_1415 <- fread(here("/data/education/rc_districts_1415.csv")) %>% 
  select("District ID", "District Name", "County", "Highly Qualified Teachers Pct", "State Four Year Graduation Rate") %>% mutate(year = 1415) %>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", 
           "Teacher.Experience.Pct"="Highly Qualified Teachers Pct", "State.On.Time.Grad.Rate" = "State Four Year Graduation Rate")) %>%
  mutate(Teacher.Experience.Pct = as.numeric(gsub("%", "", Teacher.Experience.Pct)), 
         State.On.Time.Grad.Rate = as.numeric(State.On.Time.Grad.Rate))


rc_dist_1516 <- fread(here("/data/education/rc_districts_1516.csv")) %>%
  select("District ID", "District Name", "County", "Highly Qualified Teachers Pct", "Four Year Graduation Rate 2014-15", "State Four Year Graduation Rate") %>% mutate(year =1516)%>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", "Teacher.Experience.Pct"="Highly Qualified Teachers Pct", 
           "On.Time.Grad.Rate.1415" = "Four Year Graduation Rate 2014-15",
           "State.On.Time.Grad.Rate" = "State Four Year Graduation Rate"))%>%
  mutate(Teacher.Experience.Pct = as.numeric(gsub("%", "", Teacher.Experience.Pct)), 
         On.Time.Grad.Rate.1415 = as.numeric(On.Time.Grad.Rate.1415),
         State.On.Time.Grad.Rate = as.numeric(State.On.Time.Grad.Rate))


### HIghly qualified Teachers doesn't exist here :(
rc_dist_1617 <- fread(here("/data/education/rc_districts_1617.csv")) %>%
  select("District ID", "District Name", "County", "Four Year Graduation Rate 2015-16", "State Four Year Graduation Rate") %>% mutate(year=1617)%>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", 
           "On.Time.Grad.Rate.1516" = "Four Year Graduation Rate 2015-16",
           "State.On.Time.Grad.Rate" = "State Four Year Graduation Rate"))%>%
  mutate(On.Time.Grad.Rate.1516 = as.numeric(On.Time.Grad.Rate.1516),
         Free.Reduced.Priced.Lunch = NA,
         State.On.Time.Grad.Rate = as.numeric(State.On.Time.Grad.Rate))

### HIghly qualified Teachers doesn't exist here :( | dropout rate not here
rc_dist_1718 <- fread(here("/data/education/rc_districts_1718.csv")) %>%
  select("District ID", "District Name", "County", "Free/Reduced Priced Lunch","On-Time Graduation", "On-Track to Graduate Change Value", "Oregon On-Time Graduation Average")%>% mutate(year = 1718) %>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", "Free.Reduced.Priced.Lunch" = "Free/Reduced Priced Lunch",
           "On.Time.Grad.Rate" = "On-Time Graduation","On.Time.Grad.Rate.Change" = "On-Track to Graduate Change Value",
           "State.On.Time.Grad.Rate" = "Oregon On-Time Graduation Average")) %>%
  mutate(Free.Reduced.Priced.Lunch = as.numeric(gsub(">", "",gsub("%", "", Free.Reduced.Priced.Lunch))),
         On.Time.Grad.Rate = as.numeric(gsub("%", "", On.Time.Grad.Rate)), 
         On.Time.Grad.Rate.Change = as.numeric(gsub(">", "",gsub("%", "", On.Time.Grad.Rate.Change))),
         State.On.Time.Grad.Rate = as.numeric(gsub("%", "", State.On.Time.Grad.Rate)))
rc_dist_1718$On.Time.Grad.Rate.Change[is.na(rc_dist_1718$On.Time.Grad.Rate.Change)] <- 0


#percentages included as character
rc_dist_1819 <- fread(here("/data/education/rc_districts_1819.csv")) %>%
  select("District ID", "District Name", "County", "Free/Reduced Priced Lunch","Teacher Experience","On-Time Graduation", "Oregon On-Time Graduation Average") %>% mutate(year = 1819)%>%
  rename(c("District.ID" = "District ID", "District.Name" = "District Name", "Free.Reduced.Priced.Lunch" = "Free/Reduced Priced Lunch",
           "Teacher.Experience.Pct" = "Teacher Experience",
           "On.Time.Grad.Rate" = "On-Time Graduation","State.On.Time.Grad.Rate" = "Oregon On-Time Graduation Average"))  %>%
  mutate(Free.Reduced.Priced.Lunch = as.numeric(gsub(">", "",gsub("%", "", Free.Reduced.Priced.Lunch))),
         Teacher.Experience.Pct = as.numeric(gsub("%", "", Teacher.Experience.Pct)),
         On.Time.Grad.Rate = as.numeric(gsub("%", "", On.Time.Grad.Rate)), 
         State.On.Time.Grad.Rate = as.numeric(gsub("%", "", State.On.Time.Grad.Rate)))


#merge on.time.grad rates to the correct data table. left outer join
rc_dist_1415 <- merge(x = rc_dist_1415, y = select(rc_dist_1516, -c(State.On.Time.Grad.Rate, year,Teacher.Experience.Pct)), 
                      by = c("District.ID","District.Name","County"), all.x = TRUE) %>%
                rename("On.Time.Grad.Rate" = On.Time.Grad.Rate.1415) %>% mutate(Free.Reduced.Priced.Lunch = NA)%>% 
                select(year, District.ID, District.Name, County, Free.Reduced.Priced.Lunch,Teacher.Experience.Pct, On.Time.Grad.Rate, State.On.Time.Grad.Rate)

rc_dist_1516 <- merge(x = rc_dist_1516, y = select(rc_dist_1617, -c(State.On.Time.Grad.Rate, year, Free.Reduced.Priced.Lunch)), 
                      by = c("District.ID","District.Name","County"), all.x = TRUE)  %>% 
                rename("On.Time.Grad.Rate" = On.Time.Grad.Rate.1516) %>% mutate(Free.Reduced.Priced.Lunch = NA) %>%
                select(year, District.ID, District.Name, County, Free.Reduced.Priced.Lunch, Teacher.Experience.Pct, On.Time.Grad.Rate, State.On.Time.Grad.Rate)

gradrate_1617 <-rc_dist_1718 %>% select(-c(Free.Reduced.Priced.Lunch, State.On.Time.Grad.Rate, year)) %>%
                                 transmute(District.ID = District.ID, District.Name = District.Name,
                                           County = County, On.Time.Grad.Rate = On.Time.Grad.Rate - On.Time.Grad.Rate.Change)
rc_dist_1617 <- merge(x = rc_dist_1617, y = gradrate_1617,  by = c("District.ID","District.Name","County"), all.x = TRUE) %>% 
                mutate(Teacher.Experience.Pct = NA) %>% 
                select(year, District.ID, District.Name, County, Free.Reduced.Priced.Lunch,Teacher.Experience.Pct, On.Time.Grad.Rate, State.On.Time.Grad.Rate)
rc_dist_1718 <- rc_dist_1718 %>% mutate(Teacher.Experience.Pct = NA) %>%
                select(year, District.ID, District.Name, County, Free.Reduced.Priced.Lunch,Teacher.Experience.Pct, On.Time.Grad.Rate, State.On.Time.Grad.Rate)
rc_dist_1819 <- rc_dist_1819 %>%
  select(year, District.ID, District.Name, County, Free.Reduced.Priced.Lunch,Teacher.Experience.Pct, On.Time.Grad.Rate, State.On.Time.Grad.Rate)

rc <- rbind(rc_dist_1415, rc_dist_1516, rc_dist_1617, rc_dist_1718, rc_dist_1819) %>% select(-State.On.Time.Grad.Rate,  -County)
#extract State on time grad rate and store it separately to be merged later.
rc_keep <- c("State.On.Time.Grad.Rate", "year")
rc_state <- data.frame(rbind(rc_dist_1415[1,c(State.On.Time.Grad.Rate, year)], rc_dist_1516[1,c(State.On.Time.Grad.Rate, year)],
                  rc_dist_1617[1,c(State.On.Time.Grad.Rate, year)], rc_dist_1718[1,c(State.On.Time.Grad.Rate, year)],
                  rc_dist_1819[1,c(State.On.Time.Grad.Rate, year)])) 
colnames(rc_state) <- c("State.On.Time.Grad.Rate", "year")
rc_state <- rc_state %>% mutate(On.Time.Grad.Rate = as.numeric(gsub("%", "", State.On.Time.Grad.Rate)), 
                                District.Name = "State Level", District.ID = 9999,
                                Free.Reduced.Priced.Lunch = NA,
                                Teacher.Experience.Pct = NA)%>%
  select(year, District.ID, District.Name, Free.Reduced.Priced.Lunch,Teacher.Experience.Pct, On.Time.Grad.Rate)
rc<-rbind(rc_state,rc)



##### Percent Enrolled in Pre-school #####

prek <- fread(here("/data/education/preschool_enrollment.csv"))



###### Combine Datasets (no prek yet) ##########
combined_ed <- merge(x = ela, y=absenteeism, by = c("year", "District.ID", "District.Name"), all = TRUE) #ela and absenteeism
combined_ed <- merge(x = combined_ed, y = dropout, by = c("year", "District.ID", "District.Name"), all = TRUE) #add dropout data
combined_ed <- merge(x = combined_ed, y = rc, by = c("year", "District.ID", "District.Name"), all = TRUE) #add report card data
#fwrite(combined_ed,here("/data/education/combined_education.csv"))

