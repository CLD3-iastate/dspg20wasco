library(data.table)
library(tidyverse)
library(tidycensus)
library(here)

##### Perfomance data: #####
# %ELA proficiency change between 3rd & 8th grade | columns consistent for all years
ela1415 <- fread(here("/data/education/performance_districts_1415.csv")) %>% 
  filter(Student.Group == "Total Population (All Students)",
         Grade.Level == "Grade 3" | Grade.Level == "Grade 8") %>% 
  mutate(perc_proficient = as.numeric(`Percent.Proficient.(Level.3.or.4)`)) %>%
  group_by(District) %>% summarise(Academic.Year=Academic.Year, District.ID=District.ID, perc_proficient_change = diff(perc_proficient))
  
#only need for where Student.Group == "Total Population (All Students)" and 
#Grade.Level == "Grade 3" and Grade.Level == "Grade 8"
ela1516 <- fread(here("/data/education/performance_districts_1516.csv")) %>% 
  filter(Student.Group == "Total Population (All Students)",
         Grade.Level == "Grade 3" | Grade.Level == "Grade 8")
#how to calculate the differences
filter(ela1415, Grade.Level == "Grade 8")$perc_proficient-filter(ela1415, Grade.Level == "Grade 3")$perc_proficient

##### Report card data: #####  
#Columns differ per report :(
rc_dist_1415 <- fread(here("/data/education/rc_districts_1415.csv")) %>% 
  select("District ID", "District Name", "County", "Highly Qualified Teachers Pct")
rc_dist_1516 <- fread(here("/data/education/rc_districts_1516.csv")) %>%
  select("District ID", "District Name", "County", "Highly Qualified Teachers Pct","Dropout Rate 2014-15", "Four Year Graduation Rate 2014-15")
### HIghly qualified Teachers doesn't exist here :(
rc_dist_1617 <- fread(here("/data/education/rc_districts_1617.csv")) %>%
  select("District ID", "District Name", "County","Dropout Rate 2015-16", "Four Year Graduation Rate 2015-16") 
### HIghly qualified Teachers doesn't exist here :( | dropout rate not here
rc_dist_1718 <- fread(here("/data/education/rc_districts_1718.csv")) %>%
  select("District ID", "District Name", "County", "Free/Reduced Priced Lunch","On-Time Graduation") 

#percentages included as character
rc_dist_1819 <- fread(here("/data/education/rc_districts_1819.csv")) %>%
  select("District ID", "District Name", "County", "Free/Reduced Priced Lunch","Teacher Experience","On-Time Graduation") 



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