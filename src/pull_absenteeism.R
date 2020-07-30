library(openxlsx)
library(dplyr)
library(data.table)
library(here) #work in dspg20wasco.rproj

### pulling attendance data from : https://www.oregon.gov/ode/reports-and-data/students/Pages/Attendance-and-Absenteeism.aspx
school_year <- c(1617, 1718, 1819) #year indicating the start of the school year.



#years 1415 and 1516 report url is : notchronicallyabsent_report_1516.xlsx
### Subgroup/Grade level = Student Group
### not chronically absent = regular attenders
#### number chronically absent and percent chronically absent missing.
data1415 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/notchronicallyabsent_report_1415.xlsx", sheet = 1) %>% mutate(year = 1415)
data1516 <- openxlsx::read.xlsx("https://www.oregon.gov/ode/reports-and-data/students/Documents/notchronicallyabsent_report_1516.xlsx", sheet = 1) %>% mutate(year = 1516)
colnames(data1415)[6] <- "Student.Group"
colnames(data1516)[6] <- "Student.Group"

absenteeism <- rbind(data1415, data1516) %>% transmute(District.ID = District.ID, District = District,
                                                       Institution.ID = Institution.ID, Institution = Institution,
                                                       Institution.Type = Institution.Type, 
                                                       Student.Group = Student.Group, Students.Included = as.numeric(Students.Included),
                                                       Number.Regular.Attenders = as.numeric(Students.Not.Chronically.Absent),
                                                       Percent.Regular.Attenders = as.numeric(Percent.of.Students.Not.Chronically.Absent),
                                                       Number.Chronically.Absent = Students.Included - Number.Regular.Attenders,
                                                       Percent.Chronically.Absent = 100 - Percent.Regular.Attenders,
                                                       year = year)


### create functions to automate pull of data.
make_url <- function(school_year){
  base_url <- "https://www.oregon.gov/ode/reports-and-data/students/Documents/regularattenders_report_"
  paste0(base_url, school_year, ".xlsx")
}
get_file <- function(school_year, worksheet_num = 2){
  file_url <- make_url(school_year)
  openxlsx::read.xlsx(file_url, sheet = worksheet_num)
}


## pull data for each year and make column names compatible.
data1617 <- get_file(school_year=1617, 1)[-1,] %>% mutate(year = 1617) %>% 
  rename(Number.Regular.Attenders = Regular.Attenders, Percent.Regular.Attenders = X9,
         Number.Chronically.Absent = Chronically.Absent, Percent.Chronically.Absent = X11)
absenteeism <- rbind(absenteeism, data1617)

data1718 <- get_file(school_year=1718) %>% mutate(year = 1718)%>% 
  rename(Number.Regular.Attenders = Regular.Attenders.Number, Percent.Regular.Attenders = Regular.Attenders.Percent,
         Number.Chronically.Absent = Chronically.Absent.Number) # Percent.Chronically.Absent = Chronically.Absent.%)
colnames(data1718)[11] <- "Percent.Chronically.Absent"
absenteeism <- rbind(absenteeism, data1718)


data1819 <- get_file(school_year=1819) %>% mutate(year = 1819)


### make numeric columns as.numeric type. 
### drawback: information such as <1 or >95 are erased.
absenteeism <- rbind(absenteeism, data1819) %>% mutate(Students.Included = as.numeric(Students.Included),
                                                       Number.Regular.Attenders = as.numeric(Number.Regular.Attenders),
                                                       Percent.Regular.Attenders = as.numeric(Percent.Regular.Attenders),
                                                       Number.Chronically.Absent = as.numeric(Number.Chronically.Absent),
                                                       Percent.Chronically.Absent = as.numeric(Percent.Chronically.Absent))

fwrite(absenteeism, here("data/education/absenteeism.csv"))