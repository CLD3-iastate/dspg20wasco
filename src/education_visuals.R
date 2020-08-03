library(data.table)
library(here)
library(tidyverse)
library(viridis)
library(plyr)

### Preschools : Margins of error much too high to use
# prek <- fread(here("/data/education/preschool_enrollment.csv"))%>% 
#   filter(District.Name == "South Wasco County SD 1" |
#            District.Name == "North Wasco SD 21" |
#            District.Name == "Sherman County SD 1" |
#            District.Name == "Dufur SD 29" |
#            District.Name == "Hood River County SD 1")



ed <- fread(here::here("data/education/combined_education.csv")) %>% 
  mutate(year = factor(year))
ed$year <- revalue(ed$year,c("1415" = "2014-2015", "1516" = "2015-2016", "1617" = "2016-2017",
          "1718" = "2017-2018", "1819" = "2018-2019"))

ed.scaled <- ed %>% mutate(Percent.ELA.Proficient.Change = scale(Percent.ELA.Proficient.Change),
                                  Percent.Chronically.Absent = scale(Percent.Chronically.Absent),
                                  Percent.Economically.Disadvantaged = scale(Percent.Economically.Disadvantaged),
                                  Teacher.Experience.Pct = scale(Teacher.Experience.Pct),
                                  Dropout.Rate = scale(Dropout.Rate),
                                  On.Time.Grad.Rate = scale(On.Time.Grad.Rate))
#are on time graduation and drop out rates strongly correlated? 
# temp <- filter(ed, !is.na(Dropout.Rate), !is.na(On.Time.Grad.Rate))
# cor(temp$Dropout.Rate, temp$On.Time.Grad.Rate) # result: -.598, farily correlated, but not strongly...


ed <- ed %>% filter(District.Name == "South Wasco County SD 1" |
           District.Name == "Jefferson County SD 509J" |
           District.Name == "North Wasco County SD 21" |
           District.Name == "Sherman County SD" |
           District.Name == "Dufur SD 29" |
           District.Name == "Hood River County SD") %>% 
  select(-c("District.ID" ,"Student.Group","Fall.Membership", "Free.Reduced.Priced.Lunch","Percent.Regular.Attenders" ))

ed.scaled <- ed.scaled %>% filter(District.Name == "South Wasco County SD 1" |
                      District.Name == "Jefferson County SD 509J" |
                      District.Name == "North Wasco County SD 21" |
                      District.Name == "Sherman County SD" |
                      District.Name == "Dufur SD 29" |
                      District.Name == "Hood River County SD") %>% 
  select(-c("District.ID" ,"Student.Group","Fall.Membership", "Free.Reduced.Priced.Lunch","Percent.Regular.Attenders" ))

# temp <- filter(ed, !is.na(Dropout.Rate), !is.na(On.Time.Grad.Rate))
# cor(temp$Dropout.Rate, temp$On.Time.Grad.Rate) # result: -0.5199135, farily correlated, but not strongly...



### get data ready for a heat map
ed.melt = melt(ed, id.vars = c("year", "District.Name"),
             measure.vars = c("On.Time.Grad.Rate", "Dropout.Rate" , 
                              "Percent.ELA.Proficient.Change", "Teacher.Experience.Pct", "Percent.Chronically.Absent",
                              "Percent.Economically.Disadvantaged")) 
ed.melt.scaled = melt(ed.scaled, id.vars = c("year", "District.Name"),
               measure.vars = c("On.Time.Grad.Rate", "Dropout.Rate" , 
                                "Percent.ELA.Proficient.Change", "Teacher.Experience.Pct", "Percent.Chronically.Absent",
                                "Percent.Economically.Disadvantaged")) 
# try a heat map for just south wasco 
sw <- filter(ed.melt, District.Name == "South Wasco County SD 1")
ggplot(sw, aes(y = variable, x = factor(year), fill = value)) +
  geom_tile(color = "#ADB5BD") + #gray
  coord_equal()
  

#facet wrap to show all schools
ggplot(ed.melt, aes(y = variable, x = year, fill = value)) +
  geom_tile(color = "#ADB5BD") + #gray
  geom_text(aes(label = round(value,1)), color = "black") +
  coord_equal() + facet_wrap(~District.Name) +
  #scale_fill_viridis(breaks = c(-60, -20, -10, -5, seq(0,100,5))) +
  scale_fill_viridis() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("School Year")



#scale the values.... 
ggplot(ed.melt.scaled, aes(y = variable, x = year, fill = value)) +
  geom_tile(color = "#ADB5BD") + #gray
  geom_text(aes(label = round(value,1)), color = "black") + 
  coord_equal() + facet_wrap(~District.Name) +
  #scale_fill_viridis(breaks = c(-60, -20, -10, -5, seq(0,100,5))) + 
  scale_fill_viridis() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("School Year")