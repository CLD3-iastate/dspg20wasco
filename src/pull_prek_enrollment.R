library(data.table)
library(tidyverse)
library(tidycensus)
library(here)

years <- c(1415,1516,1617,1718,1819)
##### Percent Enrolled in Pre-school #####
#C01 = total
#C02 = Percent
#C03 = Public school total 
# ...
prek <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = 2018, survey = "acs5",
                      variables = c("S1401_C01_013","S1401_C01_014","S1401_C02_014"), output = "wide", cache_table = TRUE) %>%
                mutate(NAME = gsub("\\s*\\([^\\)]+\\)","",gsub(", Oregon", "",gsub("School District", "SD", NAME)))), 
              get_acs(geography = "state", state= "OR", year = 2018, survey = "acs5",
                      variables = c("S1401_C01_013","S1401_C01_014","S1401_C02_014"), output = "wide", cache_table = TRUE) %>%
                mutate(NAME = "State Level")
)%>%
  transmute(GEOID = GEOID, District.Name = NAME, year = 1819, total_prek = S1401_C01_013E, total_prek_moe = S1401_C01_013M, total_prek_enroll = S1401_C01_014E, total_prek_enroll_moe = S1401_C01_014M,
            perc_prek_enroll = S1401_C02_014E, perc_prek_enroll_moe = S1401_C02_014M)
collect <- c(2014, 2015, 2016, 2017)
for(i in 1:length(collect)){
  temp <- rbind(get_acs(geography = "school district (unified)", state= "OR", year = collect[i], survey = "acs5",
                        variables = c("S1401_C01_013","S1401_C01_014","S1401_C02_014"), output = "wide", cache_table = TRUE) %>%
                  mutate(NAME = gsub("\\s*\\([^\\)]+\\)","",gsub(", Oregon", "",gsub("School District", "SD", NAME)))),
                get_acs(geography = "state", state= "OR", year = collect[i], survey = "acs5",
                        variables = c("S1401_C01_013","S1401_C01_014","S1401_C02_014"), output = "wide", cache_table = TRUE) %>%
                  mutate(NAME = "State Level")
  )%>%
    transmute(GEOID = GEOID, District.Name = NAME, year = years[i], total_prek = S1401_C01_013E, total_prek_moe = S1401_C01_013M, total_prek_enroll = S1401_C01_014E, total_prek_enroll_moe = S1401_C01_014M,
              perc_prek_enroll = S1401_C02_014E, perc_prek_enroll_moe = S1401_C02_014M)
  prek <- rbind(prek, temp)
}
#fwrite(prek, here("/data/education/preschool_enrollment.csv"))
