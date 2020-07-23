## Schools and districts in Wasco County
## Dufur school district 4104410
## - dufur school 410441001162
## Fossil school district 4105250
## - fossil charter school 410525001375
## Jefferson county school district 4106740
## - big muddy elementary 410674001446
## - bridges high school 410674001852
## - buff elementary school 410674001569
## - jefferson county middle school 410674000656
## - jefferson elementary school 410671000769
## - jefferson high school 410671000770
## - jefferson middle school 410671001294
## - madras elementary school 410674000451
## - madras high school 410674000456
## - metoletius middle school 410674000452
## - warm springs academy 410674000454
## south wasco county school district 4100021
## - maupin elementary 410002101166
## - south wasco county high school 410002101167
## sherman county school district 4111250
## - sherman county school 411125001068
## north wasco county schools 4100048
## - chenowith elementary 410004801151
## - colonel wright elementary 410004801155
## - dry hollow elementary 410004801156
## - moiser community 410004801153
## - the dalles high school 410004801159
## - the dalles middle school 410004801158
## - wahtonka community school 410004801833

library(educationdata)

districts <- c("4104410", "4105250", "4106740", "4100021", "4111250", "4100048")

education <-  get_education_data(level = 'schools',
                                 source = 'crdc',
                                 topic = 'enrollment',
                                 by = NA, #list('disability', 'sex'),
                                 filters = list(year = 2015,
                                                #grade = 9:12,
                                                leaid = "4104410"),
                                 add_labels = TRUE)
