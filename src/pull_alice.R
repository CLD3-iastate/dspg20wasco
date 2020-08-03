library(openxlsx)
library(here)
library(pdftools)
library(tidyverse)
#library(tabulizer)

###### ALICE Threshold ########
### county level ALICE data from 2010-2018 every two years. Source of data is from ACS 
alice_county <- openxlsx::read.xlsx("https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_OR.xlsx", sheet = 2)
colnames(alice_county)[11] <- "ALICE.Threshold.HH.under.65";colnames(alice_county)[12] <- "ALICE.Threshold.HH.65.years.and.over"

alice_place <- openxlsx::read.xlsx("https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_OR.xlsx", sheet = 4)

alice_subcounty <- openxlsx::read.xlsx("https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_OR.xlsx", sheet = 5)


### zip code level ALICE data only for 2018. Data source is ACS
alice_zipcode <- openxlsx::read.xlsx("https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_OR.xlsx", sheet = 6)










######## ALICE Household Budget #########
# reading from pdf :o
# following this tutorial: https://rstudio-pubs-static.s3.amazonaws.com/415060_553527fd13ed4f30aae0f1e4483aa970.html
# use tabulizer package: https://datascienceplus.com/extracting-tables-from-pdfs-in-r-using-the-tabulizer-package/
# note::: can't download package dependency rjava
# NO URL AVAILABLE FOR PDF, manual download only :(((((

PDF <- pdf_text("oregon_grass_and_legume_seed_crops_preliminary_estimates_2017.pdf") %>%
  readr::read_lines() #open the PDF inside your project folder
