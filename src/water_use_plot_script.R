library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(viridis)

#loading usgs water use data (already prepped for plotting)
water_use_by_sector_t <- data.table(readRDS("~/git/dspg20wasco/data/app_usgs_water_use.Rds"))
water_use_plot <- ggplot(data = water_use_by_sector_t, aes(x = year, group = 1)) +
  ggtitle("Water Use in Wasco County by Sector (1985-2015)") +
  labs(x = "Year", y = "Millions of Gallons per Day", color = NULL) +
  geom_line(aes(y = `Aquaculture Water Use (mGal/D)`, color = "Aquaculture Water Use")) +
  geom_line(aes(y = `Commercial Water Use (mGal/D)`, color = "Commercial Water Use")) +
  geom_line(aes(y = `Domestic Water Use (mGal/D)`, color = "Domestic Water Use")) +
  geom_line(aes(y = `Industrial Water Use (mGal/D)`, color = "Industrial Water Use")) +
  geom_line(aes(y = `Livestock Water Use (mGal/D)`, color = "Livestock Water Use")) +
  geom_line(aes(y = `Mining Water Use (mGal/D)`, color = "Mining Water Use")) +
  geom_line(aes(y = `Total Water supplied to Public (mGal/D)`, color = "Resident Water Use")) +
  geom_line(aes(y = `Wastewater Treatment (mGal/D)`, color = "Wastewater Treatment"))


### Mary's edits to match the rest of the plots on the dashboard.
water_use_melt <- melt(data = water_use_by_sector_t, id.vars = c("year"), 
                       measure.vars = colnames(water_use_by_sector_t)[-length(water_use_by_sector_t)]) %>%
  rename(c("sector" = "variable", "gallons" = "value"))
water_use_melt$sector <- recode(water_use_melt$sector, "Aquaculture Water Use (mGal/D)" = "Aquaculture",
                   "Commercial Water Use (mGal/D)" = "Commercial",
                   "Domestic Water Use (mGal/D)" ="Domestic",
                   "Industrial Water Use (mGal/D)" = "Industrial",
                   "Irrigation Water Use (mGal/D)" = "Irrigation",
                   "Livestock Water Use (mGal/D)" = "Livestock",
                   "Mining Water Use (mGal/D)" = "Mining",
                   "Total Water supplied to Public (mGal/D)"= "Total Water supplied to Public",
                   "Wastewater Treatment (mGal/D)" = "Wastewater Treatment")

ggplotly(ggplot(water_use_melt, aes(x=year, y=gallons, group = sector, color = sector,
                                  text = paste0("Sector: ", sector,
                                                "<br>Year: ", year,
                                                "<br>Water Use: ", gallons, " (mGal/D)"))) +
           geom_line(size = 1) + 
           geom_point(size = 1.5) +
           scale_colour_manual(name = "Sector", values = viridis(9, option = "D")) +
           theme_minimal() + ggtitle("Water Use in Wasco County by Sector (1985-2015)") + 
           ylab("Millions of Gallons per Day (mGal/D)") + xlab("Year"), tooltip = "text") %>% 
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
  
