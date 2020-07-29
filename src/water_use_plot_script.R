library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(htmlwidgets)

#loading usgs water use data (already prepped for plotting)
water_use_by_sector_t <- readRDS("~/git/dspg20wasco/data/app_usgs_water_use.Rds")
water_use_plot <- ggplot(data = water_use_by_sector_t, aes(x = year, group = 1)) +
  ggtitle("Water Use in Wasco County, OR by Sector (1985-2015)") +
  labs(x = "Year", y = "Millions of Gallons per Day", color = NULL) +
  geom_line(aes(y = `Aquaculture Water Use (mGal/D)`, color = "Aquaculture Water Use")) +
  geom_line(aes(y = `Commercial Water Use (mGal/D)`, color = "Commercial Water Use")) +
  geom_line(aes(y = `Domestic Water Use (mGal/D)`, color = "Domestic Water Use")) +
  geom_line(aes(y = `Industrial Water Use (mGal/D)`, color = "Industrial Water Use")) +
  geom_line(aes(y = `Livestock Water Use (mGal/D)`, color = "Livestock Water Use")) +
  geom_line(aes(y = `Mining Water Use (mGal/D)`, color = "Mining Water Use")) +
  geom_line(aes(y = `Total Water supplied to Public (mGal/D)`, color = "Resident Water Use")) +
  geom_line(aes(y = `Wastewater Treatment (mGal/D)`, color = "Wastewater Treatment"))

water_use_plot_interactive <- ggplotly(water_use_plot)
