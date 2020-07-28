library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)


top_10_in <- read_csv("~/git/dspg20wasco/data/app_10_inflows_wasco.csv")
top_10_out <- read_csv("~/git/dspg20wasco/data/app_10_outflows_wasco.csv")


#have some toggle to switch between inflows and outflows
ggplot(top_10_in, aes(x = year)) +
  ggtitle("Number of jobs flowing into Wasco County\nfrom other counties in Oregon from\n2015-2017") +
  labs(x = "Year", y = "Number of Jobs", colour = "County") +
  geom_line(aes(y = `Hood River County, OR`, color = "Hood River County, OR")) +
  geom_line(aes(y = `Multnomah County, OR`, color = "Multnomah County, OR")) +
  geom_line(aes(y = `Clackamas County, OR`, color = "Clackamas County, OR")) +
  geom_line(aes(y = `Marion County, OR`, color = "Marion County, OR")) +
  geom_line(aes(y = `Washington County, OR`, color = "Washington County, OR")) +
  geom_line(aes(y = `Deschutes County, OR`, color = "Deschutes County, OR")) +
  geom_line(aes(y = `Jefferson County, OR`, color = "Jefferson County, OR")) +
  geom_line(aes(y = `Lane County, OR`, color = "Lane County, OR")) +
  geom_line(aes(y = `Umatilla County, OR`, color = "Umatilla County, OR")) +
  geom_line(aes(y = `Sherman County, OR`, color = "Sherman County, OR"))


ggplot(top_10_out, aes(x = year)) +
  ggtitle("Number of jobs flowing from Wasco County\ninto other counties in Oregon from\n2015-2017") +
  labs(x = "Year", y = "Number of Jobs", colour = "County") +
  geom_line(aes(y = `Hood River County, OR`, color = "Hood River County, OR")) +
  geom_line(aes(y = `Multnomah County, OR`, color = "Multnomah County, OR")) +
  geom_line(aes(y = `Clackamas County, OR`, color = "Clackamas County, OR")) +
  geom_line(aes(y = `Deschutes County, OR`, color = "Deschutes County, OR")) +
  geom_line(aes(y = `Washington County, OR`, color = "Washington County, OR")) +
  geom_line(aes(y = `Marion County, OR`, color = "Marion County, OR")) +
  geom_line(aes(y = `Jefferson County, OR`, color = "Jefferson County, OR")) +
  geom_line(aes(y = `Umatilla County, OR`, color = "Umatilla County, OR")) +
  geom_line(aes(y = `Lane County, OR`, color = "Lane County, OR")) +
  geom_line(aes(y = `Sherman County, OR`, color = "Sherman County, OR"))
