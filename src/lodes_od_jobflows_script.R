library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)


top_12_in <- data.table(read_csv("~/git/dspg20wasco/data/app_12_inflows_wasco.csv"))
top_12_out <- data.table(read_csv("~/git/dspg20wasco/data/app_12_outflows_wasco.csv"))
graypal = "#ADB5BD"

#have some toggle to switch between inflows and outflows
ggplotly(ggplot(top_12_in, aes(x = year)) +
           ggtitle("Number of jobs flowing into Wasco County (2015-2017)") +
           labs(x = "Year", y = "Number of Jobs", colour = "County") +
           geom_line(aes(y = `Hood River County, OR`, color = "Hood River County")) +
           geom_line(aes(y = `Klickitat County, WA`, color = "Klickitat County, WA")) +
           geom_line(aes(y = `Multnomah County, OR`, color = "Multnomah County")) +
           geom_line(aes(y = `Clackamas County, OR`, color = "Clackamas County")) +
           geom_line(aes(y = `Marion County, OR`, color = "Marion County")) +
           geom_line(aes(y = `Washington County, OR`, color = "Washington County")) +
           geom_line(aes(y = `Deschutes County, OR`, color = "Deschutes County")) +
           geom_line(aes(y = `Jefferson County, OR`, color = "Jefferson County")) +
           geom_line(aes(y = `Lane County, OR`, color = "Lane County")) +
           geom_line(aes(y = `Umatilla County, OR`, color = "Umatilla County")) +
           geom_line(aes(y = `Sherman County, OR`, color = "Sherman County")) +
           geom_line(aes(y = `Skamania County, WA`, color = "Skamania County, WA")) +
           scale_x_continuous(breaks = 0:2100))

ggplotly(ggplot(top_12_out, aes(x = year)) +
           ggtitle("Number of jobs flowing out of Wasco County (2015-2017)") +
           labs(x = "Year", y = "Number of Jobs", colour = "County") +
           geom_line(aes(y = `Hood River County, OR`, color = "Hood River County")) +
           geom_line(aes(y = `Multnomah County, OR`, color = "Multnomah County")) +
           geom_line(aes(y = `Clackamas County, OR`, color = "Clackamas County")) +
           geom_line(aes(y = `Klickitat County, WA`, color = "Klickitat County, WA")) +
           geom_line(aes(y = `Deschutes County, OR`, color = "Deschutes County")) +
           geom_line(aes(y = `Washington County, OR`, color = "Washington County")) +
           geom_line(aes(y = `Marion County, OR`, color = "Marion County")) +
           geom_line(aes(y = `Jefferson County, OR`, color = "Jefferson County")) +
           geom_line(aes(y = `Umatilla County, OR`, color = "Umatilla County")) +
           geom_line(aes(y = `Lane County, OR`, color = "Lane County")) +
           geom_line(aes(y = `Sherman County, OR`, color = "Sherman County")) +
           geom_line(aes(y = `Skamania County, WA`, color = "Skamania County, WA")) +
           scale_x_continuous(breaks = 0:2100))


### Mary's edits for consistency with dashboard
top_12_in_melt <- melt(data = data.frame(top_12_in), id.vars = c("year"), 
                       measure.vars = colnames(top_12_in)[-length(top_12_in)]) %>%
  rename(c("county" = "variable", "jobs" = "value")) %>%
  mutate(neighbors = fct_other(county, keep = c("Hood River County, OR", "Klickitat County, WA",
                                              "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA"),
                               other_level = "Other Counties"),
         neighbors = factor(neighbors , levels= c("Other Counties","Hood River County, OR", "Klickitat County, WA",
                                                  "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA")))

ggplotly(ggplot(top_12_in_melt, aes(x=year, y=jobs, group = county, color = neighbors,
                                    text = paste0("County: ", county,
                                                  "<br>Year: ", year,
                                                  "<br>Number of Jobs: ", jobs))) +
           geom_line(size = 1) + 
           geom_point(size = 1.5) +
           scale_colour_manual(name = "County", values = c(graypal, viridis(5, option = "D"))) +
           scale_x_continuous(breaks = 0:2100) +
           theme_minimal() + ggtitle("Number of jobs flowing into Wasco County (2015-2017)") + 
           ylab("Number of Jobs") + xlab("Year"), tooltip = "text") %>% 
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))

top_12_out_melt <- melt(data = top_12_out, id.vars = c("year"), 
                       measure.vars = colnames(top_12_out)[-length(top_12_out)]) %>%
  rename(c("county" = "variable", "jobs" = "value"))%>%
  mutate(neighbors = fct_other(county, keep = c("Hood River County, OR", "Klickitat County, WA",
                                                "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA"),
                               other_level = "Other Counties"),
         neighbors = factor(neighbors , levels= c("Other Counties","Hood River County, OR", "Klickitat County, WA",
                                                  "Jefferson County, OR", "Sherman County, OR", "Skamania County, WA")))


ggplotly(ggplot(top_12_out_melt, aes(x=year, y=jobs, group = county, color = neighbors,
                                    text = paste0("County: ", county,
                                                  "<br>Year: ", year,
                                                  "<br>Number of Jobs: ", jobs))) +
           geom_line(size = 1) + 
           geom_point(size = 1.5) +
           scale_colour_manual(name = "County", values = c(graypal, viridis(5, option = "D"))) +
           scale_x_continuous(breaks = 0:2100) +
           theme_minimal() + ggtitle("Number of jobs flowing out of Wasco County (2015-2017)") + 
           ylab("Number of Jobs") + xlab("Year"), tooltip = "text") %>% 
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove=list("zoom2d","select2d","lasso2d",
                                     "hoverClosestCartesian", "hoverCompareCartesian","resetScale2d"))
