# ordering states by number of deaths

setwd()
data <- read.csv("cases_deaths_states.csv", header = TRUE)

library(tidyverse); library(dplyr)

# select month 
month <- data.frame(data$state,data$X12.1.2020)
month

# list top 5
month_top5 <- month %>% arrange(desc(data.X12.1.2020))
head(month_top5)

