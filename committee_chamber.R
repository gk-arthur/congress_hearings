# PROPORTION OF HOUSE COMMITTEES 
# dummy variable: house or senate chamber for each committee

library(dplyr)
library(tidyverse)

setwd()

data <- read.csv("title.csv",header=TRUE)

data <- data %>%
  mutate(house_dummy=0)

data$house_dummy[data$chamber=="H"]=1

data

write.csv(data,"title.csv")
