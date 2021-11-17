# PROPORTION OF HOUSE COMMITTEES 
# variable = raneff_house

library(dplyr)
library(tidyverse)

setwd("~/Policy - research/COVID focusing event/Congressional Record data/Final coding and data/Final clean data/data - per meeting")

data <- read.csv("committee chambers_permeeting.csv",header=TRUE)

data <- data %>%
  mutate(house_dummy=0)

data$house_dummy[data$chamber=="H"]=1

data

write.csv(data,"r_output_housechamber.csv")
