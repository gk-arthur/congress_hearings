# DESCRIPTIVE PLOTS FOR PAPER
# CASES, DEATHS & UNEMPLOYMENT

library(dplyr)
library(tidyverse)

setwd("~/Policy - research/COVID focusing event/Congressional Record data/Final coding and data/Final clean data/data - per meeting")

#COVID CASES & DEATHS - read file #### DO NOT USE
### USE ALL DISEASES SECTION BELOW INSTEAD ######
cd_data <- read.csv("COVID cases and deaths_month_forR.csv",header=TRUE)
cd_data

cd_data <- na.omit(cd_data)
cd_data

#change date format
class(cd_data$month.1)
cd_data$month.1

factor_date <- as.factor(cd_data$month.1)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

cd_data$month.1 <- new_date
str(cd_data)

cd_data <- cd_data %>% 
  mutate(yearmonth=format(month.1,"%Y-%m")) %>%
  mutate(year=format(month.1,"%Y")) 
str(cd_data)

#change class of cases/deaths columns
#NOTE: _month columns are the total cases/deaths per month
#i.e. not cumulative
cd_data$uscase_cumul<-as.numeric(cd_data$uscase_cumul)
cd_data$usdeath_cumul<-as.numeric(cd_data$uscase_cumul)
cd_data$uscase_month<-as.numeric(cd_data$uscase_month)
cd_data$usdeath_month<-as.numeric(cd_data$usdeath_month)

#COVID cases per month
cases_month <- data.frame(cd_data$yearmonth,cd_data$uscase_month)
cases_month 

cases_plot <- ggplot(cases_month,aes(x=cd_data.yearmonth,
                                       y=cd_data.uscase_month,
                                       group=1)) 

cases_plot + geom_line(colour="black") +
  labs(x="Month",y="COVID-19 Cases") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 8),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("cases_month.jpg", width = 15, height = 10, units = "cm")

#COVID deaths per month
death_month <- data.frame(cd_data$yearmonth,cd_data$usdeath_month)
death_month 

deaths_plot <- ggplot(death_month,aes(x=cd_data.yearmonth,
                                     y=cd_data.usdeath_month,
                                     group=1)) 

deaths_plot + geom_line(colour="black") +
  labs(x="Month",y="COVID-19 Deaths") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 8),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("deaths_month.jpg", width = 15, height = 10, units = "cm")

#UNEMPLOYMENT - read file
unemploy_data <- read.csv("unemployment_month_forR.csv",header=TRUE)
unemploy_data

#change date format
class(unemploy_data$month.1)
unemploy_data$month.1

factor_date <- as.factor(unemploy_data$month.1)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

unemploy_data$month.1 <- new_date
str(unemploy_data)

unemploy_data <- unemploy_data %>% 
  mutate(yearmonth=format(month.1,"%Y-%m")) %>%
  mutate(year=format(month.1,"%Y")) 
str(unemploy_data)

#unemployment per month
unemploy_month <- data.frame(unemploy_data$yearmonth,
                             unemploy_data$unemploy_month)
unemploy_month 

unemploy_plot <- ggplot(unemploy_month,aes(x=unemploy_data.yearmonth,
                                      y=unemploy_data.unemploy_month,
                                      group=1)) 

unemploy_plot + geom_line(colour="black") +
  labs(x="Month",y="Unemployment rate") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("unemploy_month.jpg", width = 15, height = 10, units = "cm")

# ALL DISEASES - COVID & INFLUENZA COMBINED ##############
# read influenza file
flu_data <- read.csv("Influenza cases deaths_per month_forR.csv",header=TRUE)
flu_data

#change date format
class(flu_data$MMWR.week.end)
flu_data$MMWR.week.end

factor_date <- as.factor(flu_data$MMWR.week.end)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

flu_data$MMWR.week.end <- new_date
str(flu_data)

flu_data <- flu_data %>% 
  mutate(yearmonth=format(MMWR.week.end,"%Y-%m"))
str(flu_data)

#count total number of deaths per month
flu_data$weektotal_deaths <- as.numeric(flu_data$weektotal_deaths)

death_month <- aggregate(flu_data$weektotal_deaths,
                        by = list(flu_data$yearmonth),
                        FUN = sum)
death_month

#convert to data frame
death_df <- data.frame(death_month)
death_df
head(death_df)

#change column names
names(death_df) <- c("month","deaths")
head(death_df)

#repeat for cases
#count total number of deaths per month
flu_data$weektotal_cases <- as.numeric(flu_data$weektotal_cases)

case_month <- aggregate(flu_data$weektotal_cases,
                         by = list(flu_data$yearmonth),
                         FUN = sum)
case_month

#convert to data frame
case_df <- data.frame(case_month)
case_df
head(case_df)

#change column names
names(case_df) <- c("month","cases")
head(case_df)

#COLLECT COVID DATA
covid_data <- read.csv("COVID cases and deaths_month_forR.csv",header=TRUE)
covid_data

#change date format
class(covid_data$month.1)
covid_data$month.1

factor_date <- as.factor(covid_data$month.1)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

covid_data$month.1 <- new_date
str(covid_data)

covid_data <- covid_data %>% 
  mutate(yearmonth=format(month.1,"%Y-%m"))
str(covid_data)

#create data frames for COVID deaths & cases
covidcase_df <- data.frame(covid_data$yearmonth,covid_data$uscase_month)
covidcase_df

coviddeath_df <- data.frame(covid_data$yearmonth,covid_data$usdeath_month)
coviddeath_df

#COMBINE INFLUENZA & COVID DATA
#first: cases
head(case_df)
head(covidcase_df)

names(case_df) <- c("month","flucase")
names(covidcase_df) <- c("month","covidcase")
head(case_df)
head(covidcase_df)

#combine into one dataframe
case_combine <- data.frame(case_df,covidcase_df)
case_combine 

#remove month columns (need to sum rows)
case_combine_2 <- subset(case_combine, select = -c(month,month.1) )
case_combine_2

#add rows together
case_combine_2 <-cbind(case_combine_2, sum = rowSums(case_combine_2))
case_combine_2

#add month and "sum" columns into one data frame
case_combine_sum <- data.frame(case_combine$month,case_combine_2$sum)
case_combine_sum

#plot cases
case_plot <- ggplot(case_combine_sum,aes(x=case_combine.month,y=case_combine_2.sum,
                                           group=1)) 

case_plot + geom_line(colour="black") +
  labs(x="Month",y="U.S. Cases") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("allcases_month.jpg", width = 15, height = 10, units = "cm")

#second: deaths
head(death_df)
head(coviddeath_df)

names(death_df) <- c("month","fludeath")
names(coviddeath_df) <- c("month","coviddeath")
head(death_df)
head(coviddeath_df)

#combine into one dataframe
death_combine <- data.frame(death_df,coviddeath_df)
death_combine 

#remove month columns (need to sum rows)
death_combine_2 <- subset(death_combine, select = -c(month,month.1) )
death_combine_2

#add rows together
death_combine_2 <-cbind(death_combine_2, sum = rowSums(death_combine_2))
death_combine_2

#add month and "sum" columns into one data frame
death_combine_sum <- data.frame(death_combine$month,death_combine_2$sum)
death_combine_sum

#plot cases
death_plot <- ggplot(death_combine_sum,aes(x=death_combine.month,y=death_combine_2.sum,
                                         group=1)) 

death_plot + geom_line(colour="black") +
  labs(x="Month",y="U.S. Deaths") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("alldeaths_month.jpg", width = 15, height = 10, units = "cm")

#export covid, influenza and combined CASE numbers to calculate rate of change
covid_flu_month <- data.frame(case_combine)
covid_flu_month

#save
write.csv(covid_flu_month,file="r_output_covidflucase_month.csv",row.names = FALSE)

#export covid, influenza and combined DEATH numbers to calculate rate of change
covid_flu_month_death <- data.frame(death_combine)
covid_flu_month_death

#save
write.csv(covid_flu_month_death,file="r_output_covidfludeath_month.csv",row.names = FALSE)

