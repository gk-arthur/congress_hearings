library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)

#https://www.tidytextmining.com/tidytext.html

setwd()
#read witness testimony (saved as csv, text copied straight in, not in single cell)
testimony <- read.csv(file="testimony_2018-2020.csv")
testimony

tail(testimony)

#mutate to character (necessary for unnest_tokens)
testimony_chr <- testimony %>%
  mutate_all(as.character)
testimony_chr

#convert to dataframe
testimony_df <- tibble(text=testimony_chr)
testimony_df

#IN EXCEL: replace "shelter in place" with "shelter_in_place"

#restructure in one-token-per-row format (read: https://stackoverflow.com/questions/61760039/tidytext-error-error-in-is-corpus-dfcorpus-ncolcorpus-2-is-not-true)
tidy_testimony <- testimony_chr %>%
  unnest_tokens(word, testimony)
tidy_testimony

#test: try searching for keyword
"lockdown" %in% tidy_testimony$word
"lockdowns" %in% tidy_testimony$word
"shelter_in_place" %in% tidy_testimony$word
"stay_at_home" %in% tidy_testimony$word
"stay_home" %in% tidy_testimony$word
"social_distancing" %in% tidy_testimony$word
"quarantine" %in% tidy_testimony$word
"isolation" %in% tidy_testimony$word
"unemployment" %in% tidy_testimony$word

#test: try counting total number of keywords
sum(tidy_testimony$word == "lockdown")
sum(tidy_testimony$word == "lockdowns")
sum(tidy_testimony$word == "shelter_in_place")
sum(tidy_testimony$word == "stay_at_home")
sum(tidy_testimony$word == "stay_home")
sum(tidy_testimony$word == "social_distancing")
sum(tidy_testimony$word == "quarantine")
sum(tidy_testimony$word == "isolation")
sum(tidy_testimony$word == "unemployment")

#question: what proportion of witnesses mention keyword?

#transform one-token-per-row format into tibble
tidy_testimony_df <- as_tibble(tidy_testimony)
tidy_testimony_df

#filter by keyword 
#filter_keyword <- tidy_testimony_df %>%
  #filter(word=="lockdown")
#filter_keyword

#change date column from character to date (see: https://ampersandacademy.com/tutorials/r-programming/how-to-convert-string-to-date-in-r-programming-using-as-date-function)
#to avoid NA, convert to POSIXt first: https://stackoverflow.com/questions/15566875/as-date-returning-na-while-converting-from-ddmmmyyyy
class(tidy_testimony_df$date)

tidy_testimony_df$date

factor_date <- as.factor(tidy_testimony_df$date)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

tidy_testimony_df$date <- new_date
tidy_testimony_df

#add new column to dataframe for capture keywords as present/absent
#note - to get the right labels for graph, use "Lockdown witnesses" instead of "present"
#and "All witnesses" instead of "absent"

tidy_testimony_df2 <- tidy_testimony_df %>%
  mutate(keyword="All witnesses")
tidy_testimony_df2

#select rows containing keyword in $word column
tidy_testimony_df2[tidy_testimony_df2$word=="shelter_in_place",]

#replace value in $keyword with "present" if a lockdown keyword is present
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="lockdown"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="lockdowns"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="shelter_in_place"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="stay_at_home"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="stay_home"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="social_distancing"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="social_distance"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="isolation"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="quarantine"] <- "Lockdown witnesses"
tidy_testimony_df2$keyword[tidy_testimony_df2$word=="unemployment"] <- "Lockdown witnesses"

tidy_testimony_df2

#plot graph of keyword and total witnesses per month

tidy_testimony_df3 <- tidy_testimony_df2 %>%
  mutate(yearmonth=format(date,"%Y-%m"))
tidy_testimony_df3

tidy_testimony_df3$yearmonth<-as.factor(tidy_testimony_df3$yearmonth)
tidy_testimony_df3$keyword<-as.factor(tidy_testimony_df3$keyword)
tidy_testimony_df3$witness_id<-as.factor(tidy_testimony_df3$witness_id)

tidy_testimony_df3%>%
  mutate(nu=1)%>%
  distinct(witness_id,yearmonth,keyword,nu)%>%
  dplyr::count(yearmonth,keyword, sort = TRUE)%>%
  arrange(yearmonth)%>%
  mutate(Keywords=as.factor(keyword))%>%
  ggplot()+
  geom_bar(aes(yearmonth, n, color=Keywords, fill=Keywords),position = "dodge",stat = "identity")+
  ylab("Count")+
  xlab("Month")+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )

ggsave("./results/lockdownmonth_plot.tiff", width = 20, height = 10, units = "cm")

#export numbers
keyword_table <- tidy_testimony_df3%>%
  mutate(nu=1)%>%
  distinct(witness_id,yearmonth,keyword,nu)%>%
  dplyr::count(witness_id,yearmonth,keyword, sort = TRUE)%>%
  mutate(present_count=0) %>%
  mutate(total_count=0)

keyword_table$witness_id<-as.integer(keyword_table$witness_id)

keyword_table <- as_tibble(keyword_table)
keyword_table

keyword_table[order(keyword_table$witness_id),]

keyword_table$present_count[keyword_table$keyword=="Lockdown witnesses"] <- 1
keyword_table$total_count[keyword_table$keyword=="All witnesses"] <- 1
keyword_table

write.csv(keyword_table,"C:\\Users\\gkarthur\\Documents\\Personal stuff\\Policy\\Policy - research\\COVID focusing event\\Congressional Record data\\Committee meetings\\Diseases\\Final clean data\\lockdown_keyword_dataframe5.csv", row.names = FALSE)

keyword_rate <- tidy_testimony_df3%>%
  mutate(nu=1)%>%
  distinct(witness_id,yearmonth,keyword,nu)%>%
  dplyr::count(yearmonth,keyword, sort = TRUE)%>%
  arrange(yearmonth)
keyword_rate

write.csv(keyword_rate,"C:\\Users\\gkarthur\\Documents\\Personal stuff\\Policy\\Policy - research\\COVID focusing event\\Congressional Record data\\Committee meetings\\Diseases\\Final clean data\\lockdown_keyword_rate.csv", row.names = FALSE)
