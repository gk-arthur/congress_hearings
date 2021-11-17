# committee members - state affiliations
# top 5 states, COVID deaths

library(tidyverse)
library(dplyr)

setwd()

data <- read.csv("committeemembers_r.csv",header=TRUE)

data <- data %>%
  mutate(jan_2020=0) %>%
  mutate(feb_2020=0) %>%
  mutate(mar_2020=0) %>%
  mutate(apr_2020=0) %>%
  mutate(may_2020=0) %>%
  mutate(jun_2020=0) %>%
  mutate(jul_2020=0) %>%
  mutate(aug_2020=0) %>%
  mutate(sep_2020=0) %>%
  mutate(oct_2020=0) %>%
  mutate(nov_2020=0) %>%
  mutate(dec_2020=0)

#change date format
class(data$date)

factor_date <- as.factor(data$date)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)

data$date <- new_date

data <- data %>%
  mutate(yearmonth=format(date,"%Y-%m"))
data

#March 2020
data$mar_2020[data$yearmonth=="2020-03" & data$state=="NY"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="LA"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="NJ"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="WA"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="MI"]=1

#April 2020
data$apr_2020[data$yearmonth=="2020-04" & data$state=="NY"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="NJ"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="CT"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="MA"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="MI"]=1

#May 2020
data$may_2020[data$yearmonth=="2020-05" & data$state=="NJ"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="CT"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="RI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MS"]=1

#June 2020
data$jun_2020[data$yearmonth=="2020-06" & data$state=="MA"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="GA"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="NJ"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="RI"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="MS"]=1

#July 2020
data$jul_2020[data$yearmonth=="2020-07" & data$state=="AZ"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="GA"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="MS"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="SC"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="TX"]=1

#August 2020
data$aug_2020[data$yearmonth=="2020-08" & data$state=="GA"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="MS"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="LA"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="FL"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="SC"]=1

#September 2020
data$sep_2020[data$yearmonth=="2020-09" & data$state=="GA"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="GU"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="AR"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="MS"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="FL"]=1

#October 2020
data$oct_2020[data$yearmonth=="2020-10" & data$state=="ND"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="GA"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="SD"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="MT"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="AR"]=1

#November 2020
data$nov_2020[data$yearmonth=="2020-11" & data$state=="SD"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="ND"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="GA"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="NM"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="MT"]=1

#December 2020
data$dec_2020[data$yearmonth=="2020-12" & data$state=="SD"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="RI"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="ND"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="IA"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="OH"]=1

#change columns to numeric

data$jan_2020 <- as.numeric(data$jan_2020)
data$feb_2020 <- as.numeric(data$feb_2020)
data$mar_2020 <- as.numeric(data$mar_2020)
data$apr_2020 <- as.numeric(data$apr_2020)
data$may_2020 <- as.numeric(data$may_2020)
data$jun_2020 <- as.numeric(data$jun_2020)
data$jul_2020 <- as.numeric(data$jul_2020)
data$aug_2020 <- as.numeric(data$aug_2020)
data$sep_2020 <- as.numeric(data$sep_2020)
data$oct_2020 <- as.numeric(data$oct_2020)
data$nov_2020 <- as.numeric(data$nov_2020)
data$dec_2020 <- as.numeric(data$dec_2020)

#calculate proportions per meeting

#first, count witnesses from top 5 states per meeting
#JAN 2020
jan_count <- aggregate(data$jan_2020,
                        by = list(data$meeting_id),
                        FUN = sum)
jan_count
jan_count_df <- data.frame(jan_count)

#FEB 2020
feb_count <- aggregate(data$feb_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
feb_count
feb_count_df <- data.frame(feb_count)

#MAR 2020
mar_count <- aggregate(data$mar_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
mar_count
mar_count_df <- data.frame(mar_count)

#APR 2020
apr_count <- aggregate(data$apr_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
apr_count
apr_count_df <- data.frame(apr_count)

#MAY 2020
may_count <- aggregate(data$may_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
may_count
may_count_df <- data.frame(may_count)

#JUN 2020
jun_count <- aggregate(data$jun_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
jun_count
jun_count_df <- data.frame(jun_count)

#JUL 2020
jul_count <- aggregate(data$jul_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
jul_count
jul_count_df <- data.frame(jul_count)

#AUG 2020
aug_count <- aggregate(data$aug_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
aug_count
aug_count_df <- data.frame(aug_count)

#SEP 2020
sep_count <- aggregate(data$sep_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
sep_count
sep_count_df <- data.frame(sep_count)

#OCT 2020
oct_count <- aggregate(data$oct_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
oct_count
oct_count_df <- data.frame(oct_count)

#NOV 2020
nov_count <- aggregate(data$nov_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
nov_count
nov_count_df <- data.frame(nov_count)

#DEC 2020
dec_count <- aggregate(data$dec_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
dec_count
dec_count_df <- data.frame(dec_count)

#second, count all witnesses per meeting
data <- data %>%
  mutate(nu=1) 

total_member <- aggregate(data$nu,
                          by = list(data$meeting_id),
                          FUN = sum)
total_member
total_member_df <- data.frame(total_member)

#add all dataframes together
names(jan_count_df)<-c("meeting_id","jan")
jan_count_df

member_states <- jan_count_df %>%
  mutate(feb=feb_count_df$x)%>%
  mutate(mar=mar_count_df$x)%>%
  mutate(apr=apr_count_df$x)%>%
  mutate(may=may_count_df$x)%>%
  mutate(jun=jun_count_df$x)%>%
  mutate(jul=jul_count_df$x)%>%
  mutate(aug=aug_count_df$x)%>%
  mutate(sep=sep_count_df$x)%>%
  mutate(oct=oct_count_df$x)%>%
  mutate(nov=nov_count_df$x)%>%
  mutate(dec=dec_count_df$x)
member_states

#add all rows together - basically want to compress
#columns, because numbers will only be present in column
#for specific meetings

#first, remove meeting_id column, don't want to calculate sum
member_states_2 <- subset (member_states, select = -meeting_id)
member_states_2

#add rows together
member_states_2<-cbind(member_states_2, sum = rowSums(member_states_2))
member_states_2

#extract total column
final <- data.frame(member_states_2$sum)
final

#add total members to dataframe
final <- final %>%
  mutate(total_member_df$x)
final

#re-add meeting_id
final <- final %>%
  mutate(meeting_id=member_states$meeting_id)
final

#rename columns to make it easier
names(final) <- c("top5","total","meeting_id")
head(final)

#calculate proportion
final <- final %>%
  mutate(lo_state=top5/total)
final

#reduce no. of decimal places to 2
final$lo_state <- round(final$lo_state,digits=2)
final

#save
write.csv(final,file="title.csv",row.names = FALSE)

#plot - PER MEETING
final_df <- data.frame(final)
head(final_df)

meeting_plot <- ggplot(final_df,aes(x=meeting_id,
                                      y=lo_state,
                                      group=1)) 

meeting_plot + geom_col(width = 0.5,
                        colour="black",fill="black") +
  labs(x="Meeting",y="Proportion of committee") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 12 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 12),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("title.jpg", width = 15, height = 10, units = "cm")
