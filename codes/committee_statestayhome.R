# committee members - state affiliations
# states with stay at home orders

library(tidyverse)
library(dplyr)

setwd()

data <- read.csv("committeemembers_r.csv",header=TRUE)

# change date to month-year format

data$date
str(data$date)

factor_date <- as.factor(data$date)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

data$date <- new_date
str(data$date)

data <- data %>%
  mutate(yearmonth=format(date,"%Y-%m"))

#create empty columns for months
data <- data %>%
  mutate(jan_2018=0) %>%
  mutate(feb_2018=0) %>%
  mutate(mar_2018=0) %>%
  mutate(apr_2018=0) %>%
  mutate(may_2018=0) %>%
  mutate(jun_2018=0) %>%
  mutate(jul_2018=0) %>%
  mutate(aug_2018=0) %>%
  mutate(sep_2018=0) %>%
  mutate(oct_2018=0) %>%
  mutate(nov_2018=0) %>%
  mutate(dec_2018=0) %>%
  mutate(jan_2019=0) %>%
  mutate(feb_2019=0) %>%
  mutate(mar_2019=0) %>%
  mutate(apr_2019=0) %>%
  mutate(may_2019=0) %>%
  mutate(jun_2019=0) %>%
  mutate(jul_2019=0) %>%
  mutate(aug_2019=0) %>%
  mutate(sep_2019=0) %>%
  mutate(oct_2019=0) %>%
  mutate(nov_2019=0) %>%
  mutate(dec_2019=0) %>%
  mutate(jan_2020=0) %>%
  mutate(feb_2020=0) %>%
  mutate(march_2020=0) %>%
  mutate(april_2020=0) %>%
  mutate(may_2020=0) %>%
  mutate(june_2020=0) %>%
  mutate(jul_2020=0) %>%
  mutate(aug_2020=0) %>%
  mutate(sep_2020=0) %>%
  mutate(oct_2020=0) %>%
  mutate(nov_2020=0) %>%
  mutate(dec_2020=0)

#Alabama
data$april_2020[data$yearmonth=="2020-04" & data$state=="AL"]=1

#Alaska
data$march_2020[data$yearmonth=="2020-03" & data$state=="AK"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="AK"]=1

#Arizona
data$march_2020[data$yearmonth=="2020-03" & data$state=="AZ"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="AZ"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="AZ"]=1

#California
data$march_2020[data$yearmonth=="2020-03" & data$state=="CA"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="CA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="CA"]=1

#Colorado
data$march_2020[data$yearmonth=="2020-03" & data$state=="CO"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="CO"]=1

#Delaware
data$march_2020[data$yearmonth=="2020-03" & data$state=="DE"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="DE"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="DE"]=1

#DC
data$april_2020[data$yearmonth=="2020-04" & data$state=="DC"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="DC"]=1

#Florida
data$april_2020[data$yearmonth=="2020-04" & data$state=="FL"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="FL"]=1

#Georgia
data$april_2020[data$yearmonth=="2020-04" & data$state=="GA"]=1

#Hawaii
data$march_2020[data$yearmonth=="2020-03" & data$state=="HI"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="HI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="HI"]=1

#Idaho
data$march_2020[data$yearmonth=="2020-03" & data$state=="ID"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="ID"]=1

#Illinois
data$march_2020[data$yearmonth=="2020-03" & data$state=="IL"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="IL"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="IL"]=1

#Indiana
data$march_2020[data$yearmonth=="2020-03" & data$state=="IN"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="IN"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="IN"]=1

#Kansas
data$march_2020[data$yearmonth=="2020-03" & data$state=="KS"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="KS"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="KS"]=1

#Louisiana
data$march_2020[data$yearmonth=="2020-03" & data$state=="LA"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="LA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="LA"]=1

#Maine
data$april_2020[data$yearmonth=="2020-04" & data$state=="ME"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="ME"]=1

#Maryland
data$march_2020[data$yearmonth=="2020-03" & data$state=="MD"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="MD"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MD"]=1

#Michigan
data$march_2020[data$yearmonth=="2020-03" & data$state=="MI"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="MI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MI"]=1

#Minnesota
data$march_2020[data$yearmonth=="2020-03" & data$state=="MN"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="MN"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MN"]=1

#Mississippi
data$april_2020[data$yearmonth=="2020-04" & data$state=="MS"]=1

#Missouri
data$april_2020[data$yearmonth=="2020-04" & data$state=="MO"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MO"]=1

#Montana
data$march_2020[data$yearmonth=="2020-03" & data$state=="MT"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="MT"]=1

#Nevada
data$april_2020[data$yearmonth=="2020-04" & data$state=="NV"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="NV"]=1

#New Hampshire
data$march_2020[data$yearmonth=="2020-03" & data$state=="NH"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="NH"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="NH"]=1

#New Jersey
data$march_2020[data$yearmonth=="2020-03" & data$state=="NJ"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="NJ"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="NJ"]=1
data$june_2020[data$yearmonth=="2020-06" & data$state=="NJ"]=1

#North Carolina
data$march_2020[data$yearmonth=="2020-03" & data$state=="NC"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="NC"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="NC"]=1

#Ohio
data$march_2020[data$yearmonth=="2020-03" & data$state=="OH"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="OH"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="OH"]=1

#Oregon
data$march_2020[data$yearmonth=="2020-03" & data$state=="OR"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="OR"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="OR"]=1

#Pennsylvania
data$april_2020[data$yearmonth=="2020-04" & data$state=="PA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="PA"]=1

#Puerto Rico
data$march_2020[data$yearmonth=="2020-03" & data$state=="PR"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="PR"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="PR"]=1

#Rhode Island
data$march_2020[data$yearmonth=="2020-03" & data$state=="RI"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="RI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="RI"]=1

#South Carolina
data$april_2020[data$yearmonth=="2020-04" & data$state=="SC"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="SC"]=1

#Tennessee 
data$april_2020[data$yearmonth=="2020-04" & data$state=="TN"]=1

#Vermont
data$march_2020[data$yearmonth=="2020-03" & data$state=="VT"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="VT"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="VT"]=1

#Virginia
data$march_2020[data$yearmonth=="2020-03" & data$state=="VA"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="VA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="VA"]=1

#Virgin Islands
data$march_2020[data$yearmonth=="2020-03" & data$state=="VI"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="VI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="VI"]=1

#Washington
data$march_2020[data$yearmonth=="2020-03" & data$state=="WA"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="WA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="WA"]=1

#West Virginia
data$march_2020[data$yearmonth=="2020-03" & data$state=="WV"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="WV"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="WV"]=1

#Wisconsin
data$march_2020[data$yearmonth=="2020-03" & data$state=="WI"]=1
data$april_2020[data$yearmonth=="2020-04" & data$state=="WI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="WI"]=1

#change columns to numeric

data$jan_2018 <- as.numeric(data$jan_2018)
data$feb_2018 <- as.numeric(data$feb_2018)
data$mar_2018 <- as.numeric(data$mar_2018)
data$apr_2018 <- as.numeric(data$apr_2018)
data$may_2018 <- as.numeric(data$may_2018)
data$jun_2018 <- as.numeric(data$jun_2018)
data$jul_2018 <- as.numeric(data$jul_2018)
data$aug_2018 <- as.numeric(data$aug_2018)
data$sep_2018 <- as.numeric(data$sep_2018)
data$oct_2018 <- as.numeric(data$oct_2018)
data$nov_2018 <- as.numeric(data$nov_2018)
data$dec_2018 <- as.numeric(data$dec_2018)
data$jan_2019 <- as.numeric(data$jan_2019)
data$feb_2019 <- as.numeric(data$feb_2019)
data$mar_2019 <- as.numeric(data$mar_2019)
data$apr_2019 <- as.numeric(data$apr_2019)
data$may_2019 <- as.numeric(data$may_2019)
data$jun_2019 <- as.numeric(data$jun_2019)
data$jul_2019 <- as.numeric(data$jul_2019)
data$aug_2019 <- as.numeric(data$aug_2019)
data$sep_2019 <- as.numeric(data$sep_2019)
data$oct_2019 <- as.numeric(data$oct_2019)
data$nov_2019 <- as.numeric(data$nov_2019)
data$dec_2019 <- as.numeric(data$dec_2019)
data$jan_2020 <- as.numeric(data$jan_2020)
data$feb_2020 <- as.numeric(data$feb_2020)
data$march_2020 <- as.numeric(data$march_2020)
data$april_2020 <- as.numeric(data$april_2020)
data$may_2020 <- as.numeric(data$may_2020)
data$june_2020 <- as.numeric(data$june_2020)
data$jul_2020 <- as.numeric(data$jul_2020)
data$aug_2020 <- as.numeric(data$aug_2020)
data$sep_2020 <- as.numeric(data$sep_2020)
data$oct_2020 <- as.numeric(data$oct_2020)
data$nov_2020 <- as.numeric(data$nov_2020)
data$dec_2020 <- as.numeric(data$dec_2020)

#calculate proportions per meeting

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
mar_count <- aggregate(data$march_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
mar_count
mar_count_df <- data.frame(mar_count)

#APR 2020
apr_count <- aggregate(data$april_2020,
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
jun_count <- aggregate(data$june_2020,
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
names(jan20_count_df)<-c("meeting_id","jan")
jan20_count_df

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
  mutate(dec=dec_count_df$x)%>%
  mutate(jan18=0)%>%
  mutate(feb18=0)%>%
  mutate(mar18=0)%>%
  mutate(apr18=0)%>%
  mutate(may18=0)%>%
  mutate(jun18=0)%>%
  mutate(jul18=0)%>%
  mutate(aug18=0)%>%
  mutate(sep18=0)%>%
  mutate(oct18=0)%>%
  mutate(nov18=0)%>%
  mutate(dec18=0)%>%
  mutate(jan19=0)%>%
  mutate(feb19=0)%>%
  mutate(mar19=0)%>%
  mutate(apr19=0)%>%
  mutate(may19=0)%>%
  mutate(jun19=0)%>%
  mutate(jul19=0)%>%
  mutate(aug19=0)%>%
  mutate(sep19=0)%>%
  mutate(oct19=0)%>%
  mutate(nov19=0)%>%
  mutate(dec19=0)
member_states

#add all rows together - basically want to compress
#columns, because numbers will only be present in column
#for specific meetings

#first, remove meeting_id column, don't want to calculate sum
member_states_2 <- subset (member_states, select = -Group.1)
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
  mutate(meeting_id=member_states$Group.1)
final

#rename columns to make it easier
names(final) <- c("stayhome","total","meeting_id")
head(final)

#calculate proportion
final <- final %>%
  mutate(lq_state=stayhome/total)
final

#reduce no. of decimal places to 2
final$lq_state <- round(final$lq_state,digits=2)
final

#save
write.csv(final,file="title.csv",row.names = FALSE)

#plot - PER MEETING
final_df <- data.frame(final)
head(final_df)

meeting_plot <- ggplot(final_df,aes(x=meeting_id,
                                    y=lq_state,
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
