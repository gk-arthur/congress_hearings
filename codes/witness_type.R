# PROPORTION OF NON PROFESSIONAL WITNESSES PER MEETING

library(dplyr)
library(tidyverse)

setwd()

data <- read.csv("witness type_clean.csv",header=TRUE)

#change date format
class(data$date)
data$date

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
str(data)

data <- data %>% 
  mutate(yearmonth=format(date,"%Y-%m")) %>%
  mutate(year=format(date,"%Y")) 
str(data)

#remove "-" and empty spaces in prof and nonprof columns
data$prof[data$prof=="-"]=NA
data$nonprof[data$nonprof=="-"]=NA
data$prof[data$prof==""]=NA
data$nonprof[data$nonprof==""]=NA

data <- na.omit(data)

#count prof witnesses per meeting_id
data$prof <- as.numeric(data$prof)

prof_count <- aggregate(data$prof,
                           by = list(data$meeting_id),
                           FUN = sum)
prof_count
prof_count_df <- data.frame(prof_count)

#count nonprof witnesses per meeting_id
data$nonprof <- as.numeric(data$nonprof)

nonprof_count <- aggregate(data$nonprof,
                        by = list(data$meeting_id),
                        FUN = sum)
nonprof_count
nonprof_count_df <- data.frame(nonprof_count)

#rename columns ready to bind
names(prof_count_df)<-c("meeting_id","prof_n")
head(prof_count_df)

names(nonprof_count_df)<-c("meeting_id","nonprof_n")
head(nonprof_count_df)

#add nonprof column to prof frame
wit_type <- data.frame(prof_count_df %>%
                              mutate(nonprof_n=nonprof_count_df$nonprof_n))
wit_type 

#add column for total number of witnesses
wit_type <- wit_type %>%
  mutate(total_wit=prof_n+nonprof_n)
wit_type

#add column for proportion of reactive attention
wit_type <- wit_type %>%
  mutate(u_nonprof=nonprof_n/total_wit)
wit_type

#reduce no. of decimal places to 2
wit_type$u_nonprof <- round(wit_type$u_nonprof,digits=2)
wit_type

#save
write.csv(wit_type,file="title.csv",row.names = FALSE)

#plot - PER MEETING
nonprof_df <- data.frame(wit_type)
head(nonprof_df)

meeting_plot <- ggplot(nonprof_df,aes(x=meeting_id,
                                   y=u_nonprof,
                                   group=1)) 

meeting_plot + geom_col(width = 0.5,
                        colour="black",fill="black") +
  labs(x="Meeting",y="Proportion of witnesses") +
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

#plot - PER MONTH
#take month data
prof_month <- aggregate(data$prof,
                        by = list(data$yearmonth),
                        FUN = sum)
prof_month

names(prof_month)<-c("yearmonth","prof")
head(prof_month)

nonprof_month <- aggregate(data$nonprof,
                        by = list(data$yearmonth),
                        FUN = sum)
nonprof_month

names(nonprof_month)<-c("yearmonth","nonprof")
head(nonprof_month)

wit_type2 <- data.frame(prof_month %>%
                         mutate(nonprof=nonprof_month$nonprof))
wit_type2 

names(wit_type2)<-c("month","Professional","Non-professional")
head(wit_type2)

data_long <- melt(wit_type2, id = "month")    
head(data_long) 

data_long$variable <- as.factor(data_long$variable)
str(data_long)

data_long$value<-as.numeric(data_long$value)

names(data_long) <- c("month","Witness type","value")
head(data_long)

data_long_df <- data.frame(data_long)
data_long_df 

head(data_long_df)

data_long_df %>% as_tibble(data_long_df) %>%
  ggplot(aes(x=month, y=value, group=Witness.type,color=Witness.type)) +
  geom_line(size = 1.2) +
  scale_color_discrete("Witness type") +
  xlab("Months") +
  ylab("Number of witnesses") +
  theme(#axis.line.x = element_line(size = 0.2, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 12, angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 12),
    axis.title.x = element_text(size = 15, margin = margin(t = 9, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

ggsave("title.jpg", width = 17, height = 10, units = "cm")
