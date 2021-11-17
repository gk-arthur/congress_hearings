#committee leaders - republican
#dummy variable, 1=republican, 0=not republican

library(tidyverse)
library(dplyr)

setwd("~/Policy - research/COVID focusing event/Congressional Record data/Final coding and data/Final clean data/data - per meeting")

data <- read.csv("committeemembers_r.csv",header=TRUE)

#add new empty column for leaders (committee chairs)
data <- data %>%
  mutate(chair=NA)

#remove non-chair rows by labelling chair rows as 1
#so all non-chair rows will be NA
data$chair[data$member_id=="chair"]=1
data

data <- na.omit(data)
data

#change D chairs to 0
data$chair[data$party=="D"]=0
data

#create dataframe with only meeting_id and chair columns
data_df <- data.frame(data$meeting_id,data$chair)
data_df

names(data_df)<-c("meeting_id","r_party")
head(data_df)

#save
write.csv(data_df,file="title.csv",row.names = FALSE)

#plot - PER MEETING
head(data_df)

meeting_plot <- ggplot(data_df,aes(x=meeting_id,
                                    y=r_party,
                                    group=1)) 

meeting_plot + geom_col(width = 0.5,
                        colour="black",fill="black") +
  labs(x="Meeting",y="Republican chair") +
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
