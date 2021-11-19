# plots and models for per-meeting data

library(nlme)
library(lme4)
library(tidyverse)
library(dplyr)

setwd("~/Policy - research/COVID focusing event/Congressional Record data/Final coding and data/Final clean data/data - per meeting")

data <- read.csv("data_summary_meeting.csv",header=TRUE)

###################################################################
# PREP DATA #######################################################
###################################################################

#divide cases and deaths by 10,000 to make scale smaller
data <- data %>%
  mutate(a_usdeath_10000=(a_usdeath/10000)) %>%
  mutate(b_uscase_10000=(b_uscase/10000))
summary(data)

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

###################################################################
# PLOTS - reactive attention DV ###################################
###################################################################

# reactive attention per meeting
rct_data<-data.frame(data$dv_rctprop,data$meeting_id)
summary(rct_data)
head(rct_data)

rct_plot <- ggplot(rct_data,aes(x=data.meeting_id,y=data.dv_rctprop,
                                  group=1)) 
rct_plot + geom_point() +
  ggtitle("Reactive attention per meeting") +
  labs(x="Meeting ID",y="Proportion") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

# reactive attention per month
rct_month<-data.frame(data$dv_rctprop,data$yearmonth)
summary(rct_month)
head(rct_month)

rctmonth_plot <- ggplot(rct_month,aes(x=data.yearmonth,y=data.dv_rctprop,
                                group=1)) 
rctmonth_plot + geom_point() +
  ggtitle("Reactive attention per month") +
  labs(x="Month",y="Proportion") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = US deaths/10,000
rct_death<-data.frame(data$dv_rctprop,data$a_usdeath_10000)
summary(rct_death)
head(rct_death)

rctdeath_plot <- ggplot(rct_death,aes(x=data.dv_rctprop,y=data.a_usdeath_10000,
                                      group=1)) 
rctdeath_plot + geom_point() +
  ggtitle("Reactive attention + US deaths") +
  labs(x="Reactive attention",y="US deaths (x10,000)") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = US cases/10,000
rct_case<-data.frame(data$dv_rctprop,data$b_uscase_10000)
summary(rct_case)
head(rct_case)

rct_case_plot <- ggplot(rct_case,aes(x=data.dv_rctprop,y=data.b_uscase_10000,
                                      group=1)) 
rct_case_plot + geom_point() +
  ggtitle("Reactive attention + US cases") +
  labs(x="Reactive attention",y="US cases (x10,000)") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = unemployment
rct_unemploy<-data.frame(data$dv_rctprop,data$h_unemploy)
summary(rct_unemploy)
head(rct_unemploy)

rctemploy_plot <- ggplot(rct_unemploy,aes(x=data.dv_rctprop,y=data.h_unemploy,
                                      group=1)) 
rctemploy_plot + geom_point() +
  ggtitle("Reactive attention + Unemployment") +
  labs(x="Reactive attention",y="Unemployment") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = inequality witnesses
rct_ineq<-data.frame(data$dv_rctprop,data$k_ineqwit)
summary(rct_ineq)
head(rct_ineq)

rct_ineq_plot <- ggplot(rct_ineq,aes(x=data.dv_rctprop,y=data.k_ineqwit,
                                          group=1)) 
rct_ineq_plot + geom_point() +
  ggtitle("Reactive attention + ineq. witnesses") +
  labs(x="Reactive attention",y="# inequality witnesses") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = non-professional witnesses
rct_nonprof<-data.frame(data$dv_rctprop,data$u_nonprof)
summary(rct_nonprof)
head(rct_nonprof)

rct_np_plot <- ggplot(rct_nonprof,aes(x=data.dv_rctprop,y=data.u_nonprof,
                                     group=1)) 
rct_np_plot + geom_point() +
  ggtitle("Reactive attention + nonprof witnesses") +
  labs(x="Reactive attention",y="# nonprof witnesses") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = committee members, top 5 states for deaths
rct_statedeath<-data.frame(data$dv_rctprop,data$lo_state)
summary(rct_statedeath)
head(rct_statedeath)

rct_sd_plot <- ggplot(rct_statedeath,aes(x=data.dv_rctprop,y=data.lo_state,
                                      group=1)) 
rct_sd_plot + geom_point() +
  ggtitle("Reactive attention + top5 (death)") +
  labs(x="Reactive attention",y="Top 5 states, deaths") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = committee members, top 5 states for unemployment
rct_stateemploy<-data.frame(data$dv_rctprop,data$lp_state)
summary(rct_stateemploy)
head(rct_stateemploy)

rct_se_plot <- ggplot(rct_stateemploy,aes(x=data.dv_rctprop,y=data.lp_state,
                                         group=1)) 
rct_se_plot + geom_point() +
  ggtitle("Reactive attention + top5 (unemploy)") +
  labs(x="Reactive attention",y="Top 5 states, unemploy.") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = Republican leaders
rct_rep<-data.frame(data$dv_rctprop,data$r_party)
summary(rct_rep)
head(rct_rep)

rct_rep_plot <- ggplot(rct_rep,aes(x=data.dv_rctprop,y=data.r_party,
                                          group=1)) 
rct_rep_plot + geom_point() +
  ggtitle("Reactive attention + R leaders") +
  labs(x="Reactive attention",y="Republican leaders") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

#DV = reactive attention, IV = committee members, top 5 states for unemployment
rct_stayhome<-data.frame(data$dv_rctprop,data$lq_state)
summary(rct_stayhome)
head(rct_stayhome)

rct_sh_plot <- ggplot(rct_stayhome,aes(x=data.dv_rctprop,y=data.lq_state,
                                       group=1)) 
rct_sh_plot + geom_point() +
  ggtitle("Reactive attention + stay home") +
  labs(x="Reactive attention",y="Stay home states") +
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 5, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"))

###################################################################
# UNIVARIATE MODELS - reactive attention DV #######################
###################################################################

#DV = reactive attention, IV = US deaths/10,000
model_death<-lm(dv_rctprop~a_usdeath_10000,data=data)
summary(model_death)
AIC(model_death)

#DV = reactive attention, IV = US cases/10,000
model_case<-lm(dv_rctprop~b_uscase_10000,data=data)
summary(model_case)
AIC(model_case)

#DV = reactive attention, IV = unemployment
model_unemploy<-lm(dv_rctprop~h_unemploy,data=data)
summary(model_unemploy)
AIC(model_unemploy)

#DV = reactive attention, IV = inequality witnesses
model_ineqwit<-lm(dv_rctprop~k_ineqwit,data=data)
summary(model_ineqwit)
AIC(model_ineqwit)

#DV = reactive attention, IV = committee, top5 states, deaths
model_top5death<-lm(dv_rctprop~lo_state,data=data)
summary(model_top5death)
AIC(model_top5death)

#DV = reactive attention, IV = committee, top5 states, unemploy
model_top5unempl<-lm(dv_rctprop~lp_state,data=data)
summary(model_top5unempl)
AIC(model_top5unempl)

#DV = reactive attention, IV = committee, stay-home states
model_stayhome<-lm(dv_rctprop~lq_state,data=data)
summary(model_stayhome)
AIC(model_stayhome)

#DV = reactive attention, IV = Republican chairs
model_rchair<-lm(dv_rctprop~r_party,data=data)
summary(model_rchair)
AIC(model_rchair)

#DV = reactive attention, IV = nonprof witnesses
model_nonprof<-lm(dv_rctprop~u_nonprof,data=data)
summary(model_nonprof)
AIC(model_nonprof)

#DV = reactive attention, IV = COVID present/absent
model_covid<-lm(dv_rctprop~covid_dummy,data=data)
summary(model_covid)
AIC(model_covid)

#DV = reactive attention, IV = house (chamber)
model_house<-lm(dv_rctprop~house_dummy,data=data)
summary(model_house)
AIC(model_house)

###################################################################
# MULTIVARIATE MODELS - reactive attention DV #####################
###################################################################

#DV = reactive attention
#IV = US deaths/10,000, unemployment, committee top5 unemploy,
#Republican chairs, nonprof witnesses & COVID dummy
multimodel1<-lm(dv_rctprop~a_usdeath_10000+
                 b_uscase_10000+
                 h_unemploy+
                 lp_state+
                 r_party+
                 u_nonprof+
                 covid_dummy,data=data)
summary(multimodel1)
AIC(multimodel1)

#DV = reactive attention
#IV = unemployment, nonprof witnesses
multimodel2<-lm(dv_rctprop~h_unemploy,data=data)
summary(multimodel2)
AIC(multimodel2)

###################################################################
# UNIVARIATE MODELS - emerg dis witness as DV #####################
###################################################################

#PER MEETING #####################################
#DV = emerg dis witneses, IV = reactive attention
model_edwit1<-lm(dv_edwit~dv_rctprop,data=data)
summary(model_edwit1)
AIC(model_edwit1)

#DV = emerg dis witneses, IV = us deaths
model_edwit2<-lm(dv_edwit~a_usdeath_10000,data=data)
summary(model_edwit2)
AIC(model_edwit2)

#DV = emerg dis witneses, IV = us cases
model_edwit3<-lm(dv_edwit~b_uscase_10000,data=data)
summary(model_edwit3)
AIC(model_edwit3)

#DV = emerg dis witneses, IV = unemployment
model_edwit4<-lm(dv_edwit~h_unemploy,data=data)
summary(model_edwit4)
AIC(model_edwit4)

#DV = emerg dis witneses, IV = inequality witnesses
model_edwit5<-lm(dv_edwit~k_ineqwit,data=data)
summary(model_edwit5)
AIC(model_edwit5)

#DV = emerg dis witneses, IV = committee, top 5 deaths
model_edwit6<-lm(dv_edwit~lo_state,data=data)
summary(model_edwit6)
AIC(model_edwit6)

#DV = emerg dis witneses, IV = committee, top 5 unemployment
model_edwit7<-lm(dv_edwit~lp_state,data=data)
summary(model_edwit7)
AIC(model_edwit7)

#DV = emerg dis witneses, IV = committee, stay-home states
model_edwit8<-lm(dv_edwit~lq_state,data=data)
summary(model_edwit8)
AIC(model_edwit8)

#DV = emerg dis witneses, IV = Republican chairs
model_edwit9<-lm(dv_edwit~r_party,data=data)
summary(model_edwit9)
AIC(model_edwit9)

#DV = emerg dis witneses, IV = Republican chairs
model_edwit10<-lm(dv_edwit~u_nonprof,data=data)
summary(model_edwit10)
AIC(model_edwit10)

#DV = emerg dis witneses, IV = Republican chairs
model_edwit11<-lm(dv_edwit~covid_dummy,data=data)
summary(model_edwit11)
AIC(model_edwit11)

###################################################################
# MULTIVARIATE MODELS - emerg dis witness as DV ###################
###################################################################

#DV = emerging disease witnesses
#IV = unemployment, COVID dummy
multi_edwit1<-lm(dv_edwit~h_unemploy+covid_dummy,data=data)
summary(multi_edwit1)
AIC(multi_edwit1)

###################################################################
# UNIVARIATE MODELS - emerg dis witness as DV #####################
# PER MONTH #######################################################

#might not be the right way to do this
#total number of ed witnesses and total number of reactive witnesses
#are not independent of each other...

#count number of ed witnesses per month
wit_month <- aggregate(data$dv_edwit,
                           by = list(data$yearmonth),
                           FUN = sum)
wit_month

#for reactive, need to pull data from another file
#because can't aggregate proportions, need to count from scratch
rct_data <- read.csv("reactive_proportion.csv",header=TRUE)

class(rct_data$date)
rct_data$date

factor_date <- as.factor(rct_data$date)
class(factor_date)
factor_date

POSIXt_date <- strptime(factor_date,format="%m/%d/%Y")
class(POSIXt_date) 
POSIXt_date

new_date <- as.Date(POSIXt_date,format="%Y-%m-%d")
class(new_date)
new_date

rct_data$date <- new_date
str(data)

rct_data <- rct_data %>% 
  mutate(yearmonth=format(date,"%Y-%m")) %>%
  mutate(year=format(date,"%Y")) 
str(rct_data)

rct_data <- na.omit(rct_data)

react_month <- aggregate(rct_data$react,
                         by = list(rct_data$yearmonth),
                         FUN = sum)
react_month

#join data together

wit_month_df <- data.frame(wit_month)

wit_month_df <- wit_month_df %>%
  mutate(react=react_month$x)
wit_month_df

#rename columns
names(wit_month_df) <- c("month","edwit_month","rct_month")

#DV = emerging disease witnesses
#IV = reactive attention
edmodel<-lm(edwit_month~rct_month,data=wit_month_df)
summary(edmodel)
AIC(edmodel)

###################################################################
# MIXED MODELS - not included in paper ############################
###################################################################

#DV = reactive attention
#IV = unemployment, nonprof witnesses
#random effect = yearmonth
mix1 <- lmer(dv_rctprop~h_unemploy+u_nonprof+
                   (1|yearmonth),data=data)
summary(mix1)
AIC(mix1)
