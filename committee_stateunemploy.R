# committee members - state affiliations
# top 5 states, COVID deaths
# variable: lp_state

library(tidyverse)
library(dplyr)

setwd("~/Policy - research/COVID focusing event/Congressional Record data/Final coding and data/Final clean data/data - per meeting")

data <- read.csv("committeemembers_r.csv",header=TRUE)

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

#Jan 2018
data$jan_2018[data$yearmonth=="2018-01" & data$state=="VI"]=1
data$jan_2018[data$yearmonth=="2018-01" & data$state=="PR"]=1
data$jan_2018[data$yearmonth=="2018-01" & data$state=="AK"]=1
data$jan_2018[data$yearmonth=="2018-01" & data$state=="DC"]=1
data$jan_2018[data$yearmonth=="2018-01" & data$state=="WV"]=1

#Feb 2018
data$feb_2018[data$yearmonth=="2018-02" & data$state=="PR"]=1
data$feb_2018[data$yearmonth=="2018-02" & data$state=="VI"]=1
data$feb_2018[data$yearmonth=="2018-02" & data$state=="AK"]=1
data$feb_2018[data$yearmonth=="2018-02" & data$state=="DC"]=1
data$feb_2018[data$yearmonth=="2018-02" & data$state=="WV"]=1

#Mar 2018
data$mar_2018[data$yearmonth=="2018-03" & data$state=="PR"]=1
data$mar_2018[data$yearmonth=="2018-03" & data$state=="VI"]=1
data$mar_2018[data$yearmonth=="2018-03" & data$state=="AK"]=1
data$mar_2018[data$yearmonth=="2018-03" & data$state=="DC"]=1
data$mar_2018[data$yearmonth=="2018-03" & data$state=="WV"]=1

#Apr 2018
data$apr_2018[data$yearmonth=="2018-04" & data$state=="PR"]=1
data$apr_2018[data$yearmonth=="2018-04" & data$state=="VI"]=1
data$apr_2018[data$yearmonth=="2018-04" & data$state=="AK"]=1
data$apr_2018[data$yearmonth=="2018-04" & data$state=="DC"]=1
data$apr_2018[data$yearmonth=="2018-04" & data$state=="WV"]=1

#May 2018
data$may_2018[data$yearmonth=="2018-05" & data$state=="PR"]=1
data$may_2018[data$yearmonth=="2018-05" & data$state=="VI"]=1
data$may_2018[data$yearmonth=="2018-05" & data$state=="AK"]=1
data$may_2018[data$yearmonth=="2018-05" & data$state=="DC"]=1
data$may_2018[data$yearmonth=="2018-05" & data$state=="WV"]=1

#Jun 2018
data$jun_2018[data$yearmonth=="2018-06" & data$state=="VI"]=1
data$jun_2018[data$yearmonth=="2018-06" & data$state=="PR"]=1
data$jun_2018[data$yearmonth=="2018-06" & data$state=="AK"]=1
data$jun_2018[data$yearmonth=="2018-06" & data$state=="DC"]=1
data$jun_2018[data$yearmonth=="2018-06" & data$state=="WV"]=1

#Jul 2018
data$jul_2018[data$yearmonth=="2018-07" & data$state=="VI"]=1
data$jul_2018[data$yearmonth=="2018-07" & data$state=="PR"]=1
data$jul_2018[data$yearmonth=="2018-07" & data$state=="AK"]=1
data$jul_2018[data$yearmonth=="2018-07" & data$state=="DC"]=1
data$jul_2018[data$yearmonth=="2018-07" & data$state=="WV"]=1

#Aug 2018
data$aug_2018[data$yearmonth=="2018-08" & data$state=="PR"]=1
data$aug_2018[data$yearmonth=="2018-08" & data$state=="VI"]=1
data$aug_2018[data$yearmonth=="2018-08" & data$state=="AK"]=1
data$aug_2018[data$yearmonth=="2018-08" & data$state=="DC"]=1
data$aug_2018[data$yearmonth=="2018-08" & data$state=="WV"]=1

#Sep 2018
data$sep_2018[data$yearmonth=="2018-09" & data$state=="PR"]=1
data$sep_2018[data$yearmonth=="2018-09" & data$state=="VI"]=1
data$sep_2018[data$yearmonth=="2018-09" & data$state=="AK"]=1
data$sep_2018[data$yearmonth=="2018-09" & data$state=="DC"]=1
data$sep_2018[data$yearmonth=="2018-09" & data$state=="WV"]=1

#Oct 2018
data$oct_2018[data$yearmonth=="2018-10" & data$state=="PR"]=1
data$oct_2018[data$yearmonth=="2018-10" & data$state=="VI"]=1
data$oct_2018[data$yearmonth=="2018-10" & data$state=="AK"]=1
data$oct_2018[data$yearmonth=="2018-10" & data$state=="DC"]=1
data$oct_2018[data$yearmonth=="2018-10" & data$state=="WV"]=1

#Nov 2018
data$nov_2018[data$yearmonth=="2018-11" & data$state=="PR"]=1
data$nov_2018[data$yearmonth=="2018-11" & data$state=="AK"]=1
data$nov_2018[data$yearmonth=="2018-11" & data$state=="VI"]=1
data$nov_2018[data$yearmonth=="2018-11" & data$state=="DC"]=1
data$nov_2018[data$yearmonth=="2018-11" & data$state=="MS"]=1

#Dec 2018
data$dec_2018[data$yearmonth=="2018-12" & data$state=="PR"]=1
data$dec_2018[data$yearmonth=="2018-12" & data$state=="VI"]=1
data$dec_2018[data$yearmonth=="2018-12" & data$state=="AK"]=1
data$dec_2018[data$yearmonth=="2018-12" & data$state=="DC"]=1
data$dec_2018[data$yearmonth=="2018-12" & data$state=="MS"]=1

#Jan 2019
data$jan_2019[data$yearmonth=="2019-01" & data$state=="PR"]=1
data$jan_2019[data$yearmonth=="2019-01" & data$state=="VI"]=1
data$jan_2019[data$yearmonth=="2019-01" & data$state=="AK"]=1
data$jan_2019[data$yearmonth=="2019-01" & data$state=="DC"]=1
data$jan_2019[data$yearmonth=="2019-01" & data$state=="MS"]=1

#Feb 2019
data$feb_2019[data$yearmonth=="2019-02" & data$state=="PR"]=1
data$feb_2019[data$yearmonth=="2019-02" & data$state=="VI"]=1
data$feb_2019[data$yearmonth=="2019-02" & data$state=="AK"]=1
data$feb_2019[data$yearmonth=="2019-02" & data$state=="DC"]=1
data$feb_2019[data$yearmonth=="2019-02" & data$state=="MS"]=1

#Mar 2019
data$mar_2019[data$yearmonth=="2019-03" & data$state=="PR"]=1
data$mar_2019[data$yearmonth=="2019-03" & data$state=="VI"]=1
data$mar_2019[data$yearmonth=="2019-03" & data$state=="AK"]=1
data$mar_2019[data$yearmonth=="2019-03" & data$state=="DC"]=1
data$mar_2019[data$yearmonth=="2019-03" & data$state=="MS"]=1

#Apr 2019
data$apr_2019[data$yearmonth=="2019-04" & data$state=="PR"]=1
data$apr_2019[data$yearmonth=="2019-04" & data$state=="AK"]=1
data$apr_2019[data$yearmonth=="2019-04" & data$state=="VI"]=1
data$apr_2019[data$yearmonth=="2019-04" & data$state=="DC"]=1
data$apr_2019[data$yearmonth=="2019-04" & data$state=="MS"]=1

#May 2019
data$may_2019[data$yearmonth=="2019-05" & data$state=="PR"]=1
data$may_2019[data$yearmonth=="2019-05" & data$state=="AK"]=1
data$may_2019[data$yearmonth=="2019-05" & data$state=="VI"]=1
data$may_2019[data$yearmonth=="2019-05" & data$state=="DC"]=1
data$may_2019[data$yearmonth=="2019-05" & data$state=="MS"]=1

#Jun 2019
data$jun_2019[data$yearmonth=="2019-06" & data$state=="PR"]=1
data$jun_2019[data$yearmonth=="2019-06" & data$state=="VI"]=1
data$jun_2019[data$yearmonth=="2019-06" & data$state=="AK"]=1
data$jun_2019[data$yearmonth=="2019-06" & data$state=="DC"]=1
data$jun_2019[data$yearmonth=="2019-06" & data$state=="MS"]=1

#Jul 2019
data$jul_2019[data$yearmonth=="2019-07" & data$state=="PR"]=1
data$jul_2019[data$yearmonth=="2019-07" & data$state=="VI"]=1
data$jul_2019[data$yearmonth=="2019-07" & data$state=="AK"]=1
data$jul_2019[data$yearmonth=="2019-07" & data$state=="MS"]=1
data$jul_2019[data$yearmonth=="2019-07" & data$state=="DC"]=1

#Aug 2019
data$aug_2019[data$yearmonth=="2019-08" & data$state=="PR"]=1
data$aug_2019[data$yearmonth=="2019-08" & data$state=="AK"]=1
data$aug_2019[data$yearmonth=="2019-08" & data$state=="VI"]=1
data$aug_2019[data$yearmonth=="2019-08" & data$state=="MS"]=1
data$aug_2019[data$yearmonth=="2019-08" & data$state=="DC"]=1

#Sep 2019
data$sep_2019[data$yearmonth=="2019-09" & data$state=="PR"]=1
data$sep_2019[data$yearmonth=="2019-09" & data$state=="AK"]=1
data$sep_2019[data$yearmonth=="2019-09" & data$state=="VI"]=1
data$sep_2019[data$yearmonth=="2019-09" & data$state=="MS"]=1
data$sep_2019[data$yearmonth=="2019-09" & data$state=="DC"]=1

#Oct 2019
data$oct_2019[data$yearmonth=="2019-10" & data$state=="PR"]=1
data$oct_2019[data$yearmonth=="2019-10" & data$state=="AK"]=1
data$oct_2019[data$yearmonth=="2019-10" & data$state=="VI"]=1
data$oct_2019[data$yearmonth=="2019-10" & data$state=="MS"]=1
data$oct_2019[data$yearmonth=="2019-10" & data$state=="DC"]=1

#Nov 2019
data$nov_2019[data$yearmonth=="2019-11" & data$state=="PR"]=1
data$nov_2019[data$yearmonth=="2019-11" & data$state=="AK"]=1
data$nov_2019[data$yearmonth=="2019-11" & data$state=="MS"]=1
data$nov_2019[data$yearmonth=="2019-11" & data$state=="DC"]=1
data$nov_2019[data$yearmonth=="2019-11" & data$state=="VI"]=1

#Dec 2019
data$dec_2019[data$yearmonth=="2019-12" & data$state=="PR"]=1
data$dec_2019[data$yearmonth=="2019-12" & data$state=="AK"]=1
data$dec_2019[data$yearmonth=="2019-12" & data$state=="GU"]=1
data$dec_2019[data$yearmonth=="2019-12" & data$state=="MS"]=1
data$dec_2019[data$yearmonth=="2019-12" & data$state=="DC"]=1

#Jan 2020
data$jan_2020[data$yearmonth=="2020-01" & data$state=="PR"]=1
data$jan_2020[data$yearmonth=="2020-01" & data$state=="AK"]=1
data$jan_2020[data$yearmonth=="2020-01" & data$state=="MS"]=1
data$jan_2020[data$yearmonth=="2020-01" & data$state=="LA"]=1
data$jan_2020[data$yearmonth=="2020-01" & data$state=="DC"]=1

#Feb 2020
data$feb_2020[data$yearmonth=="2020-02" & data$state=="PR"]=1
data$feb_2020[data$yearmonth=="2020-02" & data$state=="AK"]=1
data$feb_2020[data$yearmonth=="2020-02" & data$state=="MS"]=1
data$feb_2020[data$yearmonth=="2020-02" & data$state=="LA"]=1
data$feb_2020[data$yearmonth=="2020-02" & data$state=="DC"]=1

#Mar 2020
data$mar_2020[data$yearmonth=="2020-03" & data$state=="NV"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="LA"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="NM"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="AZ"]=1
data$mar_2020[data$yearmonth=="2020-03" & data$state=="DC"]=1

#Apr 2020
data$apr_2020[data$yearmonth=="2020-04" & data$state=="NV"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="MI"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="HI"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="RI"]=1
data$apr_2020[data$yearmonth=="2020-04" & data$state=="OH"]=1

#May 2020
data$may_2020[data$yearmonth=="2020-05" & data$state=="NV"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="HI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MI"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="CA"]=1
data$may_2020[data$yearmonth=="2020-05" & data$state=="MA"]=1

#Jun 2020
data$jun_2020[data$yearmonth=="2020-06" & data$state=="AL"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="GU"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="AK"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="AZ"]=1
data$jun_2020[data$yearmonth=="2020-06" & data$state=="AR"]=1

#Jul 2020
data$jul_2020[data$yearmonth=="2020-07" & data$state=="MA"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="NY"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="NV"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="NJ"]=1
data$jul_2020[data$yearmonth=="2020-07" & data$state=="CA"]=1

#Aug 2020
data$aug_2020[data$yearmonth=="2020-08" & data$state=="NV"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="RI"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="HI"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="NY"]=1
data$aug_2020[data$yearmonth=="2020-08" & data$state=="VI"]=1

#Sep 2020
data$sep_2020[data$yearmonth=="2020-09" & data$state=="GU"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="HI"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="VI"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="NV"]=1
data$sep_2020[data$yearmonth=="2020-09" & data$state=="CA"]=1

#Oct 2020
data$oct_2020[data$yearmonth=="2020-10" & data$state=="HI"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="NV"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="VI"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="NY"]=1
data$oct_2020[data$yearmonth=="2020-10" & data$state=="LA"]=1

#Nov 2020
data$nov_2020[data$yearmonth=="2020-11" & data$state=="VI"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="NJ"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="HI"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="NV"]=1
data$nov_2020[data$yearmonth=="2020-11" & data$state=="PR"]=1

#Dec 2020
data$dec_2020[data$yearmonth=="2020-12" & data$state=="GU"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="HI"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="NV"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="PR"]=1
data$dec_2020[data$yearmonth=="2020-12" & data$state=="CA"]=1

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
#JAN 2018-2020
jan18_count <- aggregate(data$jan_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
jan18_count
jan18_count_df <- data.frame(jan18_count)

jan19_count <- aggregate(data$jan_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
jan19_count
jan19_count_df <- data.frame(jan19_count)

jan_count <- aggregate(data$jan_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
jan_count
jan20_count_df <- data.frame(jan_count)

#FEB 2018-2020
feb18_count <- aggregate(data$feb_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
feb18_count
feb18_count_df <- data.frame(feb18_count)

feb19_count <- aggregate(data$feb_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
feb19_count
feb19_count_df <- data.frame(feb19_count)

feb_count <- aggregate(data$feb_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
feb_count
feb20_count_df <- data.frame(feb_count)

#MAR 2018-2020
mar18_count <- aggregate(data$mar_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
mar18_count
mar18_count_df <- data.frame(mar18_count)

mar19_count <- aggregate(data$mar_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
mar19_count
mar19_count_df <- data.frame(mar19_count)

mar_count <- aggregate(data$mar_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
mar_count
mar_count_df <- data.frame(mar_count)

#APR 2018-2020
apr18_count <- aggregate(data$apr_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
apr18_count
apr18_count_df <- data.frame(apr18_count)

apr19_count <- aggregate(data$apr_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
apr19_count
apr19_count_df <- data.frame(apr19_count)

apr_count <- aggregate(data$apr_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
apr_count
apr_count_df <- data.frame(apr_count)

#MAY 2018-2020
may18_count <- aggregate(data$may_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
may18_count
may18_count_df <- data.frame(may18_count)

may19_count <- aggregate(data$may_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
may19_count
may19_count_df <- data.frame(may19_count)

may_count <- aggregate(data$may_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
may_count
may_count_df <- data.frame(may_count)

#JUN 2018-2020
jun18_count <- aggregate(data$jun_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
jun18_count
jun18_count_df <- data.frame(jun18_count)

jun19_count <- aggregate(data$jun_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
jun19_count
jun19_count_df <- data.frame(jun19_count)

jun_count <- aggregate(data$jun_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
jun_count
jun_count_df <- data.frame(jun_count)

#JUL 2020
jul18_count <- aggregate(data$jul_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
jul18_count
jul18_count_df <- data.frame(jul18_count)

jul19_count <- aggregate(data$jul_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
jul19_count
jul19_count_df <- data.frame(jul19_count)

jul_count <- aggregate(data$jul_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
jul_count
jul_count_df <- data.frame(jul_count)

#AUG 2018-2020
aug18_count <- aggregate(data$aug_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
aug18_count
aug18_count_df <- data.frame(aug18_count)

aug19_count <- aggregate(data$aug_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
aug19_count
aug19_count_df <- data.frame(aug19_count)

aug_count <- aggregate(data$aug_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
aug_count
aug_count_df <- data.frame(aug_count)

#SEP 2018-2020
sep18_count <- aggregate(data$sep_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
sep18_count
sep18_count_df <- data.frame(sep18_count)

sep19_count <- aggregate(data$sep_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
sep19_count
sep19_count_df <- data.frame(sep19_count)

sep_count <- aggregate(data$sep_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
sep_count
sep_count_df <- data.frame(sep_count)

#OCT 2018-2020
oct18_count <- aggregate(data$oct_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
oct18_count
oct18_count_df <- data.frame(oct18_count)

oct19_count <- aggregate(data$oct_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
oct19_count
oct19_count_df <- data.frame(oct19_count)

oct_count <- aggregate(data$oct_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
oct_count
oct_count_df <- data.frame(oct_count)

#NOV 2018-2020
nov18_count <- aggregate(data$nov_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
nov18_count
nov18_count_df <- data.frame(nov18_count)

nov19_count <- aggregate(data$nov_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
nov19_count
nov19_count_df <- data.frame(nov19_count)

nov_count <- aggregate(data$nov_2020,
                       by = list(data$meeting_id),
                       FUN = sum)
nov_count
nov_count_df <- data.frame(nov_count)

#DEC 2018-2020
dec18_count <- aggregate(data$dec_2018,
                         by = list(data$meeting_id),
                         FUN = sum)
dec18_count
dec18_count_df <- data.frame(dec18_count)

dec19_count <- aggregate(data$dec_2019,
                         by = list(data$meeting_id),
                         FUN = sum)
dec19_count
dec19_count_df <- data.frame(dec19_count)

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

member_states <- jan20_count_df %>%
  mutate(feb=feb20_count_df$x)%>%
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
  mutate(jan18=jan18_count_df$x)%>%
  mutate(feb18=feb18_count_df$x)%>%
  mutate(mar18=mar18_count_df$x)%>%
  mutate(apr18=apr18_count_df$x)%>%
  mutate(may18=may18_count_df$x)%>%
  mutate(jun18=jun18_count_df$x)%>%
  mutate(jul18=jul18_count_df$x)%>%
  mutate(aug18=aug18_count_df$x)%>%
  mutate(sep18=sep18_count_df$x)%>%
  mutate(oct18=oct18_count_df$x)%>%
  mutate(nov18=nov18_count_df$x)%>%
  mutate(dec18=dec18_count_df$x)%>%
  mutate(jan19=jan19_count_df$x)%>%
  mutate(feb19=feb19_count_df$x)%>%
  mutate(mar19=mar19_count_df$x)%>%
  mutate(apr19=apr19_count_df$x)%>%
  mutate(may19=may19_count_df$x)%>%
  mutate(jun19=jun19_count_df$x)%>%
  mutate(jul19=jul19_count_df$x)%>%
  mutate(aug19=aug19_count_df$x)%>%
  mutate(sep19=sep19_count_df$x)%>%
  mutate(oct19=oct19_count_df$x)%>%
  mutate(nov19=nov19_count_df$x)%>%
  mutate(dec19=dec19_count_df$x)
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
  mutate(lp_state=top5/total)
final

#reduce no. of decimal places to 2
final$lp_state <- round(final$lp_state,digits=2)
final

#save
write.csv(final,file="r_output_committee_top5unemploy.csv",row.names = FALSE)

#plot - PER MEETING
final_df <- data.frame(final)
head(final_df)

meeting_plot <- ggplot(final_df,aes(x=meeting_id,
                                    y=lp_state,
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

ggsave("top5unemploy_meeting.jpg", width = 15, height = 10, units = "cm")
