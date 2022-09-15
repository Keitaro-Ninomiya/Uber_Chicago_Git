library(dplyr)
library(tidyr)

library(haven)
library(MASS)
library(fixest)

path_remote="/home/keitaro2/Uber_Chicago_Git"
setwd(paste(path_remote,"/Results/DID",sep=""))

dtaDay1 = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")
dtaDay1=dtaDay1 %>% 
  group_by(OD_Pair,Date) %>% 
  mutate(ID = cur_group_id())

#Main Results ####
sink("DIDGeo_BdNS1k.txt")
summary(regBD(dtaDay1,1000))
sink()

sink("DIDGeo_BdNS2k.txt")
summary(regBD(dtaDay1,2000))
sink()

sink("DIDGeo_BdNS3k.txt")
summary(regBD(dtaDay1,3000))
sink()

sink("DIDGeo_BdNS05k.txt")
summary(regBD(dtaDay1,500))
sink()


#Time trend ##Linear time trend
#Same time trend
results = glm(Count~GeoTrt+week+GeoTrt:Post+PairDist,
              data=dtaDay %>% 
                filter(PU_Dist<1000|DO_Dist<1000) %>% 
                mutate(week=ifelse(week<=53&week>=40,week-53,week)),
              family = poisson(link = "log"))
sink("DIDGeo_TimeTrend_Line.txt")
summary(results)
sink()

#Time trend by Distance 
results = glm(Count~GeoTrt+week+GeoTrt:Post+PairDist+PairDist:week,
              data=dtaDay %>% 
                filter(PU_Dist<1000|DO_Dist<1000) %>% 
                mutate(week=ifelse(week<=53&week>=40,week-53,week)),
              family = poisson(link = "log"))
sink("DIDGeo_TimeTrend_Dist.txt")
summary(results)
sink()

#Time trend by units
results = glm(Count~GeoTrt+week+GeoTrt:Post+PairDist+as_factor(OD_Pair)*week,
              data=dtaDay %>% 
                filter(PU_Dist<2000|DO_Dist<2000) %>% 
                mutate(week=ifelse(week<=53&week>=40,week-53,week)),
              family = poisson(link = "log"))
sink("DIDGeo_TimeTrend_Line.txt")
summary(results)
sink()


#Distance
results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+PairDist,
              data=dtaDay %>% filter(PU_Dist<2000|DO_Dist<2000) %>% 
                mutate(week=ifelse(week<=53,week-53+100,week+100)),
              family = poisson(link = "log"))
sink("DIDGeo_TimeTrend2k.txt")
summary(results)
sink()

results = glm(Count~GeoTrt+Post+as_factor(week)+GeoTrt:Post+PairDist,
              data=dtaDay %>% filter(PU_Dist<3000|DO_Dist<3000),
              family = poisson(link = "log"))
sink("DIDGeo_TimeTrend3k.txt")
summary(results)
sink()

