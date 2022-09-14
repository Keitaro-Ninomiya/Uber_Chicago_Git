library(dplyr)
library(tidyr)

library(haven)
library(MASS)
library(fixest)

dtaDay1 = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")
dtaDay1=dtaDay1 %>% 
  group_by(OD_Pair,Date) %>% 
  mutate(ID = cur_group_id())

reg=function(Dist,Char){
  results = fepois(Count~GeoTrt:Post+PairDist|OD_Pair+as_factor(week)+Post,
                   data=dtaDay1 %>% 
                     filter(PU_Dist<Dist|DO_Dist<Dist))
  return(results)
}

regPD=function(Dist,Char){
  results = fepois(Count~GeoTrt:Post+PairDist|as_factor(PU_Tract)+as_factor(DO_Tract)+as_factor(week)+Post,
                   data=dtaDay1 %>% 
                     filter(PU_Dist<Dist|DO_Dist<Dist))
  return(results)
}

report=function(results,Char){
  sink(Char)
  summary(results)
  sink()
}

setwd("/home/keitaro2/Uber_Chicago/DID/Pair_Level/Geo/MainResults")
#2k
results=reg(2000,"2k.txt")
sink("2k.txt")
summary(results,vcov="iid")
sink()
results=regPD(2000,"2k.txt")
sink("2kPD.txt") 
summary(results,vcov="twoway")
sink()

#05k
reg(500,"05k.txt")
regPD(500,"05k.txt")

#1k
reg(1000,"1k.txt")
regPD(1000,"1k.txt")

#3k
reg(3000,"3k.txt")
regPD(3000,"3k.txt")

#4k
reg(4000,"4k.txt")
regPD(4000,"4k.txt")

#5k
reg(5000,"5k.txt")
regPD(5000,"5k.txt")


#Outside
#3k
results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
              data=dtaDay2 %>% 
                filter(PU_Dist<3000|DO_Dist<3000),
              family = poisson(link = "log"))
sink("DIDGeo_TimeFE_Outside.txt")
summary(results)
sink()

#By Border ####
results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
              data=dtaDay1 %>% 
                filter(PU_Dist<1000|DO_Dist<1000,
                       PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
              family = poisson(link = "log"))
sink("DIDGeo_BdNS1k.txt")
summary(results)
sink()

results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
              data=dtaDay1 %>% 
                filter(PU_Dist<2000|DO_Dist<2000,
                       PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
              family = poisson(link = "log"))
sink("DIDGeo_BdNS2k.txt")
summary(results)
sink()

results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
              data=dtaDay1 %>% 
                filter(PU_Dist<3000|DO_Dist<3000,
                       PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
              family = poisson(link = "log"))
sink("DIDGeo_BdNS3k.txt")
summary(results)
sink()

results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
              data=dtaDay1 %>% 
                filter(PU_Dist<500|DO_Dist<500,
                       PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
              family = poisson(link = "log"))
sink("DIDGeo_BdNS05k.txt")
summary(results)
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

