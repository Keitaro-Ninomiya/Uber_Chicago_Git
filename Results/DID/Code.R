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
