library(dplyr)
library(tidyr)

library(MASS)
library(fixest)

library(modelsummary)

path_remote="/home/keitaro2/Uber_Chicago_Git"
path_remote_OL="/home/keitaro2/Uber_Chicago_OL"
setwd(paste(path_remote_OL,"/Results/DID",sep=""))

dtaDay1 = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")
dtaDay1=dtaDay1 %>% 
  group_by(OD_Pair,Date) %>% 
  mutate(ID = cur_group_id())

#Main Results ####
Result=modelsummary(lapply(c(500,1000,2000,3000),regression),output="latex")
fileConn<-file("Results.tex")
writeLines(Result, fileConn)
close(fileConn)


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
