library(dplyr)
library(tidyr)

library(MASS)
library(fixest)

library(modelsummary)

path_remote="/home/keitaro2/Uber_Chicago_Git"
path_remote_OL="/home/keitaro2/Uber_Chicago_OL"
setwd(paste(path_remote_OL,"/Results/DID",sep=""))

#Geo####
source(paste(path_remote,"/Results/DID/Regression/Code.R",sep=""))
dtaDay1 = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")
dtaDay1=dtaDay1 %>% 
  group_by(OD_Pair,Date) %>% 
  mutate(ID = cur_group_id())

#Main Results ####
#DID####
Result=regBD(dtaDay1)
fileConn<-file("Results_Geo.tex")
writeLines(Result, fileConn)
close(fileConn)
