library(dplyr)
library(tidyr)

library(MASS)
library(fixest)

library(modelsummary)

path_remote="/home/keitaro2/Uber_Chicago_Git"
path_remote_OL="/home/keitaro2/Uber_Chicago_OL"
setwd(paste(path_remote_OL,"/Results/DID",sep=""))

#Geo####
dtaDay1 = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")
dtaDay1=dtaDay1 %>% 
  group_by(OD_Pair,Date) %>% 
  mutate(ID = cur_group_id())

#Main Results ####
Result=modelsummary(lapply(c(500,1000,2000,3000),regression),output="latex")
fileConn<-file("Results.tex")
writeLines(Result, fileConn)
close(fileConn)

#Int ####
dta15=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaInt15Month.csv")
dta15 = dta15 %>% 
  mutate(IntTrt = case_when(Hour%in%c(5,22)~1,
                            TRUE~0))

Result=regBD_Int(dta15)
fileConn<-file("Results_Int.tex")
writeLines(Result, fileConn)
close(fileConn)

