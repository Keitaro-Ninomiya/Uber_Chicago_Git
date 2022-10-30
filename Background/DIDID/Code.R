library(dplyr)
library(tidyr)

library(MASS)
library(fixest)

library(modelsummary)

path_remote="/home/keitaro2/Uber_Chicago_Git"
path_remote_OL="/home/keitaro2/Uber_Chicago_OL"
setwd(paste(path_remote_OL,"/Results/DIDID",sep=""))

#Geo####
dtaDay1 = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")
dtaDay1=dtaDay1 %>% 
  group_by(OD_Pair,Date) %>% 
  mutate(ID = cur_group_id())

#Int
dta15=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaInt15Month.csv")
dta15 = dta15 %>% 
  mutate(IntTrt = case_when(Hour%in%c(5,22)~1,
                            TRUE~0),
         Time=case_when(Hour%in%c(5,6)~"Morning",
                        TRUE~"Evening"))

#Main Results ####
#Morning
source("Regression/Code.R")
Result=regBD_Int(dta15,"Morning")
fileConn<-file("Results_Int_Morning.tex")
writeLines(Result, fileConn)
close(fileConn)

#Evening
Result=regBD_Int(dta15,"Evening")
fileConn<-file("Results_Int_Evening.tex")
writeLines(Result, fileConn)
close(fileConn)

#Pooled
Result=regBD_Int(dta15,"Both")
fileConn<-file("Results_Int.tex")
writeLines(Result, fileConn)
close(fileConn)
