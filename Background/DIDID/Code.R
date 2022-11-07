library(dplyr)
library(tidyr)
library(hms)
library(lubridate)

library(MASS)
library(fixest)

library(modelsummary)

path_remote="/home/keitaro2/Uber_Chicago_Git"
path_remote_OL="/home/keitaro2/Uber_Chicago_OL"
setwd(paste(path_remote_OL,"/Background/DIDID",sep=""))

#Int
dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist_Trn.csv")
dta = dta %>% 
  mutate(Start=ymd_hms(Trip.Start.Timestamp),
         End=ymd_hms(Trip.End.Timestamp)) %>% 
  filter(minute(Start)!=0,minute(End)!=0,
         hour(Start)%in%c(5,6,21,22),
         hour(End)%in%c(5,6,21,22)) %>% 
  mutate(IntTrt = case_when(hour(End)==5 & hour(Start)==22~0,
                            TRUE~1),
         Time=case_when(hour(End)%in%c(5,6)~"Morning",
                        TRUE~"Evening"))

#Main Results ####
#Morning
source("Regression/Code.R")
Result=regBD_Int(dta,"Morning","Additional")
fileConn<-file("Results_Int_Add_Morning.tex")
writeLines(Result, fileConn)
close(fileConn)

#Evening
Result=regBD_Int(dta,"Evening","Additional")
fileConn<-file("Results_Int_Add_Evening.tex")
writeLines(Result, fileConn)
close(fileConn)

#Pooled
Result=regBD_Int(dta,"Both","Additional")
fileConn<-file("Results_Add_Int.tex")
writeLines(Result, fileConn)
close(fileConn)
