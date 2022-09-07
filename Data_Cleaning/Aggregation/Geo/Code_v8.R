#Editted at April 4th 2022
#Data is seperated for counting daily total for trips within peak-time and outside of peak time.
#This is unavoidable since DID for Geo cutoff is done at the daily level. 
#Difference from Previous Version: Using functions for merging.

source("/home/keitaro2/Uber_Chicago_Git/Data_Cleaning/Aggregation/Geo/Aggregate/Code_v3.R")
source("/home/keitaro2/Uber_Chicago_Git/Data_Cleaning/Aggregation/Geo/Slots/Code_v3.R")
source("/home/keitaro2/Uber_Chicago_Git/Data_Cleaning/Aggregation/Geo/RefineOD/Code.R")

path = "/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/Day/"
Aggregate(dta,CTListTrt,COListTrt){
  for (i in 1:3) {
    dtaDayCT =Agg(dta,c(6:21),i,CTListTrt,"Census_Tract","Day")
    dtaWeekCT=Agg(dta,c(6:21),i,CTListTrt,"Census_Tract","Week")
    dtaDayCO =Agg(dta,c(6:21),i,COListTrt,"Community_Area","Day")
    dtaWeekCO=Agg(dta,c(6:21),i,COListTrt,"Community_Area","Week")
    write.csv(dtaDayCT,paste(path,"dtaWeekCT",as.character(i),".csv",sep=""))
    write.csv(dtaWeekCT,paste(path,"dtaWeekCT",as.character(i),".csv",sep=""))
    write.csv(dtaDayCO,paste(path,"dtaWeekCO",as.character(i),".csv",sep=""))
    write.csv(dtaWeekCO,paste(path,"dtaWeekCO",as.character(i),".csv",sep=""))
  }
}


#
## Outside of peak-time
#dtaDayOut=Agg(dta,c(0:5,22,23),dtaOrg(2),CTListTrt)
#write.csv(dtaDayOut,paste(path,"dtaDay_Out.csv",sep=""))
#
#
###--------------------##
#dtaDay1=Agg(dta,c(6:21),1,CTListTrt,"Census_Tract","Day")
#write.csv(dtaDay1,paste(path,"dtaDayCT",as.character(1),".csv",sep=""))
#
#
#dtaWeek1=Agg(dta,c(6:21),1,CTListTrt,"Community_Area","Week")
#write.csv(dtaWeek1,paste(path,"dtaWeekCO",as.character(1),".csv",sep=""))
#dtaWeek3=Agg(dta,c(6:21),3,CTListTrt,"Community_Area","Week")
#write.csv(dtaWeek3,paste(path,"dtaWeekCO",as.character(3),".csv",sep=""))
#dtaWeek5=Agg(dta,c(6:21),5,CTListTrt,"Community_Area","Week")
#write.csv(dtaWeek5,paste(path,"dtaWeekCO",as.character(5),".csv",sep=""))

