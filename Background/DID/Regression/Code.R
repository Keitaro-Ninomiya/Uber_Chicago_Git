regression_Total=function(dta,Dist){
  results = feols(Trip.Total~GeoTrt+Post+GeoTrt:Post|as.factor(week),
                  data=dta %>%
                    filter(PU_Dist<=Dist|DO_Dist<=Dist,
                           PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1))
  return(results)
}

regression_Add=function(dta,Dist){
  results = feols(Additional.Charges~GeoTrt+Post+GeoTrt:Post|as.factor(week),
                   data=dta %>%
                     filter(PU_Dist<=Dist|DO_Dist<=Dist,
                            PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1))
  return(results)
}

regression_Dist=function(dta,Dist){
  dta = dta %>% mutate(DistPrice=Fare/Trip.Miles)
  results = feols(DistPrice~GeoTrt+Post+GeoTrt:Post|as.factor(week),
                  data=dta %>%
                    filter(PU_Dist<=Dist|DO_Dist<=Dist,
                           PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1))
  return(results)
}

regression_Time=function(dta,Dist){
  dta = dta %>% mutate(Trip.Minutes=Trip.Seconds/60,
                       TimePrice=Fare/Trip.Minutes)
  results = feols(TimePrice~GeoTrt+Post+GeoTrt:Post|as.factor(week),
                  data=dta %>%
                    filter(PU_Dist<=Dist|DO_Dist<=Dist,
                           PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1))
  return(results)
}

regBD=function(dta,Var){
  if(Var=="Total"){
    
  }
  if(Var=="Additional"){
    return(modelsummary(lapply(c(500,1000,2000,3000,Inf),dta=dta,regression_Add),stars = TRUE,output="latex"))
  }
  if(Var=="Dist"){
    return(modelsummary(lapply(c(500,1000,2000,3000,Inf),dta=dta,regression_Dist),stars = TRUE,output="latex"))
  }
  else{
    return(modelsummary(lapply(c(500,1000,2000,3000,Inf),dta=dta,regression_Time),stars = TRUE,output="latex"))
  }
  
}