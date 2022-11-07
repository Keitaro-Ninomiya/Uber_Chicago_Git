library(modelsummary)

#

regression=function(dta,Dist){
  results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
               data=dta %>% 
                 filter(PU_Dist<Dist|DO_Dist<Dist,
                        PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
               family = poisson(link = "log"))
}

regBD=function(dta){
  return(modelsummary(lapply(c(500,1000,2000,3000,Inf),dta=dta,regression),stars = TRUE,output="latex"))
  }

regressionBD_Int=function(dta,Dist,Time,Var){
  if(Var=="Additional"){
    if(Time=="Both"){
      results = fepois(Additional.Charges~IntTrt+GeoTrt+Post+
                         IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                         Post:GeoTrt:IntTrt+
                         PairDist|PU_Tract+DO_Tract,
                       data=dta %>% 
                         filter(PU_Dist<Dist|DO_Dist<Dist))
    }
    else{
      results = fepois(Additional.Charges~IntTrt+GeoTrt+Post+
                         IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                         Post:GeoTrt:IntTrt+
                         PairDist|PU_Tract+DO_Tract,
                       data=dta %>% 
                         filter(PU_Dist<Dist|DO_Dist<Dist,
                                Time==Time))
    }
    return(results)
  }
  
  if(Var=="Dist"){
    dta = dta %>% mutate(DistPrice=Fare/Trip.Miles)
    if(Time=="Both"){
      results = fepois(DistPrice~IntTrt+GeoTrt+Post+
                         IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                         Post:GeoTrt:IntTrt+
                         PairDist|PU_Tract+DO_Tract,
                       data=dta %>% 
                         filter(PU_Dist<Dist|DO_Dist<Dist))
    }
    else{
      results = fepois(DistPrice~IntTrt+GeoTrt+Post+
                         IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                         Post:GeoTrt:IntTrt+
                         PairDist|PU_Tract+DO_Tract,
                       data=dta %>% 
                         filter(PU_Dist<Dist|DO_Dist<Dist,
                                Time==Time))
    }
    return(results)
  }
  
  if(Var=="Time"){
    dta = dta %>% mutate(Trip.Minutes=Trip.Seconds/60,
                         TimePrice=Fare/Trip.Minutes)
    if(Time=="Both"){
      results = fepois(TimePrice~IntTrt+GeoTrt+Post+
                         IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                         Post:GeoTrt:IntTrt+
                         PairDist|PU_Tract+DO_Tract,
                       data=dta %>% 
                         filter(PU_Dist<Dist|DO_Dist<Dist))
    }
    else{
      results = fepois(TimePrice~IntTrt+GeoTrt+Post+
                         IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                         Post:GeoTrt:IntTrt+
                         PairDist|PU_Tract+DO_Tract,
                       data=dta %>% 
                         filter(PU_Dist<Dist|DO_Dist<Dist,
                                Time==Time))
    }
    return(results)
  }
    
}

regBD_Int=function(dta,Time,Var){
  return(modelsummary(lapply(c(500,1000,2000,3000,Inf),regressionBD_Int,dta=dta,Time=Time,Var=Var),stars = TRUE,output="latex"))
}
sink("DIDID_2k.txt")
summary(results)
sink()


report=function(results,Char){
  sink(Char)
  summary(results)
  sink()
}