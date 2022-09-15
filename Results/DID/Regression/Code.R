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

regression=function(dta,Dist){
  results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
               data=dta %>% 
                 filter(PU_Dist<Dist|DO_Dist<Dist,
                        PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
               family = poisson(link = "log"))
}

regBD=function(){
  return(modelsummary(lapply(c(500,1000,2000,3000),regression)))
  }

report=function(results,Char){
  sink(Char)
  summary(results)
  sink()
}