library(modelsummary)

#

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

regressionBD_Int=function(dta,Dist){
  results = fepois(Count~IntTrt+GeoTrt+Post+
                     IntTrt:GeoTrt+IntTrt:Post+Post:GeoTrt+
                     Post:GeoTrt:IntTrt+
                     PairDist|OD_Pair,
                   data=dta %>% 
                     filter(PU_Dist<Dist|DO_Dist<Dist,
                            PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1))
}

regBD_Int=function(dta){
  return(modelsummary(lapply(c(500,1000,2000,3000),regressionBD_Int,dta=dta),stars = TRUE,output="latex"))
}
sink("DIDID_2k.txt")
summary(results)
sink()


report=function(results,Char){
  sink(Char)
  summary(results)
  sink()
}