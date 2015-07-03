lumi<-function(dat,levelName,listOfFactors,listOfModels,predictor="time",response="trans",concentration="C0",absorbance="absorb"){
  #preprocessing
  dat <- absorbance(dat,response)
  dat <- concentration(dat,absorbance,concentration)

  active.dt = dat
  res <- list(previous=list())
  
  if length(listOfFactors) > 0 {
    listOfNames = unique(dat[[listOfFactors[[1]]]])
    active.dt = data.table()
    
    
    
    for (name in listOfNames) {
      subset = dat[get(listOfFactors[1])==name]
      res$previous[[name= 
        slice(subset,name,listOfFactors[-1],listOfExpr[-1],listOfExtrapolateFunc[-1],predictor,response,concentration,absorbance))]]
    }
    #da fare
    active.dt <- buildDataset(res$previous)
    
  }
  res[[name]] = levelName
  res[[dataset]] = active.dt
  res[[fit]] = fit(active.dt,predictor,response,listOfExpr[1]
  res[[row]] = list(c(listOfExtrapolateFunc[[1]](res$fit),levelName)
  
  return res
}

buildDataset<-function(previous){
  
}