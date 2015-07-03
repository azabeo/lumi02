lumiLogLevel='DEBUG'
separator = ";"
baseFactorName = "final"

lumi <- function(dat,fittings,response="trans",concentration="C0",absorbance="absorb",rpm="rpm",max.trans=95.39001){
  library(data.table)
  dat <- preprocessing(dat,response,concentration,absorbance,rpm,max.trans)
  slice(dat,fittings)
}

preprocessing <- function(dat,response="trans",conc="C0",absorb="absorb",rpm="rpm",max.trans=95.39001){
  dat[,rcf:=rpm_to_rcf(rpm)]
  setnames(dat, response, "initialTrans")
  dat[,trans:=initialTrans/max.trans]
  dat[,time.step:=rank(time),by=c('particle','repetition','dist.perc','C0','rpm')]
  absorbance(dat,response)
  concentration(dat,absorb,conc)
}

slice <- function(dat,fittings,max.iter=NULL){

  act <- dat
  res = list()
  
  nrows = nrow(fittings)
  names = c(baseFactorName,rev(fittings[['pred']]))
  
  #res <- add(res,paste0(names[c(1:(nrows+1))],collapse = '_'),dat)
  res <- add(res,getNames(names,nrows+1),dat)
  
  if(is.null(max.iter)) max.iter=nrows
  
  for (i in 1:max.iter) {
    logd("ITERATION",i,'INFO')
    
    #name = paste0(names[c(1:(nrows-i+1))],collapse = '_')
    name = getNames(names,nrows-i+1)
    logd("NAME:",name,'INFO')
    
    #by = fittings[['pred']][c(-1:-i)]
    #by = unlist(strsplit(by, separator, fixed = TRUE))
    by = getNames(fittings[['pred']],-i,TRUE)
    logd("BY:",by)
    
    fitt <- fittings[i]
    logd("FIT:",fitt)
    
    j=1
    temp=NULL
    for(model.name in unlist(strsplit(fittings[[i,'model']], separator, fixed = TRUE)) ){
      model <- get(model.name)
      logd("j",j)
      logd("MODEL:",model.name)
      
      if(!(is.null(temp))){
        temp = merge(temp,act[,model(.SD,.BY,fitt,j),by=by],by=by)
      }else{
        temp = act[,model(.SD,.BY,fitt),by=by]
      }
      j=j+1
    }
    
    
    #temp = act[,model(.SD,.BY,fitt),by=by]
    #print(temp)
    
    res <- add(res, name, temp)
    
    #logd("RES",res)
    
    act = res[[i+1]]
#     listOfModels[[1]]<-NULL
#     listOfPredictors[[1]]<-NULL
#     listOfResponses[[1]]<-NULL
    
    i=i+1
  }
  
  return (res)
}