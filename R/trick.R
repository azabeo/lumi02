trick<-function(){
  a<- loadData()
  #a$dat<-a$dat[!(particle=='NC7000' & C0==6)]
  #a$dat<-a$dat[particle=='NC7000' & rpm>=800]
  preprocessing(a$dat)
  a$dat<-a$dat[particle=='NC7000' & C0>6 & rpm>=800 | (particle=='P25' & time.step<352 & rpm<2800)]
  return(a)
}

# results<- function(){
#   plotit(res,a$fittings,"particle=='NC7000' & C0==6 & rpm==600","","time.avg","m.conc")
#   plotit(res,a$fittings,"particle=='NC7000' & C0==6","rpm","time.avg","m.conc")
#   plotit(res,a$fittings,"particle=='NC7000'","C0","rcf","k")
#   
#   plotit(res,a$fittings,"particle=='P25' & C0==5 & rpm==900","","time.avg","m.conc")
#   plotit(res,a$fittings,"particle=='P25' & C0==10","rpm","time.avg","m.conc")
#   plotit(res,a$fittings,"particle=='P25'","C0","rcf","k")
# }

