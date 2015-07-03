#' loadData
#'
#' loads necessary data from files 
#'
#' loads necessary data from files 
#'
#' @param data string. name of csv file with particle, C0, rpm, time, trans
#' @param fittings string. name of csv file fith fittings lines (see slice documentation)
#' @return vector of the two loaded tables
#' @author Alex Zabeo
#' @examples
#' decimals(84.05041,2)
#' @export
loadData<-function(data="inst/data.lumi.csv",keys=c('particle','repetition','dist.perc','C0','rpm'),fittings='inst/fittings.csv'){
  library(data.table)
  
  dat<-read.table(data,header=TRUE,sep=",",stringsAsFactors=F)
  dat<-as.data.table(dat)
  setkeyv(dat,keys)
  
  fitts<-read.table(fittings,header=TRUE,sep=",",stringsAsFactors=F)
  fitts<-as.data.table(fitts)
  
  return(list(dat=dat,fittings=fitts))
}