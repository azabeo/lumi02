concentration <- function(dat,absorbance="absorb",concentration="C0"){
  library(data.table)
  
  # keys must be set
  k=key(dat)
  dat[,conc:=(get(absorbance)/max(get(absorbance)))*get(concentration),by=k]
}