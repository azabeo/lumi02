absorbance <- function(dat,response="trans"){
  # Transmittance is first normalized in [0,1] and then the log10 is applied
  library(data.table)
  
  dat[,absorb:=-(log10(get(response)))]
}