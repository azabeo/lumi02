#' sed_rate_conc
#'
#' sedimentation rate at 1g for a specific concentration
#'
#' generates sedimentation rate (SR) dataset for a specific concentration, fits a line and calculates SR at 1g
#'
#' @param dat data frame
#' @param predictor string
#' @param response string
#' @param rpm_col string. Name of column in dataset with rpm info (NA if none)
#' @param expr expression. The function to be fitted to transmittance
#' @param par vector. Coeficients used inside expr.
#' @return mean sedimentation rate at 1g
#' @author Alex Zabeo
#' @examples
#' sed_rate_conc()
#' @export
sed_rate_all <- function(dat,expr,par.start,consts,predictor="time",response="trans",rpm_col="rpm",conc_col="conc"){
  
  data <- fitEachConc(dat,expr,par.start,consts,predictor,response,rpm_col,conc_col)
  
  res = list()
  
  # convert rpm to rcf
  rcf <- rpm_to_rcf(data$sr[,'rpm'])
  sr <- as.data.frame(cbind(rpm=data$sr[,'rpm'],rcf,k=data$sr[,'k'],std.err=data$sr[,'std.err']))
  
  print(sr)
  
  # fit a (linear?) statistical distribution to the data frame
  rpm.fit <- lm(sr$k ~ sr$rcf)
  
  # extrapolate the mean s.r. at 1g
  fit.1g <- rpm.fit$coefficients[2]*1 + rpm.fit$coefficients[1]
  names(fit.1g)<-NULL
  
  res[[1]]<-mg
  res[[2]]<-fit.1g
  res[[3]]<-rpm.fit
  res[[4]]<-sr
  res[[5]]<-data$fits
  
  lnames<-c("mg","fit.1g","rpm.fit","sr","sr.fits")
  names(res)=lnames
  
  return (res)
}

#' sed_rate_dataframe
#'
#' builds a dataset of mean sedimentation rates (s.r) for different rpms at same concentration
#'
#' subdivides input dataframe for different rpms, calculates mean s.r. and builds a resulting dataset
#'
#' @param dat data frame
#' @param predictor string
#' @param response string
#' @param rpm_col string. Name of column in dataset with rpm info (NA if none)
#' @param expr expression. The function to be fitted to transmittance
#' @param par vector. Coeficients used inside expr.
#' @return data frame with mean s.r. for different rpms
#' @author Alex Zabeo
#' @examples
#' sed_rate_dataframe()
#' @export
fitEachConc <- function(dat,expr,par.start,consts,predictor="time",response="trans",rpm_col="rpm",conc_col="conc"){
  # create a list of dataset for applying derive
  concs = unique(dat[conc_col])
  i=1
  
  fitted = list(sr=data.frame(),fits=list())
  temp_rpm = numeric()
  temp_k = numeric()
  temp_er = numeric()
  
  for(rpm in rpms$rpm){
    print(rpm)
    
    subdat = dat[dat[,rpm_col]==rpm,c(predictor,response)]
    
    print(head(subdat))
    
    fit = fit(subdat,expr,par.start,consts,mg,rpm,predictor,response)
    coeff = summary(fit)$coefficients
    
    temp_rpm[i] = rpm
    temp_k[i] = coeff[1,'Estimate']
    temp_er[i] = coeff[1,'Std. Error']
    fitted$fits[[i]] <- fit
    
    i<-i+1
  }
  fitted$sr <- as.data.frame(cbind(temp_rpm,temp_k,temp_er))
  
  print(temp_k)
  
  names(fitted$sr)=c("rpm","k","std.err")
  
  return(fitted)
}