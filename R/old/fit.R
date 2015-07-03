#' fit
#'
#' calculates average sedimentation rate from Transmittance
#'
#' Calculates absorbance's derivative from tranmisttance and returns average derivative
#'
#' @param dat dataset.
#' @param expr sring. The function to be fitted to transmittance
#' @param par vector. Coeficients used inside expr.
#' @param predictor string. Name of predictor column in dataset
#' @param response string. Name of response column in dataset
#' @return list with mean derivative of absorbance (average sedimentation rate) and vector of plots(trans,absob,deriv)
#' @author Alex Zabeo
#' @examples
#' derive(dat_derive,expr=expr,par=par_derive)
#' @export
fit <- function(dat,expr,par.start,consts,mg,rpm,predictor="time",response="conc",num.points=200){
  print("NEW")
  
  formula = paste(response,expr,sep=" ~ ")
  
  print(formula)
  
  nls(as.formula(formula), data = dat, start = par.start)
  
  new = data.frame(time = seq(min(dat$time),max(dat$time),len=200))
  lines(new$time,predict(fit,newdata=new))
}