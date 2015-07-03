
plot_sr_rcf<-function(dat){
  t <- ggplot(dat$sr.data,  aes(x=rcf,  y=sr.mean))
  t <- t + geom_point(shape=1,col='red',size=5)
  
  t <- t + geom_abline(intercept=dat$fit$coefficients[1],slope=dat$fit$coefficients[2])
  
  my_text = paste("Mg =",dat$mg)
  my_text = paste(my_text,"\nsr 1g =",format(dat$sr1g, scientific = TRUE))
  my_text = paste(my_text,"\nR2 =", decimals(summary(dat$fit)$r.squared,3))
  
  my_grob = grobTree(textGrob(my_text, x=0.1,  y=0.9, 
                              hjust=0,gp=gpar(col="blue", fontsize=15, 
                                              fontface="italic")))
  t <- t + annotation_custom(my_grob)
  
  print(t)
}

plot_derived<-function(deriv_res,predictor="time",response="trans"){
  library("ggplot2")
  library("grid")
  
  x <- seq(min(dat[predictor]),max(dat[predictor]), length.out = num.points)
  plot(dat1)
  lines(x,predict(fit,newdata=as.data.frame(x)))
  
  
  
  t <- ggplot(deriv_res$observ,  aes_string(x=predictor,  y=response)) 
  t <- t + geom_point(shape=1,col='red',size=5)
  t <- t + geom_line(data=deriv_res$trans,  size=1, aes_string(x=predictor,  y=response), col='blue', linetype='dashed')
  
  #a <- ggplot(deriv_res$observ,  aes_string(x=predictor,  y=response)) 
  #a <- a + geom_line(data=deriv_res$absorb,  size=1, aes_string(x=predictor,  y=response), col='violet', linetype='dashed')
  t <- t + geom_abline(intercept = deriv_res$sr.xy["x.a"], slope = deriv_res$sr['mean'])
  if(!is.na(deriv_res$deriv.range["start.x"])){
    t <- t + geom_vline(xintercept = deriv_res$deriv.range["start.x"], col='green')
  }
  if(!is.na(deriv_res$deriv.range["stop.x"])){
    t <- t + geom_vline(xintercept = deriv_res$deriv.range["stop.x"], col='green')
  }
  
  my_text = paste("Mg =",deriv_res$mg," - Rpm=",deriv_res$rpm)
  
  my_grob = grobTree(textGrob(my_text, x=0.6,  y=0.1, 
                              hjust=0,gp=gpar(col="blue", fontsize=15, 
                                              fontface="italic")))
  t <- t + annotation_custom(my_grob)
  
  d <- ggplot(deriv_res$observ,  aes_string(x=predictor,  y=response)) 
  d <- d + geom_line(data=deriv_res$deriv,  size=1, aes_string(x=predictor,  y=response), col='black')
  d <- d + geom_hline(yintercept = deriv_res$deriv.range["start.y"], col='red')
  d <- d + geom_hline(yintercept = deriv_res$deriv.range["stop.y"], col='red')
  d <- d + geom_hline(yintercept = deriv_res$sr['mean'], col='red')
  d <- d + geom_vline(xintercept = deriv_res$sr.mean.root, col='red')
  if(!is.na(deriv_res$deriv.range["start.x"])){
    d <- d + geom_vline(xintercept = deriv_res$deriv.range["start.x"], col='green')
  }
  if(!is.na(deriv_res$deriv.range["stop.x"])){
    d <- d + geom_vline(xintercept = deriv_res$deriv.range["stop.x"], col='green')
  }
  
  multiplot(t, d, cols=1)
  
}