library(data.table)
CO2 <- rep((runif(10, 350,359)), 300) # 10 days, 10 plots, 3 fences
count <- rep((1:10), 300) # 10 days, 10 plots, 3 fences
DOY <-rep(rep(152:161, each=10),30) # 10 measurements/day, 10 plots, 3 fences
fence <- rep(1:3, each=1000) # 10 days, 10 measurements, 10 plots 
plot <- rep(rep(1:10, each=100),3) # 10 days, 10 measurements, 3 fences
flux <- as.data.frame(cbind(CO2, count, DOY, fence, plot))
flux$period <- ifelse(flux$DOY <= 155, 1, ifelse(flux$DOY > 155 & flux$DOY < 158, 2, 3))
flux <- as.data.table(flux)

setkey(flux,count)

flux[,include:=(period==1 & count %in% 2:4) | 
       (period==2 & count %in% 2:6) | 
       (period==3 & count %in% 2:8)]
flux.subset <- flux[(include),]
setkey(flux.subset,fence,plot,DOY)

model <- function(df,name,pred,resp) {
  
  #fit <- lm(CO2 ~ count, data = df)
  fit <- lm(paste(resp,pred,sep=" ~ "), data = df)
  ret <- vector("list", 4)
  
  ret<-list(intercept=coef(fit)[1], 
            slope=coef(fit)[2],
            rsquare=summary(fit)$r.squared,
            fit=list(a=fit))
  
  names(ret)<-c(name,"slope","rsquare","fit")
  return(ret)
}

coefs_flux <- flux.subset[,model(.SD,"ciao","count","CO2"),by="fence,plot,DOY"]
#flux.subset[,c("new1","new2","new3","new4"):=model(.SD),by="fence,plot,DOY"]
