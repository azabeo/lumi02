# model.test = function(.SD, .BY, .N, .I, .GRP){
#   print("------ SD ----------------------")
#   print(.SD)
#   print("------ BY ----------------------")
#   print(.BY)
#   print("...")
#   cc<-"C0"
#   print(.BY[[cc]])
#   print("------ N ----------------------")
#   print(.N)
#   print("------ I ----------------------")
#   print(.I)
#   print("------ GRP ----------------------")
#   print(.GRP)
# }

# gen.alias <- function(df,alias){
#   ret <- list()
#   
#   if(alias!=""){
#     ret[['val']]<-list(max(df[[alias]]))
#     ret[['name']] <-list(alias)
#   }
#   return(ret)
# }
# 
# model<-function(model.name,df,by,fitting){
#   a <- gen.alias(df,fitting$alias)
#   
#   f <- get(model.name)
#   f.ret<-f(df,by,fitting)
#   
#   ret<-c(a$val,f.ret)
#   
#   names(ret)<-c(a$name,names(f.ret))
#   return(ret)
# }

model.mean <- function(df,by,fitting,i=1){
  
  name = unlist(strsplit(fitting$resp, separator, fixed = TRUE))[i]
  
  ret<-list(mean(df[[name]]), 
          sd(df[[name]]),
          NA,
          NA)
  
  name = unlist(strsplit(fitting$res.name, separator, fixed = TRUE))[i]
  
  pos=""
  if(i>1){pos=i}
  
  names(ret)<-c(name,paste0("std.err",pos),paste0("rsquare",pos),paste0("fit",pos))
  return(ret)
}

model.repetition <- function(df,by,fitting) {
  #logd("REPETITION","")
  model.mean(df,by,fitting)
}

model.conc <- function(df,by,fitting) {
  
  logd("MODEL")
  
  logd("DATA",df)
  
  consts = by[[fitting$consts]]
  logd("CONSTS",consts)
  
  #rpm =  df[,min(rpm)]
  
  formula <- as.formula(fitting$formula)
  logd("FORMULA",formula)
  
  start = eval(parse(text=paste('list(', fitting$par.start, ')'))) 
  logd("START",start)
  
  fit <- nls(formula, data = df, start = start)
  logd("FIT",fit)
  
  ret<-list(summary(fit)$parameters[1,'Estimate'], 
            summary(fit)$parameters[1,'Std. Error'],
            NA,
            fit=list(a=fit))
  
  names(ret)<-c(fitting$res.name,"std.err","rsquare","fit")
  return(ret)
}

# model.conc.old <- function(df,by,fitting) {
#   
#   print("--- MODEL")
#   
#   print(df)
#   
#   consts = by[[fitting$consts]]
#   print(consts)
#   
#   rpm =  df[,min(rpm)]
#   
#   formula <- as.formula(fitting$formula)
#   print(formula)
#   
#   start = eval(parse(text=paste('list(', fitting$par.start, ')'))) 
#   print(start)
#   
#   fit <- nls(formula, data = df, start = start)
#   print (fit)
#   
#   ret<-list(rpm,
#             summary(fit)$parameters[1,'Estimate'], 
#             summary(fit)$parameters[1,'Std. Error'],
#             NA,
#             fit=list(a=fit))
#   
#   names(ret)<-c("rpm",fitting$res.name,"std.err","rsquare","fit")
#   return(ret)
# }

model.rcf <- function(df,by,fitting) {
  
  logd("MODEL","")
  
  logd("DATA",df)
  
  logd("FITTING",fitting)
  logd("FORMULA",fitting$formula)
  
  formula <- as.formula(fitting$formula)
  logd("FORMULA",formula)
  
  #fit <- lm(paste(resp,pred,sep=" ~ "), data = df)
  fit <- lm(formula, data = df)
  logd("FIT",fit)
  
  #ret <- vector("list", 4)
  d <- data.frame(1)
  #names(d)<-c(fitting$pred)
  names(d)<-c(unlist(strsplit(fitting$pred, separator, fixed = TRUE))[1])
  logd("D",d)
  
  p <- predict(fit,d,se.fit = TRUE)
  logd("P",p)
  
  ret<-list(p$fit[[1]], 
            p$se.fit[[1]],
            summary(fit)$r.squared,
            fit=list(a=fit))
  
#   ret<-list(intercept=coef(fit)[1], 
#             slope=coef(fit)[2],
#             rsquare=summary(fit)$r.squared,
#             fit=list(a=fit))
  
  names(ret)<-c(fitting$res.name,"std.err","rsquare","fit")
  return(ret)
}

model.C0 <- function(df,by,fitting) {
  model.mean(df,by,fitting)
}

model.particle <- function(df,by,fitting) {
  model.mean(df,by,fitting)
}