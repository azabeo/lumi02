#' plotit
#'
#' plots a selected result from lumi results list
#'
#' plots a graph for each different value in the 'by' columns. The data to be plotted
#' is filtered using the where list, plots are x vs y. 
#'
#' @param res list. Result of a lumi (or slice).
#' @param fittings data.table. Fittings used for producing res
#' @param where list. List of columns to be used for filtering (e.g. list(particle='NC7000',C0=10))
#' @param by vector. Levels in this column are used for generating separate sub-plots (facets)
#' @param x string. Column to be used for x axis
#' @param y string. Column to be used for y axis
#' @return data.table used to generate the plot
#' @author Alex Zabeo
#' @examples
#' plotit(res,fittings,list(particle='NC7000',C0=10),"rpm","time","conc")
#' @export
plotit <- function(res,fittings,where,by,x,y){
  library(ggplot2)
  
  # get the data to plot points from correct res table filtered by where
  #pos = which(fittings$resp==y)
  pos = getLevel(fittings$resp,y)
  
  nrows = nrow(fittings)
  names = c(baseFactorName,rev(fittings[['pred']]))
  
  name = getNames(names,nrows-pos+2)
  #name = paste0(names[c(1:(nrows-pos+2))],collapse = '_')
  #dat = res[[name]]
  
  # create where string
  #   m = matrix(names(where))
  #   m = cbind(m,apply(m,1,function(x){class(dat[[x]])}))
  #   m = cbind(m,matrix(where))
  #   
  #   w = paste0(apply(m,1,genString),collapse = ' & ')
  
  d <- res[[name]][eval(parse(text=where))]
  
  # get the corresponding fits
  #name = paste0(names[c(1:(nrows-pos+1))],collapse = '_')
  name = getNames(names,nrows-pos+1)
  
  logd("NAME",name)
  
  l = res[[name]][eval(parse(text=where))][['fit']]
  
  # add predicted values to data table
  
  d[,pred:=predict(l[[.GRP]]),by=by]
  
  p <- ggplot(d,  aes_string(x = x, y = y)) + geom_point(colour="orange")
  p <- p + geom_line(data=d , aes_string(x = x, y = 'pred'),colour="blue" )
  if(by!=""){
    p <- p + facet_wrap( as.formula(paste("~", paste0(by,collapse = ' + '))))
  }
  p <- p + labs(title=paste(w,"BY",paste0(by,collapse = ", ")))
  
  print(p)
  return(d)
}

#' plotit
#'
#' plots a selected result from lumi results list
#'
#' plots a graph for each different value in the 'by' columns. The data to be plotted
#' is filtered using the where list, plots are x vs y. 
#'
#' @param res list. Result of a lumi (or slice).
#' @param fittings data.table. Fittings used for producing res
#' @param where list. List of columns to be used for filtering (e.g. list(particle='NC7000',C0=10))
#' @param by vector. Levels in this column are used for generating separate sub-plots (facets)
#' @param x string. Column to be used for x axis
#' @param y string. Column to be used for y axis
#' @return data.table used to generate the plot
#' @author Alex Zabeo
#' @examples
#' plotit(res,fittings,list(particle='NC7000',C0=10),"rpm","time","conc")
#' @export
plotit.old <- function(res,fittings,where,by,x,y){
  library(ggplot2)
  
  # get the data to plot points
  pos = which(fittings$resp==y)
  nrows = nrow(fittings)
  
  names = c("final",rev(fittings[['pred']]))
  
  name = paste0(names[c(1:(nrows-pos+2))],collapse = '_')
  dat = res[[name]]
  
  m = matrix(names(where))
  m = cbind(m,apply(m,1,function(x){class(dat[[x]])}))
  m = cbind(m,matrix(where))
  
  w = paste0(apply(m,1,genString),collapse = ' & ')
  
  d <- dat[eval(parse(text=w))]
  
  # get the corresponding fits
  
  name = paste0(names[c(1:(nrows-pos+1))],collapse = '_')
  
  l = res[[name]][eval(parse(text=w))][['fit']]
  
  # add predicted values to data table
  
  d[,pred:=predict(l[[.GRP]]),by=by]
  
  p <- ggplot(d,  aes_string(x = x, y = y)) + geom_point(colour="orange")
  p <- p + geom_line(data=d , aes_string(x = x, y = 'pred'),colour="blue" )
  if(by!=""){
    p <- p + facet_wrap( as.formula(paste("~", paste0(by,collapse = ' + '))))
  }
  p <- p + labs(title=paste(w,"BY",paste0(by,collapse = ", ")))
  
  print(p)
  return(d)
}

#' genString
#'
#' generates strings for where clause with quotes for characters
#'
#' generates strings for where clause with quotes for characters
#'
#' @param x vector of 3. 1 is column name, 2 is column class, 3 is column value
#' @return string of where clause (e.g. colA == 'value')
#' @author Alex Zabeo
#' @examples
#' genstring(c("colA","character","value"))
genString=function(x){
  a=''
  if(x[2]=='character'){
    a = paste("'",x[3],"'",sep="")
  }else{
    a = x[3]
  }
  paste(x[1],"==",a)
}