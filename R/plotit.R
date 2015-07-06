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
plotit <- function(res,fittings,where,by,x,y,pre.facet.label='',post.facet.label=NULL,x.label=NULL,y.label='',auto.title=FALSE,size=2,print=TRUE){
  library(ggplot2)
  
  # get the data to plot points from correct res table filtered by where
  pos = getLevel(fittings$resp,y)
  
  nrows = nrow(fittings)
  names = c(baseFactorName,rev(fittings[['pred']]))
  
  name = getNames(names,nrows-pos+2)

  d <- res[[name]][eval(parse(text=where))]
  
  # get the corresponding fits
  name = getNames(names,nrows-pos+1)
  
  l=NA
  
  has.fit <- tryCatch({
    l = res[[name]][eval(parse(text=where))][['fit']]
    
    if(any(is.na(l))){
      logwarn('No fits at this level')
      #return(NULL)
      FALSE
    }else{TRUE}
  },
  error=function(cond) {
    logwarn(cond)
    # Choose a return value in case of error
    return(FALSE)
  }
  
  )
  
  lr=NA
  
  logd("name",name)
  
  has.rsquare <- tryCatch({
    lr = res[[name]][eval(parse(text=where)),'rsquare',with=FALSE]
    
    if(any(is.na(lr))){
      logwarn('No rsquare at this level')
      #return(NULL)
      FALSE
    }else{TRUE}
  },
  error=function(cond) {
    logwarn(cond)
    # Choose a return value in case of error
    return(FALSE)
  }
  
  )
 
  has.std = any(names(d)=='std.err')
  
  name_labeller <- function(variable,value){
    return(paste0(pre.facet.label,value,post.facet.label))
  }
  
  if(by!=""){
    
    by.order = sort(unique(d[,get(by)]))
    by.order = paste(pre.facet.label,by.order,post.facet.label,sep="")
    
    
    d[,by.name := paste0(pre.facet.label,get(by),post.facet.label)]
    d$by.name <- factor(d$by.name, levels = by.order, ordered = TRUE)
  
  }
  
  p <- ggplot(d,  aes_string(x = x, y = y)) + geom_point(colour="orange",size=size)
  
  if(has.std){
    p <- p + geom_errorbar(aes_string(ymin=paste0(y,'-std.err'), ymax=paste0(y,'+std.err')),
                           width=(size/6), size=(size/3), alpha=0.35, col="green")
    
  }
  
  if(has.fit){
    # add predicted values to data table
    d[,pred:=predict(l[[.GRP]]),by=by]
    
    p <- p + geom_line(data=d , aes_string(x = x, y = 'pred'),colour="blue",size=(size/4) )
  }
  
#   if(has.rsquare){
#     p <- p + geom_text(data=lr, aes(x=1.8, y=5, label=rsquare), 
#               colour="black", inherit.aes=FALSE, parse=FALSE)
# #     my_text = paste("Mg =",deriv_res$mg," - Rpm=",deriv_res$rpm)
# #     
# #     my_grob = grobTree(textGrob(my_text, x=0.6,  y=0.1, 
# #                                 hjust=0,gp=gpar(col="blue", fontsize=15, 
# #                                                 fontface="italic")))
# #     t <- t + annotation_custom(my_grob)
#   }
  
  if(by!=""){
    # p <- p + facet_wrap( as.formula(paste("~", paste0(by.name,collapse = ' + '))), scales = "free_x")
    p <- p + facet_wrap( ~by.name, scales = "free_x")
  }
  
  if(auto.title){
    p <- p + labs(title=paste(where,"BY",paste0(by,collapse = ", ")))
  }
  
  if(!is.null(x.label)){
    p <- p + xlab(x.label)
  }
  
  if(!is.null(y.label)){
    p <- p + ylab(y.label)
  }
  
  if(print){
    print(p)
    return(d)
  }else{
    return(p)
  }
  
}
