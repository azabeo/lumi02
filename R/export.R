export<-function(res,dir='export/'){
  names=names(res)
  i=1
  for (dat in res){
    name = paste(dir,names[i],'.txt',sep="")
    logd(name,dat[,!'fit',with=FALSE])
    #write.xlsx(dat[,!'fit',with=FALSE],name, row.names = FALSE)
    write.table(dat[,!'fit',with=FALSE], name, sep="\t", row.names = FALSE)
    i=i+1
  }
}

print.export <- function(file.name,p,inch=3,ppi=300){
  logd('Saving: ',file.name)
  png(file.name, width=inch*ppi, height=inch*ppi, res=ppi)
  print(p)
  dev.off()
}

export.images <- function(res,fittings,inch=3,ppi=300,dir='export',which=1){

  parts <- unique(res[[1]][,particle])
  
  for(part in parts){
    if(which==1){
      Cs = unique(res[[1]][particle==part,C0])
      ylab=paste0(part," concentration (mg/l)")
      for(c in Cs){
        where = paste0("particle=='",part,"' & C0==",c)
        #p <- plotit(res,fittings,where,"rpm","time.avg","m.conc",print=FALSE)
        p <- plotit(res,fittings,where,"rpm","time.avg","m.conc",'RPM ','','centrifugation time (s)',ylab,print=FALSE)
        file.name = paste0(dir,part,'_',formatC(c, width=3, flag="0"),'.png')
        
        print.export(file.name,p,inch,ppi)
      }
    }
    
    if(which==2){
      Cs = unique(res[[1]][particle==part,rpm])
      ylab=paste0(part," concentration (mg/l)")
      for(c in Cs){
        where = paste0("particle=='",part,"' & rpm==",c)
        #p <- plotit(res,fittings,where,"rpm","time.avg","m.conc",print=FALSE)
        p <- plotit(res,fittings,where,"C0","time.avg","m.conc",'initial conc ','','centrifugation time (s)',ylab,print=FALSE)
        file.name = paste0(dir,part,'_',formatC(c, width=4, flag="0"),'.png')
        
        print.export(file.name,p,inch,ppi)
      }
    }
    
    
    where = paste0("particle=='",part,"'")
    #p <- plotit(res,fittings,where,"C0","rcf","k",print = FALSE)
    p <- plotit(res,fittings,where,"C0","rcf","k",'ENM initial concentration: ',' mg/l','RCF','k (s-1)',print = FALSE)
    file.name = paste0(dir,part,'_k.png')
    
    print.export(file.name,p,inch,ppi)
    
    p <- plotit(res,fittings,where,"","C0","k1g", size=4, print = FALSE)
    file.name = paste0(dir,part,'_k1g.png')
    
    print.export(file.name,p,inch,ppi)
  }

}