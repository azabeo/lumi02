test<-function(p='NC7000',c=6,rp=600,var='time'){
  library(reshape2)
  r<-res$final_particle_C0_rcf_rpm_time.step_repetition
  #p<-r[particle=='P25' & C0==5 & rpm>300,.(time.step,repetition,time)][order(time.step,repetition)]
  
  p <- r[particle==p & C0==c & rpm==rp,list(time.step,repetition,var=get(var))][order(time.step,repetition)]
  
  p1 <- melt(p,1:2)
  
  ans <- dcast.data.table(p1, time.step~repetition, fun=mean)
  
  setnames(ans,c('time.step',paste0(var,'.r1'),paste0(var,'.r2'),paste0(var,'.r3')))
  return(ans)
  
  #write.table(ans, 'esempio1.csv', sep="\t", row.names = FALSE)  
}
