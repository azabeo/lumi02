prepareData<-function(){
datNC7000c6 <- datNC7000[datNC7000[,'conc']==6,c(-1)]
expr = "consts[0]*exp(-(p1)*time)"
par.start = list(p1=1)
mg = 6
consts = mg
}