Rit.ms<-function(a,b,c,d,e,f){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("RitMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
ts=c(c),
tau=c(d),
D=c(e),
LPV=c(f)
)
, fileName=file.path(getwd(),"Ritdata.txt"),digits=5)
modelData("Ritdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F","ka"))
modelUpdate(10000)
}


