Rit.ss<-function(a,b,c,d,e){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("RitSSmodel.txt")
bugsData(
list(
c=a,
ts=b,
tau=c,
D=d,
LPV=e
)
, fileName=file.path(getwd(),"Ritdata.txt"),digits=5)
modelData("Ritdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F","ka"))
modelUpdate(10000)
}


