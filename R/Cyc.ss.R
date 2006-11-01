Cyc.ss<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CycSSmodel.txt")
bugsData(
list(
c=a,
PTD=b,
tau=c,
ts=d,
D=e,
bw=f,
Dia=g
)
, fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
modelData("Cycdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



