Cyc.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CycMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
PTD=c(c),
tau=c(d),
ts=c(e),
D=c(f),
bw=c(g),
Dia=c(h)
)
, fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
modelData("Cycdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



