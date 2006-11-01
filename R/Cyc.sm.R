Cyc.sm<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CycSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
PTD=d,
tau=e,
D=f,
bw=g,
Dia=h
)
, fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
modelData("Cycdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



