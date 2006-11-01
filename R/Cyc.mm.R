Cyc.mm<-function(J,A,B,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CycMMmodel.txt")
bugsData(
list(T=J,
X=i,
c=c(A),
ts=c(B),
PTD=c(d),
tau=c(e),
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



