ChiVal.ms<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ChiValMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
TBW=c(f),
CBZ=c(g)
)
, fileName=file.path(getwd(),"ChiValdata.txt"),digits=5)
modelData("ChiValdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



