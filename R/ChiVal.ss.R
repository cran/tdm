ChiVal.ss<-function(a,b,c,d,e,f){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ChiValSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
TBW=e,
CBZ=f
)
, fileName=file.path(getwd(),"ChiValdata.txt"),digits=5)
modelData("ChiValdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



