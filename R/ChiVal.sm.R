ChiVal.sm<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ChiValSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
TBW=f,
CBZ=g
)
, fileName=file.path(getwd(),"ChiValdata.txt"),digits=5)
modelData("ChiValdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



