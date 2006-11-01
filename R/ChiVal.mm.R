ChiVal.mm<-function(k,A,B,d,e,f,g,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ChiValMMmodel.txt")
bugsData(
list(T=k,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
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



