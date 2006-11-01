Car.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CBZSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
TBW=e,
PB=f,
VPA=g,
PHT=h,
E=i
)
, fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
modelData("CBZdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(50000)
}


