Val.ss<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ValSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
age=e,
CBZ=f,
INDI=g,
ka=h
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



