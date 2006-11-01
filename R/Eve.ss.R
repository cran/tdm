Eve.ss<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EveSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
age=f,
race=g,
Ery=h
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(50000)
}

