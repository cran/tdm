Dig.ss<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("DigSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
D=c,
scr=d,
bw=e,
age=f,
Gender=g
)
, fileName=file.path(getwd(),"Digdata.txt"),digits=5)
modelData("Digdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


