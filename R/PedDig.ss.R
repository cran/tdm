PedDig.ss<-function(a,b,c,d,e){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("PedDigSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
D=c,
bw=d,
age=e
)
, fileName=file.path(getwd(),"PedDigdata.txt"),digits=5)
modelData("PedDigdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


