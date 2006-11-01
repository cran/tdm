PedDig.ms<-function(a,b,c,d,e,f){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("PedDigMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
D=c(d),
bw=c(e),
age=c(f)
)
, fileName=file.path(getwd(),"PedDigdata.txt"),digits=5)
modelData("PedDigdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


