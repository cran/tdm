Dig.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("DigMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
D=c(d),
scr=c(e),
bw=c(f),
age=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Digdata.txt"),digits=5)
modelData("Digdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


