Ind.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("IndMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
bw=c(f),
Rit=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("ka","cl_F"))
modelUpdate(10000)
}



