Rit.mm<-function(g,A,B,d,e,f,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("RitMMmodel.txt")
bugsData(
list(T=g,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
LPV=c(f)
)
, fileName=file.path(getwd(),"Ritdata.txt"),digits=5)
modelData("Ritdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F","ka"))
modelUpdate(10000)
}


