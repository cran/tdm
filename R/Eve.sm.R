Eve.sm<-function(a,b,C,d,e,f,g,h,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EveSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(C),
tau=d,
D=e,
bw=f,
age=g,
race=h,
Ery=j
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(50000)
}

