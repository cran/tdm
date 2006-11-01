Ima.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ImaaftSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
n=d,
D=e,
OCC=f,
BW=g,
Hb=h,
WBC=i
)
, fileName=file.path(getwd(),"Imadata.txt"),digits=5)
modelData("Imadata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}


