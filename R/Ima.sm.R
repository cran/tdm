Ima.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ImaaftSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
tau=d,
n=e,
D=f,
OCC=g,
BW=h,
Hb=i,
WBC=j
)
, fileName=file.path(getwd(),"Imadata.txt"),digits=5)
modelData("Imadata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}


