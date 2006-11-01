Ima.ms<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ImaaftMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
ts=c(c),
tau=c(d),
n=c(e),
D=c(f),
OCC=c(g),
BW=c(h),
Hb=c(i),
WBC=c(j)
)
, fileName=file.path(getwd(),"Imadata.txt"),digits=5)
modelData("Imadata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}


