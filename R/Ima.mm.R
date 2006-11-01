Ima.mm<-function(J,A,B,l,d,e,f,g,h,k,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ImaaftMMmodel.txt")
bugsData(
list(T=J,
X=i,
c=c(A),
ts=c(B),
tau=c(l),
n=c(d),
D=c(e),
OCC=c(f),
BW=c(g),
Hb=c(h),
WBC=c(k)
)
, fileName=file.path(getwd(),"Imadata.txt"),digits=5)
modelData("Imadata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}


