Car.ms<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CBZMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
TBW=c(f),
PB=c(g),
VPA=c(h),
PHT=c(i),
E=c(j)
)
, fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
modelData("CBZdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(50000)
}


