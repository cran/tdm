Car.mm<-function(J,A,B,C,d,e,F,G,H,K,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("CBZMMmodel.txt")
bugsData(
list(T=J,
c=c(A),
ts=c(B),
tau=C,
D=d,
TBW=e,
PB=F,
VPA=G,
PHT=H,
E=K,
X=i
)
, fileName=file.path(getwd(),"CBZdata.txt"),digits=5)
modelData("CBZdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(50000)
}


