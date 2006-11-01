Val.mm<-function(k,A,B,d,e,f,g,h,J,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ValMMmodel.txt")
bugsData(
list(T=k,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
age=c(f),
CBZ=c(g),
INDI=c(h),
ka=c(J)
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



