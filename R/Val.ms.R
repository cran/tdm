Val.ms<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("ValMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
age=c(f),
CBZ=c(g),
INDI=c(h),
ka=c(i)
)
, fileName=file.path(getwd(),"Valdata.txt"),digits=5)
modelData("Valdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}



