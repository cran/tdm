Theinfusion.ms<-function(a,b,c,f,g,h,i,j,k){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("TheinfusionMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
D=c(f),
ht=c(g),
age=c(h),
smoke=c(i),
Gender=c(j),
CHF=c(k),
)
, fileName=file.path(getwd(),"Theinfusiondata.txt"),digits=5)
modelData("Theinfusiondata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}

