Amianhinfusion.ms<-function(a,b,c,f,g,h,i,j,k){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmianhinfusionMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
D=c(f),
ht=c(g),
age=c(h),
smoke=c(i),
Gender=c(j),
CHF=c(k)
)
, fileName=file.path(getwd(),"Amianhinfusiondata.txt"),digits=5)
modelData("Amianhinfusiondata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}

