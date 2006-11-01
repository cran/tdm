Amianhcr.ms<-function(a,b,c,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmianhcrMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
D=c(e),
ht=c(f),
age=c(g),
smoke=c(h),
Gender=c(i),
CHF=c(j)
)
, fileName=file.path(getwd(),"Amianhcrdata.txt"),digits=5)
modelData("Amianhcrdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}

