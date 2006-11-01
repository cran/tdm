Amianhir.ms<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmianhirMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
ht=c(f),
age=c(g),
smoke=c(h),
Gender=c(i),
CHF=c(j)
)
, fileName=file.path(getwd(),"Amianhirdata.txt"),digits=5)
modelData("Amianhirdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

