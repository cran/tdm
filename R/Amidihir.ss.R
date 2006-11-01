Amidihir.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmidihirSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
ht=e,
age=f,
smoke=g,
Gender=h,
CHF=i
)
, fileName=file.path(getwd(),"Amidihirdata.txt"),digits=5)
modelData("Amidihirdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

