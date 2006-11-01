Amidihinfusion.ss<-function(a,b,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmidihinfusionSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
D=e,
ht=f,
age=g,
smoke=h,
Gender=i,
CHF=j
)
, fileName=file.path(getwd(),"Amidihinfusiondata.txt"),digits=5)
modelData("Amidihinfusiondata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}

