Eno.ss<-function(a,b,c,d,e,f){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EnoSSmodel.txt")
bugsData(
list(
Amax=a,
tau=b,
D=c,
TBW=d,
Gender=e,
Scr=f
)
, fileName=file.path(getwd(),"Enodata.txt"),digits=5)
modelData("Enodata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

