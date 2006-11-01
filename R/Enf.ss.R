Enf.ss<-function(a,b,c,d,e,f){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EnfSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
Gender=f
)
, fileName=file.path(getwd(),"Enfdata.txt"),digits=5)
modelData("Enfdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F","ka"))
modelUpdate(10000)
}


