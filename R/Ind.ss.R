Ind.ss<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("IndSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
Rit=f,
Gender=g
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("ka","cl_F"))
modelUpdate(10000)
}



