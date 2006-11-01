Phe.ss<-function(a,b,c,d){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("PheSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
D=c,
bw=d
)
, fileName=file.path(getwd(),"Phedata.txt"),digits=5)
modelData("Phedata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("Vmax","Km"))
modelUpdate(50000)
}


