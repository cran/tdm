War.ss<-function(a,b,c){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("WarSSmodel.txt")
bugsData(
list(
D=a,
tau=b,
INR=c
)
, fileName=file.path(getwd(),"Wardata.txt"),digits=5)
modelData("Wardata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(25000)
samplesSet(c("Cpmax","cl_F","kc","m","v_F"))
modelUpdate(50000)
}


