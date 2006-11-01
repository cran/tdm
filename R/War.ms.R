War.ms<-function(a,b,c,d){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("WarMSmodel.txt")
bugsData(
list(N=a,
D=c(b),
tau=c(c),
INR=c(d)
)
, fileName=file.path(getwd(),"Wardata.txt"),digits=5)
modelData("Wardata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(25000)
samplesSet(c("Cpmax","cl_F","kc","m","v_F"))
modelUpdate(50000)
}


