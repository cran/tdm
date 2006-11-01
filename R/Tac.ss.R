Tac.ss<-function(a,b,c,d,e,f,g,h){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("TacSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
D=d,
Hem=e,
Alb=f,
Dil=g,
Flu=h
)
, fileName=file.path(getwd(),"Tacdata.txt"),digits=5)
modelData("Tacdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


