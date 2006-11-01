Phe.ms<-function(a,b,c,d,e){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("PheMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
D=c(d),
bw=c(e)
)
, fileName=file.path(getwd(),"Phedata.txt"),digits=5)
modelData("Phedata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("Vmax","Km"))
modelUpdate(50000)
}


