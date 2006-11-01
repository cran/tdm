Eno.ms<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EnoMSmodel.txt")
bugsData(
list(N=a,
Amax=c(b),
tau=c(c),
D=c(d),
TBW=c(e),
Gender=c(f),
Scr=c(g)
)
, fileName=file.path(getwd(),"Enodata.txt"),digits=5)
modelData("Enodata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

