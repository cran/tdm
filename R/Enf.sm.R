Enf.sm<-function(a,b,c,d,e,f,g){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EnfSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
bw=f,
Gender=g
)
, fileName=file.path(getwd(),"Enfdata.txt"),digits=5)
modelData("Enfdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F","ka"))
modelUpdate(10000)
}


