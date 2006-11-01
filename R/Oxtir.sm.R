Oxtir.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("OxtirSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
ht=f,
age=g,
smoke=h,
Gender=i,
CHF=j
)
, fileName=file.path(getwd(),"Oxtirdata.txt"),digits=5)
modelData("Oxtirdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

