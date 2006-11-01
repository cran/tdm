Eve.mm<-function(a,b,C,d,e,f,g,h,J,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EveMMmodel.txt")
bugsData(
list(T=a,
X=i,
c=c(b),
ts=c(C),
tau=d,
D=e,
bw=f,
age=g,
race=h,
Ery=J
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(50000)
}

