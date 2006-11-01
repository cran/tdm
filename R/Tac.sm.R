Tac.sm<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("TacSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
Hem=f,
Alb=g,
Dil=h,
Flu=i
)
, fileName=file.path(getwd(),"Tacdata.txt"),digits=5)
modelData("Tacdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


