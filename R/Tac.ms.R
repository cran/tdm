Tac.ms<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("TacMSmodel.txt")
bugsData(
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
Hem=c(f),
Alb=c(g),
Dil=c(h),
Flu=c(i)
)
, fileName=file.path(getwd(),"Tacdata.txt"),digits=5)
modelData("Tacdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


