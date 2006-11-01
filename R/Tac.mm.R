Tac.mm<-function(J,A,B,d,e,f,g,h,l,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("TacMMmodel.txt")
bugsData(
list(T=J,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
Hem=c(f),
Alb=c(g),
Dil=c(h),
Flu=c(l)
)
, fileName=file.path(getwd(),"Tacdata.txt"),digits=5)
modelData("Tacdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F"))
modelUpdate(10000)
}


