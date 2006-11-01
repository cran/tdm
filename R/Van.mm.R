Van.mm<-function(k,A,B,d,e,f,g,h,l,J,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("VanMMmodel.txt")
bugsData(
list(T=k,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
tin=c(e),
D=c(f),
bw=c(g),
Scr=c(h),
Gender=c(l),
age=c(J)
)
, fileName=file.path(getwd(),"Vandata.txt"),digits=5)
modelData("Vandata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F","v_F"))
modelUpdate(10000)
}



