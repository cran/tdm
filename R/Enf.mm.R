Enf.mm<-function(h,A,B,d,e,f,g,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("EnfMMmodel.txt")
bugsData(
list(T=h,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
bw=c(f),
Gender=c(g)
)
, fileName=file.path(getwd(),"Enfdata.txt"),digits=5)
modelData("Enfdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F","ka"))
modelUpdate(10000)
}


