Ind.mm<-function(J,A,B,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("IndMMmodel.txt")
bugsData(
list(T=J,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
bw=c(f),
Rit=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("ka","cl_F"))
modelUpdate(10000)
}



