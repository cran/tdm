Ami.ss<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmiSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
tin=d,
D=e,
bw=f,
Ht=g,
Scr=h,
Gender=i,
age=j
)
, fileName=file.path(getwd(),"Amidata.txt"),digits=5)
modelData("Amidata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

