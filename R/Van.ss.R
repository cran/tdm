Van.ss<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("VanSSmodel.txt")
bugsData(
list(
c=a,
tau=b,
ts=c,
tin=d,
D=e,
bw=f,
Scr=g,
Gender=h,
age=i
)
, fileName=file.path(getwd(),"Vandata.txt"),digits=5)
modelData("Vandata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F","v_F"))
modelUpdate(10000)
}



