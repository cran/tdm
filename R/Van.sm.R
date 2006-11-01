Van.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("VanSMmodel.txt")
bugsData(
list(T=a,
c=c(b),
ts=c(c),
tau=d,
tin=e,
D=f,
bw=g,
Scr=h,
Gender=i,
age=j
)
, fileName=file.path(getwd(),"Vandata.txt"),digits=5)
modelData("Vandata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("cl_F","v_F"))
modelUpdate(10000)
}



