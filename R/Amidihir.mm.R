Amidihir.mm<-function(a,b,C,d,e,f,g,h,l,J,i){
library(BRugs)
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))
modelCheck("AmidihirMMmodel.txt")
bugsData(
list(T=a,
X=i,
c=c(b),
ts=c(C),
tau=d,
D=e,
ht=f,
age=g,
smoke=h,
Gender=l,
CHF=J
)
, fileName=file.path(getwd(),"Amidihirdata.txt"),digits=5)
modelData("Amidihirdata.txt")
modelCompile(numChains=1)
modelGenInits()
modelUpdate(4000)
samplesSet(c("v_F","cl_F"))
modelUpdate(10000)
}

