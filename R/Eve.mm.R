Eve.mm<-function(a,b,C,d,e,f,g,h,J,i){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("EveMMmodel.txt")                            # Load model
bugsData(                                               # produce a BUGS data file and name it Evedata
list(T=a,
X=i,
c=c(b),
ts=c(C),
tau=d,
D=e,
bw=f,
age=g,
race=h,
Ery=J
)
, fileName=file.path(getwd(),"Evedata.txt"),digits=5)
modelData("Evedata.txt")                                # Load data
modelCompile(numChains=1)                               # compile
modelGenInits()                                         # gen inits
modelUpdate(4000)                                       # burn in 4000
samplesSet(c("v_F","cl_F"))                             # set monitored PK parameters
modelUpdate(10000)                                      # update 10000
}

