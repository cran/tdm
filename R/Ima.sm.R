Ima.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("ImaaftSMmodel.txt")                         # Load model
bugsData(                                               # produce a BUGS data file and name it Imsdata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
n=c(e),
D=f,
OCC=g,
BW=h,
Hb=i,
WBC=j
)
, fileName=file.path(getwd(),"Imadata.txt"),digits=5)
modelData("Imadata.txt")                               # Load data
modelCompile(numChains=1)                              # compile
modelGenInits()                                        # gen inits
modelUpdate(4000)                                      # burn in 4000
samplesSet(c("v_F","cl_F"))                            # set monitored PK parameters
modelUpdate(10000)                                     # update 10000
}


