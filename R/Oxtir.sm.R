Oxtir.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("OxtirSMmodel.txt")                          # Load model
bugsData(                                               # produce a BUGS data file and name it Oxtdata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
ht=f,
age=g,
smoke=h,
Gender=i,
CHF=j
)
, fileName=file.path(getwd(),"Oxtirdata.txt"),digits=5)
modelData("Oxtirdata.txt")                             # Load data
modelCompile(numChains=1)                              # compile
modelGenInits()                                        # gen inits
modelUpdate(4000)                                      # burn in 4000
samplesSet(c("v_F","cl_F"))                            # set monitored PK parameters
modelUpdate(10000)                                     # update 30000
}

