Eno.ms<-function(a,b,c,d,e,f,g){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("EnoMSmodel.txt")                              # model check
bugsData(                                                 # produce a BUGS data file and name it Enodata
list(N=a,
Amax=c(b),
tau=c(c),
D=c(d),
TBW=c(e),
Gender=c(f),
Scr=c(g)
)
, fileName=file.path(getwd(),"Enodata.txt"),digits=5)
modelData("Enodata.txt")                                  # Load data file
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # Gen inits
modelUpdate(4000)                                         # burn in 4000
samplesSet(c("v_F","cl_F"))                               # set monitored PK parameters
modelUpdate(10000)                                        # updtae 10000
}

