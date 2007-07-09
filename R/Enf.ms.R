Enf.ms<-function(a,b,c,d,e,f,g){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("EnfMSmodel.txt")                              # model check
bugsData(                                                 # produce a BUGS data file and name it Enfdata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
bw=c(f),
Gender=c(g)
)
, fileName=file.path(getwd(),"Enfdata.txt"),digits=5)
modelData("Enfdata.txt")                                  # Load data
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen inits
modelUpdate(4000)                                         # burn in 4000
samplesSet(c("v_F","cl_F","ka"))                          # set monitored PK parameters
modelUpdate(10000)                                        # update 10000
}


