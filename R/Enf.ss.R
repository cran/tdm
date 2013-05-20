Enf.ss<-function(a,b,c,d,e,f){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("EnfSSmodel.txt")                              # model check
bugsData(                                                 # produce a BUGS data file and name it Enfdata
list(
c=a,
tau=b,
ts=c,
D=d,
bw=e,
Gender=f
)
, fileName=file.path(getwd(),"Enfdata.txt"),digits=5)
modelData("Enfdata.txt")                                  # Load data
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen intis
modelUpdate(4000)                                         # burn in 4000
samplesSet(c("v_F","cl_F","ka"))                          # set monitored PK parameters
modelUpdate(10000)                                        # update 10000
}


