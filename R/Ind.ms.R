Ind.ms<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("IndMSmodel.txt")                              # model check
bugsData(                                                 # produce a BUGS data file and name it Inddata
list(N=a, 
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
bw=c(f),
Rit=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")                                  # Load data
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen init
modelUpdate(10000)                                        # burn in 25000
samplesSet(c("ka","cl_F"))                                # set monitored PK parameters 
modelUpdate(60000)                                        # update 50000
}



