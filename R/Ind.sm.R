Ind.sm<-function(a,b,c,d,e,f,g,h){
library(BRugs)                                                # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                        # set working directory
modelCheck("IndSMmodel.txt")                                  # load model
bugsData(                                                     # produce a BUGS data file and name it Inddata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=c(e),
bw=c(f),
Rit=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")                                      # Load data
modelCompile(numChains=1)                                     # compile
modelGenInits()                                               # gen inits
modelUpdate(25000)                                            # burn in 25000
samplesSet(c("ka","cl_F"))                                    # set monitored PK parameters
modelUpdate(50000)                                            # update 50000
}



