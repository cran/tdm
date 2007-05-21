Ind.mm<-function(J,A,B,d,e,f,g,h,i){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("IndMMmodel.txt")                              # model check
bugsData(                                                 # produce a BUGS data file and name it Inddata
list(T=J,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
bw=c(f),
Rit=c(g),
Gender=c(h)
)
, fileName=file.path(getwd(),"Inddata.txt"),digits=5)
modelData("Inddata.txt")                                  # Load data
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen inits
modelUpdate(25000)                                        # burn in 25000
samplesSet(c("ka","cl_F"))                                # set monitored PK parameters
modelUpdate(50000)                                        # update 50000
}



