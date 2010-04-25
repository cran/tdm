Tac.ms<-function(a,b,c,d,e,f,g,h,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("TacMSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Tacdata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
Hem=c(f),
Alb=c(g),
Dil=c(h),
Flu=c(i)
)
, fileName=file.path(getwd(),"Tacdata.txt"),digits=5)
modelData("Tacdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


