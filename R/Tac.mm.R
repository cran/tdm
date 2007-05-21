Tac.mm<-function(J,A,B,d,e,f,g,h,l,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("TacMMmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Tacdata
list(T=J,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
Hem=c(f),
Alb=c(g),
Dil=c(h),
Flu=c(l)
)
, fileName=file.path(getwd(),"Tacdata.txt"),digits=5)
modelData("Tacdata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


