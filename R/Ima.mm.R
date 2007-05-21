Ima.mm<-function(J,A,B,l,d,e,f,g,h,k,i){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("ImaaftMMmodel.txt")                       # Load model
bugsData(                                             # porduce a BUGS data file and name it Imadata
list(T=J,
X=i,
c=c(A),
ts=c(B),
tau=l,
n=c(d),
D=e,
OCC=f,
BW=g,
Hb=h,
WBC=k
)
, fileName=file.path(getwd(),"Imadata.txt"),digits=5)
modelData("Imadata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


