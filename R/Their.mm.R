Their.mm<-function(a,b,C,d,e,f,g,h,l,J,i){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("TheirMMmodel.txt")                            # model check
bugsData(                                                 # produce a BUGS data file and name it Theirdata
list(T=a,
X=i,
c=c(b),
ts=c(C),
tau=d,
D=e,
ht=f,
age=g,
smoke=h,
Gender=l,
CHF=J
)
, fileName=file.path(getwd(),"Theirdata.txt"),digits=5)
modelData("Theirdata.txt")                                # Load date
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # gen intis
modelUpdate(4000)                                         # burn in 4000
samplesSet(c("v_F","cl_F"))                               # set monitord PK parameters
modelUpdate(30000)                                        # update 30000
}

