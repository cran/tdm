Their.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("TheirSMmodel.txt")                            # model check
bugsData(                                                 # produce a BUGS data file and name it Theirdata
list(T=a,
c=c(b),
ts=c(c),
tau=d,
D=e,
ht=f,
age=g,
smoke=h,
Gender=i,
CHF=j
)
, fileName=file.path(getwd(),"Theirdata.txt"),digits=5)
modelData("Theirdata.txt")                                 # Load data
modelCompile(numChains=1)                                  # compile
modelGenInits()                                            # gen inits
modelUpdate(4000)                                          # burin in 4000
samplesSet(c("v_F","cl_F"))                                # set monitored PK parameters
modelUpdate(30000)                                         # update 30000
}

