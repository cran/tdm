Amianhir.ms<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                          # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                  # set working directory
modelCheck("AmianhirMSmodel.txt")                       # Load model
bugsData(                                               # produce a BUGS data file and name it Amianhirdata
list(N=a,
c=c(b),
tau=c(c),
ts=c(d),
D=c(e),
ht=c(f),
age=c(g),
smoke=c(h),
Gender=c(i),
CHF=c(j)
)
, fileName=file.path(getwd(),"Amianhirdata.txt"),digits=5)
modelData("Amianhirdata.txt")                                # Load data
modelCompile(numChains=1)                                    # compile
modelGenInits()                                              # gen inits
modelUpdate(4000)                                            # burn in 4000
samplesSet(c("v_F","cl_F"))                                  # set monitored PK parameters
modelUpdate(10000)                                           # update 30000
}

