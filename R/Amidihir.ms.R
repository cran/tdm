Amidihir.ms<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("AmidihirMSmodel.txt")                     # Load model
bugsData(                                             # porduce a BUGS data file and name it Amidihirdata
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
, fileName=file.path(getwd(),"Amidihirdata.txt"),digits=5)
modelData("Amidihirdata.txt")                         # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
modelUpdate(30000)                                    # update 30000
}
