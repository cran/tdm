Amianhcr.ms<-function(a,b,c,e,f,g,h,i,j){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("AmianhcrMSmodel.txt")                     # Load model
bugsData(                                             # porduce a BUGS data file and name it Amianhcrdata
list(N=a,
c=c(b),
tau=c(c),
D=c(e),
ht=c(f),
age=c(g),
smoke=c(h),
Gender=c(i),
CHF=c(j)
)
, fileName=file.path(getwd(),"Amianhcrdata.txt"),digits=5)
modelData("Amianhcrdata.txt")                         # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("cl_F"))                                 # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}

