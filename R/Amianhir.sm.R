Amianhir.sm<-function(a,b,c,d,e,f,g,h,i,j){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("AmianhirSMmodel.txt")                     # Load model
bugsData(                                             # porduce a BUGS data file and name it Amianhirdata
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
, fileName=file.path(getwd(),"Amianhirdata.txt"),digits=5)
modelData("Amianhirdata.txt")                         # Load data
modelCompile(numChains=1)                             # compile
modelGenInits()                                       # gen intis
modelUpdate(4000)                                     # burn in 4000
samplesSet(c("v_F","cl_F"))                           # set monitored PK parameters
modelUpdate(10000)                                    # update 30000
}

