War.ms<-function(a,b,c,d){
library(BRugs)                                        # active BRugs
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                # set working directory
modelCheck("WarMSmodel.txt")                          # Load model
bugsData(                                             # porduce a BUGS data file and name it Wardata
list(N=a,
D=c(b),
tau=c(c),
INR=c(d)
)
, fileName=file.path(getwd(),"Wardata.txt"),digits=5)
modelData("Wardata.txt")                              # Load data
modelCompile(numChains=1)                             # compile
bugsData(
list(
m=c(rep(0.422, a)),
Cpmax=c(rep(5.49, a)),
kc=c(rep(0.98, a)),
cl_F=c(rep(2.19, a)),
v_F=c(rep(7.5, a))
)
, fileName=file.path(getwd(),"WarInits.txt"),digits=5)
modelInits("WarInits.txt")
modelUpdate(4000)                                    # burn in 4000
samplesSet(c("Cpmax","cl_F","kc","m","v_F"))          # set monitored PK parameters
modelUpdate(10000)                                    # update 10000
}


