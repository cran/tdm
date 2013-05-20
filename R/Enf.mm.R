Enf.mm<-function(h,A,B,d,e,f,g,i){
library(BRugs)                                            # active BRugs package
oldwd<-getwd()
setwd(system.file("PK",package="tdm"))                    # set working directory
modelCheck("EnfMMmodel.txt")                              # model check
bugsData(                                                 # produce a BUGS data file and name it Enfdata
list(T=h,
X=i,
c=c(A),
ts=c(B),
tau=c(d),
D=c(e),
bw=c(f),
Gender=c(g)
)
, fileName=file.path(getwd(),"Enfdata.txt"),digits=5)
modelData("Enfdata.txt")                                  # Load date
modelCompile(numChains=1)                                 # compile
modelGenInits()                                           # Gen ints
modelUpdate(4000)                                         # burn in 4000
samplesSet(c("v_F","cl_F","ka"))                          # set moniotred PK parmaters
modelUpdate(10000)                                        # update 10000
}


