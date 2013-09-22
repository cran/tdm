Ima.sm<-function(a,b,c,d,e,f,g,h,i,j){
### library(BRugs)                                          # active BRugs
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                  # set working directory
### modelCheck("ImaaftSMmodel.txt")                         # Load model
### bugsData(                                               # produce a BUGS data file and name it Imsdata
### list(T=a,
### c=c(b),
### ts=c(c),
### tau=d,
### n=c(e),
### D=f,
### OCC=g,
### BW=h,
### Hb=i,
### WBC=j
### )
dataList= list(                                             # produce a JAGS data file and name it Phedata
T=a,
c=c(b),
ts=c(c),
tau=d,
n=c(e),
D=f,
OCC=g,
BW=h,
Hb=i,
WBC=j
)
### , fileName=file.path(getwd(),"Imadata.txt"),digits=5)
### modelData("Imadata.txt")                               # Load data
### modelCompile(numChains=1)                              # compile
### modelGenInits()                                        # gen inits
### modelUpdate(4000)                                      # burn in 4000
### samplesSet(c("v_F","cl_F"))                            # set monitored PK parameters
### modelUpdate(10000)                                     # update 10000
params = c("cl_F","v_F")                                 # The parameter(s) to be monitored.
initsList = list(cl_F=(13.8-3.81*g)*((h/80)^0.301)*((i/13)^0.897)*((j/16)^(-0.105)),
                  v_F=(252-7.82*g)*((h/80)^0.405)*((i/13)^0.676)*((j/16)^0.07))
                                                            # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "ImaaftSMmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)               # inits=initsList
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### codaSamples <- autojags(jagsModel, params, n.iter=nIter)          ### still not work!  figure out how. -YJ 

# resulting codaSamples object has these indices:
# codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS
# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]

show(summary(codaSamples))
dev.new()
plot(codaSamples) 
### dev.new()
### autocorr.plot(codaSamples)
dev.new()
gelman.plot(codaSamples)
### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ

###
### show prediction/calc Cp obtained from JAGS here
###
cat("\n")
cl_F.mat <- as.matrix(codaSamples[[1]])
v_F.mat  <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl_F.mat,v_F.mat)
vars <- t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
### cat("\n Estimated PK Parameters:\n")
### cat("---------------------------\n")
### show(params.mean)
### cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])               ### for testing purpose here! -YJ
### vd_F.mean <- as.matrix(params.mean[[2]])
### half_life.mean <-log(2)/(cl_F.mean/vd_F.mean)          ### secondary PK param.  -YJ
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now. -YJ
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2] 
half_life <- log(2)/(cl_F/v_F)
X <- data.frame(Estimated_Parameters=c("cl_F (L/hr)","vd_F (L)","Half-life (hr)"),value=c(X[1,2],X[2,2],half_life))
cat("\n\n");show(X);cat("\n\n")
}