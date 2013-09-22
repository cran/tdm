Cyc.ss<-function(a,b,c,d,e,f,g){
### library(BRugs)                                          # active BRugs
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                  # set working directory
### modelCheck("CycSSmodel.txt")                            # Load model
### bugsData(                                               # produce a BUGS data file and name it Cycdata
### list(
### c=a,
### PTD=b,
### tau=c,
### ts=d,
### D=e,
### bw=f,
### Dia=g
### )
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,  
PTD=b,
tau=c,
ts=d, 
D=e,  
bw=f, 
Dia=g
)
### , fileName=file.path(getwd(),"Cycdata.txt"),digits=5)
### modelData("Cycdata.txt")                                # Load data
### modelCompile(numChains=1)                               # compile
### modelGenInits()                                         # gen inits
### modelUpdate(4000)                                       # burn in 4000
### samplesSet(c("cl"))                                     # set monitored Pk parameters
### modelUpdate(10000)                                      # update 10000
params = c("cl")                                 # The parameter(s) to be monitored.
initsList = list(cl=0.281*f*(0.629*g+1-g))
                                                            # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "CycSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
jagsModel = jags.model(model.file, data=dataList ,    
n.chains=nChains , n.adapt=adaptSteps, inits=initsList)            # inits=initsList ,
# Burn-in:
cat("Burning in the MCMC chain...\n")
update(jagsModel, n.iter=burnInSteps)
# The saved MCMC chain:
cat("Sampling final MCMC chain...\n")
codaSamples <- coda.samples(jagsModel, params, n.iter=nIter)
### codaSamples <- autojags(jagsModel, params, n.iter=nIter)              ### still not work!  figure out how. -YJ 

# resulting codaSamples object has these indices:
# codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS
# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]

checkConvergence = TRUE
if (checkConvergence) {
  ### openGraph(width=7,height=7)   ### there is a openGraphSaveGraph.R file; not to use for now.
  show(summary(codaSamples))
  ### str(codaSamples)
  ### show(gelman.diag(codaSamples))
  ### effectiveChainLength = effectiveSize(codaSamples) 
  ### show(effectiveChainLength)
  dev.new()
  plot(codaSamples) 
  ### dev.new()
  ### autocorr.plot(codaSamples)
  ### dev.new()
  ### caterplot(codaSamples)
  ### dev.new()
  ### traplot(codaSamples)
  dev.new()
  gelman.plot(codaSamples)             ### why is this line not working?  -YJ
  # dev.new()
  # denplot(codaSamples)
  # dev.new()
  # traceplot(codaSamples)
  ### mcmcplot(codaSamples)              ### uhh... it outputs the plots as .html format. -YJ
}
###
### show prediction/calc Cp obtained from JAGS here
###
cat("\n")
cl.mat <- as.matrix(codaSamples[[1]])
posterior.estimates <- rbind(cl.mat)
vars <- t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
### cat("\n Estimated PK Parameters:\n")
### cat("---------------------------\n")
### show(params.mean)
### cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl_F.mean <- as.matrix(params.mean[[1]])               ### for testing purpose here! -YJ
### vd_F.mean <- as.matrix(params.mean[[2]])
### ka.mean   <- as.matrix(params.mean[[3]])
### half_life.mean <-log(2)/(cl_F.mean/vd_F.mean)          ### secondary PK param. -YJ
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now.  -YJ
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
vd <- 4.*g    ## assumed vd = 4.0 L/kg of bw here and in the model. --YJ
half_life<-log(2)/(cl/vd)
Params <- data.frame(Estimated_Parameters=c("cl (L/hr)","Vd (L)", "Half-life (hr)"),value=c(cl,vd,half_life))
cat("\n\n");show(Params);cat("\n\n")
}