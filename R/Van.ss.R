### Van.ss<-function(a,b,c,d,e,f,g,h,i,x,y){                      # why there was "x,y"?  I don't know. -YJ
Van.ss<-function(a,b,c,d,e,f,g,h,i){
### library(BRugs)                                                # active BRugs
library(R2jags)
### oldwd<-getwd()
### setwd(system.file("PK",package="tdm"))                        # set working directory
### modelCheck("VanSSmodel.txt")                                  # load model
### bugsData(                                                     # produce a BUGS data file and name it Vandata
### list(
### c=a,
### tau=b,
### ts=c,
### tin=d,
### D=e,
### bw=f,
### Scr=g,
### Gender=h,
### age=i
### )
dataList= list(                                             # produce a JAGS data file and name it Phedata
c=a,     
tau=b,   
ts=c,    
tin=d,   
D=e,     
bw=f,    
Scr=g,   
Gender=h,
age=i    
)
### , fileName=file.path(getwd(),"Vandata.txt"),digits=5)
### modelData("Vandata.txt")                                      # Load data
### modelCompile(numChains=1)                                     # compile
### modelGenInits()                                               # gen inits
### modelUpdate(4000)                                             # burn in 4000
### samplesSet(c("cl","v"))                                       # set monitored PK parameters
### modelUpdate(10000)                                            # update 10000
params = c("cl" , "v")                                      # The parameter(s) to be monitored.
initsList = list(cl=1.08*(140-i)*f/(72*g)*(0.85^(1-h))*60/1000,
                 v=0.98*f)                                  # initialize prior here; not working if use 'bw' instead of 'd' here.  --YJ
adaptSteps = 500                                            # Number of steps to "tune" the samplers.
burnInSteps = 6000                                          # Number of steps to "burn-in" the samplers.
nChains = 3                                                 # Number of chains to run.
numSavedSteps=100000                                        # Total number of steps in chains to save.
thinSteps=1                                                 # Number of steps to "thin" (1=keep every step).
nIter = ceiling(( numSavedSteps * thinSteps ) / nChains )   # Steps per chain.
# Create, initialize, and adapt the model:
model.file <- system.file("PK", "VanSSmodel.txt", package="tdm")   ### give the file location to find model file  -YJ
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
### show prediction/calc Cp here
###
cat("\n")
cl.mat <- as.matrix(codaSamples[[1]])
v.mat  <- as.matrix(codaSamples[[2]])
posterior.estimates <- rbind(cl.mat, v.mat)
vars <-t(as.matrix(posterior.estimates))
params.mean <- apply(vars, 1, mean)
cat("\n Estimated PK Parameters:\n")
cat("---------------------------\n")
show(params.mean)
cat("\n\n")
write.table(params.mean,file="params.csv",col.names=FALSE)
### cl.mean <- as.matrix(params.mean[[1]])                 ### for testing purpose here! -YJ
### v.mean  <- as.matrix(params.mean[[2]])
### show(Km.mean);show(Vmax.mean)                          ### Yes, it works now. --YJ
}



